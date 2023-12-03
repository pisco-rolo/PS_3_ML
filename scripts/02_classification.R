# Preparación -------------------------------------------------------------
# Preparamos el equipo para procesamiento en paralelo.
cl <- makePSOCKcluster(3)

# 1| Predicción -----------------------------------------------------------
# Nota. En la base de datos original tenemos una distribución de 80% de hogares
# no-pobres y 20% de hogares pobres. Al hacer la transformación, buscamos que
# la distribución sea 66% y 33%.

# Realizamos las modificaciones finales necesarias sobre las bases de datos.
data_hog <- data_hog |> 
  mutate(bin_pobre = factor(bin_pobre),
         cat_zona = factor(cat_zona)) |> 
  mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                  TRUE ~ num_arriendo))

data_kaggle_hog <- data_kaggle_hog |> 
  mutate(cat_zona = factor(cat_zona)) |> 
  mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                  TRUE ~ num_arriendo))

# Definimos la semilla nuevamente en caso de ser necesaria, de forma tal que
# las particiones siempre sean las mismas.
set.seed(2023)
cross_validation <- vfold_cv(data = data_hog |> 
                               select(-c('num_linea', 'num_ingreso_total')), 
                             v = 4, strata = bin_pobre)

# Definimos las métricas de evaluación. En particular, nos interesa la
# entropía cruzada con pesos diferenciales en función del desbalance de clases,
# así como el F1-score.
# metrics <- metric_set(balanced_entropy, f_meas)

# 1.1| XGBoost ------------------------------------------------------------
# Definimos la grilla donde se buscarán los hiperparámetros que maximizan el
# pronóstico por fuera de muestra.
tune_grid_xgboost <- grid_regular(
  tree_depth(range = c(3L, 8L), trans = NULL),
  trees(range = c(500L, 2000L), trans = NULL),
  learn_rate(range = c(-3, -1), trans = log10_trans()),
  mtry(range = c(3L, 6L), trans = NULL),
  levels = c(tree_depth = 3, trees = 4, learn_rate = 3, mtry = 2)
)

# Definimos el motor del modelo. En particular, dejamos fijos algunos 
# hiperparámetros que no consideramos relevantes para la validación cruzada.
xgboost_model <- boost_tree(
  tree_depth = tune(), 
  trees = tune(),
  learn_rate = tune(),
  mtry = tune(), 
  min_n = 30,
  loss_reduction = 0,
  sample_size = .5
) |> 
  set_mode('classification') |> 
  set_engine('xgboost', objective = 'binary:logistic')

# 1.1.1| Upsampling -------------------------------------------------------
# La base de datos que entra no puede tener información de la línea de pobreza.
# Por tanto, la eliminamos.
recipe_xgboost <- recipe(bin_pobre ~ ., 
                         data = data_hog |> 
                           select(-c('num_linea', 'num_ingreso_total'))) |>
  update_role(id_hogar, new_role = 'ID') |> 
  step_dummy(all_nominal_predictors()) |>
  # Con 'over_ratio = 1' hacemos que la 
  step_upsample(bin_pobre, over_ratio = 1)

# Definimos el flujo de trabajo, el cual consta de aplicar un modelo a una
# receta (es decir, una selección dada de variables explicativas para una
# variable dependiente).
wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor del F1-score en la
  # validación cruzada con upsampling.
  registerDoParallel(cl = cl)
  
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = cross_validation,
    grid      = tune_grid_xgboost,
    metrics   = metric_set(f_meas), # La métrica de interés es el F1-score.
    control   = control_grid(verbose = TRUE)
  )
  
  stopCluster(cl = cl)
  
  # El código tardó más de 3 horas en correr, por lo que es preferible no
  # ejecutarlo nuevamente. En su lugar, guardamos los resultados de los
  # hiperparámetros y, con ellos (señalados en el 'submit' de Kaggle), 
  # realizamos una estimación de la muestra de evaluación.
  saveRDS(object = tune_xgboost,
          file = paste0(directorioDatos, 'optim_parms_class_upsampling_xgboost.rds'))
  
  best_parms_xgboost <- select_best(tune_xgboost, metric = 'f_meas')
  definitive_xgboost <- finalize_workflow(
    x = wf_xgboost, 
    parameters = best_parms_xgboost
  )
  
  definitive_xgboost_fit <- fit(object = definitive_xgboost, 
                                data   = data_hog |> 
                                  select(-c('num_linea', 'num_ingreso_total')))
  
  # Mostramos las variables más importantes para el boosting.
  base_exp     = 1
  heightExp    = 1
  widthExp     = 1.2
  scale_factor = base_exp/widthExp
  
  graficaExportar <- definitive_xgboost_fit |> 
    extract_fit_parsnip() |> 
    vip(num_features = 10) +
    labs(title    = '',
         subtitle = '',
         caption  = '',
         x        = 'Porcentaje de importancia',
         y        = 'Variables más importantes') +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  nombreArchivo <- 'importancia_xgboost.png'
  ggsave(filename = paste0(directorioResultados, nombreArchivo), plot = graficaExportar,
         width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
  
  tune_xgboost |> show_best(metric = 'f_meas', n = 5) 
  
  # Finalmente, generamos la predicción de los datos por fuera de muestra y
  # guardamos los resultados para Kaggle. Con este modelo, el error es cercano
  # a los 207' por fuera de muestra, calculado con el MAE.
  prediccion <- tibble(
    id = data_kaggle_hog$id_hogar,
    pobre = predict(definitive_xgboost_fit, 
                    new_data = data_kaggle_hog,
                    type = 'class') |> 
      _$.pred_class
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'xgboost_class_upsampling_2.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_class_upsampling_xgboost.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'xgboost_class_upsampling_2.csv'))
}

# 1.1.2| SMOTE ------------------------------------------------------------
# La base de datos que entra no puede tener información de la línea de pobreza.
# Por tanto, la eliminamos.
recipe_xgboost <- recipe(bin_pobre ~ ., 
                         data = data_hog |> 
                           select(-c('num_linea', 'num_ingreso_total'))) |>
  update_role(id_hogar, new_role = 'ID') |> 
  step_dummy(all_nominal_predictors()) |>
  # step_normalize(all_numeric_predictors()) |> 
  # Con 'over_ratio = 1' hacemos que la 
  step_smote(bin_pobre, over_ratio = 1)

# Definimos el flujo de trabajo, el cual consta de aplicar un modelo a una
# receta (es decir, una selección dada de variables explicativas para una
# variable dependiente).
wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor del F1-score en la
  # validación cruzada con upsampling.
  registerDoParallel(cl = cl)
  
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = cross_validation,
    grid      = tune_grid_xgboost,
    metrics   = metric_set(f_meas), # La métrica de interés es el F1-score.
    control   = control_grid(verbose = TRUE)
  )
  
  stopCluster(cl = cl)
  
  # El código tardó más de 3 horas en correr, por lo que es preferible no
  # ejecutarlo nuevamente. En su lugar, guardamos los resultados de los
  # hiperparámetros y, con ellos (señalados en el 'submit' de Kaggle), 
  # realizamos una estimación de la muestra de evaluación.
  saveRDS(object = tune_xgboost,
          file = paste0(directorioDatos, 'optim_parms_class_smote_xgboost.rds'))
  
  best_parms_xgboost <- select_best(tune_xgboost, metric = 'f_meas')
  definitive_xgboost <- finalize_workflow(
    x = wf_xgboost, 
    parameters = best_parms_xgboost
  )
  
  definitive_xgboost_fit <- fit(object = definitive_xgboost, 
                                data   = data_hog |> 
                                  select(-c('num_linea', 'num_ingreso_total')))
  
  definitive_xgboost_fit <- fit(object = wf_xgboost, 
                                data   = data_hog |> 
                                  select(-c('num_linea', 'num_ingreso_total')))
  
  # Mostramos las variables más importantes para el boosting.
  base_exp     = 1
  heightExp    = 1
  widthExp     = 1.2
  scale_factor = base_exp/widthExp
  
  graficaExportar <- definitive_xgboost_fit |> 
    extract_fit_parsnip() |> 
    vip(num_features = 10) +
    labs(title    = '',
         subtitle = '',
         caption  = '',
         x        = 'Porcentaje de importancia',
         y        = 'Variables más importantes') +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  nombreArchivo <- 'importancia_xgboost_smote.png'
  ggsave(filename = paste0(directorioResultados, nombreArchivo), plot = graficaExportar,
         width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
  
  tune_xgboost |> show_best(metric = 'f_meas', n = 5) 
  
  # Finalmente, generamos la predicción de los datos por fuera de muestra y
  # guardamos los resultados para Kaggle. Con este modelo, el error es cercano
  # a los 207' por fuera de muestra, calculado con el MAE.
  prediccion <- tibble(
    id = data_kaggle_hog$id_hogar,
    pobre = predict(definitive_xgboost_fit, 
                    new_data = data_kaggle_hog,
                    type = 'class') |> 
      _$.pred_class
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'xgboost_class_smote.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_class_smote_xgboost.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'xgboost_class_smote.csv'))
}

# 1.2| Logit con elastic net ----------------------------------------------
# Es importante que uno de los modelos genere predicciones suaves y no a partir
# de particiones de los datos, pues puede haber regularidades que se capturan
# con funciones lineales o cuadráticas típicas. Al estar Ridge y Lasso 
# incluidos en elastic net, preferimos usar elastic net.
tune_grid_elasticNet <- grid_regular(
  penalty(range = c(-2, 3), trans = log10_trans()), # Relacionado con la penalización a la función de pérdida.
  mixture(range = c(0, 1), trans = NULL), # Relacionado con la ponderación a Lasso.
  levels = c(penalty = 20, mixture = 10)
)

elasticNet_model <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_mode('classification') |> 
  set_engine('glmnet', family = 'binomial')

recipe_elasticNet <- recipe(bin_pobre ~ ., 
                            data = data_hog |> 
                              select(-c('num_linea', 'num_ingreso_total'))) |>
  update_role(id_hogar, new_role = 'ID') |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |> 
  # Con 'over_ratio = 1' hacemos que la 
  step_upsample(bin_pobre, over_ratio = 1) |> 
  step_normalize(all_double_predictors())

wf_elasticNet <- workflow() |> 
  add_recipe(recipe_elasticNet) |> 
  add_model(elasticNet_model)

if (primeraVez == TRUE) {
  tune_elasticNet <- tune_grid(
    wf_elasticNet,
    resamples = cross_validation,
    grid      = tune_grid_elasticNet,
    metrics   = metric_set(f_meas), # La métrica de interés es el F1-score.
    control   = control_grid(verbose = TRUE)
  )
  
  saveRDS(object = tune_elasticNet,
          file = paste0(directorioDatos, 'optim_parms_elasticnet_2.rds'))
  
  best_parms_elasticNet <- select_best(tune_elasticNet, metric = 'f_meas')
  definitive_elasticNet <- finalize_workflow(
    x = wf_elasticNet,
    parameters = best_parms_elasticNet
  )
  
  definitive_elasticNet_fit <- fit(object = definitive_elasticNet,
                                   data   = data_hog |> 
                                     select(-c('num_linea', 'num_ingreso_total')))
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 192.1', 
  # calculado con el MAE, y tiene una desviación estándar de 5.2'. 
  tune_elasticNet |> show_best(metric = 'f_meas', n = 5) 
  
  prediccion <- tibble(
    id = data_kaggle_hog$id_hogar,
    pobre = predict(definitive_elasticNet_fit, 
                    new_data = data_kaggle_hog,
                    type = 'class') |> 
      _$.pred_class
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'elasticNet_class_hip1.csv'),
            row.names = FALSE)
  
} else {
  tune_elasticNet <- readRDS(file = paste0(directorioDatos,
                                           'optim_parms_elasticnet_2.rds'))
  prediccion_elasticNet <- read.csv(file = paste0(directorioResultados, 
                                                  'elasticNet_class_hip1.csv'))
}

# 1.3| Red neuronal -------------------------------------------------------
# Para las redes neuronales no contamos con validación cruzada, sino con un
# enfoque de validación, donde crearemos:
# - Conjuntos de entrenamiento, para estimar el modelo.
# - Conjuntos de validación, para estimar los hiperparámetros óptimos.
# - Conjuntos de prueba, para evaluar el desempeño predictivo.
set.seed(2023)
split_data <- initial_split(data_hog |> select(-c('num_linea', 'num_ingreso_total')), 
                            prop = 0.8, strata = bin_pobre)
train_data <- training(split_data)
test_data  <- testing(split_data)

# El conjunto de entrenamiento lo partimos, nuevamente, para generar el 
# conjunto de evaluación.
split_data <- initial_split(train_data, prop = 0.8, strata = bin_pobre)
train_data <- training(split_data)
val_data   <- testing(split_data)

# Separamos las variables predictoras y la variable objetivo.
x_train <- train_data |> select(-c('id_hogar', 'bin_pobre'))
y_train <- train_data |> pull(bin_pobre)
x_val   <- val_data |> select(-c('id_hogar', 'bin_pobre'))
y_val   <- val_data |> pull(bin_pobre)
x_test  <- test_data |> select(-c('id_hogar', 'bin_pobre'))
y_test  <- test_data |> pull(bin_pobre)

# Normalizamos las variables numéricas.
recipe_nn <- recipe(~ ., x_train) |>
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |> 
  # step_upsample(bin_pobre, over_ratio = 1) |> 
  step_normalize(all_numeric_predictors())

x_train <- as.matrix(prep(recipe_nn) |> bake(new_data = x_train))
x_test  <- as.matrix(prep(recipe_nn) |> bake(new_data = x_test))
x_val   <- as.matrix(prep(recipe_nn) |> bake(new_data = x_val))

# Definimos variables de control.
METRICS <- list(
  metric_precision(name = 'precision'),
  metric_recall(name = 'recall')
)
EPOCHS <- 30
BATCH_SIZE <- 2048
tf$random$set_seed(2023)
early_stopping <- callback_early_stopping(monitor = 'val_loss', 
                                          patience = 3,
                                          restore_best_weights = TRUE)

# Para entrenar el modelo utilizamos el sesgo inicial.
initial_bias <- log(as.numeric(sum(y_train == 1))/as.numeric(sum(y_train == 0)))

nn_model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = 'relu',
              input_shape = dim(x_train)[2],
              kernel_initializer = initializer_random_uniform()) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid',
              bias_initializer = initializer_constant(value = initial_bias))

nn_model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-3),
  loss = 'binary_crossentropy',
  metrics = METRICS
)

nn_model %>% fit(
  x = x_train,
  y = y_train,
  batch_size = BATCH_SIZE,
  epochs = EPOCHS,
  validation_data = list(x_val, y_val),
  verbose = 0,
  seed = 12,
  callbacks = list(early_stopping)
)

resultado_sesgo <- nn_model |> evaluate(x_test, y_test, verbose = FALSE)
f1_score <- 2*resultado_sesgo['precision'] * resultado_sesgo['recall']/(resultado_sesgo['precision']+resultado_sesgo['recall'])
as.numeric(f1_score)

# 2| Algoritmo más votado -------------------------------------------------


