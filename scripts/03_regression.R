# 1| Predicción -----------------------------------------------------------
# Nota. En el problema de regresión no tenemos datos desbalanceados. En su 
# lugar, la distribución del ingreso es asimétrica. Por consiguiente, no es 
# necesario implementar una estrategia de sobre-muestreo para la clase 
# minoritaria.

# Realizamos las modificaciones finales necesarias sobre las bases de datos.
# En particular, eliminamos valores de NA, correspondientes a infantes menores
# de 11 años de edad.
data_p <- data_p |> drop_na()

# Definimos la semilla nuevamente en caso de ser necesaria, de forma tal que
# las particiones siempre sean las mismas.
set.seed(666)
cross_validation <- vfold_cv(data_p, v = 4)

# 1.1| Elastic net -------------------------------------------------------------------
tune_grid_ridge <- grid_regular(
  penalty(range = c(-2, 3), trans = log10_trans()), # Relacionado con la penalización a la función de pérdida.
  mixture(range = c(0, 1), trans = NULL), # Relacionado con la ponderación a Lasso.
  levels = c(penalty = 20, mixture = 20)
)

ridge_model <- linear_reg(
  # Mixture 0 implica que se le da 0% de ponderación a Lasso y, por tanto,
  # la estimación es meramente un Ridge.
  mixture = tune(),
  penalty = tune()
) |>
  set_mode("regression") |>
  set_engine("glmnet")

recipe_ridge <- recipe(num_ingreso_individual ~ .,
                       data = data_p) |> 
  update_role(id_hogar, new_role = 'id_hogar') |> 
  # Una muestra de entrenamiento puede no tener todas las localidades, por lo 
  # que es necesario que se asigne categorías anteriormente no vistas a la
  # categoría 'new'.
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors())

wf_ridge <- workflow() |> 
  add_recipe(recipe_ridge) |> 
  add_model(ridge_model)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor del RMSE.
  tune_ridge <- tune_grid(
    wf_ridge,
    resamples = cross_validation,
    grid = tune_grid_ridge,
    metrics = metric_set(rmse)
  )

  saveRDS(object = tune_ridge,
          file = paste0(directorioDatos, 'optim_parms_elasticnet_1.rds'))
  
  best_parms_ridge <- select_best(tune_ridge, metric = 'rmse')
  definitive_ridge <- finalize_workflow(
    x = wf_ridge,
    parameters = best_parms_ridge
  )
  
  definitive_ridge_fit <- fit(object = definitive_ridge,
                              data   = data_p)
  
  # TODO. Corregir con los valores encontrados.
  # Evaluamos el RMSE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 192.1', 
  # calculado con el MAE, y tiene una desviación estándar de 5.2'. 
  tune_ridge |> show_best(metric = 'rmse', n = 5) 
  
  prediccion <- tibble(
    id_hogar = data_kaggle_p$id_hogar,
    num_edad = data_kaggle_p$num_edad,
    num_ingreso_individual = predict(definitive_ridge_fit, new_data = data_kaggle_p) |> 
      _$.pred
  ) |> 
    mutate(num_ingreso_individual = case_when(num_edad <= 11 ~ 0,
                                              num_ingreso_individual < 0 ~ 0,
                                              TRUE ~ num_ingreso_individual))
  
  prediccion <- prediccion |> 
    group_by(id_hogar) |>
    summarise(num_ingreso_total = sum(num_ingreso_individual, na.rm = TRUE)) |> 
    right_join(y = data_kaggle_hog, by = 'id_hogar') |> 
    mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                    TRUE ~ num_arriendo)) |> 
    mutate(num_ingreso_total = num_ingreso_total + num_arriendo) |> 
    mutate(pobre = as.numeric(num_ingreso_total < num_linea*num_personas)) |> 
    select(c('id' = 'id_hogar', 'pobre'))
  
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'elasticnet_reg_hip1.csv'),
            row.names = FALSE)
  
} else {
  tune_ridge <- readRDS(file = paste0(directorioDatos,
                                      'optim_parms_elasticnet_1.rds'))
  prediccion_ridge <- read.csv(file = paste0(directorioResultados, 
                                             'elasticnet_reg_hip1.csv'))
}

# 1.2| xgboost ------------------------------------------------------------
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
  set_mode('regression') |> 
  set_engine('xgboost', objective = 'reg:squarederror')

# La base de datos que entra no puede tener un campo de geometría porque la
# implementación no lo permite. Por tanto, la eliminamos.
recipe_xgboost <- recipe(num_ingreso_individual ~ .,
                         data = data_p) |> 
  update_role(id_hogar, new_role = 'id_hogar') |> 
  step_dummy(all_nominal_predictors()) 

# Definimos el flujo de trabajo, el cual consta de aplicar un modelo a una
# receta (es decir, una selección dada de variables explicativas para una
# variable dependiente).
wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor del RMSE.
  registerDoParallel(cl = cl) 
  
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = cross_validation,
    grid = tune_grid_xgboost,
    metrics = metric_set(rmse)
  )
  
  stopCluster(cl = cl)
  
  # El código tardó más de 3 horas en correr, por lo que es preferible no
  # ejecutarlo nuevamente. En su lugar, guardamos los resultados de los
  # hiperparámetros y, con ellos (señalados en el 'submit' de Kaggle), 
  # realizamos una estimación de la muestra de evaluación.
  saveRDS(object = tune_xgboost,
          file = paste0(directorioDatos, 'optim_parms_reg_xgboost.rds'))
  
  best_parms_xgboost <- select_best(tune_xgboost, metric = 'rmse')
  definitive_xgboost <- finalize_workflow(
    x = wf_xgboost, 
    parameters = best_parms_xgboost
  )
  
  definitive_xgboost_fit <- fit(object = definitive_xgboost, 
                                data   = data_p)
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
  
  nombreArchivo <- 'importancia_xgboost_reg.png'
  ggsave(filename = paste0(directorioResultados, nombreArchivo), plot = graficaExportar,
         width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
  
  tune_xgboost |> show_best(metric = 'rmse', n = 5) 
  
  # TODO. Validar. Una idea es utilizar la predicción del modelo a nivel de
  # individuos y mezclarlo con el modelo a nivel de hogares, de forma tal
  # que usamos ambas fuentes de información.
  prediccion <- tibble(
    id_hogar = data_kaggle_p$id_hogar,
    num_edad = data_kaggle_p$num_edad,
    num_ingreso_individual = predict(definitive_xgboost_fit, new_data = data_kaggle_p) |> 
      _$.pred
  ) |> 
    mutate(num_ingreso_individual = case_when(num_edad <= 11 ~ 0,
                                              num_ingreso_individual < 0 ~ 0,
                                              TRUE ~ num_ingreso_individual))
  
  prediccion <- prediccion |> 
    group_by(id_hogar) |>
    summarise(num_ingreso_total = sum(num_ingreso_individual, na.rm = TRUE)) |> 
    right_join(y = data_kaggle_hog, by = 'id_hogar') |> 
    mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                    TRUE ~ num_arriendo)) |> 
    mutate(num_ingreso_total = num_ingreso_total + num_arriendo) |> 
    mutate(pobre = as.numeric(num_ingreso_total < num_linea*num_personas)) |> 
    select(c('id' = 'id_hogar', 'pobre'))
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'xgboost_reg_hip1.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_reg_xgboost.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'xgboost_reg_hip1.csv'))
}

# 1.3| Red neuronal -------------------------------------------------------


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



# arquitectura del modelo
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = 'relu',
              input_shape = dim(x_train)[2],
              kernel_initializer = initializer_random_uniform()) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# el compilador del modelo
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-3),
  loss = 'binary_crossentropy',
  metrics = METRICS
)

# entrenamiento
historia_modelo_basico <- model %>% fit(
  x = x_train,
  y = y_train,
  batch_size = BATCH_SIZE,
  epochs = EPOCHS,
  validation_data = list(x_val, y_val),
  verbose = 0,
  seed = 12
)


# Evaluar el modelo
results <- model %>% evaluate(x_test, y_test, verbose = 0)
results


#Calcular el F1_score
f1_score <- 2*results['precision'] * results['recall']/(results['precision']+results['recall'])
f1_score





# TODO. Validar. Una idea es utilizar la predicción del modelo a nivel de
# individuos y mezclarlo con el modelo a nivel de hogares, de forma tal
# que usamos ambas fuentes de información.
prediccion <- tibble(
  id_hogar = data_kaggle_p$id_hogar,
  num_edad = data_kaggle_p$num_edad,
  num_ingreso_individual = predict(historia_modelo_basico, new_data = data_kaggle_p) |> 
    _$.pred
) |> 
  mutate(num_ingreso_individual = case_when(num_edad <= 11 ~ 0,
                                            num_ingreso_individual < 0 ~ 0,
                                            TRUE ~ num_ingreso_individual))

prediccion <- prediccion |> 
  group_by(id_hogar) |>
  summarise(num_ingreso_total = sum(num_ingreso_individual, na.rm = TRUE)) |> 
  right_join(y = data_kaggle_hog, by = 'id_hogar') |> 
  mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                  TRUE ~ num_arriendo)) |> 
  mutate(num_ingreso_total = num_ingreso_total + num_arriendo) |> 
  mutate(pobre = as.numeric(num_ingreso_total < num_linea*num_personas)) |> 
  select(c('id' = 'id_hogar', 'pobre'))

# Nota. Dejamos comentada la exportación para no modificar el archivo que ya
# publicamos en Kaggle.
write.csv(x = prediccion,
          file = paste0(directorioResultados, 'nn_reg_hip1.csv'),
          row.names = FALSE)

