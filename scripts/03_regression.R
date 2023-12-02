# 1| Predicción -----------------------------------------------------------
# Definimos la semilla nuevamente en caso de ser necesaria.
data_hog <- data_hog |> 
  mutate(bin_pobre = factor(bin_pobre),
         cat_zona = factor(cat_zona))

#cuál es la proporción de hogares pobres con el total de hogares y cómo se 
#distribuye a lo largo de los diferentes datasets
table(data_hog$bin_pobre)
table(data_hog$bin_pobre)/nrow(data_hog)

set.seed(666)
df_fold <- vfold_cv(data_hog, v = 7)

# 2.4| Ridge -------------------------------------------------------------------
tune_grid_ridge <- grid_regular(
  # La penalización va desde 0.0001 hasta 1,000.
  penalty(range = c(0.001, 1000), trans = NULL), # Relacionado con la penalización a la función de pérdida.
  levels  = 20
)

ridge_model <- linear_reg(
  # Mixture 0 implica que se le da 0% de ponderación a Lasso y, por tanto,
  # la estimación es meramente un Ridge.
  mixture = 0,
  penalty = tune()
) |>
  set_mode("regression") |>
  set_engine("glmnet")

#Se está entrenando con la variable de si es pobre
#|> 
#select(-c('num_linea', 'bin_pobre', 'cat_zona'))
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
  tune_ridge <- tune_grid(
    wf_ridge,
    resamples = df_fold,
    grid = tune_grid_ridge,
    metrics = metric_set(mae)
  )
  
  saveRDS(object = tune_ridge,
          file = paste0(directorioDatos, 'optim_parms_ridge_1.rds'))
  
  best_parms_ridge <- select_best(tune_ridge, metric = 'mae')
  definitive_ridge <- finalize_workflow(
    x = wf_ridge,
    parameters = best_parms_ridge
  )
  
  definitive_ridge_fit <- fit(object = definitive_ridge,
                              data   = data_p)
  
  prediccion <- tibble(
    id_sdfsdf = data_p$id_sdfsdfgsdf,
    precio_obs = data_p$num_ingreso_individual,
    precio_est = predict(definitive_ridge_fit, new_data = data_hog) |> _$.pred
  )
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 192.1', 
  # calculado con el MAE, y tiene una desviación estándar de 5.2'. 
  tune_ridge |> show_best(metric = 'mae', n = 5) 
  
  prediccion <- tibble(
    property_id = data_kaggle_hog$id_hogar,
    price = predict(definitive_ridge_fit, new_data = data_kaggle_hog) |> 
      _$.pred,
    num_linea = data_kaggle_hog$num_linea
  )
  
  # Crear columna binaria basada en el umbral
  prediccion$bin_pobre <- ifelse(prediccion$price > prediccion$num_linea, 0, 1)
  f1_score <- F1_Score(
    prediccion$columna_real, datos_de_prueba$prediccion_binaria)
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'ridge_imp1_hip1.csv'),
            row.names = FALSE)
  
} else {
  tune_ridge <- readRDS(file = paste0(directorioDatos,
                                      'optim_parms_ridge_1.rds'))
  prediccion_ridge <- read.csv(file = paste0(directorioResultados, 
                                             'ridge_imp1_hip2.csv'))
}

# 2.5| Lasso -------------------------------------------------------------------
tune_grid_lasso <- grid_regular(
  # La penalización va desde 0.0001 hasta 1,000.
  penalty(range = c(0.001, 1000), trans = NULL), # Relacionado con la penalización a la función de pérdida.
  levels  = 200
)

lasso_model <- linear_reg(
  mixture = 1, 
  penalty = tune()
) |>
  set_mode("regression") |>
  set_engine("glmnet")

recipe_lasso <- recipe(num_precio ~ .,
                       data = dataset |> 
                         st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID') |> 
  # Una muestra de entrenamiento puede no tener todas las localidades, por lo 
  # que es necesario que se asigne categorías anteriormente no vistas a la
  # categoría 'new'.
  step_novel(all_nominal_predictors()) |> 
  step_poly(num_distancia_calles, num_distancia_mall, num_distancia_tm,
            degree = 2) |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_interact(terms = ~ bin_zonaResidencial:bin_parqueadero +
                  bin_casa:bin_parqueadero
  ) |>
  step_normalize(all_predictors())

wf_lasso <- workflow() |> 
  add_recipe(recipe_lasso) |> 
  add_model(lasso_model)

if (primeraVez == TRUE) {
  tune_lasso <- tune_grid(
    wf_lasso,
    resamples = block_folds,
    grid = tune_grid_lasso,
    metrics = metric_set(mae)
  )
  
  saveRDS(object = tune_lasso,
          file = paste0(directorioDatos, 'optim_parms_lasso_1.rds'))
  
  best_parms_lasso <- select_best(tune_lasso, metric = 'mae')
  definitive_lasso <- finalize_workflow(
    x = wf_lasso,
    parameters = best_parms_lasso
  )
  
  definitive_lasso_fit <- fit(object = definitive_lasso,
                              data   = dataset)
  
  prediccion <- tibble(
    id_hogar = dataset$id_hogar,
    precio_obs = dataset$num_precio,
    precio_est = predict(definitive_lasso_fit, new_data = dataset) |> _$.pred
  )
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 192.7', 
  # calculado con el MAE, y tiene una desviación estándar de 4.8'. 
  tune_lasso |> show_best(metric = 'mae', n = 5) 
  
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_lasso_fit, new_data = dataset_kaggle) |> 
      _$.pred
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'lasso_imp1_hip1.csv'),
            row.names = FALSE)
  
} else {
  tune_lasso <- readRDS(file = paste0(directorioDatos,
                                      'optim_parms_lasso_1.rds'))
  prediccion_lasso <- read.csv(file = paste0(directorioResultados, 
                                             'lasso_imp1_hip1.csv'))
}

# 2.1| xgboost ------------------------------------------------------------
# Definimos la grilla donde se buscarán los hiperparámetros que maximizan el
# pronóstico por fuera de muestra.
tune_grid_xgboost <- grid_regular(
  tree_depth(range = c(1L, 8L), trans = NULL),
  trees(range = c(100L, 2000L), trans = NULL),
  learn_rate(range = c(0.01, 0.3), trans = NULL),
  mtry(range = c(3L, 6L), trans = NULL),
  levels = c(tree_depth = 4, trees = 5, learn_rate = 4, mtry = 2)
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
recipe_xgboost <- recipe(num_precio ~ ., 
                         data = dataset |> st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID') |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_distancia_calles, degree = 2)

# Definimos el flujo de trabajo, el cual consta de aplicar un modelo a una
# receta (es decir, una selección dada de variables explicativas para una
# variable dependiente).
wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

# Ponemos una semilla adicional, pues pasos anteriores pueden haber tenido un
# impacto en el generador de números pseudo-aleatorios. Así mismo, realizamos
# una validación espacial por bloques, incluyendo cinco folds.
set.seed(123)
block_folds <- spatial_block_cv(dataset, v = 5)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor de MAE en la
  # validación cruzada por bloques espaciales.
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = block_folds,
    grid = tune_grid_xgboost,
    metrics = metric_set(mae)
  )
  
  # El código tardó más de 3 horas en correr, por lo que es preferible no
  # ejecutarlo nuevamente. En su lugar, guardamos los resultados de los
  # hiperparámetros y, con ellos (señalados en el 'submit' de Kaggle), 
  # realizamos una estimación de la muestra de evaluación.
  saveRDS(object = tune_xgboost,
          file = paste0(directorioDatos, 'optim_parms_xgboost_2.rds'))
  
  best_parms_xgboost <- select_best(tune_xgboost, metric = 'mae')
  definitive_xgboost <- finalize_workflow(
    x = wf_xgboost, 
    parameters = best_parms_xgboost
  )
  
  definitive_xgboost_fit <- fit(object = definitive_xgboost, 
                                data   = dataset)
  
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
  
  # - Variables originales. Evaluamos el MAE de la validación cruzada para 
  #   tener una noción del error que podríamos encontrar. En particular, el 
  #   error es cercano a los 156.7', calculado con el MAE, y tiene una 
  #   desviación estándar de 5.4'. 
  # - Extensión. Evaluamos el MAE de la validación cruzada para 
  #   tener una noción del error que podríamos encontrar. En particular, el 
  #   error es cercano a los 156.3', calculado con el MAE, y tiene una 
  #   desviación estándar de 5.1'. Por tanto, hay una mejora en la predicción
  #   al incluir variables relacionadas con la remodelación, el walking closet,
  #   y si el edificio tiene ascensor o no. 
  prediccion <- tibble(
    id_hogar = dataset$id_hogar,
    precio_obs = dataset$num_precio,
    precio_est = predict(definitive_xgboost_fit, new_data = dataset) |> _$.pred
  )
  
  tune_xgboost |> show_best(metric = 'mae', n = 5) 
  
  # Finalmente, generamos la predicción de los datos por fuera de muestra y
  # guardamos los resultados para Kaggle. Con este modelo, el error es cercano
  # a los 207' por fuera de muestra, calculado con el MAE.
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_xgboost_fit, new_data = dataset_kaggle) |> 
      _$.pred
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'xgboost_imp1_hip2.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_xgboost_2.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'xgboost_imp1_hip2.csv'))
}


