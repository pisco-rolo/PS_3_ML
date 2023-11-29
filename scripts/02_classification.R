# 1| Predicción -----------------------------------------------------------
# Definimos la semilla nuevamente en caso de ser necesaria.
data_hog <- data_hog |> 
  mutate(bin_pobre = factor(bin_pobre),
         cat_zona = factor(cat_zona))

# 1.1| XGBoost ------------------------------------------------------------
# Definimos la grilla donde se buscarán los hiperparámetros que maximizan el
# pronóstico por fuera de muestra.
tune_grid_xgboost <- grid_regular(
  tree_depth(range = c(1L, 8L), trans = NULL),
  trees(range = c(100L, 2000L), trans = NULL),
  learn_rate(range = c(-3, 1), trans = log10_trans()),
  mtry(range = c(3L, 6L), trans = NULL),
  levels = c(tree_depth = 4, trees = 5, learn_rate = 5, mtry = 2)
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
  set_engine('xgboost')

xgboost_model <- boost_tree(
  tree_depth = 3, 
  trees = 1525,
  learn_rate = 0.3,
  mtry = 3, 
  min_n = 30,
  loss_reduction = 0,
  sample_size = .5
) |> 
  set_mode('classification') |> 
  set_engine('xgboost', objective = 'binary:logistic')

# La base de datos que entra no puede tener información de la línea de pobreza.
# Por tanto, la eliminamos.
recipe_xgboost <- recipe(bin_pobre ~ ., 
                         data = data_hog |> 
                           select(-c('num_linea', 'num_ingreso_total'))) |>
  update_role(id_hogar, new_role = 'ID') |> 
  step_dummy(all_nominal_predictors()) |>
  step_rose(bin_pobre, over_ratio = 1)

# Definimos el flujo de trabajo, el cual consta de aplicar un modelo a una
# receta (es decir, una selección dada de variables explicativas para una
# variable dependiente).
wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

set.seed(2023)
cross_validation <- vfold_cv(data = data_hog |> 
                               select(-c('num_linea', 'num_ingreso_total')), 
                             v = 5, strata = bin_pobre)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor del F1-score en la
  # validación cruzada con upsampling.
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = cross_validation,
    grid = tune_grid_xgboost,
    metrics = metric_set(f_meas)
  )
  
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
                    new_data = data_kaggle_hog |> 
                      mutate(cat_zona = factor(cat_zona))) |> 
      _$.pred_class
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'xgboost_class_upsampling.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_class_upsampling_xgboost.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'xgboost_class_upsampling.csv'))
}
