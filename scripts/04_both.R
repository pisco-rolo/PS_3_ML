# Preparamos los datos de entrenamiento.
prediccion <- tibble(
  id_hogar = data_p$id_hogar,
  num_edad = data_p$num_edad,
  num_ingreso_individual = predict(definitive_xgboost_fit, new_data = data_p) |> 
    _$.pred
) |> 
  mutate(num_ingreso_individual = case_when(num_edad <= 11 ~ 0,
                                            num_ingreso_individual < 0 ~ 0,
                                            TRUE ~ num_ingreso_individual)) |> 
  select(-c('num_edad'))

data_hog <- prediccion |> 
    group_by(id_hogar) |>
    summarise(num_ingreso_total = sum(num_ingreso_individual, na.rm = TRUE)) |> 
    right_join(y = data_hog |> select(-c('num_ingreso_total')), by = 'id_hogar') |> 
    mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                    TRUE ~ num_arriendo)) |> 
    mutate(num_ingreso_total = num_ingreso_total + num_arriendo) |> 
    mutate(proxy_pobre = num_ingreso_total/(num_linea*num_personas)) |> 
    select(-c('num_arriendo', 'num_linea', 'num_ingreso_total'))

saveRDS(object = data_hog, 
        file   = paste0(directorioDatos, 'data_comb.rds'))

# Y también los datos de evaluación.
prediccion <- tibble(
  id_hogar = data_kaggle_p$id_hogar,
  num_edad = data_kaggle_p$num_edad,
  num_ingreso_individual = predict(definitive_xgboost_fit, new_data = data_kaggle_p) |> 
    _$.pred
) |> 
  mutate(num_ingreso_individual = case_when(num_edad <= 11 ~ 0,
                                            num_ingreso_individual < 0 ~ 0,
                                            TRUE ~ num_ingreso_individual)) |> 
  select(-c('num_edad'))

data_kaggle_hog <- prediccion |> 
  group_by(id_hogar) |>
  summarise(num_ingreso_total = sum(num_ingreso_individual, na.rm = TRUE)) |> 
  right_join(y = data_kaggle_hog, by = 'id_hogar') |> 
  mutate(num_arriendo = case_when(is.na(num_arriendo) ~ 0,
                                  TRUE ~ num_arriendo)) |> 
  mutate(num_ingreso_total = num_ingreso_total + num_arriendo) |> 
  mutate(proxy_pobre = num_ingreso_total/(num_linea*num_personas)) |> 
  select(-c('num_arriendo', 'num_linea', 'num_ingreso_total'))

saveRDS(object = data_kaggle_hog,
        file   = paste0(directorioDatos, 'data_kaggle_comb.rds'))

set.seed(2023)
cross_validation <- vfold_cv(data = data_hog, v = 4, strata = bin_pobre)

xgboost_model <- boost_tree(
  tree_depth = 8, 
  trees = 2000,
  learn_rate = 0.01,
  mtry = 6, 
  min_n = 30,
  loss_reduction = 0,
  sample_size = .5
) |> 
  set_mode('classification') |> 
  set_engine('xgboost', objective = 'binary:logistic')

recipe_xgboost <- recipe(bin_pobre ~ ., 
                         data = data_hog) |>
  update_role(id_hogar, new_role = 'ID') |> 
  step_dummy(all_nominal_predictors()) |>
  step_upsample(bin_pobre, over_ratio = 1)

wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

definitive_xgboost_fit <- fit(object = wf_xgboost, 
                              data   = data_hog)

prediccion <- tibble(
  id = data_kaggle_hog$id_hogar,
  pobre = predict(definitive_xgboost_fit, 
                  new_data = data_kaggle_hog) |> 
    _$.pred_class
)

write.csv(x = prediccion,
          file = paste0(directorioResultados, 'comb_reg_class.csv'),
          row.names = FALSE)
