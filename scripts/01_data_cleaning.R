if (primeraVez == TRUE) {
  # 1| Importar -------------------------------------------------------------
  # Importamos la base de datos de entrenamiento y de evaluación.
  # Nota. En ningún momento se unen los datos para limpieza en tanto esto
  # supone el riesgo de contaminación entre datos de entrenamiento y de 
  # evaluación.
  data_hog <- read.csv(file = paste0(directorioDatos, 'train_hogares.csv'))
  data_kaggle_hog <- read.csv(file = paste0(directorioDatos,'test_hogares.csv'))
  
  data_p <- read.csv(file = paste0(directorioDatos, 'train_personas.csv'))
  data_kaggle_p <- read.csv(file = paste0(directorioDatos,'test_personas.csv'))
  
  # 2| Limpieza -------------------------------------------------------------
  # Es necesario limpiar los datos del hogar y de las personas individualmente,
  # pues si bien predecimos la pobreza a nivel hogar, algunos tratamientos se
  # hacen a nivel de persona. Por ejemplo, la predicción del salario de cada
  # individuo perteneciente al hogar.
  
  # 2.1| Datos de entrenamiento ---------------------------------------------
  # Nota. Si bien la documentación está en la carpeta de 'references', dejamos en 
  # las funciones una guía de las variables que nos parecieron relevantes para el 
  # análisis a nivel de persona.
  resultado <- data_cleaning_personas(
    .dataset = data_p,
    .old_var = c('id', 'ingtot',  'p6020', 'p6040', 'p6050', 'p6090', 'p6210', 
                 'p6210s1', 'p6430', 'oc', 'des', 'ina', 'p6510', 'p6545', 
                 'p6585s1', 'p6585s2', 'p6585s3', 'p6585s4', 'p7495', 'p7505', 
                 'p7510s2', 'p7510s3', 'p7510s5', 'p6870'),
    .new_var = c('id_hogar', 'num_ingreso_individual', 'bin_mujer', 'num_edad', 
                 'cat_parentesco', 'bin_cotizante', 'cat_educacion', 'num_educacion', 
                 'cat_ocupacion', 'bin_ocupado', 'bin_desocupado', 
                 'bin_inactivo', 'bin_ingresoAdicional_horasExtra',
                 'bin_ingresoAdicional_prima', 'bin_ingresoAdicional_alimentacion',
                 'bin_ingresoAdicional_transporte', 'bin_ingresoAdicional_auxilio',
                 'bin_ingresoAdicional_educacion', 'bin_ingresoAdicional_realEstate',
                 'bin_ingresoAdicional_rendimientosFinancieros',
                 'bin_ingresoAdicional_transferenciasInternacionales',
                 'bin_ingresoAdicional_ayudasGobierno',
                 'bin_ingresoAdicional_rendimientosFinancieros2',
                 'cat_tamanoEmpresa')
  )
  
  data_p <- resultado[[1]]
  data_hog <- data_cleaning_hogares(
    .dataset = data_hog,
    .old_var = c('id', 'clase', 'dominio', 'p5010', 'p5090', 'p5130', 
                 'npersug', 'ingtotugarr', 'lp', 'pobre'),
    .new_var = c('id_hogar', 'cat_zona', 'cat_ubicacion', 'num_cuartos', 
                 'cat_propietario', 'num_arriendo', 'num_personas', 
                 'num_ingreso_total', 'num_linea', 'bin_pobre')
  ) |> left_join(y = resultado[[2]], by = 'id_hogar') 
  
  # 2.2| Datos de entrenamiento ---------------------------------------------
  resultado <- data_cleaning_personas(
    .dataset = data_kaggle_p,
    .old_var = c('id', 'ingtot',  'p6020', 'p6040', 'p6050', 'p6090', 'p6210', 
                 'p6210s1', 'p6430', 'oc', 'des', 'ina', 'p6510', 'p6545', 
                 'p6585s1', 'p6585s2', 'p6585s3', 'p6585s4', 'p7495', 'p7505', 
                 'p7510s2', 'p7510s3', 'p7510s5', 'p6870'),
    .new_var = c('id_hogar', 'num_ingreso_individual', 'bin_mujer', 'num_edad', 
                 'cat_parentesco', 'bin_cotizante', 'cat_educacion', 'num_educacion', 
                 'cat_ocupacion', 'bin_ocupado', 'bin_desocupado', 
                 'bin_inactivo', 'bin_ingresoAdicional_horasExtra',
                 'bin_ingresoAdicional_prima', 'bin_ingresoAdicional_alimentacion',
                 'bin_ingresoAdicional_transporte', 'bin_ingresoAdicional_auxilio',
                 'bin_ingresoAdicional_educacion', 'bin_ingresoAdicional_realEstate',
                 'bin_ingresoAdicional_rendimientosFinancieros',
                 'bin_ingresoAdicional_transferenciasInternacionales',
                 'bin_ingresoAdicional_ayudasGobierno',
                 'bin_ingresoAdicional_rendimientosFinancieros2',
                 'cat_tamanoEmpresa')
  )
  
  data_kaggle_p <- resultado[[1]]
  data_kaggle_hog <- data_cleaning_hogares(
    .dataset = data_kaggle_hog,
    .old_var = c('id', 'clase', 'dominio', 'p5010', 'p5090', 'p5130', 
                 'npersug', 'ingtotugarr', 'lp', 'pobre'),
    .new_var = c('id_hogar', 'cat_zona', 'cat_ubicacion', 'num_cuartos', 
                 'cat_propietario', 'num_arriendo', 'num_personas', 
                 'num_ingreso_total', 'num_linea', 'bin_pobre')
  ) |> left_join(y = resultado[[2]], by = 'id_hogar') 
  
  rm(resultado)
  
  saveRDS(object = data_p, 
          file   = paste0(directorioDatos, 'data_p.rds'))
  saveRDS(object = data_hog, 
          file   = paste0(directorioDatos, 'data_hog.rds'))
  saveRDS(object = data_kaggle_p, 
          file   = paste0(directorioDatos, 'data_kaggle_p.rds'))
  saveRDS(object = data_kaggle_hog, 
          file   = paste0(directorioDatos, 'data_kaggle_hog.rds'))
} else {
  data_p          <- readRDS(file = paste0(directorioDatos, 'data_p.rds'))
  data_hog        <- readRDS(file = paste0(directorioDatos, 'data_hog.rds'))
  data_kaggle_p   <- readRDS(file = paste0(directorioDatos, 'data_kaggle_p.rds'))
  data_kaggle_hog <- readRDS(file = paste0(directorioDatos, 'data_kaggle_hog.rds'))
}

# 3| Estadística descriptiva ----------------------------------------------
# Realizamos un ejercicio de estadística descriptiva antes de la imputación
# de datos. Por ejemplo, la censura de datos atípicos.
summary(data_hog)

# Datos a nivel de hogar
data_hog <- data_hog |> 
  mutate(num_arriendo = ifelse(num_arriendo >20000000, 20000000, num_arriendo ))

estadistica_descriptiva <- data_hog |> 
  select(c(starts_with('num_'), starts_with('bin_'), starts_with('prop_'))) |>
  tbl_summary(include = everything(),
              type = c(starts_with('prop_'),starts_with('num_')) ~ 'continuous2',
              label = list(num_cuartos ~ 'Números de habitaciones',
                           num_personas ~ 'Número de miembros del hogar',
                           num_cuartos_por_persona ~ 'Total de habitaciones por persona',
                           num_ingreso_total ~ 'Ingreso total del hogar',
                           num_arriendo ~ 'Costo arriendo',
                           num_linea ~ 'Línea de pobreza',
                           bin_pobre ~ 'Dummy de pobreza',
                           bin_mujer_y_jefaHogar ~ 'Dummy mujer jefe de hogar',
                           bin_madre_soltera ~ 'Dummy madre soltera',
                           num_edad_promedio ~ 'Edad promedio en el hogar',
                           prop_hogarSubsidiado ~ 'Proporción de subsidios en el hogar',
                           bin_hogar_ayudadoPorGobierno ~ 'Dummy ayuda del Gobierno' ,
                           prop_hogarSubsidiado2 ~ 'Subsidios por miembros del hogar' ,
                           prop_empleados ~ 'Proporción de empleados en el hogar incluye menores' ,
                           prop_empleados2 ~ 'Proporción empleados en el hogar sin menores' ,
                           prop_empleados3 ~ 'Proporción empleados cotizantes en el hogar incluye menores' ,
                           num_dependencia ~ 'Número de dependientes en el hogar' ),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "({min}, {max})"),
                               all_categorical() ~ "{n}  ({p}%)"),  #  / {N}
              missing_text = "(Valores faltantes)", 
              sort = all_categorical() ~ "alphanumeric") |> 
  add_stat_label(label = list(all_categorical() ~ "",
                              all_continuous() ~ c("Promedio (Desviación std)",
                                                   "Mínimo y máximo"))) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() 

gtsave(as_gt(estadistica_descriptiva), 
       filename="views/estadisticaDescriptiva.tex")

#-------------------------------------------
# Nivel persona

estadistica_descriptiva2 <- data_p |> 
  select(c(starts_with('num_'), starts_with('bin_'), starts_with('cat_'))) |>
  select(-c('num_experiencia2')) |>
  tbl_summary(include = everything(),
              type = starts_with('num_') ~ 'continuous2',
              label = list(num_ingreso_individual ~ 'Ingreso individual' ,
                           num_edad ~ 'Edad' ,
                           bin_mujer ~ 'Dummy mujer',
                           cat_parentesco ~ 'Parentesco jefe de hogar',
                           bin_cotizante ~ 'Dummy cotizante',
                           cat_educacion ~ 'Nivel Educativo',
                           num_educacion ~ 'Grado escolar aprobado',
                           cat_ocupacion ~ 'Ocupación actividad principal',
                           bin_ocupado ~ 'Dummy ocupado',
                           bin_desocupado ~ 'Dummy desocupado',
                           bin_inactivo ~ 'Dummy inactivo' ,
                           bin_ingresoAdicional_horasExtra ~ 'Dummy ingreso adicional horas extra',
                           bin_ingresoAdicional_prima ~ 'Dummy ingreso adicional prima',
                           bin_ingresoAdicional_alimentacion ~ 'Dummy ingreso adicional alimentación' ,
                           bin_ingresoAdicional_transporte ~ 'Dummy ingreso adicional transporte', 
                           bin_ingresoAdicional_auxilio ~ 'Dummy ingreso adicional auxilio',
                           bin_ingresoAdicional_educacion ~ 'Dummy ingreso adicional educación' ,
                           bin_ingresoAdicional_realEstate ~ 'Dummy ingreso adicional alquileres',
                           bin_ingresoAdicional_rendimientosFinancieros ~ 'Dummy ingreso adicional rendimientos financieros',
                           bin_ingresoAdicional_transferenciasInternacionales ~ 'Dummy ingreso adicional transferencias internacionales',
                           bin_ingresoAdicional_ayudasGobierno ~ 'Dummy ingreso adicional ayudas Gobierno',
                           bin_ingresoAdicional_rendimientosFinancieros2 ~ 'Dummy otros ingresos financieros',
                           cat_tamanoEmpresa ~ 'Tamaño de empresa', 
                           num_experiencia ~ 'Años de experiencia'         ),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "({min}, {max})"),
                               all_categorical() ~ "{n}  ({p}%)"),  #  / {N}
              missing_text = "(Valores faltantes)", 
              sort = all_categorical() ~ "alphanumeric") |> 
  add_stat_label(label = list(all_categorical() ~ "",
                              all_continuous() ~ c("Promedio (Desviación std)",
                                                   "Mínimo y máximo"))) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() 

gtsave(as_gt(estadistica_descriptiva2), 
       filename="views/estadisticaDescriptiva2.tex")
