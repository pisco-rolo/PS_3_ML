#' Limpieza de los datos a nivel individual
#'
#' @param .dataset Dataframe que contiene los datos a nivel individual.
#' @param .old_var Vector de cadenas de texto que contiene los nombres de las
#' variables que van a ser empleadas durante el análisis.
#' @param .new_var Vector de cadenas de texto que contiene los nombres que van
#' a ser asignados a las variables que serán empleadas durante el análisis.
#'
#' @return Retorna una lista con dos objetos. El primero, un dataframe que 
#' contiene las variables de interés a nivel individual, así como la limpieza 
#' de datos de cada uno de ellos. El segundo, un dataframe que contiene las
#' variables de interés a nivel de hogar, así como la limpieza de datos de
#' cada uno de ellos.
data_cleaning_personas <- function(.dataset, .old_var, .new_var) {
  # .dataset <- data_kaggle_p
  
  # Notas.
  # - El estrato no aparece. Debería llamarse 'estrato1'.
  # - Como el ingreso contempla dimensiones más allá de la laboral, no vamos a
  #   utilizar el ingreso por hora, sino el ingreso total para la modelación.
  #   Sin embargo, bastaría con agregar la variable 'p6800'.
  # .old_var <- c('id', 'ingtotes',  'p6020', 'p6040', 'p6050', 'p6090', 'p6210', 
  #               'p6210s1', 'p6430', 'oc', 'des', 'ina', 'p6510', 'p6545', 
  #               'p6585s1', 'p6585s2', 'p6585s3', 'p6585s4', 'p7495', 'p7505', 
  #               'p7510s2', 'p7510s3', 'p7510s5', 'p6870')
  # .new_var <- c('id_hogar', 'num_ingreso_individual', 'bin_mujer', 'num_edad', 
  #               'cat_parentesco', 'bin_cotizante', 'cat_educacion', 'num_educacion', 
  #               'cat_ocupacion', 'bin_ocupado', 'bin_desocupado', 
  #               'bin_inactivo', 'bin_ingresoAdicional_horasExtra',
  #               'bin_ingresoAdicional_prima', 'bin_ingresoAdicional_alimentacion',
  #               'bin_ingresoAdicional_transporte', 'bin_ingresoAdicional_auxilio',
  #               'bin_ingresoAdicional_educacion', 'bin_ingresoAdicional_realEstate',
  #               'bin_ingresoAdicional_rendimientosFinancieros',
  #               'bin_ingresoAdicional_transferenciasInternacionales',
  #               'bin_ingresoAdicional_ayudasGobierno',
  #               'bin_ingresoAdicional_rendimientosFinancieros2',
  #               'cat_tamanoEmpresa')
  
  # Características generales -----------------------------------------------
  # - Estrato1: Estrato socioeconómico.
  # - P6020: Binaria. Sexo.
  # - P6040: Numérica. Años cumplidos.
  # - P6050: Categórica. Parentesco con el jefe o jefa del hogar.
  # - P6090: Binaria. Cotizante de seguridad social.
  # - P6210: Categórica. Nivel educativo más alto aprobado.
  # - P6210s1: Numérica. Número de años escolares aprobados.
  # - P6430: Categórica. Tipo de trabajo.
  
  # Estado laboral ----------------------------------------------------------
  # - Oc: Binaria. Ocupado. Recomiendo usar P6240 (variable categórica que indica 
  #   la actividad principal) para complementar en los casos en que no contamos
  #   con información disponible.
  # - Des: Binaria. Desocupado.
  # - Ina: Binaria. Inactivo.
  
  # Ingresos ----------------------------------------------------------------
  # La más recomendada es ingtotes, pues tiene el ingreso total después de la
  # imputación de múltiples dimensiones -más allá de la salarial-. 
  # - Ingtotes: Numérica. Ingreso total después de la imputación.
  # - P6500: Numérica. Ingreso salarial.
  # - P6800: Numérica. Número de horas trabajadas en actualmente.
  
  # A continuación incluimos variables que no se encuentran en la base de datos
  # de evaluación, pero sí en la de entrenamiento, para poder ejecutar el modelo
  # estimado sobre la base de datos de evaluación y estimar el ingreso de cada
  # persona.
  # - P6510: Binaria. Ingreso adicional por horas extra.
  # - P6545: Binaria. Ingreso adicional por prima.
  # - P6585s1. Binaria. Auxilio o subsidio de alimentación.
  # - P6585s2. Binaria. Auxilio de subsidio de transporte. Este es especialmente
  #   importante en tanto se deja de percibir subsidio de transporte a partir de 
  #   cierto ingreso.
  # - P6585s3. Binaria. Auxilio familiar.
  # - P6585s4. Binaria. Subsidio educativo.
  # - P7495: Binaria. Ingreso adicional por arriendos y/o pensiones.
  # - P7505: Binaria. Ingreso adicional por rendimientos financieros.
  # - P7510s2: Binaria. Ingreso adicional por transferencias internacionales.
  # - P7510s3: Binaria. Ingreso adicional de instituciones gubernamentales.
  # - P7510s5: Binaria. Ingreso adicional por otros rendimientos financieros.
  # - P6870. Categórica. Tamaño de la empresa.

  # 1| Limpieza -------------------------------------------------------------
  # 1.1| Selección de variables ---------------------------------------------
  # Permitimos el 'any_of()' para que no haya problemas en caso que alguna base
  # de datos no cuente con los datos.
  colnames(.dataset) <- tolower(colnames(.dataset))
  .dataset <- .dataset |> select(any_of(.old_var)) 
  
  # Detectamos con qué variables contamos para hacer el renombre, pues datos
  # de la base de datos de evaluación no cuentan con ingresos.
  .new_var <- .new_var[.old_var %in% colnames(.dataset)]
  .old_var <- .old_var[.old_var %in% colnames(.dataset)]
  .dataset <- .dataset |> rename(!!!setNames(.old_var, .new_var))
  
  # 1.2| Codificación de variables ------------------------------------------
  .dataset <- .dataset |> 
    mutate(bin_mujer = case_when(bin_mujer == 1 ~ 0L,
                                 bin_mujer == 2 ~ 1L,
                                 TRUE ~ NA_integer_),
           cat_parentesco = factor(cat_parentesco, 
                                   levels = 1:9, 
                                   labels = c('Jefe del hogar', 'Pareja',
                                              'Hijo', 'Nieto', 'Otro pariente',
                                              'Empleado doméstico', 
                                              'Pensionista', 'Empleado no-doméstico',
                                              'No-pariente')),
           bin_cotizante = case_when(bin_cotizante == 1 ~ 1L,
                                     bin_cotizante == 2 ~ 0L,
                                     bin_cotizante == 9 ~ NA_integer_,
                                     TRUE ~ NA_integer_),
           cat_educacion = factor(cat_educacion,
                                  levels = c(1:6, 9),
                                  labels = c('Ninguno', 'Preescolar',
                                             'Primaria', 'Secundaria',
                                             'Media', 'Universitaria',
                                             'Ninguno')),
           cat_ocupacion = factor(cat_ocupacion,
                                  levels = 1:9,
                                  labels = c('Empresa particular', 'Empresa gubernamental',
                                             'Empleado doméstico', 'Emprendedor',
                                             'Empleador', 'Trabajador familiar',
                                             'Trabajador sin remuneración',
                                             'Jornalero', 'Otro')),
           bin_ocupado = case_when(bin_ocupado == 1 ~ 1L,
                                   TRUE ~ 0L),
           bin_desocupado = case_when(bin_desocupado == 1 ~ 1L,
                                      TRUE ~ 0L),
           bin_inactivo = case_when(bin_inactivo == 1 ~ 1L,
                                    TRUE ~ 0L),
           cat_tamanoEmpresa = factor(cat_tamanoEmpresa, 
                                      levels = 1:9, 
                                      labels = c('1 persona', '2-3 personas',
                                                 '4-5 personas', '6-10 personas',
                                                 '11-19 personas', '20-30 personas',
                                                 '31-50 personas', '51-100 personas',
                                                 '+101 personas'))) |> 
    mutate(across(starts_with('bin_ingresoAdicional_'), ~ case_when(. == 1 ~ 1L,
                                                                    . == 2 ~ 0L,
                                                                    . == 9 ~ 0L,
                                                                    TRUE ~ 0L))) |>
    # - Si una persona no reporta que cotiza, imputamos cero.
    # - La educación puede imputarse por la edad. Por ejemplo, si una persona no
    #   tiene más de 5 años, imputamos 'ninguno'.
    # - Si no reporta su ocupación, creamos una nueva variable que indica que
    #   no la reportó.
    # - Si no se reporta el tamaño de la empresa, es porque la persona no está
    #   trabajando.
    # TODO. Posiblemente, al momento de hacer el one-hot-encoding, la categoría
    # de 'no trabaja' en el tamaño de la empresa, al ser idéntica a la variable
    # de 'bin_desocupado', generará problemas de colinealidad.
    replace_na(replace = list(bin_cotizante = 0, 
                              cat_educacion = as.factor('Ninguno'))) |> 
    mutate(cat_ocupacion = fct_na_value_to_level(cat_ocupacion, 'No reporta'),
           cat_tamanoEmpresa = fct_na_value_to_level(cat_tamanoEmpresa, 'No trabaja')) |> 
    # La variable de número de años estudiando está mal calculada para
    # universitarios. Es necesario sumarle 11 años previos si está en 
    # universidad. Así mismo, imputamos el valor promedio a los valores 
    # faltantes.
    mutate(num_educacion = case_when(cat_educacion == 'Universitaria' ~ num_educacion + 11,
                                     cat_educacion %in% c('Ninguno', 'Preescolar') ~ 0,
                                     TRUE ~ num_educacion)) |> 
    group_by(cat_educacion) |> 
    mutate(num_educacion = ifelse(test = is.na(num_educacion), 
                                  yes = mean(num_educacion, na.rm = TRUE), 
                                  no = num_educacion)) |> 
    ungroup()
  
  # 2| Creación de variables ------------------------------------------------
  # A continuación creamos variables. Las que guardamos en '.dataset' se 
  # encuentran a nivel de individuo, mientras que las guardadas en 
  # '.dataset_hogar' se encuentran a nivel de hogar.
  .dataset_hogar <- .dataset |> distinct(id_hogar)
  
  # Variables a nivel de hogar ----------------------------------------------
  # 2.1| Mujer jefa del hogar -----------------------------------------------
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                mutate(bin_mujer_y_jefaHogar = as.numeric(cat_parentesco == 'Jefe del hogar' & bin_mujer == 1)) |>
                                group_by(id_hogar) |> 
                                summarise(bin_mujer_y_jefaHogar = max(bin_mujer_y_jefaHogar)) |> 
                                select(c('id_hogar', 'bin_mujer_y_jefaHogar')),
                              by = 'id_hogar') 
  
  # 2.2| Madre soltera cabeza del hogar -------------------------------------
  # Primero es necesario determinar qué hogares viven con la pareja. Si un
  # hogar vive con la pareja, ya no aplica. Posteriormente, validamos si el
  # hogar tiene al menos un hijo menor de 18 años.
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                group_by(id_hogar) |> 
                                filter(any(cat_parentesco == 'Pareja')) |> 
                                distinct(id_hogar) |> 
                                mutate(bin_pareja = 1),
                              by = 'id_hogar') |> 
    # Si no se identificó que en el hogar hubiese una pareja, marcamos que el
    # jefe o jefa del hogar no tiene pareja.
    replace_na(list(bin_pareja = 0))
  
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                group_by(id_hogar) |> 
                                filter(any(cat_parentesco == 'Hijo')) |> 
                                filter(any(num_edad < 18)) |> 
                                distinct(id_hogar) |> 
                                mutate(bin_hijo = 1),
                              by = 'id_hogar') |> 
    # Si no se identificó que en el hogar hubiese un descendiente, marcamos que el
    # jefe o jefa del hogar no tiene hijo o hija.
    replace_na(list(bin_hijo = 0))
  
  # Con las dos variables previamente creadas, adicional a la de mujer jefa
  # del hogar, determinamos los hogares que son de madres solteras.
  .dataset_hogar <- .dataset_hogar |> 
    mutate(bin_madre_soltera = as.numeric(bin_mujer_y_jefaHogar == 1 & bin_pareja == 0 & bin_hijo == 1)) |> 
    select(-c('bin_pareja', 'bin_hijo'))
  
  # 2.3| Edad promedio de los integrantes del hogar -------------------------
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                group_by(id_hogar) |> 
                                summarise(num_edad_promedio = mean(num_edad, na.rm = TRUE)) |> 
                                select(c('id_hogar', 'num_edad_promedio')),
                              by = 'id_hogar')
  
  # 2.4| Proporción de la familia con al menos un subsidio ------------------
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                mutate(bin_un_subsidio = as.numeric(bin_ingresoAdicional_alimentacion == 1 | bin_ingresoAdicional_transporte == 1 | bin_ingresoAdicional_auxilio == 1 | bin_ingresoAdicional_educacion == 1 | bin_ingresoAdicional_ayudasGobierno == 1)) |> 
                                group_by(id_hogar) |> 
                                summarise(prop_hogarSubsidiado = sum(bin_un_subsidio, na.rm = TRUE)/n(),
                                          bin_hogar_ayudadoPorGobierno = max(bin_ingresoAdicional_ayudasGobierno, na.rm = TRUE)) |>
                                select(c('id_hogar', 'prop_hogarSubsidiado', 'bin_hogar_ayudadoPorGobierno')),
                              by = 'id_hogar')
  
  # 2.5| Proporción de la familia con múltiples subsidios -------------------
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                mutate(num_subsidios = bin_ingresoAdicional_alimentacion + bin_ingresoAdicional_transporte + bin_ingresoAdicional_auxilio + bin_ingresoAdicional_educacion + bin_ingresoAdicional_ayudasGobierno ) |> 
                                group_by(id_hogar) |> 
                                summarise(prop_hogarSubsidiado2 = sum(num_subsidios, na.rm = TRUE)/(5*n())) |>
                                select(c('id_hogar', 'prop_hogarSubsidiado2')),
                              by = 'id_hogar')
  
  # 2.6| Proporción de la familia que trabaja -------------------------------
  # El cálculo asume que un individuo menor de 18 años puede trabajar.
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                group_by(id_hogar) |> 
                                summarise(prop_empleados = mean(bin_ocupado, na.rm = TRUE)) |> 
                                select(c('id_hogar', 'prop_empleados')),
                              by = 'id_hogar')
  
  # El cálculo asume que un individuo menor de 18 años NO puede trabajar.
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                mutate(bin_ocupado = case_when(num_edad < 18 & bin_ocupado == 1 ~ 0,
                                                               TRUE ~ bin_ocupado)) |>
                                group_by(id_hogar) |> 
                                summarise(prop_empleados2 = mean(bin_ocupado, na.rm = TRUE)) |> 
                                select(c('id_hogar', 'prop_empleados2')),
                              by = 'id_hogar')
  
  # El cálculo asume que solo cuentan individuos que cotizan a salud y pensión.
  # TODO. Revisar el cálculo. Lo calculé con el número de personas que viven en
  # el hogar, pero no sé si deba ser con el número de personas que trabajan.
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                mutate(bin_ocupado = case_when(bin_cotizante == 0 & bin_ocupado == 1 ~ 0,
                                                               TRUE ~ bin_ocupado)) |>
                                group_by(id_hogar) |> 
                                summarise(prop_empleados3 = mean(bin_ocupado, na.rm = TRUE)) |> 
                                select(c('id_hogar', 'prop_empleados3')),
                              by = 'id_hogar')

  # 2.7| Indicador de dependencia -------------------------------------------
  # Cuando nadie en el hogar trabaja, el indicador se dispara a infinito. Por 
  # tanto, imputamos el máximo valor perteneciente a los reales en la base
  # de datos.
  .dataset_hogar <- left_join(x = .dataset_hogar,
                              y = .dataset |> 
                                group_by(id_hogar) |>
                                summarise(num_dependencia = (n() - sum(bin_ocupado, na.rm = TRUE))/sum(bin_ocupado, na.rm = TRUE)) |> 
                                select(c('id_hogar', 'num_dependencia')) |> 
                                ungroup() |> 
                                mutate(num_dependencia = case_when(num_dependencia == Inf ~ NA_real_,
                                                                   TRUE ~ num_dependencia)) |> 
                                mutate(num_dependencia = case_when(is.na(num_dependencia) ~ max(num_dependencia, na.rm = TRUE),
                                                                   TRUE ~ num_dependencia)),
                              by = 'id_hogar')

  # Variables a nivel individual --------------------------------------------
  # 2.8| Experiencia potencial ----------------------------------------------
  # Como no hay un indicador de experiencia, empleamos la experiencia potencial
  # y utilizamos la aproximación tanto lineal como cuadrática -por los efectos
  # marginales decrecientes de la experiencia-. Esta es calculada como:
  # max(edad - años de estudio - 5, 0)
  # Nota. Hay personas con mucha experiencia, por lo que ponemos un techo en 40
  # años de experiencia.
  .dataset <- .dataset |> 
    rowwise() |> 
    mutate(num_experiencia = max(c(num_edad - num_educacion - 5, 0))) |> 
    mutate(bin_temporal = as.numeric(num_experiencia >= 40)) |>
    mutate(num_experiencia = case_when(bin_temporal == 1 ~ 40,
                                       TRUE ~ num_experiencia)) |> 
    select(-c('bin_temporal')) |> 
    mutate(num_experiencia2 = num_experiencia^2)
  
  return(list(.dataset, .dataset_hogar))
}

#' Limpieza de los datos a nivel hogar
#'
#' @param .dataset Dataframe que contiene los datos a nivel hogar.
#' @param .old_var Vector de cadenas de texto que contiene los nombres de las
#' variables que van a ser empleadas durante el análisis.
#' @param .new_var Vector de cadenas de texto que contiene los nombres que van
#' a ser asignados a las variables que serán empleadas durante el análisis.
#'
#' @return Retorna un dataframe con los datos agregados a nivel de hogar y
#' con la variable de cuartos por persona creada.
data_cleaning_hogares <- function(.dataset, .old_var, .new_var) {
  # .dataset <- data_kaggle_hog
  # .old_var <- c('id', 'clase', 'dominio', 'p5010', 'npersug', 'ingtotugarr',
  #               'lp', 'pobre')
  # .new_var <- c('id_hogar', 'cat_zona', 'cat_ubicacion', 'num_cuartos', 
  #               'num_personas', 'num_ingreso_total', 'num_linea', 'bin_pobre')
  
  # Características del hogar -----------------------------------------------
  # - id: Cadena de texto. Identificador del hogar.
  # - Clase. Binaria. Identificador del tipo de ubicación. 
  #   Cabecera municipal o zona rural.
  # - Dominio. Categórica. Identificador de la zona geográfica donde se ubica.
  # - P5010. Numérica. Número de cuartos donde duermen los integrantes del hogar.
  # - Npersug. Numérica. Número de personas en la unidad de gasto.
  # - Ingtotugarr. Numérica. Ingreso total de la unidad de gasto después de la
  #   imputación.
  # - Lp. Línea de pobreza.
  # - Pobre. Variable objetivo.
  
  # Selección de variables --------------------------------------------------
  # Permitimos el 'any_of()' para que no haya problemas en caso que alguna base
  # de datos no cuente con los datos.
  colnames(.dataset) <- tolower(colnames(.dataset))
  .dataset <- .dataset |> select(any_of(.old_var)) 
  
  # Detectamos con qué variables contamos para hacer el renombre, pues datos
  # de la base de datos de evaluación no cuentan con ingresos.
  .new_var <- .new_var[.old_var %in% colnames(.dataset)]
  .old_var <- .old_var[.old_var %in% colnames(.dataset)]
  .dataset <- .dataset |> 
    rename(!!!setNames(.old_var, .new_var)) |> 
    mutate(num_cuartos_por_persona = num_cuartos/num_personas)
  
  return(.dataset)
}
