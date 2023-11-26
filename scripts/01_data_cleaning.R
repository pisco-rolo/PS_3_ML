# 1| Importar -------------------------------------------------------------
# Importamos la base de datos de entrenamiento y de evaluación.
data_hog <- read.csv(file = paste0(directorioDatos, 'train_hogares.csv'))
data_kaggle_hog <- read.csv(file = paste0(directorioDatos,'test_hogares.csv'))

data_p <- read.csv(file = paste0(directorioDatos, 'train_personas.csv'))
data_kaggle_p <- read.csv(file = paste0(directorioDatos,'test_personas.csv'))

summary(data_hog$Ingpcug)

# 33024 hogares pobres 
table(data_hog$Pobre)

table(data_hog$Npobres)
table(data_hog$Depto)


# Factor de expansión departamental
table(data_hog$Fex_dpto)

table(data_hog$Clase)
table(data_hog$Dominio)

#----------------------------------------------------------------------------
# Variables que estan en train y no test
# HOGAR
names(data_hog)
names(data_kaggle_hog)
diff_vars_hog <- base::setdiff(names(data_hog), names(data_kaggle_hog))
diff_vars_hog
# "Ingtotug"    "Ingtotugarr" "Ingpcug"  "Pobre"       "Indigente"   "Npobres"     "Nindigentes"



# PERSONA
names(data_p)
names(data_kaggle_p)
#conflicted::conflicts_prefer(base::setdiff)
diff_vars_persona <- base::setdiff(names(data_p), names(data_kaggle_p))
diff_vars_persona

# Basicamente se borraron todas las variables cuantitativas de algún tipo de ingreso.

#----------------------------------------------------------------------------
#
table(data_hog$Npersug)

data_merged_train <- left_join(data_hog, data_p, by = c("id", "Clase", "Dominio") )

# Predicción final, según documento será a nivel de hogar. Si son pobres o no. Y, predecir su ingreso monetario. 
# Pero información sobre todo está en individuos. 

# crear variables de región de residencia: andina, pacifico, caribe, amazonia, Bogota
# Cabera
table(data_kaggle_p$P6050)


#------------------------------------
# Caracterización
# P6020: Sexo
# 

# P6040: años cumplidos
# P6050: Parentezco con jefe de hogar
# P6090: Cotizante de alguna entidad seguridad social
# P6100: regímen de seguridad social afiliado // no importa

# P6210: Nivel educativo más alto aprobado
# P6210s1: Grado escolar aprobado


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Dummy de jefe de hogar mujeres 
#data_p$jef_hog_mujer <- ifelse(data_p$P6050 == 1 & data_p$P6020 == 2 , 1 , 0) 

data_p <- data_p %>% 
  group_by(id) %>%
  mutate( jef_hog_mujer = ifelse(any(P6050 == 1 & P6020 == 2) , 1 , 0)  )


# explorando esposos hombres y no jefes de hogar
table(data_p$jef_hog_mujer)

table(data_p$P6050[data_p$P6020 == 1] )
table(data_p$P6050[data_p$P6020 == 2] )  
# c. hijo d.nieto  #92k tienen hijo


# dummy de hombres que son pareja del jefe de hogar mujer
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(pareja_nojef_hombre = ifelse(any(P6050==2 & P6020==1) , 1 ,0)  )

table(data_p$pareja_nojef_hombre[data_p$jef_hog_mujer==1])

# Dummy de personas menores de 18 años en el hogar
data_p$menores18 <- ifelse(data_p$P6040 <= 18  , 1 , 0) 

data_p <- data_p %>% 
  group_by(id) %>%
  mutate(bin_menores18 =  ifelse(any(menores18 == 1 ) , 1 , 0) )

table(data_p$menores18)

# dummy de madre soltera (mujer jefa de hogar sin pareja hombre, ojo: sin considerar si hay menores en la casa)
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(madre_soltera = ifelse(any(jef_hog_mujer == 1 & pareja_nojef_hombre == 1) , 1 , 0))

table(data_p$madre_soltera) 
# comprobamos que todas las mujeres jefe de hogar no tienen una pareja hombre 

#////////////////////////////////////////////////////////////////////
# dummy de madre soltera y le agregamos hijos
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(madre_soltera = ifelse(any(jef_hog_mujer == 1 & pareja_nojef_hombre == 1 & menores18 == 1) , 1 , 0))

table(data_p$madre_soltera)  # solo hay 44959 observaciones de madres solteras con menores de 18 en la casa
#////////////////////////////////////////////////////////////////////

#edad promedio de los miembros del hogar. 

data_p <- data_p %>% 
  group_by(id) %>%
  mutate( mean_years_hog = round( mean(P6040 , na.rm = TRUE), 1) )             

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

#------------------------------------
# Oficio

# Si el jefe de hogar no tiene empleo
# Si la mayoria de adultos no tienen empleo --> usar  P6240: actividad principal 

# P6426: tiempo trabajando en actividad principal
# P6430: Rol en actividad principal

#-----------------------------------------
# Preguntas dummy de algún tipo de ingreso:
# Recibió ingresos por horas extras: P6510
# Recibió primas: P6545
# Recibió bonificaciones: P6580

#------------------------------------------------------- SUBSIDIOS
# Recibió Auxilio o subsidio de alimentación: P6585s1
# Recibió Auxilio de subsidio de transporte:  P6585s2
# Recibió Subsidio familiar: P6585s3
# Recibió subsidio educativo: P6585s4

# # P7510s3  c. ayudas en dinero de instituciones del país     

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Dummy si recibió alguno de los subsidios 
data_p <- data_p %>%
  mutate(un_subsidio = ifelse(P6585s1 == 1 | P6585s2 == 1 | P6585s3 == 1 | P6585s4 == 1 | P7510s3 == 1  , 1, 0 )) %>%
    mutate(un_subsidio = ifelse(is.na(un_subsidio), 0 , un_subsidio ))

# Sumo las dummys de al menos un subsidio
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(sum_un_subsidio = sum(un_subsidio)  )  ## Falta dividir por número de miembros hogar 

table(data_p$P6585s1)
table(data_p$P7510s3)

# convertiremos datos de subsidios a dummy 0 - 1 para facilitar la suma
data_p <- data_p %>%
  mutate(P6585s1 = ifelse(P6585s1 == 2 | P6585s1 == 9 | is.na(P6585s1) , 0 , P6585s1 )) %>%
  mutate(P6585s2 = ifelse(P6585s2 == 2 | P6585s2 == 9 | is.na(P6585s2) , 0 , P6585s2 )) %>%
  mutate(P6585s3 = ifelse(P6585s3 == 2 | P6585s3 == 9 | is.na(P6585s3) , 0 , P6585s3 )) %>%
  mutate(P6585s4 = ifelse(P6585s4 == 2 | P6585s4 == 9 | is.na(P6585s4) , 0 , P6585s4 )) %>%
  mutate(P7510s3 = ifelse(P7510s3 == 2 | P7510s3 == 9 | is.na(P7510s3) , 0 , P7510s3 ))


data_p <- data_p %>%
  mutate(multi_subsidios = (P6585s1 + P6585s2 +  P6585s3 + P6585s4 + P7510s3 ))

data_p <- data_p %>% 
  group_by(id) %>%
  mutate(tot_subsidios = sum(multi_subsidios))   ## Falta dividir por número de miembros hogar 

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#--------------------------------------------------------------------
# Pet: población en edad de trabajar  1:sí
# Oc: ocupado 1:sí
# Des: desocupado 1:sí
# Ina: Inactivo 1:sí
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Proporción de la familia que trabaja (incluyendo mayores a 12 años)
data_p$Pet <- ifelse(is.na(data_p$Pet), 0 , data_p$Pet)
table(data_p$Pet)

# Proporción de la familia que trabaja (excluyendo a mayores de 18 años)
data_p$Pet_mayores <- ifelse(data_p$Pet == 1 & data_p$P6040 >= 18 , 1 , 0)
#///////////////////////////////////////////////////////////////////////////////// ojo acá podría usar "Ocupados" Oc
data_p$Oc <- ifelse(is.na(data_p$Oc), 0 , data_p$Oc)
table(data_p$Oc,data_p$P6240)  ## Todos las personas "Trabajando" están como ocupados. 

data_p <- data_p %>%
  group_by(id) %>%
  mutate(total_pet_mayores = sum(Pet_mayores)) %>%
  mutate(total_pet12 = sum(Pet))             

# Cotiza actualmente en fondo de pensiones: P6920
# Proporción de personas que tienen un trabajo formal hoy (proxy: cotizan hoy fondo de pensión)
table(data_p$P6920)
# Quitaremos a los ya pensionados, pues en Colombia 1 de cada 4 mayores accede a pensión.
data_p$P6920 <- ifelse(data_p$P6920 == 2 | data_p$P6920 == 3 | is.na(data_p$P6920) , 0 , 1 )

# Sumamos miembros por hogar que cotizan
data_p <- data_p %>%
  group_by(id) %>%
  mutate(tot_cotizan = sum(P6920))


# ---------------------------
# Personas ocupadas
table(data_p$Oc)
data_p$working <-  ifelse(data_p$Oc == 1 , 1 , 0) 
data_p$working <- ifelse(is.na(data_p$working) , 0 , data_p$working )
# total de personas que trabajan en la familia
data_p <- data_p %>%
  group_by(id) %>%
  mutate(tot_workers = sum(working, na.rm = TRUE))

# Total de dependientes en el hogar. Sumaremos a los menores de 18 años en el hogar
summary(data_p$menores18)

data_p <- data_p %>%
  group_by(id) %>%
  mutate(tot_menores = sum(menores18, na.rm = TRUE)) 

#summary(data_p$prop_dep_workers)
table(data_p$tot_workers)


# Proporción de personas dependientes vs personas con trabajos en el hogar
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(prop_dep_workers = tot_menores / tot_workers )
summary(data_p$prop_dep_workers) #ojo: 33704 NA's  y hay mean "Inf"
## este dato_ de prop_dep_workers hace que se pierdan observaciones del resto de variables al utilizar aggregate
# ---------------------------


##//////////////////////////////////
# crear una dummy de trabajador_actual <- que sea 1 si la persona está ocupada o está "trabajando" como actividad principal"





# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Variables creadas a nivel hogar que me quedo
# jef_hog_mujer   madre_soltera  mean_years_hog  prop_dep_workers

#Variables que faltan dividir por el número de miembros del hogar
# sum_un_subsidio tot_subsidios  total_pet_mayores total_pet12  tot_cotizan  tot_workers


# agregamos los datos
data_p_ag <- aggregate(cbind(jef_hog_mujer, madre_soltera, mean_years_hog,  # quito esta prop_dep_workers para no perder obs
                             sum_un_subsidio, tot_subsidios,  total_pet_mayores, total_pet12,
                             tot_cotizan, tot_workers) ~ id, data = data_p, FUN = mean  ) # mean no afecta porque ya está agregado

# merge de datos de hogar y personas
data_hog <- merge(data_hog, data_p_ag, by = "id", all = TRUE)

# -----------------------------------------------------------------------------
# generación de datos pendientes por miembros del hogar

data_hog <- data_hog %>%
  mutate(prop_un_subsidio = sum_un_subsidio / Nper ) %>%
  mutate(prop_multi_subsidios = tot_subsidios / Nper ) %>%
  mutate(prop_pet_mayores = total_pet_mayores / Nper) %>%
  mutate(prop_pet12 =  total_pet12 / Nper) %>%
  mutate(prop_cotizan = tot_cotizan / Nper) %>%
  mutate(prop_workers = tot_workers / Nper)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# Fex_c: Factor de expansión anualizado
# Dpto: Departamento
# Fex_dpto: Factor de expansión departamental


#----------------------------------------------------------
# HOGAR
# P5000 

# P5010: cuartos que pueden dormir
# CALCULAR--> número de personas que duermen por cuarto
data_hog$per_bedrooms <- data_hog$P5010 / data_hog$Nper

# P5090: si es propietaria de sus propia casa. Usar dummys.  // 
# P5100: cuanto pagas por amortización //no incluir

# Estimación consolidada de arriendo aproximada, considera las dos variables:
# P5130: estimación si tuviera que pagar arriendo //
# P5140: estimación del arriendo  // SI queda. 
summary(data_hog$P5130)
data_hog$tot_arriendo <- ifelse(is.na(data_hog$P5130), data_hog$P5140, data_hog$P5130 )
summary(data_hog$tot_arriendo)


# Nper sí
# Npersug // NO VA 
# dummy de departamento

#--------------------------------------------------------
# construcción de la variable dependiente para predecir ingresos a nivel per capita (unidad de gasto)
# construir  Ingtotug  + (reciben ingreso) arriendo / Nper

# en caso  ingtotutgarr - ingtotug > 0 , ingtotutgarr, ingtotug   
sum(data_hog$Ingtotugarr - data_hog$Ingtotug == 0)

sum(data_hog$ingtotutgarr) # QUEDA ESTA VARIABLE PARA PREDECIR.


names(data_hog)



# Limitamos el df a las variables que usaremos y las renombramos
data_hog <- data_hog[, c('id', 'Clase', 'Dominio', 'Ingtotugarr',
             'prop_multi_subsidios', 'prop_pet_mayores', 'prop_pet12', 
             'prop_cotizan', 'prop_workers', 'per_bedrooms', 'tot_arriendo')]

# Definimos los nombres de las columnas tal y como trabajaremos en el futuro.
nombres_variables   <- c('id_hogar', 'cat_clase', 'cat_dominio', 
                         'num_ingtotutgarr', 'nper_subsidios',
                         'nper_pet_mayores', 'nper_pet12', 'nper_cotizan',
                         'nper_workers', 'nper_bedrooms', 'nper_arriendo')

colnames(data_hog) <- nombres_variables








