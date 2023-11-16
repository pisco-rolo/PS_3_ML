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
# Variables que podriamos extraer de Personas


#----------------------------------------------------------------------------
# Variables que estan en train y no test
# HOGAR
names(data_hog)
names(data_kaggle_hog)
diff_vars_hog <- setdiff(names(data_hog), names(data_kaggle_hog))
diff_vars_hog
# "Ingtotug"    "Ingtotugarr" "Ingpcug"  "Pobre"       "Indigente"   "Npobres"     "Nindigentes"



# PERSONA
names(data_p)
names(data_kaggle_p)
#conflicted::conflicts_prefer(base::setdiff)
diff_vars_persona <- setdiff(names(data_p), names(data_kaggle_p))
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


table(data_p$P6050)
table 

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Dummy de jefe de hogar mujeres 
#data_p$jef_hog_mujer <- ifelse(data_p$P6050 == 1 & data_p$P6020 == 2 , 1 , 0) 

data_p <- data_p %>% 
  group_by(id) %>%
  mutate( jef_hog_mujer = ifelse(P6050 == 1 & P6020 == 2 , 1 , 0)  )


# explorando esposos hombres y no jefes de hogar
table(data_p$jef_hog_mujer)

table(data_p$P6050[data_p$P6020 == 1] )
table


# dummy de hombres que son pareja del jefe de hogar mujer
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(pareja_nojef_hombre = ifelse(P6050==2 & P6020==1 , 1 ,0)  )

table(data_p$pareja_nojef_hombre[data_p$jef_hog_mujer==0])


# Dummy de personas menores de 18 años en el hogar
data_p$menores18 <- ifelse(data_p$P6040 <= 18  , 1 , 0) 

data_p <- data_p %>% 
  group_by(id) %>%
  mutate(menores18 =  ifelse(menores18 == 1  , 1 , 0) )

table(data_p$menores18)


# dummy de madre soltera (mujer jefa de hogar sin pareja hombre, ojo: sin considerar si hay menores en la casa)
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(madre_soltera = ifelse(jef_hog_mujer == 1 & pareja_nojef_hombre == 0 , 1 , 0))

table(data_p$madre_soltera) 
# comprobamos que todas las mujeres jefe de hogar no tienen una pareja hombre 


#////////////////////////////////////////////////////////////////////
# dummy de madre soltera y le agregamos hijos
data_p <- data_p %>% 
  group_by(id) %>%
  mutate(madre_soltera = ifelse(jef_hog_mujer == 1 & pareja_nojef_hombre == 0 & menores18 == 1 , 1 , 0))

table(data_p$madre_soltera)  # solo hay 344 observaciones de madres solteras con menores de 18 en la casa
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

# pendiente. DATO SOBRE EL HOGAR
#--------------------------------------------------------------------



# Además de salario, recibió alimentos como parte de pago: P6590
# Además de salario, recibió vivienda como parte de pago: P6600
# Utiliza transporte de la empresa para trasladarse: P6610
# Además de salario, recibió ingresos en especie: P6620


##
# Emprendimiento,cuántas horas a la semana trabaja normalmente en ese trabajo: P6800

# Cotiza actualmente en fondo de pensiones: P6920 ---- //// suma de dummys / miembros del hogar
#------------------------------------- Suma de dummy / miembro que trabajan 
# ------------------------------------ Suma de dummy / miembros en edad de trabajar 

# SUMA Personas dependientes / numero de personas que trabajan 

# 

# 

# Actividad secundaria. Otro trabajo o negocio: P7040
# Num horas trabajo en actividad secundaria: P7045
# Rol en esa actividad secundaria: P7050

# quiere trabajar más horas? P7090
# P7110
# P7120
# P7150 cambiar trabajo
# P7160 poder empezar nuevo trabajo

# P7310 ha buscado trabajo o ha trabajo al menos dos semanas
# (preguntas para desocupados) #------------
# P7350 su rol en este trabajo)
# P7422 y P7472 (pgtas idénticas) recibió o ganó el mes pasado ingresos por trabajo?
# ------------

#----
# P7495 Recibió pagos por arriendos y/o pensiones 
# P7500s2  recibió b. pensiones o jubilación
# P7500s3  recibió c. pensión alimenticia por paternidad, divorcio,etc
#----

#---------------
# P7505 En último año, recibió dinero de otros hogares, personas, instituciones gub, por intereses, dividendos
# utilidades o por cesantías? 
# # P7510s1  a. dinero de otros hogares residentes en el país
# # P7510s2  b. dinero de otros hogares residentes fuera del país

# # P7510s3  c. ayudas en dinero de instituciones del país           ////si

# # P7510s5  d. dinero por intereses de préstamos o CDT's
# # P7510s6  e. dinero por concepto de cesantías
# # P7510s7  f. dinero de otras fuentes diferentes a las anteriores
#---------------

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

data_p <- data_p %>%
  group_by(id) %>%
  mutate(total_pet_mayores = sum(Pet_mayores)) %>%
  mutate(total_pet12 = sum(Pet))             



#Variables que faltan dividir por el número de miembros del hogar
# sum_un_subsidio tot_subsidios  total_pet_mayores total_pet12

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
# P5090: si es propietaria de sus propia casa. Usar dummys.  // 

# P5100: cuanto pagas por amortización //no incluir

# consolidar estas dos variables en una
# P5130: estimación si tuviera que pagar arriendo //
# P5140: estimación del arriendo  // SI queda. 

# Nper sí
# Npersug // NO VA 

# 
summary(data_hog$Li)
summary(data_kaggle_hog$Li)

summary(data_hog$Lp)
summary(data_kaggle_hog$Lp)

sort(unique(data_kaggle_hog$Lp))

# dummy de departamento


# cómo agregar los datos? a nivel de porcentajes por miembros del hogar

#--------------------------------------------------------
# construcción de la variable dependiente para predecir ingresos a nivel per capita (unidad de gasto)
# construir  Ingtotug  + (reciben ingreso) arriendo / Nper

# en caso  ingtotutgarr - ingtotug > 0 , ingtotutgarr, ingtotug   
sum(data_hog$Ingtotugarr - data_hog$Ingtotug == 0)



sum(data_hog$ingtotutgarr) # QUEDA ESTA VARIABLE PARA PREDECIR.
