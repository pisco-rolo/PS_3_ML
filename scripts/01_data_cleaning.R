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
# Estrato1 , 

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


data_merged_train <- left_join(data_hog, data_p, by = c("id", "Clase", "Dominio") )

# Predicción final, según documento será a nivel de hogar. Si son pobres o no. Y, predecir su ingreso monetario. 
# Pero información sobre todo está en individuos. 

# crear variables de región de residencia: andina, pacifico, caribe, amazonia, Bogota
# Cabera
table(data_kaggle_p$P6050)


# Caracterización
# P6020: Sexo
# P6040: años cumplidos
# P6050: Parentezco con jefe de hogar
# P6090: Cotizante de alguna entidad seguridad social
# P6100: regímen de seguridad social afiliado
# P6210: Nivel educativo más alto aprobado
# P6210s1: Grado escolar aprobado


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
# Recibió Auxilio o subsidio de alimentación: P6585s1
# Recibió Auxilio de subsidio de transporte:  P6585s2
# Recibió Subsidio familiar: P6585s3
# Recibió subsidio educativo: P6585s4
# Además de salario, recibió alimentos como parte de pago: P6590
# Además de salario, recibió vivienda como parte de pago: P6600
# Utiliza transporte de la empresa para trasladarse: P6610
# Además de salario, recibió ingresos en especie: P6620


##
# Emprendimiento,cuántas horas a la semana trabaja normalmente en ese trabajo: P6800
# Cotiza actualmente en fondo de pensiones: P6920
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
# # P7510s3  c. ayudas en dinero de instituciones del país
# # P7510s5  d. dinero por intereses de préstamos o CDT's
# # P7510s6  e. dinero por concepto de cesantías
# # P7510s7  f. dinero de otras fuentes diferentes a las anteriores
#---------------

# Pet: población en edad de trabajar  1:sí
# Oc: ocupado 1:sí
# Des: desocupado 1:sí
# Ina: Inactivo 1:sí



# Fex_c: Factor de expansión anualizado
# Dpto: Departamento
# Fex_dpto: Factor de expansión departamental











# cómo agregar los datos? a nivel de porcentajes por miembros del hogar
