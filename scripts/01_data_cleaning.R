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


