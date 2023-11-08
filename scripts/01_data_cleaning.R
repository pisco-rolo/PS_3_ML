# 1| Importar -------------------------------------------------------------
# Importamos la base de datos de entrenamiento y de evaluación.
dataset_hog <- read.csv(file = paste0(directorioDatos, 'train_hogares.csv'))
dataset_kaggle_hog <- read.csv(file = paste0(directorioDatos,'test_hogares.csv'))

dataset_p <- read.csv(file = paste0(directorioDatos, 'train_personas.csv'))
dataset_kaggle_p <- read.csv(file = paste0(directorioDatos,'test_personas.csv'))

