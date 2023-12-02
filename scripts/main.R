# Predicting poverty -------------------------------------------------------
# Autores: Rojas, J., Obando, J.C. & Zegarra, D.
# Materia: Big Data and Machine Learning para Economía Aplicada
# Año: 2023

# 1| Preparacion ----------------------------------------------------------
rm(list = ls())
graphics.off()
options(scipen=999)
set.seed(123)                       # Replicabilidad en las simulaciones.

# 1.1| Librerias ----------------------------------------------------------
librerias    <- c('here', 'tidyverse', 'tidymodels', 'conflicted', 'xtable',
                  'gtsummary', 'gt', 'stringr', 'extrafont', 'xgboost',
                  'lightgbm', 'bonsai', 'vip', 'randomForest', 'rpart', 
                  'baguette', 'themis', 'doParallel')
noInstaladas <- librerias[!(librerias %in% rownames(installed.packages()))]

if(length(noInstaladas)){
  install.packages(noInstaladas)
}

invisible(sapply(librerias, library, character.only = TRUE, quietly = TRUE))
loadfonts(device = 'win') # Carga las fuentes de Windows. Primero ejecutar font_import().
conflict_prefer(name = 'filter', winner = 'dplyr')
conflict_prefer(name = 'slice', winner = 'dplyr')
conflict_prefer(name = 'spec', winner = 'yardstick')
conflict_prefer(name = 'step', winner = 'recipes')
conflict_prefer(name = 'setdiff', winner = 'base')
conflict_prefer(name = 'starts_with', winner = 'tidyselect')

# 1.2| Directorio ---------------------------------------------------------
directorioPrincipal  = enc2native(here())
directorioCodigo     = paste0(directorioPrincipal, '/scripts/')
directorioDatos      = paste0(directorioPrincipal, '/stores/')
directorioResultados = paste0(directorioPrincipal, '/views/')
setwd(directorioPrincipal)

# 1.3| Funciones ----------------------------------------------------------
source(paste0(directorioCodigo, 'functions/cleaning.R'), encoding = 'UTF-8')

# 2| Resultados -----------------------------------------------------------
# Al definir 'primeraVez := TRUE', se realiza el análisis desde ceros. 
primeraVez <- FALSE
source(paste0(directorioCodigo, '01_data_cleaning.R'), encoding = 'UTF-8')
source(paste0(directorioCodigo, '02_classification.R'), encoding = 'UTF-8')
source(paste0(directorioCodigo, '03_regression.R'), encoding = 'UTF-8')
