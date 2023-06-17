library(ggplot2)
#library(DMwR)
#library(devtools)
#library(corrplot)
library(PerformanceAnalytics)
library(nortest)
library(randomForest)
library(caret)
library(rpart)
library(dplyr)
library(cowplot)
library(grid)
library(fastICA)

data <- read.csv('/home/jesus/Bankruptcy-Prediction/Data/data_resampled.csv')
# Identificar columnas constantes
constant_columns <- apply(data, 2, function(x) var(x) == 0)

# Eliminar columnas constantes
data <- data[, !constant_columns]

#Analisis de componenetes Independientes
data_std <- scale(data)

ica_result <- fastICA(data_std, n.comp = 3)
ica_result$X
