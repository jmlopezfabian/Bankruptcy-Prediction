library(ggplot2)
library(PerformanceAnalytics)
library(nortest)
library(randomForest)
library(caret)
library(rpart)
library(dplyr)
library(cowplot)
library(grid)


#------Funciones------#
NamesVarInteger <- function(x){
  #Retorna un vector que contiene las variables de tipo integer
  names <- c()
  for(col in names(data)){
    if(is.integer(data[[col]]) == TRUE){
      names <- c(names,col)
    }
  }
  return(names)
}

NamesVarNumeric <- function(x){
  #Retorna un vector que contiene las variables distintas a integer
  names <- c()
  for(col in names(data)){
    if(is.numeric(data[[col]]) && !is.integer(data[[col]])){
      names <- c(names,col)
    }
  }
  return(names)
}

normalize_min_max <- function(data){
  if(!is.data.frame(data)){
    stop("El agumento debe de ser un DataFrame")
  }
  normalized_data <- data
  
  if(!all(sapply(data, is.numeric))) {
    stop("El DataFrame debe contener solo numericas.")
  }
  
  for (col in names(normalized_data)){
    min_val <- min(normalized_data[[col]])
    max_val <- max(normalized_data[[col]])
    normalized_data[[col]] <- (normalized_data[[col]] - min_val) / (max_val - min_val)
  }
  
  return (normalized_data)
}

normalize_z_score <- function(data) {
  normalized_data <- data
  
  for (col in names(normalized_data)) {
    if (is.numeric(normalized_data[[col]])) {
      mean_val <- mean(normalized_data[[col]], na.rm = TRUE)
      sd_val <- sd(normalized_data[[col]], na.rm = TRUE)
      normalized_data[[col]] <- (normalized_data[[col]] - mean_val) / sd_val
    }
  }
  
  return(normalized_data)
}

exportar_histogramas <- function(data, directorio){
  for(variable in names(data)){
    png(paste(directorio,"/", variable, ".png", sep = ""), width = 800, height = 600, res = 150)
    hist(data[[variable]], xlab = "Valor", ylab = "Frecuencia", main = variable)
    dev.off()
  }
}

exportar_boxplots <- function(data, directorio) {
  for (variable in names(data)) {
    if (is.numeric(data[[variable]])) {
      png(paste(directorio, "/", variable, "_boxplot.png", sep = ""), width = 800, height = 600, res = 150)
      boxplot(data[[variable]], main = variable, ylab = "Valor")
      dev.off()
    }
  }
}

exportar_GraficasUnivariable <- function(data, directorio, colores, pch = 16, cex = 1){
  variables <- names(data)
  if(!dir.exists(directorio)){
    dir.create(directorio)
  }
  for(variable in variables){
    png(paste0(directorio,"/",variable,".png"),
        width = 800, height = 600)
    #png(paste0(directorio,"/",variable,".png"))
    plot(data[[variable]], col = colores, pch = pch, cex = cex, main = variable)
    legend("topright", legend = c("1 Bankrupt", "0 Bankrupt"),
           col = colores, pch = "o", cex = cex)
    dev.off()
  }
}

#------Limpieza de datos------#

data <- read.csv("/home/jesus/Bankruptcy-Prediction/Data/data.csv")
head(data)


cat("Numero de instancias: ", dim(data)[2])
cat("Numero de registros: ", dim(data)[1])

null_counts <- colSums(is.na(data))
datos_nulos <- data.frame(columna = names(null_counts), nulos = null_counts)
duplicated <- duplicated(data)

ggplot(datos_nulos, aes(x = columna, y = nulos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Número de valores nulos por columna", x = "Columna", y = "Cantidad de nulos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

cat("Total de datos nulos: ", sum(null_counts))
cat("Total de duplicados: ",sum(duplicated))

summary(data)


#------Analisis exploratorio------#
str(data)

categoricalVariables <- NamesVarInteger(data)
numericalVariables <- NamesVarNumeric(data)

#Creamos dos dataframes
data_categoric <- data[,categoricalVariables]
data_numeric <- data[,numericalVariables]

cat("Numero de variables categoricas: ", length(categoricalVariables))
print(categoricalVariables)

cat("Numero de variables numericas: ", length(numericalVariables))
print(numericalVariables)

#Comenzaremos analizando las variables categoricas
#Iniciando con nuestra variable objetivo
ggplot(data_categoric, aes(x = Bankrupt.)) +
  geom_bar(fill = "#605F62") + 
  labs(title = "Bankrupt?")

table(data_categoric$Bankrupt.)
#6599 empresas no se fueron a bancarrota
#220 empresas se fueron a bancarrota

ggplot(data_categoric, aes(x = Liability.Assets.Flag)) +
  geom_bar(fill = "#605F62") + 
  labs(title = "Liability Assets Flag")
table(data_categoric$Liability.Assets.Flag)

ggplot(data_categoric, aes(x = Net.Income.Flag)) +
  geom_bar(fill = "#605F62") + 
  labs(title = "Net Income Flag")
table(data_categoric$Net.Income.Flag)
#Tiene sentido deshacerse de esta variable, ya que no cambia para
#ninguna compañia

#Comparación de Liability Assets Flag con Bankrupt
bankrupt_table <- table(data_categoric$Bankrupt.)
liabilityFlag_table <- table(data_categoric$Liability.Assets.Flag)
frecuencias_combinadas <- rbind(bankrupt_table,liabilityFlag_table)

print(frecuencias_combinadas)
barplot(frecuencias_combinadas, beside = TRUE, legend.text = c("Bankrupt", "Liability Assets Flag")
        , col = c("#424242","#19191a"))
title("Bankrupt vs Liability Assets Flag")

#Analizaremos las variables numericas


exportar_histogramas(data_numeric, "/home/jesus/Bankruptcy-Prediction/Histogramas/")

exportar_boxplots(data_numeric, "/home/jesus/Bankruptcy-Prediction/Boxplots/")

colores <- ifelse(data$Bankrupt. == 1, "#031c18", "#9caccd")

exportar_GraficasUnivariable(data = data_numeric, 
      directorio = "/home/jesus/Bankruptcy-Prediction/Univariable/",
      colores = colores)
 
#Graficar

plot(data_numeric$ROA.C..before.interest.and.depreciation.before.interest, col = colores, pch = 16, cex = 0.5)
