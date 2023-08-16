library(ggplot2)
library(PerformanceAnalytics)
library(nortest)
library(randomForest)
library(caret)
library(rpart)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)
library(MLmetrics)



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
    plot(data[[variable]], col = colores, pch = pch, cex = cex, main = variable, )
    legend("topright", legend = c("1 Bankrupt", "0 Bankrupt"),
           col = colores, pch = "o", cex = cex)
    dev.off()
  }
}

exportar_graficasDensidad <- function(data, directorio){
  variables <- names(data)
  df_bankrupt_1 <- subset(data, Bankrupt. == 1)
  df_bankrupt_0 <- subset(data, Bankrupt. == 0)
  
  for(variable in variables){
    png(paste0(directorio,"/",variable,".png"),
        width = 800, height = 600)
    combined_plot <- ggplot() +
      geom_density(data = df_bankrupt_1, aes(x = .data[[variable]], fill = "Bankrupt = 1"), alpha = 0.5) +
      geom_density(data = df_bankrupt_0, aes(x = .data[[variable]], fill = "Bankrupt = 0"), alpha = 0.5) +
      labs(title = paste("Gráfico de Densidad para", variable, "con Bankrupt = 1 y Bankrupt = 0")) +
      scale_fill_manual(values = c("blue", "red"), 
                        labels = c("Bankrupt = 1", "Bankrupt = 0"),
                        name = variabel)
    
    print(combined_plot)
    dev.off()
  }
}

obtener_variables_significativas <- function(data){
  variables_significativas <- c()  # Crear un vector vacío para almacenar los nombres de las variables significativas
  
  for (variable in names(data)){
    if(variable != "Bankrupt." & variable != "Net.Income.Flag"){
      p_value <- t.test(data[[variable]] ~ Bankrupt., data = data, conf.level = 0.99)$p.value  # Realizar la prueba t de Student y obtener el p-value
      cat("Intervalos de confianza de la variable: ",variable);
      test <- t.test(data[[variable]], conf.level = 0.99)$conf.int  
      print(test)
      cat("\n")
      if (p_value < 0.01){
        variables_significativas <- c(variables_significativas, variable)  # Agregar el nombre de la variable a la lista de variables significativas
      }
    }
  }
  return(variables_significativas)  # Devolver la lista de nombres de variables significativas
}



#------Limpieza de datos------#

data_original <- read.csv("/home/jesus/Bankruptcy-Prediction/Data/data.csv")
data_aumentada <- read.csv("/home/jesus/Bankruptcy-Prediction/Data/data_resampled.csv")
data_reducida <- read.csv("/home/jesus/Bankruptcy-Prediction/Data/data_reduced.csv")

head(data_original)


cat("Numero de instancias: ", dim(data_original)[2])
cat("Numero de registros: ", dim(data_original)[1])

null_counts <- colSums(is.na(data_original))
datos_nulos <- data.frame(columna = names(null_counts), nulos = null_counts)
duplicated <- duplicated(data_original)

ggplot(datos_nulos, aes(x = columna, y = nulos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Número de valores nulos por columna", x = "Columna", y = "Cantidad de nulos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

cat("Total de datos nulos: ", sum(null_counts))
cat("Total de duplicados: ",sum(duplicated))

summary(data_original)


#------Analisis exploratorio------#
str(data_original)

categoricalVariables <- NamesVarInteger(data_original)
numericalVariables <- NamesVarNumeric(data_original)

#Creamos dos dataframes
data_categoric <- data_original[,categoricalVariables]
data_numeric <- data_original[,numericalVariables]

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

colores <- ifelse(data_original$Bankrupt. == 1, "#031c18", "#9caccd")

exportar_GraficasUnivariable(data = data_numeric, 
      directorio = "/home/jesus/Bankruptcy-Prediction/Univariable/",
      colores = colores)


matriz_corr <- cor(data_numeric)


#Inferencia estadistica

#Aplicando la prueba chi cuadrada a las variables liability assets flag y
#bankrupt, con el proposito de saber si son independientes o no

tabla_contingencia <- table(data_categoric$Bankrupt., data_categoric$Liability.Assets.Flag)
resultado <- chisq.test(tabla_contingencia)

#Hipotesis nula: No existe asociacion entre las variables
#Hipotesis alternativa: Existe asociacion entre las variables

valor_p <- resultado$p.value
print(valor_p)

#El valor de p es extremadamente pequeño. Esto indica una fuerte evidencia en contra
#de la hipotesis nula

#Por lo tanto, existe asosiacion entre las variables


exportar_graficasDensidad(data_original,"/home/jesus/Bankruptcy-Prediction/GraficasDensidad/")


#Entrenando el modelo con la data original



constant_columns <- apply(data_original, 2, function(x) var(x) == 0)
data_original <- data_original[, !constant_columns]
data_original <- normalize_min_max(data_original)
variables_significativas <- obtener_variables_significativas(data_original)

data_significativa <- data_original[,c(variables_significativas,"Bankrupt.")]

set.seed(123)
indices <- createDataPartition(data_significativa$Bankrupt., p = 0.7, list = FALSE)
conjunto_entrenamiento <- data_significativa[indices, ]
conjunto_prueba <- data_significativa[-indices, ]

# Ajustar el modelo de regresión logística
modelo <- glm(Bankrupt. ~ ., data = conjunto_entrenamiento, family = "binomial")

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = conjunto_prueba, type = "response")

# Convertir las predicciones en valores de clase (0 o 1)
predicciones_clase <- ifelse(predicciones > 0.5, 1, 0)

# Calcular la matriz de confusión
matriz_confusion <- confusionMatrix(factor(predicciones_clase), factor(conjunto_prueba$Bankrupt.))
print(matriz_confusion)

# Calcular la matriz de confusión para el F1 Score
matriz_confusion <- table(predicciones_clase, conjunto_prueba$Bankrupt.)

# Extraer los valores de la matriz de confusión
vn <- as.numeric(matriz_confusion[1, 1])  # Verdaderos negativos
vp <- as.numeric(matriz_confusion[2, 2])  # Verdaderos positivos
fn <- as.numeric(matriz_confusion[2, 1])  # Falsos negativos
fp <- as.numeric(matriz_confusion[1, 2])  # Falsos positivos

# Calcular precision, recall y F1 score
precision <- vp / (vp + fp)
recall <- vp / (vp + fn)
f1_score <- 2 * precision * recall / (precision + recall)

# Imprimir el resultado
cat("F1 score:", f1_score, "\n")

#Entrenando el modelo con la data aumentada

constant_columns <- apply(data_aumentada, 2, function(x) var(x) == 0)
data_aumentada <- data_aumentada[, !constant_columns]
data_aumentada <- normalize_min_max(data_aumentada)

variables_significativas <- obtener_variables_significativas(data_aumentada)

data_significativa <- data_aumentada[,c(variables_significativas,"Bankrupt.")]


set.seed(123)
indices <- createDataPartition(data_significativa$Bankrupt., p = 0.7, list = FALSE)
conjunto_entrenamiento <- data_significativa[indices, ]
conjunto_prueba <- data_significativa[-indices, ]

# Ajustar el modelo de regresión logística
modelo <- glm(Bankrupt. ~ ., data = conjunto_entrenamiento, family = "binomial")

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = conjunto_prueba, type = "response")

# Convertir las predicciones en valores de clase (0 o 1)
predicciones_clase <- ifelse(predicciones > 0.5, 1, 0)

# Calcular la matriz de confusión
matriz_confusion <- confusionMatrix(factor(predicciones_clase), factor(conjunto_prueba$Bankrupt.))
print(matriz_confusion)

# Calcular la matriz de confusión para el F1 Score
matriz_confusion <- table(predicciones_clase, conjunto_prueba$Bankrupt.)

# Extraer los valores de la matriz de confusión
vn <- as.numeric(matriz_confusion[1, 1])  # Verdaderos negativos
vp <- as.numeric(matriz_confusion[2, 2])  # Verdaderos positivos
fn <- as.numeric(matriz_confusion[2, 1])  # Falsos negativos
fp <- as.numeric(matriz_confusion[1, 2])  # Falsos positivos

# Calcular precision, recall y F1 score
precision <- vp / (vp + fp)
recall <- vp / (vp + fn)
f1_score <- 2 * precision * recall / (precision + recall)

# Imprimir el resultado
cat("F1 score:", f1_score, "\n")



save(modelo, file = "./Bankruptcy-Prediction/Models/modelo.rda")

primer_renglon <- data_significativa[1, ]
write.csv(primer_renglon, file = "./Bankruptcy-Prediction/Data/Data_muestra.csv", row.names = FALSE)



#-------Entrenando el modelo con la data reducida-------

constant_columns <- apply(data_reducida, 2, function(x) var(x) == 0)
data_reducida <- data_reducida[, !constant_columns]
data_reducida <- normalize_min_max(data_reducida)
variables_significativas <- obtener_variables_significativas(data_reducida)

data_significativa <- data_reducida[,c(variables_significativas,"Bankrupt.")]

set.seed(123)
indices <- createDataPartition(data_significativa$Bankrupt., p = 0.7, list = FALSE)
conjunto_entrenamiento <- data_significativa[indices, ]
conjunto_prueba <- data_significativa[-indices, ]

# Ajustar el modelo de regresión logística
modelo <- glm(Bankrupt. ~ ., data = conjunto_entrenamiento, family = "binomial")

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = conjunto_prueba, type = "response")

# Convertir las predicciones en valores de clase (0 o 1)
predicciones_clase <- ifelse(predicciones > 0.5, 1, 0)

# Calcular la matriz de confusión
matriz_confusion <- confusionMatrix(factor(predicciones_clase), factor(conjunto_prueba$Bankrupt.))
print(matriz_confusion)

# Calcular la matriz de confusión para el F1 Score
matriz_confusion <- table(predicciones_clase, conjunto_prueba$Bankrupt.)

# Extraer los valores de la matriz de confusión
vn <- as.numeric(matriz_confusion[1, 1])  # Verdaderos negativos
vp <- as.numeric(matriz_confusion[2, 2])  # Verdaderos positivos
fn <- as.numeric(matriz_confusion[2, 1])  # Falsos negativos
fp <- as.numeric(matriz_confusion[1, 2])  # Falsos positivos

# Calcular precision, recall y F1 score
precision <- vp / (vp + fp)
recall <- vp / (vp + fn)
f1_score <- 2 * precision * recall / (precision + recall)

# Imprimir el resultado
cat("F1 score:", f1_score, "\n")
