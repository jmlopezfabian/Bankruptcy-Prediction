library(ggplot2)
library(PerformanceAnalytics)
library(nortest)
library(randomForest)
library(caret)
library(rpart)
library(dplyr)
library(cowplot)
library(grid)


data <- read.csv('/home/jesus/Bankruptcy-Prediction/Data/data_resampled.csv')
# Identificar columnas constantes
constant_columns <- apply(data, 2, function(x) var(x) == 0)

# Eliminar columnas constantes
data <- data[, !constant_columns]


data_std <- scale(data)

#pca_result <- prcomp(data_std[-94], scale = TRUE)
pca_result <- prcomp(data_std)
data_pca <- pca_result$X[,1]
data_pca <- data$Bankrupt.


plot(pca_result$x[, 1], pca_result$x[, 2], xlab = "Componente Principal 1", ylab = "Componente Principal 2", main = "Gráfico de Dispersión de PCA")

biplot(pca_result, scale = 0, cex = 0.8)
# Añadir etiquetas de muestras personalizadas
text(pca_result$x[, 1], pca_result$x[, 2], labels = rownames(data), cex = 0.6)

var_exp <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_var_exp <- cumsum(var_exp)
plot(cum_var_exp, xlab = "Número de componentes", ylab = "Varianza explicada acumulada", type = "b")



data.pca <- prcomp(data[-96], scale=T)
summary(data.pca)
prop_varianza <- data.pca$sdev^2 / sum(data.pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum


ggplot(data = data.frame(prop_varianza, pc = 1:95), aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,0.2)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

df_prop_varianza <- data.frame(pc = 1:length(prop_varianza_acum), prop_varianza_acum)
ggplot(df_prop_varianza, aes(x = pc, y = prop_varianza_acum)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0,length(prop_varianza_acum), by = 5)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Número de Componente Principal",
       y = "Proporción acumulada de Varianza Explicada")

umbral <- 0.85
primer_componente <- which(prop_varianza_acum >= umbral)[1]
nombres_variables <- colnames(data)
variables_por_debajo_umbral <- nombres_variables[1:primer_componente]

df_seleccionado <- data[,variables_por_debajo_umbral]
df_seleccionado <- data.frame(df_seleccionado, Bankrupt. = data$Bankrupt.)

#Regresion logistica

  #Separando el conjunto de datos en test y train
set.seed(123)
indices <- createDataPartition(df_seleccionado$Bankrupt., p = 0.7, list = FALSE)
conjunto_entrenamiento <- df_seleccionado[indices, ]
conjunto_prueba <- df_seleccionado[-indices, ]

# Ajustar el modelo de regresión logística
modelo <- glm(Bankrupt. ~ ., data = conjunto_entrenamiento, family = "binomial")

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = conjunto_prueba, type = "response")

# Convertir las predicciones en valores de clase (0 o 1)
predicciones_clase <- ifelse(predicciones > 0.5, 1, 0)

# Calcular la matriz de confusión
matriz_confusion <- confusionMatrix(factor(predicciones_clase), factor(conjunto_prueba$Bankrupt.))
print(matriz_confusion)

#Arboles de decision
model <- rpart(Bankrupt. ~ ., data  = conjunto_entrenamiento, method = "class")
predictions <- predict(model, conjunto_prueba, type = "class")
confusion_matriz_DT <- confusionMatrix(predictions,factor(conjunto_prueba$Bankrupt.))
print(confusion_matriz_DT)

#Random forest
model <- randomForest(Bankrupt. ~ ., data = conjunto_entrenamiento)
predictions <- predict(model, conjunto_prueba, type = "class")
mc <- with(conjunto_entrenamiento, table(predictions,conjunto_prueba$Bankrupt.))



num_com <- 50

data.pca <- prcomp(data[-96], scale=T)
data_pca <- as.data.frame(data.pca$x)
data_pca <- data_pca[,1:num_com]
data_pca$Bankrupt <- data$Bankrupt


# Dividir el conjunto de datos en entrenamiento y prueba
set.seed(123)  # Establecer una semilla para reproducibilidad
indices <- createDataPartition(data_pca$Bankrupt, p = 0.7, list = FALSE)
conjunto_entrenamiento <- data_pca[indices, ]
conjunto_prueba <- data_pca[-indices, ]

modelo <- glm(Bankrupt ~ ., data = conjunto_entrenamiento)

predicciones <- predict(modelo, newdata = conjunto_prueba, type = "response")
predicciones_clase <- ifelse(predicciones > 0.5, 1, 0)

matriz_confusion <- confusionMatrix(factor(predicciones_clase), factor(conjunto_prueba$Bankrupt))
print(matriz_confusion)

loadings <- data.pca$rotation

variable_names <- colnames(data)[-96]
important_variables <- apply(loadings[,1:num_com], 2, function(x) variable_names[order(abs(x), decreasing = TRUE)[1]])
print(important_variables)