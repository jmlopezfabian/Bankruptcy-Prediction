library(ggplot2)
#library(DMwR)
#library(devtools)
#library(corrplot)
library(PerformanceAnalytics)
library(nortest)
library(randomForest)
library(caret)
library(rpart)

#data <- read.csv('/home/jesus/Bankruptcy-Prediction/Data/data.csv')

data <- read.csv('/home/jesus/Bankruptcy-Prediction/Data/data_resampled.csv')


dimensiones <- dim(data)
cat("El conjunto de datos tiene",dimensiones[2],"instancias y", dimensiones[1]
    ,"registros")

null_counts <- colSums(is.na(data))
duplicated_count <- sum(duplicated(data))

cat("Numero de registros nulos: ", sum(null_counts))
cat("Numero de registros duplicados: ", duplicated_count)
#No hay duplicados ni valores nulos en el dataframe

summary(data)
#Todas las 96 columnas son de tipo numeric o integer

num_columnas <- dimensiones[2]
num_filas <- dimensiones[1]

#par(mfrow = c(num_filas,num_columnas))

#for(i in 1:num_columnas){
#  hist(data[,i], main = colnames(data)[i] , xlab = "", ylab = "Frecuencia")
#}

#Todas las empresas tienen un Net Income Flag en 1, por lo que podemos
#eliminar esta caracteristica

sum(data$Operating.Profit.Rate <= 0.99)

#La gran mayoria de las empresas tienen una rentabilidad por arriba 99%
#Operating Profit Rate puede ser un indicador relevante para salud financiera
#de una empresa, pero su influencia en la posiblidad de que una empresa
#se declare en banca rota no puede determinarse por este indicador.

#Grafico de Rentabilidad de las empresas que se fueron a bancarrota
plot(ifelse(data$Bankrupt. == 1, data$Operating.Profit.Rate, NA))

#Grafico de Rentabilidad de las empresas que se NO fueron a bancarrota
plot(ifelse(data$Bankrupt. == 0, data$Operating.Profit.Rate, NA))

#Se observa que las empresas que se fueron a banca rota presentan una
#alta rentabilidad, al igual que las empresas que no se fueron a bancarrota.
#Por lo que esta variable no es relevante para nuestro analisis.

#Eliminando caracteristicas

borrar <- c("Operating.Profit.Rate","Pre.tax.net.Interest.Rate",
            "After.tax.net.Interest.Rate", "Non.industry.income.and.expenditure.revenue",
            "Continuous.interest.rate..after.tax.","Realized.Sales.Gross.Profit.Growth.Rate",
            "Operating.Profit.Growth.Rate", "After.tax.Net.Profit.Growth.Rate",
            "Regular.Net.Profit.Growth.Rate", "Continuous.Net.Profit.Growth.Rate",
            "Net.Value.Growth.Rate","Total.Asset.Return.Growth.Rate.Ratio",
            "Current.Ratio","Quick.Ratio","Interest.Expense.Ratio",""
            )

data$Interest.Expense.Ratio
data_subset <- data[, names(data) %in% borrar]
head(data_subset)

#Seleccion de variables importantes con random forest
rf_model <- randomForest(data[,- data$Bankrupt.], data$Bankrupt., importance = TRUE)
var_importance <- importance(rf_model)
selected_variables <- names(var_importance)[which(var_importance$MeanDecreaseGini > 0)]

#Seleccion de variables importantes con caret
selected_variables_caret <- c()
formula <- as.formula(paste('Bankrupt. ~',paste(names(data[,-which(names(data) == "Bankrupt.")]),
                                                collapse = "+")))

model <- train(formula, data = data, method = "glm", family = "binomial", trControl = trainControl(method = "none"), preProcess = c("center", "scale"), tuneLength = 1)

selected_variables_caret <- varImp(model)$importance

#Armando el data frame con las variables mas significativas
new_df <- data[,c("Bankrupt.","Persistent.EPS.in.the.Last.Four.Seasons",
                  "Total.debt.Total.net.worth","Total.Asset.Turnover",
                  "Accounts.Receivable.Turnover","Average.Collection.Days",
                  "Cash.Total.Assets","Net.Income.to.Total.Assets","Equity.to.Liability")]

new_df <- data[,c("Tax.rate..A.","Net.Value.Growth.Rate","Accounts.Receivable.Turnover",
                  "Average.Collection.Days","Inventory.Turnover.Rate..times.","Cash.Flow.to.Liability")]

new_df <- data[,c("Bankrupt.",'ROA.C..before.interest.and.depreciation.before.interest',
                  'ROA.A..before.interest.and...after.tax',
                  'ROA.B..before.interest.and.depreciation.after.tax',
                  'Operating.Gross.Margin', 'Realized.Sales.Gross.Margin',
                  'Cash.flow.rate', 'Tax.rate..A.', 'Net.Value.Per.Share..B.',
                  'Net.Value.Per.Share..A.',  'Net.Value.Per.Share..C.',
                  'Persistent.EPS.in.the.Last.Four.Seasons', 'Cash.Flow.Per.Share',
                  'Operating.Profit.Per.Share..Yuan...',
                  'Per.Share.Net.profit.before.tax..Yuan...', 'Debt.ratio..',
                  'Net.worth.Assets', 'Operating.profit.Paid.in.capital',
                  'Net.profit.before.tax.Paid.in.capital',
                  'Operating.profit.per.person', 'Working.Capital.to.Total.Assets',
                  'Quick.Assets.Total.Assets', 'Cash.Total.Assets',
                  'Current.Liability.to.Assets', 'Operating.Funds.to.Liability',
                  'Retained.Earnings.to.Total.Assets', 'Total.expense.Assets',
                  'CFO.to.Assets', 'Current.Liability.to.Current.Assets',
                  'Net.Income.to.Total.Assets', 'Gross.Profit.to.Sales')]


#data_aumentada <- data_aumentada[,c("Bankrupt.","Persistent.EPS.in.the.Last.Four.Seasons",
#                               "Total.debt.Total.net.worth","Total.Asset.Turnover",
#                               "Accounts.Receivable.Turnover","Average.Collection.Days",
#                               "Cash.Total.Assets","Net.Income.to.Total.Assets","Equity.to.Liability")]



#Regresion logistica

  #Separando el conjunto de datos en test y train

set.seed(123)
indices <- createDataPartition(new_df$Bankrupt., p = 0.7, list = FALSE)
conjunto_entrenamiento <- new_df[indices, ]
conjunto_prueba <- new_df[-indices, ]

# Ajustar el modelo de regresión logística
modelo <- glm(Bankrupt. ~ ., data = conjunto_entrenamiento, family = "binomial")

# Realizar predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = conjunto_prueba, type = "response")

# Convertir las predicciones en valores de clase (0 o 1)
predicciones_clase <- ifelse(predicciones > 0.5, 1, 0)

# Calcular la matriz de confusión
matriz_confusion <- confusionMatrix(factor(predicciones_clase), factor(conjunto_prueba$Bankrupt.))
print(matriz_confusion)

#SVM
set.seed(101)
y_train <- conjunto_entrenamiento$Bankrupt.
X_train <- conjunto_entrenamiento[,-conjunto_entrenamiento$Bankrupt.]

y_test <- conjunto_prueba$Bankrupt.
y_test <- factor(y_test, levels = levels(y_train))
X_test <- conjunto_prueba[,-conjunto_prueba$Bankrupt.]



model <- svm(x = X_train, y = y_train)

predictions <- predict(model,newdata = X_test)
predictions <- factor(predictions, levels = levels(y_test))

confusion <- confusionMatrix(data = predictions, reference = y_test)


#Arboles de decisión

model <- rpart(Bankrupt. ~ ., data  = conjunto_entrenamiento, method = "class")
predictions <- predict(model, conjunto_prueba, type = "class")
confusion_matriz_DT <- confusionMatrix(predictions,factor(conjunto_prueba$Bankrupt.))
print(confusion_matriz_DT)


#Random Forest
model <- randomForest(Bankrupt. ~ ., data = conjunto_entrenamiento)
predictions <- predict(model, conjunto_prueba type = "class")
mc <- with(conjunto_entrenamiento, table(predictions,conjunto_prueba$Bankrupt.))



#Prueba del modelo entranado con la data sin aumentar sobre data aumentada

#predicciones_aumentada <- predict(modelo,newdata = data_aumentada, type = "response")
#predicciones_aumentada_clase <- ifelse(predicciones_aumentada > 0.5, 1,0)
#matriz_confusion_aumentada <- confusionMatrix(factor(predicciones_aumentada_clase), factor(data_aumentada$Bankrupt.))
#print(matriz_confusion_aumentada)


#relevant_variables <- c()
#for(col in names(data)){
#  if(col != 'Bankrupt.'){
#    t_result <- t.test(data[data$Bankrupt. == 0, col], data[data$Bankrupt. == 1, col])
#    if(t_result$p.value < 0.05){
#      revevant_variables <- c(relevant_variables,col)
#    }
#  }
#}


data_subset <- data_subset[ ,-c(23,27)]

value_counts <- table(data$Bankrupt.)

cat("Número compañias que no estan en bancarrota: ",value_counts[1])
cat("Número compañias en bancarrota: ",value_counts[2])

ggplot(data, aes(x = data$Bankrupt.)) +
  geom_bar() + labs(title = "Bankrupt?")
#Hay un desvalance en el Target Value



categorical_features <- names(data)[sapply(data,is.integer)]
numeric_features <- names(data)[sapply(data,is.numeric) & !names(data) %in% categorical_features]

#Hay 3 variables categoricas, vamos a explorar estas columnas primero.

#Liability.Assets.Flag se refiere al estado de una organizacion, si el
#total de pasivos excede el total de activos, el valor sera 1, en caso contrario
#sera 0

liability_assets_flag <- table(data$'Liability.Assets.Flag')
cat("Pasivos excede activos: ",liability_assets_flag[1])
cat("Pasivos NO excede activos: ",liability_assets_flag[2])
ggplot(data, aes(x = data$Liability.Assets.Flag)) + geom_bar()
#Los activos de la gran mayoria de compañias supera sus pasivos.


#Comparacion con empresas en bancarrota

print(table(data[c('Liability.Assets.Flag','Bankrupt.')]))

#ggplot(data,aes(x = data$Liability.Assets.Flag, fill = data$Bankrupt.)) +
#  geom_bar(position = "fill") +
#  labs(x = 'Liability.Assets.Flag', y = 'Count') +
#  theme_minimal()

#Una pequeña porcion de las organizaciones esta banca rota, a pesar de que sus
#activos superan sus pasivos.

#No es determinante para nuestro objetivo.


net_income_flag_count <- table(data$Net.Income.Flag)
print(net_income_flag_count)
#Todas las empresas tienen perdidas en los dos años anteriores.

#Muchas de las organizaciones que han presentado perdidas durante los ultimos dos
#años han establezido su negocio.

print(table(data[c('Net.Income.Flag','Bankrupt.')]))

#Tampoco es determinante

pairs(data$Current.Liability.to.Assets ~ data$Debt.ratio..)
pairs(data$Borrowing.dependency ~ data$Liability.to.Equity)

data[numeric_features]

correlations <- cor(data[numeric_features], data[,'Bankrupt.'])
sorted_correlations <- sort(correlations, decreasing = TRUE)




matriz_correlacion <- cor(data)
indices <- which(matriz_correlacion > 0.9,arr.ind = TRUE)
indices <- indices[indices[,"row"] != indices[,"col"], ]
variables <- colnames(matriz_correlacion)[indices[,"col"]]
for (i in 1:nrow(indices)){
  var1 <- rownames(matriz_correlacion)[indices[i,"row"]]
  var2 <- colnames(matriz_correlacion)[indices[i,"col"]]
  print(paste(var1, "-", var2))
}
# 
#Correlaciones
cor(data$Net.Worth.Turnover.Rate..times.,data$Total.Asset.Turnover)
correlation <- cor(data[numeric_features], data['Bankrupt.'])
sort(correlation)
max(correlation)

pairs(data$Bankrupt. ~ data$Debt.ratio..)
pairs(data$Bankrupt. ~ data$Current.Liability.to.Assets)
pairs(data$Debt.ratio.. ~ data$Current.Liability.to.Assets)

