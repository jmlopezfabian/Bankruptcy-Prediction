library(ggplot2)
library(DMwR)
library(devtools)
library(corrplot)
library(PerformanceAnalytics)

data <- read.csv('/home/jesus/ProyectoProgramacionCD/Data/data.csv')

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

