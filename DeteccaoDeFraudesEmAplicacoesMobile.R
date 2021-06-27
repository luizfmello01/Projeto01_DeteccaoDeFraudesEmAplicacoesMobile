### Parte 1: Análise exploratória dos dados ###
## Nesse parte do script será realizado a compreensão dos dados,
## Medidas de tendencia central, dispersão, histogramas, boxplots, análise de correlação,
## tabela de contingência, proporção da categoria, etc.


#### Definir local de trabalho ####
setwd("C:/repos/Projeto01/Projeto01_DeteccaoDeFraudesEmAplicacoesMobile")
getwd()


#### Carregar pacotes e arquivos utilitários ####
source("Utils.R", encoding = "UTF-8")
# install.packages("data.table")
# install.packages("gmodels")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("e1071")
# install.packages("performanceEstimation")
# install.packages("ROCR")
# install.packages(c("xts", "quantmod"))
# install.packages("Packages/DMwR_0.4.1.tar.gz", repos = NULL, type = "source")
# install.packages("class")
library(data.table)
library(gmodels)
library(randomForest)
library(caret)
library(e1071)
library(DMwR)
library(class)
library(ROCR)


#### Carregar o dataset ####
# ?fread
apps <- fread("Datasets/train_sample.csv", sep = ",", header = TRUE)

# Visualizar o dataset
str(apps)
head(apps)


#### Conversão de tipo de variável ####
# Converter variáveis numéricas para fator (Obs: Em nosso conjunto de dados
# Não tem nenhuma variável numérica)
variaveis_categoricas <- c("ip", "app", "device",
                           "os", "channel", "is_attributed")
apps <- to.factor(apps, variaveis_categoricas)

# Visualizar o dataset
str(apps)
summary(apps)


#### Verificar valores NA das variáveis ####
# IP
any(is.na(apps$ip))
# Resultado: Não contem nenhuma variável NA

# APP
any(is.na(apps$app))
# Resultado: Não contem nenhuma variável NA

# DEVICE
any(is.na(apps$device))
# Resultado: Não contem nenhuma variável NA

# OS
any(is.na(apps$os))
# Resultado: Não contem nenhuma variável NA

# CHANNEL
any(is.na(apps$channel))
# Resultado: Não contem nenhuma variável NA

# CLICK_TIME
any(is.na(apps$click_time))
# Resultado: Não contem nenhuma variável NA

# ATTRIBUTED_TIME
any(is.na(apps$attributed_time))
sum(is.na(apps$attributed_time))
# Resultado: A variável attributed_time tem muitos valores NA

# IS_ATTRIBUTED
any(is.na(apps$is_attributed))
# Resultado: Não contem nenhuma variável NA


#### Teste do Qui-quadrado para verificar relacionamento das variáveis ####
# Váriavel IP e app
chisq.test(apps$ip, apps$app)
# Resultado: Tem um bom relacionamento

# Váriavel app e device (Target)
chisq.test(apps$app, apps$device)
# Resultado: Tem um bom relacionamento

# Váriavel device e is_attributed (Target)
chisq.test(apps$device, apps$os)
# Resultado: Tem um bom relacionamento

# Proporça da variável target (is_attributed)
prop.table(table(apps$is_attributed))
# Resultado: As classes estão desbalanceadas

# Verificado que temos duas variáveis de data, não são boas para o modelo
# de machine learning, transformar em categoria ou retira-la



### Parte 2: Manipulação dos dados ###
## Nesse parte do script será realizado a alteração de nome das variáveis
## Limpeza e remoção de valores NA.
## Balanceamento das variável target


#### Alterar nome das variáveis do dataset ####
novos_nomes <- c("endereco_ip", "id_aplicativo", "id_dispositivo",
                 "id_sistema_operacional", "id_canal", "hora_click",
                 "hora_download", "download")

colnames(apps) <- novos_nomes
str(apps)


#### Remoção dos valores NA ####
# Durante a análise exploratório foi verificado que na coluna 
# attributed_time tem muitos valores NA, remover essa coluna porque ela
# não vai ser necessária no modelo preditivo.
apps$hora_download <- NULL
any(is.na(apps))



### Parte 3: Treinamento do modelo preditivo ###
## Nesse parte do script será realizado o treinamento dos modelos do projeto.

#### Feature selection ####
# Na versão V1 do modelo, não iremos utilizar a variável "hora_click"
apps_models <- apps
apps_models$hora_click <- NULL

# Utilizar algoritmo Random Forrest para seleção de variáveis
# ?randomForest
selection_rf <- randomForest(download ~ as.numeric(as.character(apps_models$endereco_ip))
                             + as.numeric(as.character(apps_models$id_aplicativo))
                             + as.numeric(as.character(apps_models$id_dispositivo))
                             + as.numeric(as.character(apps_models$id_sistema_operacional))
                             + as.numeric(as.character(apps_models$id_canal)),
                             data = apps_models,
                             ntree = 150,
                             nodesize = 15,
                             importance = TRUE)

varImpPlot(selection_rf)

##### Modelos de machine learning #####

#### Modelo v1 ####
# O modelo de machine learning escolhido para a primeira versão é NaiveBayes

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v1.train <- apps_models[trainIndex,]
v1.test <- apps_models[-trainIndex,]


# Treinamento da primeira versão do modelo
modelo_v1 <- naiveBayes(download ~ .,
                        data = v1.train)



#### Modelo v2 ####
# Modelo de machine learning com NaiveBayes, porém usando variáveis mais
# Relevantes para o modelo

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v2.train <- apps_models[trainIndex,]
v2.test <- apps_models[-trainIndex,]


# Treinamento da primeira versão do modelo
modelo_v2 <- naiveBayes(download ~ . - id_dispositivo,
                        data = v2.train)



#### Modelo v3 ####
# Modelo de machine learning com NaiveBayes, balanceando as classes

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v3.train <- apps_models[trainIndex,]
v3.test <- apps_models[-trainIndex,]

# Balancear os dados de treino
formula_train <- as.formula("download ~ .")
v3.train_balanceados <- SMOTE(formula_train, v3.train)


# Treinamento da primeira versão do modelo
modelo_v3 <- naiveBayes(formula_train,
                        data = v3.train_balanceados)



#### Modelo v4 ####
# O modelo de machine learning escolhido para a quarta versão é o KNN

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v4.train <- apps_models[trainIndex,]
v4.test <- apps_models[-trainIndex,]
target.train <- unlist(v4.train[,6])
target.test <- unlist(v4.test[,6])


# Treinamento da primeira versão do modelo
modelo_v4 <- knn(v4.train, v4.test, cl = target.train)



#### Modelo v5 ####
# O modelo de machine learning escolhido para a quinta versão é o KNN, porém com as
# variáveis mais relevantes para o modelo.

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v5.train <- apps_models[trainIndex, -"id_dispositivo"]
v5.test <- apps_models[-trainIndex, -"id_dispositivo"]
target.train <- unlist(v5.train[,"download"])
target.test <- unlist(v5.test[,"download"])


# Treinamento da primeira versão do modelo
modelo_v5 <- knn(v5.train, v5.test, cl = target.train)



#### Modelo v6 ####
# O modelo de machine learning escolhido para a sexta versão é o KNN, com a classe
# balanceada

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v6.train <- apps_models[trainIndex,]
v6.test <- apps_models[-trainIndex,]
target.test <- unlist(v6.test[,"download"])

# Balancear os dados de treino
formula_train <- as.formula("download ~ .")
v6.train_balanceados <- SMOTE(formula_train, v6.train)
target.train <- unlist(v6.train_balanceados[,"download"])

# Treinamento da primeira versão do modelo
modelo_v6 <- knn(v6.train_balanceados, v6.test, cl = target.train)



#### Modelo v7 ####
# O modelo de machine learning escolhido para a sétima versão é o Random Forest,
# Com os dados de treino balanceados

# Converter de factor to Inteiro
apps_rf <- to.integer(apps_models, c("endereco_ip", "id_aplicativo", "id_dispositivo",
                                     "id_sistema_operacional", "id_canal"))

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_rf$download, p = .7,
                                  list = FALSE)
v7.train <- apps_rf[trainIndex,]
v7.test <- apps_rf[-trainIndex,]

# Balancear os dados de treino
formula_train <- as.formula("download ~ .")
v7.train_balanceados <- SMOTE(formula_train, v7.train)

# Treinamento da primeira versão do modelo
modelo_v7 <- randomForest(formula_train,
                          data = v7.train_balanceados,
                          ntree = 100,
                          nodesize = 10)



#### Modelo v8 ####
# O modelo de machine learning escolhido para a sétima versão é o Random Forest,
# Com os dados de treino balanceados e variáveis mais relevantes

# Converter de factor to Inteiro
apps_rf <- to.integer(apps_models, c("endereco_ip", "id_aplicativo", "id_dispositivo",
                                     "id_sistema_operacional", "id_canal"))

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_rf$download, p = .7,
                                  list = FALSE)
v8.train <- apps_rf[trainIndex, -"id_dispositivo"]
v8.test <- apps_rf[-trainIndex, -"id_dispositivo"]

# Balancear os dados de treino
formula_train <- as.formula("download ~ endereco_ip + id_aplicativo + id_sistema_operacional + id_canal")
v8.train_balanceados <- SMOTE(formula_train, v8.train)

# Treinamento da primeira versão do modelo
modelo_v8 <- randomForest(formula_train,
                          data = v8.train_balanceados,
                          ntree = 100,
                          nodesize = 10)



### Parte 4: Avaliação dos modelos preditivos ###
## Nesse parte do script será avaliado os modelos criados na parte 3.


#### Avaliação dos modelos do algoritmo Naive Bayes ####
metricas_bayes <- list()
previsao_v1 <- predict(modelo_v1, v1.test)
cm_v1 <- confusionMatrix(previsao_v1, reference = v1.test$download, positive = "1")
metricas_bayes <- append(metricas_bayes, list(metrics.models(cm_v1)))
predvec <- ifelse(previsao_v1=="0", 0, 1)
realvec <- ifelse(v1.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v1 <- performance(pred, "tpr", "fpr")


previsao_v2 <- predict(modelo_v2, v2.test)
cm_v2 <- confusionMatrix(previsao_v2, reference = v2.test$download, positive = "1")
metricas_bayes <- append(metricas_bayes, list(metrics.models(cm_v2)))
predvec <- ifelse(previsao_v2=="0", 0, 1)
realvec <- ifelse(v2.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v2 <- performance(pred, "tpr", "fpr")


previsao_v3 <- predict(modelo_v3, v3.test)
cm_v3 <- confusionMatrix(previsao_v3, reference = v3.test$download, positive = "1")
metricas_bayes <- append(metricas_bayes, list(metrics.models(cm_v3)))
predvec <- ifelse(previsao_v3=="0", 0, 1)
realvec <- ifelse(v3.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v3 <- performance(pred, "tpr", "fpr")

# Exibir as métricas de cada modelo
for ( i in 1:3 ) {
  print(paste("Métricas Naive Bayes da versão", i))
  print(metricas_bayes[[i]])
  cat("\n")
}

# Exibir a curva ROC dos 3 modelos
par(mfrow=c(2,2))
plot(perf_v1, col = rainbow(10), main = "Versão 1")
plot(perf_v2, col = rainbow(10), main = "Versão 2")
plot(perf_v3, col = rainbow(10), main = "Versão 3")

## Resultado Naive Bayes ##
# No modelo de Naive Bayes, o modelo que foi escolhido como melhor perfomance,
# Foi o modelo V2, foi utlizado para esse caso, utilizamos o F1Score como referencia


#### Avaliação dos modelos do algoritmo KNN ####
metricas_KNN <- list()
cm_v4 <- confusionMatrix(modelo_v4, reference = v4.test$download, positive = "1")
metricas_KNN <- append(metricas_KNN, list(metrics.models(cm_v4)))
predvec <- ifelse(modelo_v4=="0", 0, 1)
realvec <- ifelse(v4.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v4 <- performance(pred, "tpr", "fpr")

cm_v5 <- confusionMatrix(modelo_v5, reference = v5.test$download, positive = "1")
cm_v5
metricas_KNN <- append(metricas_KNN, list(metrics.models(cm_v5)))
predvec <- ifelse(modelo_v5=="0", 0, 1)
realvec <- ifelse(v5.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v5 <- performance(pred, "tpr", "fpr")

cm_v6 <- confusionMatrix(modelo_v6, reference = v6.test$download, positive = "1")
metricas_KNN <- append(metricas_KNN, list(metrics.models(cm_v6)))
predvec <- ifelse(modelo_v6=="0", 0, 1)
realvec <- ifelse(v6.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v6 <- performance(pred, "tpr", "fpr")

# Exibir as métricas de cada modelo
for ( i in 1:3 ) {
  print(paste("Métricas KNN da versão", i))
  print(metricas_bayes[[i]])
  cat("\n")
}

# Exibir a curva ROC dos 3 modelos
par(mfrow=c(2,2))
plot(perf_v4, col = rainbow(10), main = "Versão 4")
plot(perf_v5, col = rainbow(10), main = "Versão 5")
plot(perf_v6, col = rainbow(10), main = "Versão 6")

## Resultado KNN ##
# No modelo de KNN, o modelo que foi escolhido como melhor perfomance,
# Foi o modelo V5, foi utlizado para esse caso, a maior métrica de FScore


#### Avaliação dos modelos de RandomForest ####
metricas_rf <- list()
previsao_v7 <- predict(modelo_v7, v7.test)
cm_v7 <- confusionMatrix(previsao_v7, reference = v7.test$download, positive = "1")
metricas_rf <- append(metricas_rf, list(metrics.models(cm_v7)))
predvec <- ifelse(previsao_v7=="0", 0, 1)
realvec <- ifelse(v7.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v7 <- performance(pred, "tpr", "fpr")

previsao_v8 <- predict(modelo_v8, v8.test)
cm_v8 <- confusionMatrix(previsao_v8, reference = v8.test$download, positive = "1")
metricas_rf <- append(metricas_rf, list(metrics.models(cm_v8)))
predvec <- ifelse(previsao_v8=="0", 0, 1)
realvec <- ifelse(v8.test$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v8 <- performance(pred, "tpr", "fpr")

for ( i in 1:2 ) {
  print(paste("Métricas RF da versão", i))
  print(metricas_rf[[i]])
  cat("\n")
}


# Avaliação final, entre os modelos V2, V4 e V7
metricas_final <- list()
previsao_v2 <- predict(modelo_v2, v2.test)
cm_v2 <- confusionMatrix(previsao_v2, reference = v2.test$download, positive = "1")
metricas_final <- append(metricas_final, list(metrics.models(cm_v2)))

cm_v4 <- confusionMatrix(modelo_v4, reference = v4.test$download, positive = "1")
metricas_final <- append(metricas_final, list(metrics.models(cm_v4)))

previsao_v7 <- predict(modelo_v7, v7.test)
cm_v7 <- confusionMatrix(previsao_v7, reference = v7.test$download, positive = "1")
metricas_final <- append(metricas_final, list(metrics.models(cm_v7)))

# Exibir as métricas de cada modelo
for ( i in 1:3 ) {
  print(paste("Métricas Final da versão", i))
  print(metricas_final[[i]])
  cat("\n")
}

print("Confusion Matrix da V2")
cm_v2$table

print("Confusion Matrix da V4")
cm_v4$table

print("Confusion Matrix da V7")
cm_v7$table

# Será realizado otimização no modelo Naive Bayes e Random Forest



### Parte 5: Otimização do modelo preditivo ###

#### Pré-processamento nos dados ####
# Converter a variável "hora_click" para tipo caracter
apps_otimizado <- apps
apps_otimizado$hora_c <- sapply(apps_otimizado$hora_click, hour.to.factor)
apps_otimizado$hora_f <- factor(apps_otimizado$hora_c, levels = c("Madrugada", "Manhã", "Tarde", "Noite"))
apps_otimizado$hora_click <- NULL
apps_otimizado$hora_c <- NULL


#### Otimizando modelo Naive Bayes ####

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_otimizado$download, p = .7,
                                  list = FALSE)

otimizado.train_v1 <- apps_otimizado[trainIndex,]
otimizado.test_v1 <- apps_otimizado[-trainIndex,]

# Balancear a classe Target
formula_treinamento <- as.formula("download ~ .")
otimizado.train_v1 <- SMOTE(formula_treinamento,
                            data = otimizado.train_v1)

# Treino do modelo
modelo_otimizado_v1 <- naiveBayes(formula_treinamento,
                                  data = otimizado.train_v1)

# Avaliação do modelo
previsao_v1 <- predict(modelo_otimizado_v1, otimizado.test_v1)
cm_v1 <- confusionMatrix(previsao_v1, otimizado.test_v1$download, positive = "1")
cm_v1
metrics.models(cm_v1)

# Roc curve
predvec <- ifelse(previsao_v1=="0", 0, 1)
realvec <- ifelse(otimizado.test_v1$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v1 <- performance(pred, "tpr", "fpr")
plot(perf_v1)



#### Otimizando modelo RandomForest ####
apps_rf <- to.integer(apps_otimizado, c("endereco_ip", "id_aplicativo", "id_dispositivo",
                                        "id_sistema_operacional", "id_canal"))

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_rf$download, p = .7,
                                  list = FALSE)

otimizado.train_v2 <- apps_rf[trainIndex,]
otimizado.test_v2 <- apps_rf[-trainIndex,]

# Balancear a classe Target
formula_treinamento <- as.formula("download ~ .")
otimizado.train_v2 <- SMOTE(formula_treinamento,
                            data = otimizado.train_v2)

# Treino do modelo
modelo_otimizado_v2 <- randomForest(formula_treinamento,
                                    data = otimizado.train_v2)

# Avaliação do modelo
previsao_v2 <- predict(modelo_otimizado_v2, otimizado.test_v2)
cm_v2 <- confusionMatrix(previsao_v2, otimizado.test_v2$download)
cm_v2
metrics.models(cm_v2)

# Roc curve
predvec <- ifelse(previsao_v2=="0", 0, 1)
realvec <- ifelse(otimizado.test_v2$download=="0", 0, 1)
pred <- prediction(predvec, realvec)
perf_v2 <- performance(pred, "tpr", "fpr")
plot(perf_v2)

# Modelo de entrega -> "modelo_otimizado_v2", utilizando o algoritmo de randomForest

## Salvando arquivo físico do modelo que será publico em produção ##
saveRDS(modelo_otimizado_v2, "Modelo/model.rds")