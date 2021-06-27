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