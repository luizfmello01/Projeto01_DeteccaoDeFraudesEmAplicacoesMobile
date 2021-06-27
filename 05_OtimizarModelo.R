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
