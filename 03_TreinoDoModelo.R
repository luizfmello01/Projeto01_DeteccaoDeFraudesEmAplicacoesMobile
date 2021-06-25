### Parte 3: Treinamento do modelo preditivo ###
## Nesse parte do script será realizado o treinamento dos modelos do projeto.

#### Feature selection ####
# Na versão V1 do modelo, não iremos utilizar a variável "hora_click"
apps_models <- apps
apps_models$hora_click <- NULL

# Utilizar algoritmo Random Forrest para seleção de variáveis
?randomForest
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

# Balancear a variável target com o método upSample
v1.train <- upSample(v1.train[, 1:5],
                  v1.train$download,
                  yname = "download")

# Treinamento da primeira versão do modelo
modelo_v1 <- naiveBayes(download ~ .,
                           data = v1.train)

sum(v1.train$download == predict(modelo_v1, v1.train)) / nrow(v1.train)

previsao_v1 <- predict(modelo_v1, v1.test)

cm_v1 <- confusionMatrix( table(v1.test$download, previsao_v1), positive = "1", mode = "prec_recall"  )
cm_v1


#### Modelo v2 ####
# Modelo de machine learning com NaiveBayes, porém usando variáveis mais
# Relevantes para o modelo

# Separar dados de treino e de teste
trainIndex <- createDataPartition(apps_models$download, p = .7,
                                  list = FALSE)
v2.train <- apps_models[trainIndex,]
v2.test <- apps_models[-trainIndex,]

# Balancear a variável target com o método upSample
v2.train <- upSample(v2.train[, 1:5],
                     v2.train$download,
                     yname = "download")

# Treinamento da primeira versão do modelo
modelo_v2 <- naiveBayes(download ~ endereco_ip
                             + id_canal
                             + id_aplicativo,
                             data = v2.train)

sum(v2.train$download == predict(modelo_v2, v2.train)) / nrow(v2.train)

previsao_v2 <- predict(modelo_v2, v2.test)

cm_v2 <- confusionMatrix( table(v2.test$download, previsao_v2), positive = "1", mode = "prec_recall"  )
cm_v2


# TO-DO: AVALIAR MELHOR O MODELO