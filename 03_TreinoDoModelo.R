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


# Treinamento da primeira versão do modelo
modelo_v1 <- naiveBayes(download ~ .,
                           data = v1.train)

acuracia_treino_v1 <- round(sum(v1.train$download == predict(modelo_v1, v1.train)) / nrow(v1.train), 2)

paste("Nivel de acurácia dos dados de treino:", acuracia_treino_v1, ",o modelo aprendeu bem com os dados de treino")


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


acuracia_treino_v2 <- round(sum(v2.train$download == predict(modelo_v2, v2.train)) / nrow(v2.train), 2)
paste("Nivel de acurácia dos dados de treino:", acuracia_treino_v2, ",o modelo aprendeu bem com os dados de treino")


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


acuracia_treino_v3 <- round(sum(v3.train_balanceados$download == predict(modelo_v3, v3.train_balanceados)) / nrow(v3.train_balanceados), 2)
paste("Nivel de acurácia dos dados de treino:", acuracia_treino_v3, ",o modelo aprendeu bem com os dados de treino")