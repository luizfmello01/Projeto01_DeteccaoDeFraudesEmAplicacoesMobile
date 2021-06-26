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