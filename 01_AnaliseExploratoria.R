### Parte 1: Análise exploratória dos dados ###
## Nesse parte do script será realizado a compreensão dos dados,
## Medidas de tendencia central, dispersão, histogramas, boxplots, análise de correlação,
## tabela de contingência, proporção da categoria, etc.


#### Definir local de trabalho ####
setwd("C:/repos/Projeto01/Projeto01_DeteccaoDeFraudesEmAplicacoesMobile")
getwd()


#### Carregar pacotes e arquivos utilitários ####
source("Utils.R")
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

# O dataset tem muitos dados, vamos realizar um split no dataset
# indexes <- sample.int(nrow(apps), 2000000)
# apps <- apps[indexes,]

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