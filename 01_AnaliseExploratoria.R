### Parte 1: Análise exploratória dos dados ###
## Nesse parte do script será realizado a compreensão dos dados,
## Medidas de tendencia central, dispersão, histogramas, boxplots, análise de correlação,
## tabela de contingência, proporção da categoria, etc.

# Definir local de trabalho
setwd("C:/repos/Projeto01/Projeto01_DeteccaoDeFraudesEmAplicacoesMobile")
getwd()

# Carregar arquivo utils no script
source("Utils.R")

# Instalação e carga de pacotes
# install.packages("data.table")
# install.packages("ggplot2")
# install.packages("gmodels")
library(data.table)
library(ggplot2)
library(gmodels)
library(dplyr)

# Carregar o dataset
# ?fread
apps <- fread("Datasets/train.csv", sep = ",", header = TRUE)
indexes <- sample.int(nrow(apps), 2000000)
apps <- apps[indexes,]
indexes <- NULL


# Visualizar o dataset
str(apps)
head(apps)

# Converter variáveis numéricas para fator (Obs: Em nosso conjunto de dados
# Não tem nenhuma variável numérica)
variaveis_categoricas <- c("ip", "app", "device",
                           "os", "channel", "is_attributed")
apps <- to.factor(apps, variaveis_categoricas)

# Visualizar o dataset
str(apps)
summary(apps)

# Teste do Qui-quadrado para verificar relacionamento das variáveis
# Váriavel IP e app
chisq.test(apps$ip, apps$app)
# Resultado: Tem um bom relacionamento

# Váriavel app e device (Target)
chisq.test(apps$app, apps$device)
# Resultado: Tem um bom relacionamento

# Váriavel device e is_attributed (Target)
chisq.test(apps$device, apps$os)
# Resultado: Tem um bom relacionamento