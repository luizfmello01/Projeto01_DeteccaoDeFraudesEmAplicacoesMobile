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