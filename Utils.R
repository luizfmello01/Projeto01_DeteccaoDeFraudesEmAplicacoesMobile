### Biblioteca para esse projeto ###

library(data.table)

# Função para converter as variáveis númericas em Categorias
to.factor <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  
  return(df)
}

# Função para converter as variáveis em catacter
to.caracter <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.character(df[[variable]])
  }
  
  return(df)
}

# Função para converter as variáveis para inteiro
to.integer <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.integer(df[[variable]])
  }
  
  return(df)
}

# Função para retornar métricas do modelo a partir de uma confusion matrix
metrics.models <- function(cm) {
  return (c(Acuracia = cm$overall[["Accuracy"]],
    Precisao = cm$byClass[["Precision"]],
    Recall = cm$byClass[["Recall"]],
    F1Score = cm$byClass[["F1"]]))
}

# Função para converter as variáveis de hora em catacter
hour.to.factor <- function(variable) {
  hora <- hour(variable)
  resultado <- ""
  
  if ( hora >= 0 & hora <= 5  ) {
    resultado <- "Madrugada"
  } else if( hora >= 6 & hora <= 12  ) {
    resultado <- "Manhã"
  } else if ( hora >= 13 & hora <= 18 ) {
    resultado <- "Tarde"
  } else {
    resultado <- "Noite"
  }
  
  return(resultado)
}