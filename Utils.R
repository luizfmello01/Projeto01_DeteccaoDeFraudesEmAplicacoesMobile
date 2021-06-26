### Biblioteca para esse projeto ###

# Função para converter as variáveis númericas em Categorias
to.factor <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
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