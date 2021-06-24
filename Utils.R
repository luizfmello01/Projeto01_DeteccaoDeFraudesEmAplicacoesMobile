### Biblioteca para esse projeto ###

# Função para converter as variáveis númericas em Categorias
to.factor <- function(df, variables) {
  for (variable in variables) {
    df[[variable]] <- as.factor(df[[variable]])
  }
  
  return(df)
}