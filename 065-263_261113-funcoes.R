fun.acum <- function(vet, fun = median, ...) {
  # -----------------------------------------------------------------
  # Cálculo de mediana média de vetor
  # vet: vetor com observações
  # fun: comando que se deseja acumular
  # fun: função a ser aplicada
  # Adaptado de Gardener, M. Beginning R: The statistical programming
  # language. John Wiley & Sons, 2012.
  # -----------------------------------------------------------------
  saida <- sapply(seq_along(vet), function(x) fun(vet[1:x]))
  cat('\n', deparse(substitute(fun)),'de', deparse(substitute(vet)),'\n')
  print(saida)
} # Fim

set.seed(666)
amostra <- rexp(30)

mediana_acum <- fun.acum(vet = amostra, fun = median)

desvio_acum <- fun.acum(vet = amostra, fun = sd)


# Mostra mensagem e aguarda usuário - estatística acumulada
fun.acum.usr <- function(vet) {
  # ------------------------------------------
  # Cálculo de mediana média de vetor
  # vet: vetor com observações
  # ------------------------------------------
  # fun: função a ser aplicada
  nome <- readline(prompt = "Que função você quer aplicar?: ")
  # transforma string em comando
  fun <- eval(parse(text = nome))
  # calcula estatística acumulada
  saida <- sapply(seq_along(vet), function(x) fun(vet[1:x]))
  cat('\n', nome,'de', deparse(substitute(vet)),'\n')
  print(saida)
} # Fim

obj <- fun.acum.usr(amostra)
