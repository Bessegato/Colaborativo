tabela <- c(189, 104, 10845, 10933)
dim(tabela) <- c(2,2)
X <- tabela
gama <- 0.95
RC <- prod(diag(X))/(X[1,2]*X[2,1])
RC

ep.logRC <- sqrt(sum(1/X))

alfa <- 1 - gama

intervalo <- exp(log(RC) + qnorm(p = c(alfa/2, 1 - alfa/2))*ep.logRC)

intervalo[1]
intervalo[2]
