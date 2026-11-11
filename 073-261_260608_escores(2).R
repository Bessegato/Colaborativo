	# --------------------------------------------------------------- #
	#		Disciplina: EST073 - Análise Multivariada			#
	#											#
	#		   		Escores Fatoriais					#
	# 											#
	# 		Prof. Lupercio F. Bessegato - 2026-1			#
	# --------------------------------------------------------------- #

		# ------------  Stock-price Data  --------------
		# Exemplos 	8.5 (p. 451); 9.4 (p. 493; 9.5 (p. 497); 
		#		9.7 (P. 503), 9.10 (p. 510); 9.12 (p. 517);
		#		9.13 (p. 519)
		#
		# Período: Jan 2004 a Dez 2005
		# Fonte: Johnson & Wichern
		# ----------------------------------------------------
## taxa retorno = preço fechamento semana (atual - passada)/passada

acoes <- read.table("T8-4.DAT")
nomes <- c("JP Morgan", "Citibank", "W. Fargo", "Shell", "Exxon")
names(acoes) <- nomes
str(acoes)
head(acoes)
dim(acoes)
# [1] 103   5

n <- dim(acoes)[1]
p <- dim(acoes)[2]



x_bar <- colMeans(acoes)
round(x_bar, 4)
# JP Morgan  Citibank  W. Fargo     Shell     Exxon 
#    0.0011    0.0007    0.0016    0.0040    0.0040

  # Conjunto de dados padronizados
acoes.pad <- scale(acoes)

  # verificação correlação entre observações

acf(acoes[1])$acf
sapply(acoes, acf)
# aparenta estar não correlacionado


  # dimensões
n <- dim(acoes)[1]
p <- dim(acoes)[2]

  # matriz de covariâncias das variáveis padronizadas
R <- cor(acoes)
round(R, 3)
#           JP Morgan Citibank W. Fargo Shell Exxon
# JP Morgan     1.000    0.632    0.510 0.115 0.154
# Citibank      0.632    1.000    0.574 0.322 0.213
# W. Fargo      0.510    0.574    1.000 0.182 0.146
# Shell         0.115    0.322    0.182 1.000 0.683
# Exxon         0.154    0.213    0.146 0.683 1.000


  		# -----  Estimacao por componentes Principais - Ex. 9.4 -----


acoes.av <- eigen(R)
lambda <- acoes.av$values
E <- acoes.av$vectors

L.cp <-   E %*% diag(sqrt(lambda)) 

rownames(L.cp) <- nomes
colnames(L.cp) <- paste0("F", 1:5)
round(L.cp, 3)
##               F1     F2     F3     F4     F5
## JP Morgan -0.732  0.437  0.428  0.230  0.194
## Citibank  -0.831  0.280  0.096 -0.398 -0.251
## W. Fargo  -0.726  0.374 -0.546  0.183  0.036
## Shell     -0.605 -0.694 -0.066 -0.241  0.300
## Exxon     -0.563 -0.719  0.077  0.312 -0.251

  # modelo fatorial ortogonal (m = 2)

L.cp2 <- L.cp[, 1:2]

  # comunalidades
comum <- rowSums(L.cp2**2)

  # variância específica
unica <- 1 - comum
psiInv.cp2 <- diag(1/unica)
psiInv.cp2
  # tabela ajuste - Componentes Principais (m = 2)
tab.cp2 <- cbind(L.cp2, comum, unica)
round(tab.cp2, 3)
#               F1     F2 comum unica
# JP Morgan -0.732  0.437 0.727 0.273
# Citibank  -0.831  0.280 0.770 0.230
# W. Fargo  -0.726  0.374 0.667 0.333
# Shell     -0.605 -0.694 0.847 0.153
# Exxon     -0.563 -0.719 0.834 0.166

  # Resíduos do ajuste
res.cp2 <- R - L.cp2 %*% t(L.cp2) - diag(unica)
sum(res.cp2**2)
## [1] 0.1860762

  # resíduos usando tcrossprod()
sum((R - tcrossprod(L.cp2) - diag(unica))**2)


  		# -----  Estimacao por máxima verossimilhança - Ex. 9.5  -----


  # modelo fatorial ortogonal (m = 2)
acoes.fat2 <- factanal(x = acoes, factors = 2, 
                       scores = 'none', # c('none, 'regression', 'Bartlett'), 
                       rotation = 'varimax' # 'none' # 'varimax'
                       )
str(acoes.fat2)

L.mv2 <- loadings(acoes.fat2)

rownames(L.mv2) <- nomes
colnames(L.mv2) <- paste0("F", 1:2)

L.mv2
# Loadings:
#           Factor1 Factor2
# JP Morgan  0.121   0.754 
# Citibank   0.328   0.786 
# W. Fargo   0.188   0.650 
# Shell      0.997         
# Exxon      0.685         

#                Factor1 Factor2
# SS loadings      1.621   1.610
# Proportion Var   0.324   0.322
# Cumulative Var   0.324   0.646

colSums(unclass(L.mv2)^2)

  # Gráfico dos loadings
plot(L.mv2, type = "n", ylim = c(0, 1), xlim = c(0, 1))
text(L.mv2, labels = names(acoes), cex = .9)

  # comunalidade
comum <- rowSums(L.mv2**2)
round(comum, 2)
# JP Morgan  Citibank  W. Fargo     Shell     Exxon 
#      0.58      0.73      0.46      1.00      0.47

  # variância específica
unica <- 1 - comum
round(unica, 2)
# JP Morgan  Citibank  W. Fargo     Shell     Exxon 
#      0.42      0.27      0.54      0.00      0.53
psiInv.mv2 <- diag(1/unica)
psiInv.mv2

  # Tabela de resultados
tab.mv2 <- cbind(L.mv2[, 1:2], comum, unica)
round(tab.mv2, 3)
#           Factor1 Factor2 comum unica
# JP Morgan   0.121   0.754 0.583 0.417
# Citibank    0.328   0.786 0.725 0.275
# W. Fargo    0.188   0.650 0.458 0.542
# Shell       0.997  -0.007 0.995 0.005
# Exxon       0.685   0.026 0.470 0.530

  # Variância total explicada
SQ.fat <- colSums(unclass(L.mv2)^2)
var.prop <- SQ.fat/p
var.acum <- cumsum(var.prop) 

tab.fat <- rbind(L.mv2, SQ.fat, var.prop, var.acum)

tab.com <- cbind(c(comum, soma <- sum(comum), soma/p, soma/p), 
		     c(1 - comum, soma <- sum(1 - comum), soma/p, soma/p))

tab.mv2 <- cbind(tab.fat, tab.com)
round(tab.mv2, 3)
##           Factor1 Factor2            
## JP Morgan   0.121   0.754 0.583 0.417
## Citibank    0.328   0.786 0.725 0.275
## W. Fargo    0.188   0.650 0.458 0.542
## Shell       0.997  -0.007 0.995 0.005
## Exxon       0.685   0.026 0.470 0.530
## SQ.fat      1.622   1.610 3.232 1.768
## var.prop    0.324   0.322 0.646 0.354
## var.acum    0.324   0.646 0.646 0.354


SQ.tot <- colSums(tab.mv2[, 3:4])/p
explica <- c(explica.fat, explica.tot)


tab.mv2 <- rbind(tab.mv2, explica)
round(tab.mv2, 3)
#           Factor1 Factor2 comum unica
# JP Morgan   0.121   0.754 0.583 0.417
# Citibank    0.328   0.786 0.725 0.275
# W. Fargo    0.188   0.650 0.458 0.542
# Shell       0.997  -0.007 0.995 0.005
# Exxon       0.685   0.026 0.470 0.530
# explica     0.324   0.322 0.646 0.354


  # matriz de resíduos
res.mv2 <- R - L.mv2 %*% t(L.mv2) - diag(unica)

  # soma resíduos quadráticos estimação MV
sum(res.mv2**2)
# [1] 0.007612007

sum((R - tcrossprod(L.mv2) - diag(unica))**2)
## [1] 0.007612007

  # soma resíduos quadráticos estimação CP
sum(res.cp2**2)
# [1] 0.1860762

  		# -----  Teste Quantidade de Fatores - Ex. 9.7 (p. 503) ------

  			# H0: m fatores são suficientes
 			# H1: necessários mais que m faores

  		# ---- Estatística de Bartlett
m <- 2
correcao <- (n - 1 - (2*p + 4*m + 5)/6)
correcao
## [1] 98.16667

det(R)
## [1] 0.1752028

det(tcrossprod(L.mv2) + diag(unica))
## [1] 0.178817

T <-  correcao * log(det(tcrossprod(L.mv2) + diag(unica))/det(R))
T
## [1] 2.00445

  # p-valor
gl <- ((p - m)**2 - p - m)/2
gl
## [1] 

p.valor <- 1 - pchisq(q = T, df = gl)
p.valor
## [1] 0.1568381

  		# ---- Estatística factanal

  # máxima verossimilhança (ou LOG VEROSSIMILHANÇA)
logVeros <- acoes.fat2$criteria['objective']
logVeros
# objective 
# 0.02011018


estatistica <- correcao * logVeros
estatistica
## objective 
##  1.974149

acoes.fat2$STATISTIC
## 1.974149

p.valor <- 1 - pchisq(estatistica, gl)
p.valor
## objective 
## 0.1600081

acoes.fat2$PVAL
## 0.1600081


  		# -----  Qualidade Ajuste - AIC & BIC - Ex. 9.7 ------


# -----------  PAREI AQUI  ------
- CALCULAR BIC MANUAL
- CALCULAR AIC MANUAL
- CALCULAR ESCORES FATORIAIS MQ ponderados, MQO, Regressão (MANUAL e COMANDO)
- Comparar com saídas
- Plots



psych::KMO(R)

  		# -----  Rotação Ortogonal - Ex. 9.10 ------

 # rotação Varimax - loadings por CP
L.cpr <- varimax(L.cp2, normalize = TRUE, eps = 1e-5)
round(L.cpr$loadings, 3)
# Loadings:
#     [,1]   [,2]  
# [1,] -0.852       
# [2,] -0.849 -0.220
# [3,] -0.812       
# [4,] -0.126 -0.912
# [5,]        -0.910
#                 [,1]  [,2]
# SS loadings    2.128 1.717
# Proportion Var 0.426 0.343
# Cumulative Var 0.426 0.769

  # matriz de rotação
L.cpr$rotmat
#            [,1]      [,2]
# [1,]  0.8368530 0.5474277
# [2,] -0.5474277 0.8368530

 # rotação Varimax - loadings por MV
L.mvr <- varimax(L.mv2, normalize = TRUE, eps = 1e-5)

round(L.mvr$loadings, 3)
# Loadings:
#           Factor1 Factor2
# JP Morgan         0.763  
# Citibank  0.232   0.819  
# W. Fargo  0.108   0.668  
# Shell     0.991   0.113  
# Exxon     0.677   0.108  
# 
#                Factor1 Factor2
# SS loadings      1.507   1.725
# Proportion Var   0.301   0.345

print(L.mvr, digits = 3, cutoff = 0.1, sort = TRUE)
str(L.mvr$loadings)
  # matriz de rotação
round(L.mvr$rotmat, 3)
#            [,1]      [,2]
# [1,]  0.9927706 0.1200276
# [2,] -0.1200276 0.9927706

unclass(L.mvr$loadings)

loadings(L.mvr, sort = TRUE)

  # rotação - argumento função factanal()
acoes.fat2Rot <- factanal(x = acoes, factors = 2, 
				scores = 'Bartlett', # c('none, 'regression', 'Bartlett'),
        rotation = 'varimax' # 'none' # 
			   )
acoes.fat2Rot

# Uniquenesses:
# JP Morgan  Citibank  W. Fargo     Shell     Exxon 
#     0.417     0.275     0.542     0.005     0.530 
# Loadings:
#           Factor1 Factor2
# JP Morgan 0.763          
# Citibank  0.819   0.232  
# W. Fargo  0.668   0.108  
# Shell     0.113   0.991  
# Exxon     0.108   0.677  
#                Factor1 Factor2
# SS loadings      1.725   1.507
# Proportion Var   0.345   0.301
# Cumulative Var   0.345   0.646


	# ---- Rotação Quartimax

ajuste.quartimax <- GPArotation::quartimax(acoes.fat2$loadings)
print(ajuste.quartimax$loadings, digits = 3, cutoff = 0.1, sort = TRUE)
## Loadings:
##           Factor1 Factor2
## JP Morgan         0.764  
## Citibank  0.222   0.822  
## W. Fargo  0.101   0.669  
## Shell     0.990   0.124  
## Exxon     0.676   0.116  
## 
##             Factor1 Factor2
## SS loadings   1.496   1.736

  # matriz de rotação
ajuste.quartimax$Th

	# ---- Rotação Orthomax


	# ---- Rotação Orthomax

GPArotation::equamax(acoes.fat2$loadings)
## Orthogonal rotation method Crawford-Ferguson Varimax converged.
## Loadings:
##           Factor1 Factor2
## JP Morgan 0.763          
## Citibank  0.820   0.232  
## W. Fargo  0.668   0.108  
## Shell     0.113   0.991  
## Exxon     0.108   0.677  
## 
##                Factor1 Factor2
## SS loadings      1.725   1.507
## Proportion Var   0.345   0.301
## Cumulative Var   0.345   0.646
## AUC              0.819   0.920
## FSI              0.166   0.398


  		# -----  Escores Fatoriais - Ex. 9.12 e 9.13 ------

ze <- c(0.5, -1.40, -0.20, -0.70, 1.40)


  			# --- Escores por Mínimos Quadrados Ponderados - Ex. 9.13

L.Rot <- acoes.fat2Rot$loadings
L.Rot <- L.mv2
L.Rot
Loadings:
#           Factor1 Factor2
# JP Morgan 0.763          
# Citibank  0.819   0.232  
# W. Fargo  0.668   0.108  
# Shell     0.113   0.991  
# Exxon     0.108   0.677  

#                Factor1 Factor2
# SS loadings      1.725   1.507
# Proportion Var   0.345   0.301
# Cumulative Var   0.345   0.646

unica <- acoes.fat2Rot$uniquenesses)

parcela1 <- t(L.Rot) %*% psiInv.mv2  # diag(1/unica)
W.mqp <- solve(parcela1 %*% L.Rot, parcela1)
round(W.mqp, 3)
##      [,1]   [,2]   [,3]   [,4]  [,5]
## F1  0.408  0.645  0.271 -0.201 0.012
## F2 -0.048 -0.072 -0.031  1.027 0.005

  # escores fatoriais de z
W.mqp %*% ze
#               [,1]
# Factor1 -0.5963067
# Factor2 -0.6288642

  # Escores por mínimos quadrados dos dados padronizados
F.mq <- acoes.pad %*% t(W.mqp)
head(F.mq)
plot(F.mq, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), asp = 1)
  # vetor de médias dos fatores
colMeans(F.mq)
#       Factor1       Factor2 
# -1.317042e-17  1.759986e-18

  # Matriz de covariâncias dos fatores
cov(F.mq)

psych::cortest.bartlett(cov(F.mq), n = n)
# $chisq
# [1] 0.07037275
# $p.value
# [1] 0.7907948
# $df
# [1] 1

  # Gráfico dos escores fatoriais
plot(F.mq, xlab = 'Fator 1', ylab = 'Fator 2')
abline(h = 0, v = 0, lty = 2)


  			# --- Escores por Regressão - Exemplo 9.12

W.reg <- t(L.Rot) %*% solve(R)
round(W.reg, 3)
#         JP Morgan Citibank W. Fargo  Shell Exxon
# Factor1     0.333    0.526    0.221 -0.142 0.010
# Factor2    -0.039   -0.058   -0.025  1.015 0.005

  # escores fatoriais de z
W.reg %*% ze
#               [,1]
# Factor1 -0.5010185
# Factor2 -0.6369007

  # Escores por mínimos quadrados dos dados padronizados
F.reg <- acoes.pad %*% t(W.reg)

  # vetor de médias dos fatores
colMeans(F.reg)
#       Factor1       Factor2 
# -2.469034e-17  8.505198e-18

  # Matriz de covariâncias dos fatores
cov(F.reg)
#            Factor1    Factor2
# Factor1 0.81757553 0.02145502
# Factor2 0.02145502 0.99244004


psych::cortest.bartlett(cov(F.reg), n = n)
# $chisq
# [1] 0.0462726
# $p.value
# [1] 0.829681
# $df
# [1] 1

  # Gráfico dos escores fatoriais
plot(F.reg, xlab = 'Fator 1', ylab = 'Fator 2')
abline(h = 0, v = 0, lty = 2)

	# Concordância entre os escores por MQP e Regressão
cor(F.mq, F.reg)
#              Factor1       Factor2
# Factor1 9.997163e-01 -9.981377e-16
# Factor2 2.126077e-17  9.997163e-01

sapply(1:2, FUN = function(x) cor(F.mq[, x], F.reg[, x]))
# [1] 0.9997163 0.9997163

  # Concordância entre os escores
par(mfrow = c(1, 2))
for(i in 1:2){
	plot(F.mq[, i] ~ F.reg[, i], xlim = c(-2.5, 2.5), ylim = c(-2.5, 3.1),
		main = paste('Fator', i), 
		ylab = 'Mínimos Quadrados Ponderados', xlab = 'Regressão')
}
par(mfrow = c(1, 1))

  			# --- Escores por Ad Hoc - Exemplo 9.13


