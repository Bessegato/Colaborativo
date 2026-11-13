	# --------------------------------------------------------------- #
	#		Disciplina: EST073 - Análise Multivariada			#
	#											#
	#	     	  Análise Fatorial - Ajuste Decatlo - J&W			#
	# 											#
	# 		Prof. Lupercio F. Bessegato - 2026-3			#
	# --------------------------------------------------------------- #

		# ------------  Olympic Decathlon Data  --------------
		# Exemplos 	9.6 (p. 499); 9.11 (p. 511)
		#
		# Dados: E9-6. DAT (matriz de correlações
		# n = 280 complete strats 
		# Período: 1960 through 2004
		# Fonte: Johnson & Wichern
		# ----------------------------------------------------

## Valores padronizados para cada evento e sinais dos eventos de tempo trocados 
tal que escores grandes são bons para todos os eventos

decatlo <- read.table("../dados/E9-6.DAT", header = F)
decatlo

nomesBR <- c('100m', 'S_Dist', 'A_Peso', 'S_Alt', '400m', '110b', 'L_Disco', 
		  'S_Vara', 'L_Dardo', '1500m')
names(decatlo) <- nomesBR

R <- decatlo

	# ---- Decomposição Espectral

decatlo.av <- eigen(R)
lambda <- decatlo.av$values
E <- decatlo.av$vectors

print(lambda, digits = 3)
# [1] 4.213 1.390 1.059 0.918 0.670 0.576 0.421 0.322 0.253 0.179

print(cumsum(lambda*10), digits = 3)
##  [1]  42.1  56.0  66.6  75.8  82.5  88.3  92.5  95.7  98.2 100.0

plot(lambda, type = "b")
abline(h = 1, col = 'gray', lwd = 2)


## Sugestão de m - 3 ou m =4


	# ----- Estimação por Componentes Principais


# FAZER FUNÇÃO PARA ESTIMAÇÃO POR COMPONENTES PRINCIPAIS


L <- E %*% diag(sqrt(lambda))
rownames(L) <- nomesBR
colnames(L) <- paste0('F', 1:10)

  # Substituir valores pequenos por NA - Aternativa 1
cutoff <- 0.30
L_filtro <- L
L_filtro[abs(L_filtro) < cutoff] <- NA
print(L_filtro, digits = 2)
##         F1    F2    F3    F4    F5    F6    F7 F8 F9  F10
100m    -0.70    NA  0.47  0.42    NA    NA    NA NA NA   NA
S_Dist  -0.79    NA    NA    NA    NA    NA    NA NA NA   NA
A_Peso  -0.77  0.43    NA    NA    NA    NA    NA NA NA 0.31
S_Alt   -0.71    NA    NA -0.37  0.34    NA    NA NA NA   NA
400m    -0.60 -0.55    NA  0.40    NA    NA    NA NA NA   NA
110b    -0.51    NA  0.37 -0.56 -0.41  0.30    NA NA NA   NA
L_Disco -0.69  0.46    NA    NA    NA  0.31    NA NA NA   NA
S_Vara  -0.76    NA    NA -0.30    NA    NA -0.49 NA NA   NA
L_Dardo -0.52    NA -0.52    NA -0.53 -0.32    NA NA NA   NA
1500m      NA -0.75 -0.49    NA    NA    NA    NA NA NA   NA

  # Substituir valores pequenos por NA - Aternativa 2
cutoff <- 0.30
L_filtro <- L
L_filtro[abs(L_filtro) < cutoff] <- NA

  # 1) formatar preservando decimais e alinhamento
L_car <- formatC(L_filtro, format = "f", digits = 2)

  # 2) trocar NA por espaço em branco
L_car[is.na(L_filtro)] <- ""

  # 3) restaurar dimensão e dimnames (formatC preserva, mas garanta)
dim(L_car) <- dim(L_filtro)
dimnames(L_car) <- dimnames(L_filtro)

print(L_car, quote = FALSE)
##         F1    F2    F3    F4    F5    F6    F7    F8 F9 F10 
## 100m    -0.70       0.47  0.42                              
## S_Dist  -0.79                                               
## A_Peso  -0.77 0.43                                      0.31
## S_Alt   -0.71             -0.37 0.34                        
## 400m    -0.60 -0.55       0.40                              
## 110b    -0.51       0.37  -0.56 -0.41 0.30                  
## L_Disco -0.69 0.46                    0.31                  
## S_Vara  -0.76             -0.30             -0.49           
## L_Dardo -0.52       -0.52       -0.53 -0.32                 
## 1500m         -0.75 -0.49                  

		# ----- Modelo com 3 fatores  ----
L.cp3 <- L[, 1:3]
comum.cp3 <- rowSums(L.cp3**2)
comum.cp3

unica.cp3 <- 1 - comum.cp3

var.cp3 <- colSums(L.cp3**2)

  # tabela de apresentação
m <- 3
tab.cp3 <- matrix(ncol = m + 2, nrow = 11)
mat <- cbind(comum.cp3, unica.cp3)
mat <- rbind(mat, colSums(mat))
mat_car <- formatC(mat, format = "f", digits = 2)
var_car <- formatC(var.cp3, format = 'f', digits = 2)
tab.cp3[, (m +1): (m + 2)] <- mat_car
tab.cp3[1:10, 1:m] <- L_car[1:10, 1:3]
tab.cp3[11, 1:m] <- var_car
rownames(tab.cp3) <- c(rownames(L.cp3), 'Var. Total')
colnames(tab.cp3) <- c(colnames(L.cp3), 'Comum', 'Espec.')
print(tab.cp3, quote = F)
##            F1    F2    F3    Comum Espec.
## 100m       -0.70       0.47  0.70  0.30  
## S_Dist     -0.79             0.70  0.30  
## A_Peso     -0.77 0.43        0.82  0.18  
## S_Alt      -0.71             0.54  0.46  
## 400m       -0.60 -0.55       0.67  0.33  
## 110b       -0.51       0.37  0.41  0.59  
## L_Disco    -0.69 0.46        0.77  0.23  
## S_Vara     -0.76             0.61  0.39  
## L_Dardo    -0.52       -0.52 0.60  0.40  
## 1500m            -0.75 -0.49 0.85  0.15  
## Var. Total 4.21  1.39  1.06  6.66  3.34 

  # Resíduos
sum((R - tcrossprod(L.cp3) - diag(unica.cp3))**2)
## [1] 0.7239806


		# ----- Modelo com 4 fatores  ----
L.cp4 <- L[, 1:4]
comum.cp4 <- rowSums(L.cp4**2)
comum.cp4

unica.cp4 <- 1 - comum.cp4
unica.cp4

var.cp4 <- colSums(L.cp4**2)
var.cp4

  # tabela de apresentação
m <- 4
tab.cp4 <- matrix(ncol = m + 2, nrow = 11)
mat <- cbind(comum.cp4, unica.cp4)
mat <- rbind(mat, colSums(mat))
mat_car <- formatC(mat, format = "f", digits = 2)
var_car <- formatC(var.cp4, format = 'f', digits = 2)
tab.cp4[, (m +1): (m + 2)] <- mat_car
tab.cp4[1:10, 1:m] <- L_car[1:10, 1:m]
tab.cp4[11, 1:m] <- var_car
rownames(tab.cp4) <- c(rownames(L.cp4), 'Var. Total')
colnames(tab.cp4) <- c(colnames(L.cp4), 'Comum', 'Espec.')
print(tab.cp4, quote = F)
##            F1    F2    F3    F4    Comum Espec.
## 100m       -0.70       0.47  0.42  0.88  0.12  
## S_Dist     -0.79                   0.71  0.29  
## A_Peso     -0.77 0.43              0.83  0.17  
## S_Alt      -0.71             -0.37 0.67  0.33  
## 400m       -0.60 -0.55       0.40  0.83  0.17  
## 110b       -0.51       0.37  -0.56 0.72  0.28  
## L_Disco    -0.69 0.46              0.77  0.23  
## S_Vara     -0.76             -0.30 0.70  0.30  
## L_Dardo    -0.52       -0.52       0.61  0.39  
## 1500m            -0.75 -0.49       0.85  0.15  
## Var. Total 4.21  1.39  1.06  0.92  7.58  2.42 

  # Resíduos
sum((R - tcrossprod(L.cp4) - diag(unica.cp4))**2)
## [1] 0.4996217

0.7239806/0.4996217
## [1] 1.449058


	# ----- Estimação por Máxima Verossimilhança


		# ----- Modelo com 3 fatores  ----
		
decatlo.mv3 <- factanal(covmat = as.matrix(R), 
				factors = 3, 
				n.obs = 280,
         			scores = 'none', # c("none", "regression", "Bartlett"),
         			rotation = 'none' #c("none", "varimax")
				)

  # Teste de suficiência de fatores (conservativo)
decatlo.mv3$PVAL
##    objective 
## 1.111412e-10

  # Verossimilhança do modelo
decatlo.mv3$criteria['objective']
## objective 
## 0.3116704

  # ---- Cargas Fatoriais ---

L.mv3 <- loadings(decatlo.mv3)
rownames(L.mv3) <- nomes.br

print(L.mv3, digits = 2, cutoff = 0.35)
## 
## Loadings:
##         Factor1 Factor2 Factor3
## 100m     1.00                  
## S_Dist   0.65                  
## A_Peso   0.51    0.80          
## S_Alt            0.41    0.55  
## 400m     0.56            0.38  
## 110b                           
## L_Disco  0.38    0.73          
## S_Vara   0.42    0.41    0.51  
## L_Dardo          0.47          
## 1500m                    0.48  
## 
##                Factor1 Factor2 Factor3
## SS loadings       2.57    1.85    1.12
## Proportion Var    0.26    0.18    0.11
## Cumulative Var    0.26    0.44    0.55

  # Especificidade
unica.mv3 <- decatlo.mv3$uniquenesses
print(unica.mv3, digits = 2)
##    100m  S_Dist  A_Peso   S_Alt    400m    110b L_Disco  S_Vara L_Dardo   1500m 
##  0.005   0.399   0.083   0.414   0.549   0.796   0.311   0.401   0.736   0.766

  # Resíduos
sum((R - tcrossprod(L.mv3) - diag(unica.mv3))**2)
## [1] 0.2457985


		# ----- Modelo com 4 fatores  ----
		
decatlo.mv4 <- factanal(covmat = as.matrix(R), 
				factors = 4, 
				n.obs = 280,
         			scores = 'none', # c("none", "regression", "Bartlett"),
         			rotation = 'none' #c("none", "varimax")
				)

  # Teste de suficiência de fatores (conservativo)
decatlo.mv4$PVAL
## objective 
## 0.1509172

  # Verossimilhança do modelo
decatlo.mv4$criteria['objective']
## objective 
## 0.05784567

  # ---- Cargas Fatoriais ---

L.mv4 <- loadings(decatlo.mv4)
rownames(L.mv4) <- nomes.br

print(L.mv4, digits = 2, cutoff = 0.35)
## Loadings:
##         Factor1 Factor2 Factor3 Factor4
## 100m     0.99                          
## S_Dist   0.67                          
## A_Peso   0.53    0.78                  
## S_Alt    0.36    0.43    0.42    0.42  
## 400m     0.57            0.62          
## 110b                                   
## L_Disco  0.40    0.72                  
## S_Vara   0.44    0.41    0.39          
## L_Dardo          0.46                  
## 1500m                    0.61          
## 
##                Factor1 Factor2 Factor3 Factor4
## SS loadings       2.69    1.79    1.19    0.54
## Proportion Var    0.27    0.18    0.12    0.05
## Cumulative Var    0.27    0.45    0.57    0.62

  # Especificidade
unica.mv4 <- decatlo.mv4$uniquenesses
print(unica.mv4, digits = 2)
##   100m  S_Dist  A_Peso   S_Alt    400m    110b L_Disco  S_Vara L_Dardo   1500m 
## 0.0095  0.3880  0.0894  0.3271  0.1962  0.7341  0.3040  0.4201  0.7253  0.6000

print(cbind(unica.mv3, unica.mv4), digits = 2)
##         unica.mv3 unica.mv4
## 100m        0.005    0.0095
## S_Dist      0.399    0.3880
## A_Peso      0.083    0.0894
## S_Alt       0.414    0.3271
## 400m        0.549    0.1962
## 110b        0.796    0.7341
## L_Disco     0.311    0.3040
## S_Vara      0.401    0.4201
## L_Dardo     0.736    0.7253
## 1500m       0.766    0.6000

  # Resíduos
sum((R - tcrossprod(L.mv4) - diag(unica.mv4))**2)
## [1] 0.02882793 

0.2457985/0.02882793 # mv3
## [1] 8.526401

0.4996217/0.02882793 # cp4
## [1] 17.33117

0.7239806/0.02882793 # cp3
## [1] 25.11386


## Modelo fatorial escolhido - Estimação por máxima Verossimilhança - 4 fatores


		# ----- Modelo com Rotação Ortogonal - varimax

L.mv4_rot <- varimax(L.mv4, normalize = TRUE, eps = 1E-5)

print(L.mv4_rot, digits = 2, cutoff = 0.35)
## Loadings:
##        Factor1 Factor2 Factor3 Factor4
## 100m     0.93                          
## S_Dist   0.45                    0.55  
## A_Peso           0.88                  
## S_Alt                            0.74  
## 400m     0.52            0.70          
## 110b                             0.47  
## L_Disco          0.79                  
## S_Vara                           0.61  
## L_Dardo          0.48                  
## 1500m                    0.62          
## 
##                Factor1 Factor2 Factor3 Factor4
## SS loadings       1.47    1.96    1.06    1.72
## Proportion Var    0.15    0.20    0.11    0.17
## Cumulative Var    0.15    0.34    0.45    0.62
## 
## $rotmat
##        [,1]  [,2]   [,3] [,4]
## [1,]  0.908  0.26  0.019 0.32
## [2,] -0.361  0.89  0.053 0.29
## [3,] -0.084 -0.20  0.913 0.35
## [4,] -0.195 -0.32 -0.404 0.83


decatlo.mv4_rot2 <- factanal(covmat = as.matrix(R), 
				   factors = 4, 
				   n.obs = 280,
         			   scores = 'none', # c("none", "regression", "Bartlett"),
         			   rotation = 'varimax' #c("none", "varimax")
				   )
L.mv4_rot2 <- loadings(decatlo.mv4_rot)
rownames(L.mv4_rot2) <- nomesBR
print(L.mv4_rot2, digits = 2, cutoff = 0.35)
Loadings:
        Factor1 Factor2 Factor3 Factor4
100m                     0.93          
S_Dist           0.55    0.45          
A_Peso   0.88                          
S_Alt            0.74                  
400m                     0.52    0.70  
110b             0.47                  
L_Disco  0.79                          
S_Vara           0.61                  
L_Dardo  0.48                          
1500m                            0.62  

               Factor1 Factor2 Factor3 Factor4
SS loadings       1.96    1.72    1.47    1.06
Proportion Var    0.20    0.17    0.15    0.11
Cumulative Var    0.20    0.37    0.51    0.62

# AGREGAR VARIÂNCIA DOS FATORES NO GRÁFICO

par(mfrow = c(1, 2))
plot(L.mv4_rot2[, 1], L.mv4_rot2[, 2], pch = 19, 
	xlim = c(0, 1), ylim = c(0, 1),
	xlab = 'Fator 1', ylab = 'Fator 2')
text(L.mv4_rot2[, 1], L.mv4_rot2[, 2], label = nomesBR, pos = 3, cex = 0.85)

plot(L.mv4_rot2[, 1], L.mv4_rot2[, 3], pch = 19, 
	xlim = c(0, 1), ylim = c(0, 1),
	xlab = 'Fator 1', ylab = 'Fator 3')
text(L.mv4_rot2[, 1], L.mv4_rot2[, 3], label = nomesBR, pos = 3, cex = 0.85)
par(mfrow = c(1, 1))