	# --------------------------------------------------------------- #
	#	  Disciplina: EST065 - Estatística Computacional I		#
	#											#
	#		   		Álgebra Matricial					#
	# 											#
	# 		Prof. Lupercio F. Bessegato - 2026-3			#
	# --------------------------------------------------------------- #



pairs(~ mpg + wt + disp + hp + drat, data = mtcars, 
      pch = 18, col = "steelblue")

GGally::ggpairs(mtcars[, c('mpg', 'wt', 'disp', 'hp', 'drat')])


mtcars.ml <- lm(mpg ~ wt + disp + hp + drat, data = mtcars)
mtcars.ml
## Coefficients:
## (Intercept)           wt         disp           hp         drat  
##   29.148738    -3.479668     0.003815    -0.034784     1.768049  

beta <- mtcars.ml$coefficients

X <- cbind(intercepto = rep(1, dim(mtcars)[1]), 
           mtcars[, c( 'wt', 'disp', 'hp', 'drat')])

Y <- mtcars[, 'mpg']

class(X)
## [1] "data.frame"

dim(X)
## [1] 32  5
X <- as.matrix(X)
as.vector(X[linha, ])
linha <- 3 
sum(X[linha, ] * beta)

ajuste <- function(xis) (sum(xis *beta))
apply(X, MARGIN = 1, FUN = ajuste)

mtcars.ml$fitted.values[1]

Y <- as.vector(mtcars[, 'mpg'])

Y <- mtcars[, 'mpg']
is.vector(Y)

solve(t(X) %*% X) %*% t(X) %*% Y
##                   [,1]
## intercepto 19.34429256
## disp       -0.01923223
## hp         -0.03122932
## drat        2.71497521

solve(crossprod(X), t(X))%*%Y


  # An example of multiple regression with R using mtcars, by Jim Milks
https://rstudio-pubs-static.s3.amazonaws.com/743575_238ff8744804400c9acf10eda9ed3a42.html

  # Análise do Consumo Médio de Combustível de Carros Modelos 1973/1974
https://curso-r.github.io/trabalhos-regressao-linear/posts/2021-02-05-anlise-do-consumo-mdio-de-combustvel-de-carros-modelos-19731974/

  # !! optim()
  # Regressão Linear - mtcars
https://scopinho.github.io/ML/REGRESSAO/RegLinear-01.html

  # Análise Exploratória do Conjunto de Dados mtcars: Desempenho, Eficiência e 
  # Relações Entre Variáveis, by Raphael Silvestre Barbosa
https://rstudio-pubs-static.s3.amazonaws.com/1363881_a70215fe83594c529843927badf3bf17.html


# -----------------------

  # matrizes
X <- cbind(intercepto = rep(1, dim(mtcars)[1]), 
           mtcars[, c( 'wt', 'disp', 'hp', 'drat')])
X <- as.matrix(X)

Y <- mtcars[, 'mpg']

  # verificação dos resultados
formula <- as.formula(mpg ~wt + disp + hp + drat)

beta <- lm(formula, data = mtcars)$coefficients

beta <- solve(t(X) %*% X) %*% t(X) %*% Y

beta <- solve(crossprod(X), t(X))%*%Y


beta  
##  (Intercept)           wt         disp           hp         drat 
## 29.148737553 -3.479667528  0.003815241 -0.034783534  1.768048769



rbenchmark::benchmark('mod' = beta <- lm(formula, data = mtcars)$coefficients,
                      'pseudoinversa' = beta <- solve(t(X) %*% X) %*% t(X) %*% Y,
                      'sistema linear' = beta <- solve(crossprod(X), t(X))%*%Y,
                      replications = 1000, 
                      columns = c("test", "elapsed", "relative", "user.self", 
                                  "sys.self")
                      )
##             test elapsed relative user.self sys.self
## 1            mod   0.983   32.767     0.983        0
## 2  pseudoinversa   0.046    1.533     0.046        0
## 3 sistema linear   0.030    1.000     0.029        0
> 


args(plot)

set.seed(666)
N <- 500
amostra <- rexp(N)
hist(amostra, freq = F)
mean(amostra)

cumsum(amostra)

278.960139/300
#[1] 0.9298671

media.acum <- cumsum(amostra)/1:N
abline(h = mean(amostra), lty = 2)

plot(media.acum, type = 'l')
abline(h =1, lty = 2)


  # ----

# REPETE COM FUNÇÃO
desenha.linha <- function(){
  amostra <- rexp(N)
  media.acum <- cumsum(amostra)/1:N
  lines(media.acum, lty = 1, col = 'gray90')  
}

N <- 500
plot(0, xlim = c(0, N), ylim = c(0.6, 1.4))

invisible(replicate(n = 1E3, expr = desenha.linha()))


abline(h =1, lty = 2, lwd = 2, col = 'white')




http://127.0.0.1:47353/graphics/cc93f0ff-bdb2-4c6d-8d3a-7ec2b14ad0b5.png
hist(amostra, freq = FALSE)
