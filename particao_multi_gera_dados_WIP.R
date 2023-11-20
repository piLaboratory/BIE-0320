## Script para gerar dados melhores para a particao multivariada
## Work in progress, não usado ainda para os roteiros


################################################################################
## Funcoes ##
## Funcao para plotar a imagem de uma variavel no grid
## X, Y : vetores com as cordenadas X e Y
## Z: vetor com os valores da variavel para todas as combinacoes de X e Y tal como obtido de expand.grid(X, Y)
f1 <- function(X, Y, Z, ...){
    m1 <- matrix(Z, ncol=length(Y))
    image(X, Y, m1, ...)
}

## Funcao para colocar coeficientes de correlacao em uma das diagonais
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
     {
         usr <- par("usr"); on.exit(par(usr))
         par(usr = c(0, 1, 0, 1))
         r <- abs(cor(x, y))
         txt <- format(c(r, 0.123456789), digits = digits)[1]
         txt <- paste0(prefix, txt)
         if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
         text(0.5, 0.5, txt, cex = cex.cor * r)
     }

# Define function to draw random samples from a multivariate normal
# distribution (http://rstudio-pubs-static.s3.amazonaws.com/9688_a49c681fab974bbca889e3eae9fbb837.html)
rmvn <- function(n, mu = 0, V = matrix(1)) {
    p <- length(mu)
    if (any(is.na(match(dim(V), p)))) 
        stop("Dimension problem!")
    D <- chol(V)
    t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}


################################################################################
## Cria parcelas distribuidas em um grid com dois gradientes ambientais
## N de linhas e colunas de parcelas
X <- 1:10
Y <- 1:10
N <- length(X)*length(Y)

## 1. Dataframe com as coordenadas das parcelas
coords <- expand.grid(X=X, Y=Y)

## 2. matriz de dados ambientais
set.seed(68)
amb <- data.frame(
    ## Gradiente na diagonal da parcela
    x1 = rnorm(N, mean= 5*coords$X + 5*coords$Y, sd = 1.5),
    x2 = rnorm(N, mean= 7*coords$X + 3*coords$Y, sd = 2.5),
    ## Tabuleiro de damas com 4 casas (1o passo)
    x3 = rnorm(N, mean = 100, sd= 15),
    x4 = rnorm(N, mean = 100, sd=15),
    ## Sem estrutura espacial
    x5 = rnorm(N, mean=50, sd=10)
    )
## Cria manchas nas variaveis 3 e 4
amb$x3[(coords$X<=4&coords$Y<=4)|(coords$X>6&coords$Y>6)] <-
    rnorm(sum((coords$X<=4&coords$Y<=4)|(coords$X>6&coords$Y>6)),
          mean=50, sd=15)
amb$x4[(coords$X<=5&coords$Y<=5)|(coords$X>5&coords$Y>5)] <- rnorm(N/2, mean=50, sd=10)
## valores negativos se tornam zero
amb[amb<0] <- 0

## 3. Manchas espaciais autocorrelacionadas (para simular manchas de plantas sem correlacao com variaveis ambientais)
mancha <- runif(N, 0, 5)
index <-  (coords$X>1&coords$X<4&coords$Y>1&coords$Y<4) | (coords$X>2&coords$X<5&coords$Y>4&coords$Y<7)
mancha[index] <- rnorm(sum(index), 50, 5)

## 4. Dataframe com especies ##
sp <- data.frame(
    sp1 = rpois(N, lambda = (amb$x1)/2), #relacao positiva com variavel 1
    sp2 = rpois(N, lambda = (amb$x2)/4), # relacao positiva com variavel 2
    ##sp3 = rpois(N, lambda = (amb$x2+amb$x1)/6), # relacao postica com variaveis 1 e 2
    sp3 = rpois(N, lambda = max(amb$x1)/2 - (amb$x1)/2), # relacao negativa com variavel 1
    sp4 = rpois(N, lambda = max(amb$x2+amb$x1)/4 - (amb$x2+amb$x1)/4), ## Relacao negativa com variaveis 1 e 2
    sp5 = rpois(N, lambda = (amb$x3)/2), # positiva com variavel 3
    sp6 = rpois(N, lambda = (amb$x4)/3), # positiva com variavel 4
    ##sp7 = rpois(N, lambda = (amb$x4+amb$x3)/4), # positiva com 3 e 4
    sp7 = rpois(N, lambda =  max(amb$x3)/3-(amb$x3)/3), # negativa com 3
    sp8 = rpois(N, lambda =  max(amb$x4)/4-(amb$x4)/4), # negativa com 4
    sp9  = rpois(N, lambda = (amb$x5)/2), # positiva com variavel 5
    sp10 = rpois(N, lambda = mancha) # apenas estrutura espacial agregada (duas manchas)
    )

###grava os csv
write.csv2(coords, "data/coordenadas.csv", row.names=FALSE)
write.csv2(amb, "data/ambientais.csv", row.names=FALSE)
write.csv2(sp, "data/especies.csv", row.names=FALSE)

################################################################################

## Verificando ##
## paleta de cinza
col1 <- gray.colors(10, start=0.93, end=0.2)
## Valores no espaço ##
## Variaveis ambientais
par(mfrow=c(2,3), mar=c(1.5,1,1.5,1))
for(i in 1:5){
    f1(X, Y, amb[,i], col=col1, axes=FALSE, xlab="", ylab="", main=paste("var",i), cex.main=2)
}
## Especies
par(mfrow=c(2,5), mar=c(1.5,1,1.5,1))
for(i in 1:10){
    f1(X, Y, sp[,i], col=col1, axes=FALSE, xlab="", ylab="", main=paste("sp",i))
}

## Scatterplot matrices
## Entre ambientais
pairs(amb, upper.panel=panel.cor)
## Entre especies
pairs(sp, upper.panel=panel.cor)
## sp x ambientais
par(mfrow=c(10,5), mar=c(0.1,0.1,0.1,0.1))
for(i in 1:10){
    for(j in 1:5){
        plot(sp[,i] ~ amb[,j], axes=FALSE)
        box()
    }
}


### pca das especies
sp.pca <- rda(sp, scale=TRUE)
par(mfrow=c(1,1))
biplot(sp.pca, scaling=2, type = c("text", "text"))
summary(sp.pca)

### RDA constrained
rda.sp.amb<-rda(sp, amb) 

## Biplot------------------------------------------------------------------------
plot(rda.sp.amb, type="none", choices=c(1,2))
points(rda.sp.amb, display="sites", cex=0.01) ##Adicionando os pontos referentes às parcelas
text(rda.sp.amb, display="sites", cex=0.5) ##Adicionando os nomes das parcelas
text(rda.sp.amb, display="bp", col="blue") ##Adicionando as setas das variáveis ambientais
spamb2.scores<-scores(rda.sp.amb, choices=1:2, display="sp") ##selecionando os escores das espécies (foram selecionados apenas os eixos 1 e 2)
arrows(0,0,spamb2.scores[,1],spamb2.scores[,2], length=0, lty=1, col="red") ##adicionando linhas para as espécies
text(rda.sp.amb, display = "species", cex=0.8, adj=-0.3, col="red") ##Adicionando nomes às linhas das espécies

## ------------------------------------------------------------------------
coords<-read.csv2 ("coordenadas.csv")
head (coords)
summary(coords)
