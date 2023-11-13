
dados <- read.csv("data/roteiro1.csv")
## Plot de sp x ambiente
plot(abund ~ pH, data= dados, ylab= "Abundância de L. nom")
regr.pH <- lm(abund ~ pH, data = dados)
abline(regr.pH)
(X <- summary(regr.pH)$r.squared)
## Plot de sp x espaco
plot(abund ~ space, data= dados,
     xlab = "Variável de estrutura espacial", ylab= "Abundância de L. nom")
regr.space <- lm(abund ~ space, data = dados)
abline(regr.space)
(W <- summary(regr.space)$r.squared)
## Tudo
regr.tudo <- lm(abund ~ pH + space, data = dados)
(d <- 1 - summary(regr.tudo)$r.squared)
## pH x espaco
plot(pH ~ space, data = dados, xlab = "Variável de estrutura espacial")
## Calculos da particao
##b
(b <- X + W + d - 1)
##a
X - b
## c
W - b

## Plot de sp x MO
png("images/abund_MO.png")
par(cex.lab=1.5, cex=1.25)
plot(abund ~ MO, data= dados, xlab = "Concentração de matério orgânica", ylab = "Abudância da espécie")
regr.MO <- lm(abund ~ MO, data = dados)
abline(regr.MO)
dev.off()
(X <- summary(regr.pH)$r.squared)

## Plot de sp x MO
png("images/espaco_MO.png")
par(cex.lab=1.5, cex=1.25)
plot(MO ~ space, data= dados, xlab = "Padrão espacial", ylab = "Abudância da espécie")
regr.MO2 <- lm(MO~space, data = dados)
abline(regr.MO2)
dev.off()


## Mapa da especie e do ambiente

## Funcoes ##
## Funcao para plotar a imagem de uma variavel no grid
## X, Y : vetores com as cordenadas X e Y
## Z: vetor com os valores da variavel para todas as combinacoes de X e Y tal como obtido de expand.grid(X, Y)
f1 <- function(X, Y, Z, ...){
    m1 <- matrix(Z, ncol=length(Y))
    image(X, Y, m1, ...)
}


png(filename="images/mapas_single%1d.png")
f1(Y, Y, dados$abund, col=col1, axes=FALSE, xlab="", ylab="", main= "Abundância da espécie")
f1(Y, Y, dados$pH, col=col1, axes=FALSE, xlab="", ylab="", main = "pH")
f1(Y, Y, dados$space, col=col1, axes=FALSE, xlab="", ylab="", main = "Padrão espacial")
f1(Y, Y, dados$MO, col=col1, axes=FALSE, xlab="", ylab="", main = "Matéria Orgânica")
dev.off()



png(filename="images/mapas%1d.png", height=480, width=960)
par(mfrow=c(1,2))
f1(Y, Y, dados$abund, col=col1, axes=FALSE, xlab="", ylab="", main= "Abundância da espécie")
f1(Y, Y, dados$pH, col=col1, axes=FALSE, xlab="", ylab="", main = "pH")
par(mfrow=c(1,1))
 
## Mapa da especie e do espaco
par(mfrow=c(1,2))
f1(Y, Y, dados$abund, col=col1, axes=FALSE, xlab="", ylab="", main= "Abundância da espécie")
f1(Y, Y, dados$space, col=col1, axes=FALSE, xlab="", ylab="", main = "Padrão espacial")
par(mfrow=c(1,1))
dev.off()

png(filename="images/mapas_triplos%1d.png", height=480, width=3*480)
## Mapa da especie e do espaco e do ambiente, com MO
par(mfrow=c(1,3))
f1(Y, Y, dados$abund, col=col1, axes=FALSE, xlab="", ylab="", main= "Abundância da espécie")
f1(Y, Y, dados$MO, col=col1, axes=FALSE, xlab="", ylab="", main = "Matéria Orgânica")
f1(Y, Y, dados$space, col=col1, axes=FALSE, xlab="", ylab="", main = "Padrão espacial")
dev.off()

## Grafico de barra ficticios para o exercicio
foo.bar <- function(a,b,c){
    barplot(matrix(c(a,b,c)*100,3,1), beside=FALSE, width = 0.05, xlim=c(0,.2),
        axes=FALSE, ylab = "R2 x 100", ylim=c(0,75), cex.lab=1.5)
    axis(2, cex.axis=1.5)
    text(x=c(0.08,0.08,0.08), y = c(a/1.5, a+b/2, (a+b)+c/2)*100, labels = c("[a]", "[b]", "[c]"), cex=1.75)
    }

particao <- function(sp, amb, espaco){
    X <- summary(lm(sp ~ amb))$r.squared
    W <- summary(lm(sp ~ espaco))$r.squared
    XW <- summary(lm(sp ~ amb + espaco))$r.squared
    a <- XW - W
    b <- X - a
    c <- W - b
    d - 1 - XW
    return(c(a=a, b=b, c=c, d=d))
    }
## Original
foo.bar(0.045, 0.64, 0.015)
## Com espaco explicando o mesmo mas ambiente pouco
particao(dados$abund, dados$MO, dados$space) ## ops, tem valor negativo

png("images/particao_univ_exercicios%1d.png")
foo.bar(0.045, 0.64, 0.015)
a <- 0.02; b <- 0.01; c <- 0.67
barplot(matrix(c(a,b,c)*100,3,1), beside=FALSE, width = 0.05, xlim=c(0,.2),
        axes=FALSE, ylab = "R2 x 100", ylim=c(0,75), cex.lab=1.5)
    axis(2, cex.axis=1.5)
    text(x=c(0.08,0.08,0.08), y = c(a*.95, (a+b)*1.5, (a+b)+c/2)*100, labels = c("[a]", "[b]", "[c]"), cex=1.75)
## O contrario
a <- 0.66; b <- 0.01; c <- 0.02
barplot(matrix(c(a,b,c)*100,3,1), beside=FALSE, width = 0.05, xlim=c(0,.2),
        axes=FALSE, ylab = "R2 x 100", ylim=c(0,75), cex.lab=1.5)
axis(2, cex.axis=1.5)
text(x=c(0.08,0.08,0.08), y = c(a/2, a+b/3, (a+b+c)*1.01)*100, labels = c("[a]", "[b]", "[c]"), cex=1.75)
dev.off()
