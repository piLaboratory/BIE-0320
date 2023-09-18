library(spatstat)
################################################################################
## Figuras de exemplos dos 4 processos, para os exercicios
################################################################################
## Poisson homogeneo
pp.1 <- rpoispp(35, win = unit.square())
## Poisson inhomogeneous
##Z <- as.im(function(x,y){ifelse(x>0.5, 25, 100)}, unit.square())
## Using a raster image
Z1 <- as.im(function(x,y){100 * exp(-2.5*x + 3*y)}, unit.square())
pp.2 <- rpoispp(Z1)
## Thomas homogeneous
pp.3 <- rThomas(kappa = mean(Z1/10), scale = 0.05, mu = 10)
##inhomogeneous
##Z2 <- as.im(function(x,y)10 * exp(2.5*x - 3*y), unit.square())
janela <- owin(c(10,51), c(10,77.5))
Z2 <- as.im(volcano, W=janela)
##pp.4 <- rThomas(kappa = function(x,y){10 * exp(2.5*x - 3*y)}, 0.05, 10, win = unit.square())
pp.4 <- rThomas(kappa = exp(as.im(volcano)/10-15)*1.e-3, scale = 1.5, mu = 20, win = janela)


## Save plots
png("images/point_pattern_examples%02d.png")
plot(pp.1, main = "")
plot(pp.2, main = "")
plot(pp.3, main = "")
plot(pp.4, main = "")
image(Z1, main = "")
points(pp.2, pch=19)
plot(Z2, main ="")
points(pp.4)
dev.off()

################################################################################
## Exemplo das Vitoria-regia
################################################################################
vit <- read.csv("data/vit_regia_coords.csv")
## convert to meters (approx)
vit$xm <- vit$x*0.3752972
vit$ym <- vit$y*0.3752972
## point pattern
vitoria.pp <- ppp(vit$xm, vit$ym, window = owin(c(0,36.513*0.3752972), c(0,20.505*0.3752972)))
## Salva o arquivo em format ppp
##save(vitoria.pp, file = "data/vitoria_regia.rds")
write.csv(vit, file = "data/vit_regia_coord_m.csv")

################################################################################
## L de Ripley para os exemplos do primeiro roteiro
################################################################################
## Poisson homogeneo
PH <- rpoispp(lambda=100, win = unit.square())
PH.L <- envelope(PH, fun = Lest, nsim = 1000)
## Aqui definimos uma funcao que descreve como a intensidade muda na area
## No caso, a itensidade muda de 180 para 20 a partir do valor da coordenada X = 0.5
padrao1 <- function(x,y) ifelse(test = x > 0.5, yes = 20, no = 180)
## Gera o padrão de pontos
IP <- rpoispp(lambda = padrao1, win = unit.square())
## L de Ripley
IP.L <- envelope(IP, fun = Lest, nsim = 1000)
## Thomas homogeneo
TH <- rThomas(kappa = 15, scale = 0.02, mu = 10, win = unit.square())
TH.L <- envelope(TH, fun = Lest, nsim = 1000)
## Vitorias regias
## carrega um objeto com as coordenadas dos pontos de inserção dos pecíolos de cada folha
vit <- read.csv("data/vit_regia_coord_m.csv")
## Cria objeto com o padrão de pontos para analisar
## Define a área de observação
vit.w <- owin(xrange = c(0, 13.70323), yrange = c(0, 7.695469))
## Cria o padrão de pontos nesta área
vit.p <- ppp(x = vit$xm, y = vit$ym, window = vit.w)
## Envelope
vit.env <- envelope(vit.p, fun = Lest, nsim = 1000)

png("images/figsL_Ripley%02d.png")
## Plota os grafico
plot(IP, main = "")
## Uma linha para marcar as duas regiões
abline(v = 0.5, lty =2 , col = "blue")
plot(TH, main = "")
## Grafico do L
plot(IP.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "A")
plot(TH.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "B")
plot(PH.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "C")
plot(vit.env, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "D")
dev.off()

################################################################################
## exemplos para roteiro de ajuste de modelos
################################################################################
## Preparacao dos arquivos
## Area 100x100, variave ambiental em escala compativel com MO no solo
Z3 <- as.im(function(x,y) 20 + 0.005*x*y, W = owin(c(0,100), c(0,100)), eps = 1)
write.table(as.matrix(Z3), file = "data/EP_roteiro3_solo.txt", row.names=FALSE, col.names=FALSE)
## Poisson nao homogeneo
p1 <- rpoispp(Z3/250)
write.csv(as.data.frame(p1), file = "data/EP_roteiro3_padrao1.csv", row.names=FALSE)
## Um padrao dificil de distinguir só com os envelopes
ilogit <- function(x,a,b) exp(a+b*x)/(1+exp(a+b*x))
p2 <- rpoispp(ilogit(Z3, -3.5, .2)/10)
write.csv(as.data.frame(p2), file = "data/EP_roteiro3_padrao2.csv", row.names=FALSE)
## Padrao Thomas não homogêneo para desafio
p3 <- rThomas(kappa = 0.05, scale = 2.5, mu = exp(MO.solo/25),  win = owin(c(0,100), c(0,100)))
write.csv(as.data.frame(p3), file = "data/EP_roteiro3_padrao3.csv", row.names=FALSE)

################################################################################
## Inicio dos codigos do exercício
################################################################################
## carrega dados solo
MO.solo <- as.im(as.matrix(read.table("data/EP_roteiro3_solo.txt")), owin(c(0,100), c(0,100)), eps=1)
plot(MO.solo, main = "% de M.O. no solo superficial")
## Carrega padrao de pontos
p1 <- as.ppp(read.csv("data/EP_roteiro3_padrao1.csv"), owin(c(0,100), c(0,100)))
plot(p1, main = "Padrão 1")
## Modelos
## Poisson homogeneo
p1.m1 <- ppm(p1)
p1.m1.L <- envelope(p1.m1, fun = Lest)
plot(p1.m1.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
## Poisson nao homogeneo
p1.m2 <- ppm(p1 ~ MO.solo)
p1.m2.L <- envelope(p1.m2, fun = Lest)
plot(p1.m2.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
## AIC
AIC(p1.m1)
AIC(p1.m2)
## Previsto pelo modelo selecionado
plot(predict(p1.m2), main = "Padrão 1, previsto Poisson não-homogêneo")
points(p1, cex=.5)
## Para ilustrar o AIC: um padrao dificil de identificar
p2 <- as.ppp(read.csv("data/EP_roteiro3_padrao2.csv"), owin(c(0,100), c(0,100)))
plot(p2, main = "Padrão 2")
## Modelos
## Poisson homogeneo
p2.m1 <- ppm(p2)
p2.m1.L <- envelope(p2.m1, fun = Lest)
plot(p2.m1.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
## Poisson nao homogeneo
p2.m2 <- ppm(p2 ~ MO.solo)
p2.m2.L <- envelope(p2.m2, fun = Lest)
plot(p2.m2.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
## AIC
AIC(p2.m1)
AIC(p2.m2)

################################################################################
## Para o desafio: um processo de Thomas nao homogeneo
################################################################################
p3 <- as.ppp(read.csv("data/EP_roteiro3_padrao3.csv"), owin(c(0,100), c(0,100)))

## Modelos
## Poisson homogeneo
p3.m1 <- ppm(p3)
p3.m1.L <- envelope(p3.m1, fun = Lest)

## Poisson nao homogeneo
p3.m2 <- ppm(p3 ~ MO.solo)
p3.m2.L <- envelope(p3.m2, fun = Lest)

## Thomas homogeneo
p3.m3 <- kppm(p3, method = "palm")
p3.m3.L <- envelope(p3.m3, fun = Lest)

## Thomas não-homogêneo
p3.m4 <- kppm(p3 ~ MO.solo, method = "palm")
p3.m4.L <- envelope(p3.m4, fun = Lest)

## Comparando os Thomas com AIC
## Obs: não dá para comparar modelos ajustados com as funcoes ppm com modelos ajustados com kppm
AIC(p3.m3)
AIC(p3.m4)


## Grava as figuras
png("images/EP_roteiro3_%01d.png")
plot(p3, "Padrão mistêrio-ôso")
plot(predict(p3.m4), main = "Padrão misterioso \n Intensidade Prevista pelo melhor modelo")
plot(p3.m1.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "Padrão misterioso - Poisson homogêneo")
plot(p3.m2.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "Padrão misterioso - Poisson não-homogêneo")
plot(p3.m3.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "Padrão misterioso - Thomas homogêneo")
plot(p3.m4.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "Padrão misterioso - Thomas não-homogêneo")
dev.off()

## Nao ajuda muito: plot de n de pontos por MO em quadrados de 1x1
## Grafico de N de pontos x MO em cada pixel de 1x1
p1.im <- pixellate(p1, W = owin(c(0,10), c(0,10)), eps = 1) 
X <- as.matrix(MO.solo)
Y <- p1.im$v
class(Y) <- "matrix"
plot(X, Y, xlab = "% de MO no solo", ylab = "Densidade de pontos por m2")
p1.m1.cf <- coef(p1.m1)
p1.m3.cf <- coef(p1.m3)
curve(exp(p1.m1.cf[1] + p1.m1.cf[2]*x), add=TRUE, col = "blue")
curve(exp(p1.m3.cf[1] + p1.m3.cf[2]*x), add=TRUE, col = "red")

## Alternativa em quadrado de 10x10
##MO.solo <- as.im(function(x,y) 20 + 0.5*x*y, W = owin(c(0,10), c(0,10)), eps = 1)
## p1 <- rThomas(kappa = 1, scale = 0.3, mu = exp(MO.solo/20),  win = owin(c(0,10), c(0,10)))
##


################################################################################
## Um outro conjunto de dados
################################################################################
Z4 <- as.im(function(x,y) sqrt((x^2 + y^2)), owin(c(0,10), c(0,10)), eps = 1)
set.seed(42)
p2 <- rThomas(kappa = function(x,y) sqrt((x^2 + y^2))/5,
                 scale = 0.5, mu = 5, win = owin(c(0,10), c(0,10)), eps = 1)
plot(p2)
## modelos
p2.m0 <- ppm(p2 ~ 1)
p2.m1 <- ppm(p2 ~ MO.solo)
p2.m2 <- kppm(p2 ~ 1 , cluster = "Thomas", method = "palm")
p2.m3 <- kppm(p2 ~ MO.solo , cluster = "Thomas", method = "palm")

AIC(p2.m0)
AIC(p2.m1)
AIC(p2.m2)
AIC(p2.m3)

p2.m0.L <- envelope(p2.m0, fun = Lest)
p2.m1.L <- envelope(p2.m1, fun = Lest)
p2.m2.L <- envelope(p2.m2, fun = Lest)
p2.m3.L <- envelope(p2.m3, fun = Lest)
##
plot(p2.m0.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
plot(p2.m1.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
plot(p2.m2.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
plot(p2.m3.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")

plot(predict(p2.m3))
points(p2)
    
## People in park
plot(gordon)
gordon.im <- as.im(function(x,y) y/33, W = gordon$window)
plot(gordon.im)
gordon.m1 <- kppm(gordon~1, "Thomas")
gordon.m1.env <- envelope(gordon.m1, fun=Lest)
plot(gordon.m1.env, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")

gordon.m2 <- kppm(gordon~gordon.im, "Thomas")
gordon.m2.env <- envelope(gordon.m2, fun=Lest)
plot(gordon.m2.env, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")

## Bei
plot(bei)
plot(bei.extra$elev)

bei.m1 <- ppm(bei ~ 1)
bei.m2 <- ppm(bei ~ grad + I(grad^2), data = bei.extra)
bei.m3 <- kppm(bei ~ 1, cluster = "Thomas", method = "palm")
bei.m4 <- kppm(bei ~ grad + I(grad^2), data = bei.extra, cluster = "Thomas", method = "palm")

bei.m1.L <- envelope(bei.m1, fun = Lest)
bei.m2.L <- envelope(bei.m2, fun = Lest)
bei.m3.L <- envelope(bei.m3, fun = Lest)
bei.m4.L <- envelope(bei.m4, fun = Lest)

plot(bei.m1.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
plot(bei.m2.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
plot(bei.m3.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")
plot(bei.m4.L, . -r ~ r, ylab = "L de Ripley", legend=FALSE, main = "")

AIC(bei.m1)
AIC(bei.m2)
AIC(bei.m3)
AIC(bei.m4)
