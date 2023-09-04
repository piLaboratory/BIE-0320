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
dev.off()
