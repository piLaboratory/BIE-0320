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
Z2 <- as.im(function(x,y)10 * exp(2.5*x - 3*y), unit.square())
pp.4 <- rThomas(kappa = function(x,y){10 * exp(2.5*x - 3*y)}, 0.05, 10, win = unit.square())


## Save plots
png("images/point_pattern_examples%02d.png")
plot(pp.1, main = "")
plot(pp.2, main = "")
plot(pp.3, main = "")
plot(pp.4, main = "")
image(Z1, main = "")
points(pp.2)
image(Z2, main = "")
points(pp.4)
dev.off()


################################################################################
## ## r  L de Ripley
################################################################################

LTH1 <- Lest(TH1)
LIP1 <- Lest(IP1)
plot(LTH1, iso-r ~ r)
plot(LIP1, iso-r ~ r, add=TRUE)

## Poisson homogeneo
HP2 <- rpoispp(lambda = 100, win = unit.square(), nsim = 100)
LHP2 <- lapply(HP2, Lest)
plot(LHP2[[1]], iso~r)
abline(0,1, col="blue")
plot(LTH1, iso-r ~ r,  main="")
lapply(LHP2, plot, add = TRUE, iso-r~r, col="gray")
plot(LIP1, iso-r ~ r, add = TRUE)
## Envelopes
HP1.e <- envelope(HP1[[1]], Lest,nsim=39)

## To set common limits
a <- plot(HP1.e, . -r ~ r, limitsonly=TRUE)
b <- plot(LTH1, iso-r ~ r, limitsonly=TRUE)
xlim <- range(a$xlim, b$xlim)
ylim <- range(a$ylim, b$ylim)
##
plot(HP1.e, . -r ~ r, legend = FALSE, main = "", xlim = xlim, ylim = ylim)
plot(LTH1, iso-r ~ r, add = TRUE)
## Xarada:
TH2 <- rThomas(kappa = 10, scale = 0.1, mu = 20, win = unit.square())
plot(TH2)
plot(envelope(TH2, Lest, nsim =100), .-r ~r, legend=FALSE) 
################################################################################



## Fit models for homogeneous and inhomogeneous Poisson
m0 <- ppm(pp ~ 1)
m1 <- ppm(pp ~ Z)
AIC(m0)
AIC(m1)

## Observed K
K.pp <- Kinhom(pp)
plot(K.pp)
## Envelopes das funcoes K
m0.env <- envelope(m0, nsim =1000)
m1.env <- envelope(m1, nsim =1000)
plot(m0.env)
plot(m1.env)

## Previstos pelos modelos
m0.pred <- predict(m0)
m1.pred <- predict(m1, type = "trend")
image(m1.pred)
image(m0.pred)

################################################################################
## Examples of Thomas Process (simula a intensidade do cluster , verificar se é o mesmo que o artigo
##homogeneous
X <- rThomas(kappa = mean(Z/10), scale = 0.05, mu = 10)
plot(X)
##inhomogeneous
Z2 <- as.im(function(x,y){ifelse(x>0.5, 25, 100)}, owin(c(-2,2), c(-2,2)))
Y <- rThomas(kappa = Z2, 0.01, 10, win = unit.square())
Y <- rThomas(kappa = function(x,y){10 * exp(-2.5*x + 3*y)}, 0.05, 10, win = unit.square())
image(Z)
points(Y)
plot(Y, main = "A")

## Observed K
K.X <- Kinhom(X)
plot(K.X)


################################################################################

## Beilschmiedia pendula in BCI.: aqui acho que precisa reescalonar , pq as densidades estao altissimas
## Usa o mapa de elevacao
bci.E <- exp(bei.extra$elev/10 - 20)*5e-6
bci.pp <- rpoispp(bci.E)
image(bei.extra$elev, main = "")
##points(bei, cex = 0.5, pch = 19)
points(bci.pp)
## Densidade
bci.lambda <- summary(bci.pp)$intensity

##
bci.Z <- bei.extra$elev
bci.m0 <- ppm(bci.pp ~ 1)
bci.m1 <- ppm(bci.pp ~ bci.Z)
image(predict(bci.m1, type = "trend"))

## Envelopes das funcoes L de Ripley
bci.m0.env <- envelope(bci.m0, Kest,  nsim =1000, transform=expression(sqrt(./pi)-r))
bci.m1.env <- envelope(bci.m1, Kest, nsim =1000, transform=expression(sqrt(./pi)-r))
plot(bci.m0.env)
plot(bci.m1.env)
AIC(bci.m1)
AIC(bci.m0)
