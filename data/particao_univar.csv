################################################################################
## O roteiro coeficientes de partição
################################################################################
dados <- read.csv2("roteiro1.csv")
head(dados)
tail(dados)
plot(abund ~ pH, data = dados)
regr.pH <- lm(abund ~ pH, data = dados)
abline(regr.pH)
(X<-summary(regr.pH)$r.squared)
plot(abund ~ space, data= dados, xlab = "Variável de estrutura espacial", 
     ylab= "Abundância de L. nom")
regr.space <- lm(abund ~ space, data = dados)
abline(regr.space)
(W <- summary(regr.space)$r.squared)
plot(pH ~ space, data = dados, xlab = "Variável de estrutura espacial")
plot(pH ~ space, data = dados, xlab = "Variável de estrutura espacial")
regr.tudo <- lm(abund ~ pH + space, data = dados)
summary(regr.tudo)$r.squared
(d <- 1 - summary(regr.tudo)$r.squared)
(b <- X + W + d - 1)
(a <- X - b)
(c <- W - b)
(XW <- 1 - d)
