## ----settings, echo=FALSE-----------------------------------------------------
library(knitr)


## -----------------------------------------------------------------------------
dados <- read.csv("https://raw.githubusercontent.com/piklprado/BIE-0320/main/data/roteiro1b.csv")


## ---- eval=FALSE--------------------------------------------------------------
## head(dados)
## tail(dados)


## ---- eval=FALSE--------------------------------------------------------------
## plot(abund ~ pH, data = dados,
##      ylab= "Abundância de Non ilum")


## -----------------------------------------------------------------------------
regr.pH <- lm(abund ~ pH,  data = dados)


## ---- eval=FALSE--------------------------------------------------------------
## abline(regr.pH)


## ---- echo=FALSE--------------------------------------------------------------
X <- summary(regr.pH)$r.squared


## ---- eval=FALSE--------------------------------------------------------------
## (X <- summary(regr.pH)$r.squared)


## ---- eval=FALSE--------------------------------------------------------------
## plot(abund ~ space, data= dados, xlab = "Variável de estrutura espacial",
##       ylab= "Abundância de Non ilum")


## -----------------------------------------------------------------------------
regr.space <- lm(abund ~ space, data = dados)


## ---- eval=FALSE--------------------------------------------------------------
## abline(regr.space)


## ---- echo=FALSE--------------------------------------------------------------
W <- summary(regr.space)$r.squared


## ---- eval=FALSE--------------------------------------------------------------
## (W <- summary(regr.space)$r.squared)


## ---- eval=FALSE--------------------------------------------------------------
## plot(pH ~ space, data = dados, xlab = "Variável de estrutura espacial")


## -----------------------------------------------------------------------------
regr.tudo <- lm(abund ~ pH + space, data = dados)


## ---- eval=FALSE--------------------------------------------------------------
## summary(regr.tudo)$r.squared


## ---- echo=FALSE--------------------------------------------------------------
XW <- summary(regr.tudo)$r.squared


## ---- eval=FALSE--------------------------------------------------------------
## XW <- summary(regr.tudo)$r.squared
## 1 - XW


## ---- eval=FALSE--------------------------------------------------------------
## (a <- XW - W)


## ---- eval=FALSE--------------------------------------------------------------
## (b <- X - a)


## ---- eval=FALSE--------------------------------------------------------------
## (c <- W - b)


## ---- eval=FALSE--------------------------------------------------------------
## (d <- 1 - XW)


## ---- echo=FALSE--------------------------------------------------------------
a <- XW-W
b <- X-a
c <- W-b
barplot(matrix(c(a,b,c)*100,3,1), beside=FALSE, width = 0.05, xlim=c(0,.2),
        axes=FALSE, ylab = "R2 x 100", ylim=c(0,75), cex.lab=1.5)
axis(2, cex.axis=1.5)
text(x=c(0.08,0.08,0.08), y = c(a/1.5, a+b/2, (a+b)+c/2)*100, labels = c("[a]", "[b]", "[c]"), cex=1.75)

