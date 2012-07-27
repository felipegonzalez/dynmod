library(dlm)

y <- share$SHARE
plot(y, type='o')

X.1 <- share[, c('PROM','CPROM','PRICE')]
#X.1$relprom.s <- X.1$relprom /sd(X.1$relprom)
#X.1$relprom <- NULL

plot(X.1$PRICE)
#plot(X.1$relprom.s, type="l")
plot(share$promdiff)
mod.2 <- unclass(
	dlmModPoly(1, dV = 0, m0=42, C0 = (5^2) )+ 
	dlmModReg(X = X.1,  addInt=FALSE, C0 = (2)^2 * diag(3))
	)

mod.2$k <- 1
mod.2$n0 <- 1
mod.2$d0 <- 1


salida.f <- dlm.filt(y, mod.2, bloques=list(1, 2:4), 
	descuento=c(0.98,0.95), delta=0.99)
salida <- salida.f$filter

forecast.1 <- plot.df(salida.f)
acf(unlist(salida$e))

plot(y, type='o')
lines(sapply(salida$a, function(el){el[1,1]}), col=2, type='l')


## Nivel suavizado
salida.s <- dlm.smooth(salida.f)
a.suave <- sapply(salida.s$a.smooth, function(el){el[1,1]})
sd.a <- sqrt(sapply(salida.s$R.smooth, function(el){el[1,1]}))
plot(y, type='o')
lines(a.suave, col=2, type='l')
lines(a.suave-1.7*sd.a, col=2, type='l')
lines(a.suave+1.7*sd.a, col=2, type='l')




