y <- log(uk.seats$ksi)
plot(y, type='o')
dummy.law <- c(rep(0, 169), rep(1,length(y)-169))
uk.seats$law <- dummy.law
X.1 <- data.frame(scale(uk.seats[,c('km.driven','petrol.price')]), law=dummy.law)

mod.2 <- unclass(
	dlmModPoly(2, dV = 1, m0=c(2,0), C0 = diag(c(0.01,0.01)^2) )+ 
 	dlmModTrig(s=12, q=4, C0 = (0.5)^2 * diag(nrow = 8), dV = 0) +
	dlmModReg(X = X.1, 
		m0=c(0.00,-0.00,-0.00), addInt=FALSE, C0 = (0.05)^2 * diag(nrow = 3))
	)
mod.2$k <- 1
mod.2$n0 <- 1
mod.2$d0 <- 0.02


salida.filt <- dlm.filt(y, mod.2, bloques=list(1:2, 3:10,11:13), 
	descuento=c(0.85,0.95,0.995), delta=0.99)
salida <- salida.filt$filter

## Pronósticos a un paso y residuales
forecast.1 <- plot.df(salida.filt)
acf(unlist(salida$e)[-c(1:50)])

plot(y, type='o')
lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='o')
#lines(sapply(salida$a, function(el){el[1,1]}), col=4, type='o')

plot(y, type='p', ylim=c(1.9,2.1))
lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='l')
## coeficientes
plot(sapply(salida$m, function(el){el[3,1]}), col=2, type='o')

plot(sapply(salida$m, function(el){el[11,1]}), col=2, type='o')
plot(sapply(salida$m, function(el){el[12,1]}), col=2, type='o')
plot(sapply(salida$m, function(el){el[13,1]}), col=2, type='o')



## Nivel filtrado
plot(y, type='o')
lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='o')




## Suavizamento
salida.smooth <- dlm.smooth(salida.filt)

## Extraer suavizado de tendencia y estacionalidad
comps.1y2 <- extract.comps.smooth(salida.smooth, comps=1:2)
plot(y, type='o')
lines(unlist(comps.1y2$comp), col=2, type='o')


sd.nivel <- sqrt(sapply(salida.smooth$R.smooth, function(el){el[1,1]}))
## Nivel suavizado
plot(y, type='p')
lines(nivel.suave <- sapply(salida.smooth$a, function(el){el[1,1]}), col=2, type='l')
lines(sapply(salida$m, function(el){el[1,1]}), col=5)

lines(nivel.suave - 1.7*sd.nivel, col=2, type='l', lty=3)
lines(nivel.suave + 1.7*sd.nivel, col=2, type='l', lty=3)


plot(sapply(salida.smooth$a, function(el){el[12,1]}), col=2, type='l')



