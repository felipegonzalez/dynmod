y <- log(uk.seats$ksi)
plot(y)
dummy.law <- c(rep(0, 169), rep(1,length(y)-169))
uk.seats$law <- dummy.law
X.1 <- data.frame(scale(uk.seats[,c('km.driven','petrol.price')]), law=dummy.law)
mod.2 <- unclass(
	dlmModPoly(2, dV = V, m0=c(2,0), C0 = diag(c(0.01,0.01)^2) )+ 
 	dlmModTrig(s=12, q=4, C0 = (0.5)^2 * diag(nrow = 8), dV = 0) +
	dlmModReg(X = X.1, 
		m0=c(0.02,-0.03,-0.00), addInt=FALSE, C0 = (0.1)^2 * diag(nrow = 3))
	)
mod.2$k <- 1
mod.2$n0 <- 1
mod.2$d0 <- 0.01


salida <- dlm.filt(y, mod.2, bloques=list(1:2, 3:10,11:13), 
	descuento=c(0.90,0.97,0.99), delta=0.99)

plot(y, type='p')
lines(sapply(salida$f, function(el){el[1,1]}), col=2, type='l')

plot(y, type='o')
lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='o')
#lines(sapply(salida$a, function(el){el[1,1]}), col=4, type='o')

plot(sapply(salida$m, function(el){el[3,1]}), col=2, type='o')

sd.1 <- sqrt(unlist(salida$n)*unlist(salida$Q)/(unlist(salida$n)-2))
plot(sd.1)


plot(y, type='p', ylim=c(1.9,2.1))
lines(sapply(salida$f, function(el){el[1,1]}), col=2, type='l')
lines(1.7*sd.1+  sapply(salida$f, function(el){el[1,1]}), col=2, type='l',lty=3)
lines(-1.7*sd.1+  sapply(salida$f, function(el){el[1,1]}), col=2, type='l',lty=3)
acf(unlist(salida$e))

plot(y, type='p', ylim=c(1.9,2.1))
lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='l')
## coeficientes
plot(sapply(salida$m, function(el){el[3,1]}), col=2, type='o')

plot(sapply(salida$m, function(el){el[11,1]}), col=2, type='o')
plot(sapply(salida$m, function(el){el[12,1]}), col=2, type='o')
plot(sapply(salida$m, function(el){el[13,1]}), col=2, type='o')
