y.1 <- c(1.317000e+02, 3.226000e+02, 2.856000e+02, 1.057000e+02, 8.040000e+01, 2.851000e+02, 3.478000e+02, 6.890000e+01, 2.033000e+02, 3.759000e+02, 4.159000e+02, 6.580000e+01, 1.770000e+02, 4.383000e+02, 4.632000e+02, 1.360000e+02, 1.922000e+02, 4.428000e+02, 5.096000e+02, 2.012000e+02, 1.960000e+02, 4.786000e+02, 6.886000e+02, 2.598000e+02, 3.525000e+02, 5.081000e+02, 7.015000e+02, 3.256000e+02, 3.059000e+02, 4.222000e+02, 7.710000e+02, 3.293000e+02, 3.840000e+02, 4.720000e+02, 8.520000e+02)


mod.1 <- unclass(
	dlmModPoly(2, dV = V, 
  	m0=c(15,0),
  	C0 = diag(c(0.75,0.3)^2) )+ 
 	dlmModSeas(frequency=4,  m0 = c(-4,4,4), C0 = (0.5)^2 * diag(nrow = 4 - 1), dV = 0) 
	)
mod.1$k <- 1
mod.1$n0 <- 1
mod.1$d0 <- 1
#y <- log(uk.seats$ksi)
y.2 <- sqrt(y.1)
#plot(y.2)

salida.filt <- dlm.filt(y.2, mod.1, bloques=list(1:2, 2:4), 
	descuento=c(0.90,0.80), delta=0.8)
salida <- salida.filt$filter


comp.1 <- extract.comps(salida.filt, comps=1)

# plot(y.2, type='p')
# lines(sapply(salida$f, function(el){el[1,1]}), col=2, type='l')

# plot(y.2, type='o')
# lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='o')
# #lines(sapply(salida$a, function(el){el[1,1]}), col=4, type='o')

# plot(sapply(salida$m, function(el){el[3,1]}), col=2, type='o')

sd.1 <- sqrt(unlist(salida$n)*unlist(salida$Q)/(unlist(salida$n)-2))
# plot(sd.1)

# Forecast
plot(y.2, type='p',ylim=c(0,40))
lines(sapply(salida$f, function(el){el[1,1]}), col=2, type='l')
lines(1.7*sd.1+  sapply(salida$f, function(el){el[1,1]}), col=2, type='l',lty=3)
lines(-1.7*sd.1+  sapply(salida$f, function(el){el[1,1]}), col=2, type='l',lty=3)


