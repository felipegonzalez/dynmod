
## 1 Usamos constructor de dlm

library(dlm)


## Función de filtrado
dlm.filt <- function(y, mod, bloques, descuento, delta){
	a <- list()
	m <- list()
	W <- list()
	P <- list()
	R <- list()
	S <- list()
	A <- list()
	n <- list()
	d <- list()
	f <- list()
	Q <-list()
	e <- list()
	C <- list()
	FF <- list()
	for(t in seq(length.out = length(y))){
		
		#print(t)
		if(t==1){
			m.ant <- mod$m0
			C.ant <- mod$C0
		} else {
			m.ant <- m[[t-1]]
			C.ant <- C[[t-1]]
		}
	
		S.ant <- ifelse(t==1, mod$d0/mod$n0, S[[t-1]])	
		n.ant <- ifelse(t==1, mod$n0, n[[t-1]])
		d.ant <- ifelse(t==1, mod$d0, d[[t-1]])
		
		a[[t]] <- mod$GG %*% m.ant
		P[[t]] <- (mod$GG)%*%C.ant%*%t(mod$GG)
		
		# W descontadas
		lista.mat <- lapply(1:length(bloques), 
        	function(j){ 
          		P[[t]][bloques[[j]],bloques[[j]]] * ((1-descuento[j])/descuento[j] )
        	}
      	)
      	W[[t]] <- as.matrix(Reduce(Matrix:::bdiag, lista.mat))
      	R[[t]] <- P[[t]] + W[[t]]

      	## Forecast
      	if(!is.null(mod$JFF)){
      		FF[[t]] <- cbind(matrix(mod$FF[mod$JFF==0],nrow=1), mod$X[t, mod$JFF[mod$JFF>0], drop=FALSE ] )
      	} else {
      		FF[[t]] <- mod$FF
      	}

      	f[[t]] <- (FF[[t]])%*%a[[t]]
      	Q[[t]] <- (FF[[t]])%*%R[[t]]%*%t(FF[[t]]) + mod$k*S.ant

      	## Update
      	A[[t]] <- R[[t]]%*%t(FF[[t]])/as.numeric(Q[[t]])
      	e[[t]] <- y[t] - f[[t]]
		n[[t]] <- delta*n.ant + 1
		d[[t]] <- as.numeric(delta*d.ant + S.ant*(e[[t]]^2)/as.numeric(Q[[t]]))
      	m[[t]] <- a[[t]] + A[[t]]%*%e[[t]]
      	S[[t]] <- d[[t]]/n[[t]]
      	C[[t]] <- (S[[t]]/S.ant)*(R[[t]] - tcrossprod(A[[t]]) *as.numeric(Q[[t]]) )
	}

	list(a = a, R = R, n=n, d=d, m=m, C=C, e=e,f=f,Q=Q)
}


dlm.smooth(filt) <- function(){
	
}

#debugonce(dlm.filt)




# modBuild <- function(V) {
#   dlmModPoly(1, dV = V, 
#   	dW = c(1), m0=c(1.9),
#   	C0 = 0.1 * diag(nrow = 1)) + 
#   dlmModSeas(frequency=12,  C0 = 0.1 * diag(nrow = 12 - 1), dV = 0) #+ 
#  # dlmModReg(X=scale(uk.seats[,c('km.driven', 'petrol.price')]), addInt=FALSE, C0 = 0.1 * diag(nrow = 2), dV=0)
# }

#seatsMLE <- dlmMLE(log(uk.seats$front), rep(0,3), modBuild); seatsMLE$conv
V <- 0.01
y.1 <- c(1.317000e+02, 3.226000e+02, 2.856000e+02, 1.057000e+02, 8.040000e+01, 2.851000e+02, 3.478000e+02, 6.890000e+01, 2.033000e+02, 3.759000e+02, 4.159000e+02, 6.580000e+01, 1.770000e+02, 4.383000e+02, 4.632000e+02, 1.360000e+02, 1.922000e+02, 4.428000e+02, 5.096000e+02, 2.012000e+02, 1.960000e+02, 4.786000e+02, 6.886000e+02, 2.598000e+02, 3.525000e+02, 5.081000e+02, 7.015000e+02, 3.256000e+02, 3.059000e+02, 4.222000e+02, 7.710000e+02, 3.293000e+02, 3.840000e+02, 4.720000e+02, 8.520000e+02)

y <- log(uk.seats$front)
mod <- seatsMod


mod.1 <- unclass(
	dlmModPoly(2, dV = V, 
  	m0=c(15,0),
  	C0 = diag(c(0.75,0.3)^2) )+ 
 	dlmModSeas(frequency=4,  m0 = c(-4,4,4), C0 = (0.5)^2 * diag(nrow = 4 - 1), dV = 0) 
	)
mod.1$k <- 1
mod.1$n0 <- 5
mod.1$d0 <- 5
#y <- log(uk.seats$ksi)
y.1 <- c(1.317000e+02, 3.226000e+02, 2.856000e+02, 1.057000e+02, 8.040000e+01, 2.851000e+02, 3.478000e+02, 6.890000e+01, 2.033000e+02, 3.759000e+02, 4.159000e+02, 6.580000e+01, 1.770000e+02, 4.383000e+02, 4.632000e+02, 1.360000e+02, 1.922000e+02, 4.428000e+02, 5.096000e+02, 2.012000e+02, 1.960000e+02, 4.786000e+02, 6.886000e+02, 2.598000e+02, 3.525000e+02, 5.081000e+02, 7.015000e+02, 3.256000e+02, 3.059000e+02, 4.222000e+02, 7.710000e+02, 3.293000e+02, 3.840000e+02, 4.720000e+02, 8.520000e+02)
y.2 <- sqrt(y.1)
plot(y.2)
salida <- dlm.filt(y.2, mod.1, bloques=list(1:2, 2:4), 
	descuento=c(0.90,0.80), delta=0.8)

plot(y.2, type='p')
lines(sapply(salida$f, function(el){el[1,1]}), col=2, type='l')

plot(y.2, type='o')
lines(sapply(salida$m, function(el){el[1,1]}), col=2, type='o')
#lines(sapply(salida$a, function(el){el[1,1]}), col=4, type='o')

plot(sapply(salida$m, function(el){el[3,1]}), col=2, type='o')

sd.1 <- sqrt(unlist(salida$n)*unlist(salida$Q)/(unlist(salida$n)-2))
plot(sd.1)


plot(y.2, type='p',ylim=c(0,40))
lines(sapply(salida$f, function(el){el[1,1]}), col=2, type='l')
lines(1.7*sd.1+  sapply(salida$f, function(el){el[1,1]}), col=2, type='l',lty=3)
lines(-1.7*sd.1+  sapply(salida$f, function(el){el[1,1]}), col=2, type='l',lty=3)


