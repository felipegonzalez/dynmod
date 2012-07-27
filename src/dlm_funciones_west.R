
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

	list(filter=list(y=y, a = a, R = R, n=n, d=d, m=m, C=C, e=e,f=f,Q=Q), bloques=bloques, model=mod, FF=FF)
}


extract.comps.filt <- function(mod.filt, comps){
	comp <- list()
	comp.var <- list()
	indices <- unlist(mod.filt$bloques[[comps]])
	for(t in seq(length.out = length(mod.filt$filter$y))){
		FF <- mod.filt$FF[[t]][1, indices, drop = FALSE]
		C <- mod.filt$filter$C[[t]][indices, indices, drop = FALSE]
		comp[[t]] <- FF%*%(mod.filt$filter$a[[t]][indices])
		comp.var[[t]] <- (FF)%*%C%*%t(FF)
	}
	list(comp = comp, comp.var = comp.var)
}

extract.comps.smooth <- function(mod.smooth, comps){
	comp <- list()
	comp.var <- list()
	mod.filt <- mod.smooth$mod.filt
	indices <- unlist(mod.filt$bloques[comps])
	for(t in seq(length.out = length(mod.filt$filter$y))){
		FF <- mod.filt$FF[[t]][1, indices, drop = FALSE]
		R <- mod.smooth$R.smooth[[t]][indices, indices, drop = FALSE]
		comp[[t]] <- FF%*%(mod.smooth$a.smooth[[t]][indices])
		comp.var[[t]] <- (FF)%*%R%*%t(FF)
	}
	list(comp = comp, comp.var = comp.var)
}

plot.df <- function(mod.filt,ylim=NULL){
	salida <- mod.filt$filter
	y <- mod.filt$filter$y
	sd.1 <- sqrt(unlist(salida$n)*unlist(salida$Q)/(unlist(salida$n)-2))
	f <- sapply(salida$f, function(el){el[1,1]})
	plot(y, type='p', ylim=ylim)
	lines(f, col=2, type='l')
	lines(1.7*sd.1+  f, col=2, type='l',lty=3)
	lines(-1.7*sd.1+ f, col=2, type='l',lty=3)
	list(f=f,sd.f=sd.1)
}

dlm.smooth <- function(mod.filt){
	a.smooth <- list()
	R.smooth <- list()
	B <- list()
	T <- length(mod.filt$filter$y)
	a.smooth[[1]] <- mod.filt$filter$m[[T]]
	R.smooth[[1]] <- mod.filt$filter$R[[T]]
	for(k in 2:T){
		C <- mod.filt$filter$C[[T - k + 1]]
		GG <- mod.filt$model$GG
		R <- mod.filt$filter$R[[T - k + 2]]
		B[[k]] <- C%*%t(GG)%*%solve(R)

		m <- mod.filt$filter$m[[T - k + 1]]
		a <- mod.filt$filter$a[[T - k + 2]]
		a.smooth.ant <- a.smooth[[k - 1]]
		R.smooth.ant <- R.smooth[[k - 1]]
		a.smooth[[k]] <- m - B[[k]]%*%(a - a.smooth.ant)
		R.smooth[[k]] <- C - B[[k]]%*%(R-R.smooth.ant)%*%t(B[[k]])
	}
	list(a.smooth = rev(a.smooth), R.smooth = rev(R.smooth), mod.filt=mod.filt)
}



