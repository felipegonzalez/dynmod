library(dlm)

modBuild <- function(V) {
  dlmModPoly(2, dV = V, 
  	dW = c(1,1), m0=c(1.9,0),
  	C0 = 1 * diag(nrow = 2)) + 
  dlmModTrig(s=12, q=6,  C0 = 1 * diag(nrow = 12 - 1), dV = 0)
}

#seatsMLE <- dlmMLE(log(uk.seats$front), rep(0,3), modBuild); seatsMLE$conv
V <- 0.01
seatsMod <- modBuild(V=V)


sfilt <- dlmFilterDF.2(log(uk.seats$front), seatsMod, 
	bloques=list(1:2,3:13), descuento=c(0.98,0.95))
plot(log(uk.seats$front), col=1, type="l")
lines(sfilt$f, col=2)

y <- log(uk.seats$front)
alpha0 <- 1
beta0 <- 1

## Check precision
mean(V/rgamma(2000,shape=alpha0,s=beta0))
sd(V/rgamma(2000,shape=alpha0,s=beta0))


out <- residuals(sfilt)
beta <- beta0 + cumsum(out$res^2)/2
alpha <- alpha0 + (1:length(y))/2
Ctilde <- unlist(  lapply(
	dlmSvd2var(sfilt$U.C, sfilt$D.C), function(mat){ mat[1,1] }))  [-1]
prob <- 0.90
tt <- qt(prob, df = 2*alpha)
lower <- dropFirst(sfilt$m[,1]) - tt*sqrt(Ctilde*beta/alpha)
upper <- dropFirst(sfilt$m[,1]) + tt*sqrt(Ctilde*beta/alpha)
plot(log(uk.seats$front), type='o', col='gray')
lines(dropFirst(sfilt$m[,1]))
lines(lower, col='red')
lines(upper, col='red')


