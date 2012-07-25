modBuild <- function(par) {
  dlmModPoly(2, dV = exp(par[1]), dW = c(exp(par[2]), exp(par[3]))) + dlmModTrig(s=12, q=6)
}
seatsMLE <- dlmMLE(uk.seats$front, rep(0,3), modBuild); seatsMLE$conv
seatsMod <- modBuild(seatsMLE$par)
V(seatsMod)
W(seatsMod)

seatsFilt2 <- dlmFilterDF.2(uk.seats$front, seatsMod, bloques=list(1:2,3:13), descuento=c(0.95,0.95))
plot(uk.seats$front, col=1, type="l")
lines(seatsFilt2$f, col=2)

