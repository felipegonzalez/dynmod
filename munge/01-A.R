#y.1 <- scan("http://www.stat.pitt.edu/stoffer/tsa2/data/HL.dat")
#y.2 <- scan("http://www.stat.pitt.edu/stoffer/tsa2/data/Folland.dat")
#data.p <- data.frame(y.1, y.2)


uk.seats <- read.table(file="./data/UKfrontrearseatKSI_2.wsv", sep="",
    header=TRUE, strip.white=TRUE)
