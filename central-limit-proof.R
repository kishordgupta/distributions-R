r=1/3
n = 25
x <- rexp(10000,rate=r)
pdf("CLT.pdf", width=8.5, height=5)
for (i in 1:1000) { xbar[i]=mean(rexp(n,rate=r)) }
hist(xbar,probability = TRUE)
mtext(paste("Population mean is ",mean(x),",Average of 1000 sample means is ", mean(xbar)))

sample.std <- rep(0,1000)
for (i in 1:1000) { sample.std[i]=sd(rexp(n,rate=r)) }
hist(sample.std,probability = TRUE)
mtext(paste("Population std is ",sd(x),",Average of 1000 sample std is ", mean(sample.std)))
dev.off()

  
#We will see sample means average mean and standard deviation creating a normal distribution and getting closer to population mean, which proves the normality theorem.
