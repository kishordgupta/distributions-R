sd.plot<-function(x){
  start_a <- mean(x)
  end_a <- mean(x)+sd(x)
  xx <- seq(start_a, end_a, length=100)
}
var.plot<-function(x){
  start_a <- mean(x)
  end_a <- mean(x)+var(x)
  xx <- seq(start_a, end_a, length=100)
}
pdf("distribution.pdf", width=8.5, height=5)


////////////////////////////////////////////////Normal distribution/////////////////////////////////
m <- 10 
s <- 3
x <- rnorm(100,mean=m,sd=s)
hist(x, probability=TRUE,ylim=c(0,1),main = NULL)
q_25 <- qnorm(.25,mean=m,sd=s)
q_50 <- qnorm(.5,mean=m,sd=s)
q_75 <- qnorm(.75,mean=m,sd=s)
rect(xleft = min(x),ybottom = 0,ytop = 1,xright = q_25,col= rgb(.5,.5,1.0,alpha=0.5))
rect(xleft = q_25,ybottom = 0,ytop = 1,xright = q_50,col= rgb(0,0,1.0,alpha=0.5))
rect(xleft = q_50,ybottom = 0,ytop = 1,xright = q_75,col= rgb(0,1,0,alpha=0.5))
rect(xleft = q_75,ybottom = 0,ytop = 1,xright = max(x),col= rgb(1,0,0,alpha=0.5))
abline(v = mean(x), col = "blue", lwd = 6)
text(mean(x),.8,paste("mean is:",mean(x)))
xx <- seq(min(x), max(x), length=1000)
lines(xx, dnorm(xx, mean=m,sd=s))
sd_a <-sd.plot(x)
lines(sd_a,array(.4,length(sd_a)),lwd=3,col="blue")
text(sd_a[50],.5,paste("std is:",sd(x)))
var_a <- var.plot(x)
lines(var_a,array(.2,length(var_a)),lwd=3,col="blue")
text(var_a[50],.25,paste("var is:",var(x)))
title("Normal distribution")
mtext("1st Quartile = light sky blue, 2nd Quartile = sky blue, 3rd Quartile = light green,4th Quartile = red")
////////////////////////////////////////////////

////////////////////////////////////////////////Exponential distribution/////////////////////////////////
r=1/3
x <- rexp(100,rate = r)
hist(x, probability=TRUE,ylim=c(0,1),main = NULL)
q_25 <- qexp(.25,rate=r)
q_50 <- qexp(.5,rate=r)
q_75 <- qexp(.75,rate=r)
rect(xleft = min(x),ybottom = 0,ytop = 1,xright = q_25,col= rgb(.5,.5,1.0,alpha=0.5))
rect(xleft = q_25,ybottom = 0,ytop = 1,xright = q_50,col= rgb(0,0,1.0,alpha=0.5))
rect(xleft = q_50,ybottom = 0,ytop = 1,xright = q_75,col= rgb(0,1,0,alpha=0.5))
rect(xleft = q_75,ybottom = 0,ytop = 1,xright = max(x),col= rgb(1,0,0,alpha=0.5))
abline(v = mean(x), col = "blue", lwd = 6)
text(mean(x),.8,paste("mean is:",mean(x)))
xx <- seq(min(x), max(x), length=1000)
lines(xx, dexp(xx,rate=r))
sd_a <-sd.plot(x)
lines(sd_a,array(.4,length(sd_a)),lwd=3,col="blue")
text(sd_a[50],.5,paste("std is:",sd(x)))
var_a <- var.plot(x)
lines(var_a,array(.2,length(var_a)),lwd=3,col="blue")
text(var_a[50],.25,paste("var is:",var(x)))
title("Exponential distribution")
mtext("1st Quartile = light sky blue, 2nd Quartile = sky blue, 3rd Quartile = light green,4th Quartile = red")

//////////////////////////////////////////////////////////
////////////////////////////////////////////////Binomial distribution/////////////////////////////////

s<-10
p<-.5
x <- rbinom(100,size=s,prob = p)
hist(x, probability=TRUE,ylim=c(0,1),main = NULL)
q_25 <- qbinom(.25,size=s,prob = p)
q_50 <- qbinom(.5,size=s,prob = p)
q_75 <- qbinom(.75,size=s,prob = p)
rect(xleft = min(x),ybottom = 0,ytop = 1,xright = q_25,col= rgb(.5,.5,1.0,alpha=0.5))
rect(xleft = q_25,ybottom = 0,ytop = 1,xright = q_50,col= rgb(0,0,1.0,alpha=0.5))
rect(xleft = q_50,ybottom = 0,ytop = 1,xright = q_75,col= rgb(0,1,0,alpha=0.5))
rect(xleft = q_75,ybottom = 0,ytop = 1,xright = max(x),col= rgb(1,0,0,alpha=0.5))
abline(v = mean(x), col = "blue", lwd = 6)
text(mean(x),.8,paste("mean is:",mean(x)))
points(x, dbinom(x,size=s,prob = p),pch=16,cex=2,col="dark red")
sd_a <-sd.plot(x)
lines(sd_a,array(.4,length(sd_a)),lwd=3,col="blue")
text(sd_a[50],.5,paste("std is:",sd(x)))
var_a <- var.plot(x)
lines(var_a,array(.2,length(var_a)),lwd=3,col="blue")
text(var_a[50],.22,paste("var is:",var(x)))
title("Binomial distribution")
mtext("1st Quartile = light sky blue, 2nd Quartile = sky blue, 3rd Quartile = light green,4th Quartile = red")
 
////////////////////////////////////////////////Geometric distribution/////////////////////////////////
p<-.5
x <- rgeom(100,prob = p)
hist(x, probability=TRUE,ylim=c(0,1),main = NULL)
q_25 <- qgeom(.25,prob = p)
q_50 <- qgeom(.5,prob = p)
q_75 <- qgeom(.75,prob = p)
rect(xleft = min(x),ybottom = 0,ytop = 1,xright = q_25,col= rgb(.5,.5,1.0,alpha=0.5))
rect(xleft = q_25,ybottom = 0,ytop = 1,xright = q_50,col= rgb(0,0,1.0,alpha=0.5))
rect(xleft = q_50,ybottom = 0,ytop = 1,xright = q_75,col= rgb(0,1,0,alpha=0.5))
rect(xleft = q_75,ybottom = 0,ytop = 1,xright = max(x),col= rgb(1,0,0,alpha=0.5))
abline(v = mean(x), col = "blue", lwd = 6)
text(mean(x),.8,paste("mean is:",mean(x)))
points(x, dgeom(x,prob = p),pch=16,cex=2,col="dark red")
sd_a <-sd.plot(x)
lines(sd_a,array(.4,length(sd_a)),lwd=3,col="blue")
text(sd_a[50],.5,paste("std is:",sd(x)))
var_a <- var.plot(x)
lines(var_a,array(.2,length(var_a)),lwd=3,col="blue")
text(var_a[50],.22,paste("var is:",var(x)))
title("Geometric distribution")
mtext("1st Quartile = light sky blue, 2nd Quartile = sky blue, 3rd Quartile = light green,4th Quartile = red")
dev.off()
 
