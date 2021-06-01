
x<-1:100-50
x<-rep(x,5)
y<-10*(x+rnorm(500,sd=4))-0.2*(x+rnorm(500,sd=4))^2
y<-y[order(x)] #need to have y ordered according to x
x<-x[order(x)]
plot(x,y)
require(TTR)
aa<-SMA(y,n=10)
aa
lines(x,aa,lwd=3)

bb<-SMA(y,n=40)
lines(x,bb,lwd=3,col="blue")