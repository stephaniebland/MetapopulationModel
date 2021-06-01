library(ISwR)
install.packages("ISwR")

plot(rnorm(1000))
x<-2
weight<-c(60,72,57,90,95,72)
height<-c(1.75,1.8,1.65,1.9,1.74,1.91)
bmi<-weight/height^2
sum(weight)/length(weight)
xbar<-mean(weight)
sqrt(sum((weight-xbar)^2)/(length(weight)-1))
sd(weight)
t.test(bmi,mu=22.5)
sd(bmi)
plot(height,weight)
plot(height,weight,pch=2)
hh<-c(1.65,1.7,1.75,1.8,1.85,1.9)
lines(hh,22.5*hh^2)

