library(ISwR)

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

x<-c(1,2,3)
y<-c(10,20)
temp<-c(x,y,5)
x<-c(red="Huey", blue="Dewey",green="Louie")
oops=c(7,9,13)

x=matrix(c("bkl",2,3,4,5,6,7,8,9,10,11),nrow=3, byrow=T)
rownames(x)<-letters[1:3]   [8]=colnames(x)=LETTERS[4:7]
 
pain=c(0,3,2,2,1)
fpain=factor(pain,levels=0:3)
levels(fpain)=c("None","Mild","Medium","Severe")
fpain
as.numeric(fpain)
levels(fpain)

pain=c(0,76,6)
fpain=factor(pain)
levels(fpain)=c("N","Fl","Ml","bk")


intake.pre=c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
intake.post=c(3910,4220,3885,5160,5645,4680,5265,5975,6790,6900,7335) 
mylist=list(before=intake.pre,after=intake.post)
mylist
mylist$after
d=data.frame(intake.pre,intake.post)
intake.pre[5]<-NA

thuesen$blood.glucose[c(1,2,4,13,14)]<-NA

lapply(thuesen,mean, na.rm=T)

caff.marital=matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67),ncol=4,byrow=T)
for (i in 1:3), {caff.marital[i,]<-(caff.marital[i,]/(sum(caff.marital[i,]))); cat(i); caff.marital}
caff.marital                                                   
