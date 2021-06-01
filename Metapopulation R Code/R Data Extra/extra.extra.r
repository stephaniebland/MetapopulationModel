

setwd("C:/lab computer/shared file with Stephanie and Ben")
label<-paste("landscape ",time,"  alpha: ",alpha.count, "time",unique.lab,".txt",sep="")
write.table(data=summary.stats,file=label,row.names=F,sep="/t")

###############################################################
#########################DISPLAY###############################
###############################################################

#DISPLAY - Not necessary, just nice
xxx<-1:10; d[xxx,xxx];sp[xxx,xxx]
min(d-sp);max(d-sp)

#DISPLAY - Plot patches with accurate size
windows()
plot(x, y,asp=1)
symbols(x,y,circles=r,inches=F)


#DISPLAY - Plot patches with accurate size
        disp.r<-sqrt(A.rem/pi)
windows()
plot(x, y,col="white",asp=1)
symbols(x,y,circles=disp.r,inches=F)

windows()
plot(prop.area,eigen.patch.rm)
points(prop.area,eigen.islands,col="red")
points(prop.area,eigen.quality,col="blue")

pred<-undisturbed.eigen*prop.area^2
lines(prop.area,pred,col="pink")

windows()
plot(prop.area,eigen.patch.rm,log="y")
points(prop.area,eigen.islands,col="red")
points(prop.area,eigen.quality,col="blue")

###############################################################
###############################################################
###############################################################

#put in more replicates, then do boxplot for each prop.area (binned)
#index of disp. (boxes  random loc 10x10 squares) and nearest neighbor dist. for each landscape

bp=c(9,90,6,7,2)
bp<-sort(bp,decreasing=T)
bp
bp<-sp[1,]
bp
bp<-subset(bp,bp>0)
bp<-sort(bp)
bp[1]



sites<-1:num
for (removal in 1:(.9*num)) {
  for (i in 1:num) {
  to.remove<-sample(sites,n=removal,replace=F)

  ?sample



m[1:10,1:16]

par(mfrow=c(3,3))
x<-c(1:20)
dim(x)=20
for (i in 1:9) {
  alpha<-1/5
  y<-exp(-1*alpha*x)
  plot(x,y,main=alpha,type="l")
  }

plot(alpha,y)

##########TRASH?????
# I don't know how you want to do this to make it work smoothly with the rest of the model, but
# here is a change in alpha - dont know how it should be incorporated though.
#new.alpha=alpha
#m.alpha<-rep(0,num^2);dim(m.alpha)<-c(num,num); 
#eigen.alpha=c(); count=1 # if you incorporate it with the rest.
#for (k in 1:num) {  
#    for (l in 1:num)  {
#      if (k!=l) { 
#      m.alpha[k,l]<-mu^2*A[k]*A[l]*exp(-1*metric[k,l]/new.alpha)}
#    } }
#sol<-eigen(m.alpha,symmetric=F)
#aa<-sol$values[order(sol$values,decreasing=T)]
#eigen.alpha[count]<-Re(aa[1]) 
#