#Assign Number of patches (num)
num=100

#Assign Total Spatial Area over which the patches are distributed (Tx by Ty)
Tx=100
Ty=100

#So let us call Ai the area of patch i (pi).  Assume circular patches for now, centered at (xi,yi).

#Patch Size
A<-rep(0,num); dim(A)<-c(num)
  for (i in 1:num) {
    A[i]=1
    }

#Since patches are circular, ri=radius of ith patch.
r<-rep(0,num); dim(r)<-c(num)
  for(i in 1:num) {
    r[i]=sqrt(A[i]/pi)
    }

#Assign xi and yi
x<-rep(0,num); dim(x)<-c(num)
y<-rep(0,num); dim(y)<-c(num)

#Spatial Distribution
  #A) Uniform distribution
    p.x<-seq(from=5,to=95,by=10)
    p.x<-rep(p.x,10)
    p.y<-rep(seq(from=5,to=95,by=10),each=10)

 


 #distance among sites
d<-rep(0,num^2);dim(d)<-c(num,num)   #before giving a value to a subscripted variable, the variable vector needs to be defined
    for (i in 1:num) {
      for (j in 1:num)  {
        d[i,j]<-((p.x[i]-p.x[j])^2+(p.y[i]-p.y[j])^2)^.5
        sp[i,j]<-r[i]+r[j]
      }}
      
cat(d)

#hist(d)
plot(p.x,p.y)
#starting landscape matrix M
alpha<-5
mu<-1
m<-rep(0,num^2);dim(m)<-c(num,num)   #before giving a value to a subscripted variable, the variable vector needs to be defined
    for (i in 1:num) {
      for (j in 1:num)  {
      if (i==j) {m[i,j]<-0}
        else {
         m[i,j]<-mu^2*p.area[i]*p.area[j]*exp(-1*d[i,j]/alpha)}
      }}

   sol<-eigen(m,symmetric=F)
   aa<-sol$values[order(sol$values,decreasing=T)]
   leading<-Re(aa[1])
   leading

sites<-1:num
for (removal in 1:(.9*num)) {
  for (i in 1:num) {
  to.remove<-sample(sites,n=removal,replace=F)
  
  ?sample



m[1:10,1:16]

par(mfrow=c(3,3))
x<-seq(1:20)
for (i in 1:9) {
  alpha<-1/5
  y<-exp(-1*alpha*x)
  plot(x,y,main=alpha,type="l")
  }
  
plot(alpha,y)


####################################################################################################
####################################################################################################




matrix.a<-rnorm(n=20)
matrix.a
dim(matrix.a)<-c(4:5)
matrix.a

#Patch Size
p.area<-rep(1,n)
cat(p.area)