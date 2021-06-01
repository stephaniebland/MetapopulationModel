#Patch Size
A<-rep(0,10); dim(A)<-c(10)
  for (i in 1:10) {
    A[i]=1
    }

#Since patches are circular, ri=radius of ith patch.
r<-rep(0,10); dim(r)<-c(10)
  for(i in 1:10) {
    r[i]=sqrt(A[i]/pi)
    }

#Assign xi and yi
x<-rep(0,10); dim(x)<-c(10)
y<-rep(0,10); dim(y)<-c(10)

cat('Laundry\n')
x
y
r

d<-rep(0,10^2)
dim(d)<-c(10,10)
sp<-rep(0,10^2)
dim(d)<-c(10,10)

  for (i in 1:10) {
    x[i]<-runif(1,0,10)
    y[i]<-runif(1,0,10)
      for (j in 1:10)  {
        d[i,j]<-((x[i]-x[j])^2+(y[i]-y[j])^2)^.5
        #sp[i,j] is space between patches
#        sp[i,j]<-d[i,j]-r[i]-r[j]
      }
#    while ((min(sp[i,j],true))<=0)  {
#      x[i]=runif(1,0,Tx)
#      y[i]=runif(1,0,Ty)
#      for (j in 1:(i-1))  {
#        d[i,j]<-((x[i]-x[j])^2+(y[i]-y[j])^2)^.5
        #sp[i,j] is space between patches
#        sp[i,j]<-d[i,j]-r[i]-r[j]
#        }
#      }
    }

for (i in 1:10) {
  for (j in 1:10) {
    d[i,j]<-((x[i]-x[j])^2+(y[i]-y[j])^2)^.5
  }
}

dim(sp)=c(10:10)
dim(d)=c(10:10)
for (i in 1:10) {
  for (j in 1:10) {
    sp[i,j]=d[i,j]-r[i]-r[j]
  }
}

cat('\n')
cat('x and y\n')
x
y
cat('now for d:\n')
d
cat('now sp\n')
sp
plot(x,y)