
num=4
A<-rep(0,num); dim(A)<-c(num)
  for (i in 1:num) {
    A[i]=i
    }

B<-rnorm(n=20)
B
dim(B)<-c(4:5)
B[1,1]=3
B
Cm<-rep(0,4*5)
Dm<-rep(0,4*5)
dim(Cm)=(4:5)
dim(Dm)=(4:5)
for (i in 1:4) {
  for (j in 1:5) {
    Cm[i,j]=A[i]+B[i,j]
   }
   }
Dm=B-3
cat('hello to you\n')
B
Cm
Dm

