

min.dist<-function(x1,x2,y1,y2,rad1,rad2)



number<-(100-j)
dis.tmp<-c();dis.tmp<-rep(NA,number^2);dim(dis.tmp)<-c(number,number)
  for (i in 1:number) {
    for (j in 1:number) {
       dis.tmp[i,j]<-sqrt((loc.x[i]-loc.x[j])^2+(loc.y[i]-loc.y[j])^2)-site.rad[i]-site.rad[j]
        if (i==j) {dis.tmp[i,j]<-0}
     }}


     d1<-d2<-seq(1:100)
     d<-data.frame(d1,d2)
     head(d)
     
     rem<-sample(1:100,size=42,replace=F)
     ?sample
     
     
     x=(1:10)
     remove=x[5]
     y=sample(1:10,size=4,replace=F)
     for (i in 1:10) {
      print (x[i])
      }
      
      
     x<-rnorm(10^2)
     length(x)
     matrix.one<-x;dim(matrix.one)<-c(10,10)
     matrix.one
     
     to.rem<-sample(1:10,size=2,replace=F)
     to.rem
     
     matrix.one[to.rem,]<-0
     matrix.one[,to.rem]<-0
     matrix.one