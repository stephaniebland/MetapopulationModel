p.x<-runif(n=num,min=0,max=Tx)
p.y<-runif(n=num,min=0,max=Ty)
     #need to generate rule to make sure that no two sites are touching (dij>radius i + radius j)
cat('p.x=',p.x,'\n')
cat('p.y=',p.y,'\n')

d<-rep(0,num^2);dim(d)<-c(num,num)
sp<-rep(0,num^2);dim(d)<-c(num,num)

  for (i in 1:num) {
    x[i]<-runif(1,0,Tx)
    y[i]<-runif(1,0,Ty)
      for (j in 1:(i-1))  {
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

