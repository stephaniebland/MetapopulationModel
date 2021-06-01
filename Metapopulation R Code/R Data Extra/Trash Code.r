sizes<-rlnorm(n=num,mean=0,sd=1)
sizes<-sizes/(sum(sizes))*num



    #B) Log normal distribution of site sizes
        for (i in 1:num) {
        #  A[i]=rlnorm(n=num,mean=0,sd=1)
        #Alternatives:
        #  A[i]=rnbinom(n=num,size=.5,mu=1)
          A[i]=rpois(n=num,lambda=1)
          }
        A<-(num*A)/(sum(A))




matrix.a<-rnorm(n=20)
matrix.a
dim(matrix.a)<-c(4:5)
matrix.a

#Patch Size
p.area<-rep(1,n)
cat(p.area)



##########################
#Some Failed attempts to plot actual character size.
##########################
#plot(x,y,cex=r) # Note cex does not give actual size of patches, just  scales them according to faulty system.
#plot(x,y,symbols(x,y,circles=r))
#symbols(x,y,squares=r)
#symbols(x,y,circles=r)



xyz=5
cat("hello ",xyz,"\n")





        #I dont think this to complex - but could reduce by doing m.smallermu=m*(prop.area^2)
        new.mu=mu*prop.area
            for (k in 1:num) {
               for (l in 1:num)  {
               if (k==l) {m.smallermu[k,l]<-0}
                 else {
                  m.smallermu[k,l]<-new.mu^2*A[k]*A[l]*exp(-1*d[k,l]/alpha)} #I dont think this to complex - but could reduce by doing m.smallermu=m*(prop.area^2)
               }}
               
               


        # ISLAND MODEL Reduce the area of each patch    
        new.A=prop.area*A
        new.r=sqrt(new.A/pi)        
        new.sp<-rep(0,num^2);dim(new.sp)<-c(num,num)   
        for (k in 1:num) {
          for (l in 1:num)  {
            if (l!=k) {new.sp[k,l]<-(((x[k]-x[l])^2+(y[k]-y[l])^2)^.5)-(new.r[k]+new.r[l])}}    
        for (k in 1:num) {  #dont need to reset new2.m since k!=l always constant
           for (l in 1:num)  {
             if (k!=l) { new2.m[k,l]<-mu^2*new.A[k]*new.A[l]*exp(-1*d[k,l]/alpha)}#Still uses d since indep. of shrinkage, points stay fixed.  Caution:  If using sp instead use new.sp
        }}
        sol<-eigen(new2.m,symmetric=F)
        aa<-sol$values[order(sol$values,decreasing=T)]
        eigen.islands[count]<-Re(aa[1])
        }  
        
        
        
       # m.smallermu=m*(prop.area[count]^2)
#        sol<-eigen(m.smallermu,symmetric=F)
#        aa<-sol$values[order(sol$values,decreasing=T)]
#        eigen.quality[count]<-Re(aa[1]) 