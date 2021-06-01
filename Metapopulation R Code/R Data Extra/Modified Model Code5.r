#OPTIONS
 #1.  Choose to set metric to:
      # A)                      "sp" - Space between patch edges - Note:  Use for island model
      # B)                      "d"  - Space between patch centres 
     metric.lab="sp"        
 #2.  Assign Number of patches (num)
     num=100                        
 #3.  Assign Total Spatial Area over which the patches are distributed (Tx by Ty)
     Tx=100
     Ty=100
 #4.  Choose patch size distribution:
      # A Higher value is more clustered, 0 is uniform
     size.dist=1
 #5.  Choose spatial dist.  u=uniform, r=random, c=clustered
     spatial.dist="u"
      # For clustered, specify by how much.  
     clustering=0.5
 #6.  Block Number for clustering/random - May not be necessary to change,
      # and a little complicated to modify, so not included. 10x10 patches
      # probably OK for initial analyses.
      # Find under SPATIAL DIST. B) 
 #7.  Uniform Distribution - May want to modify to have patches go right to the edge.
      # However, we already have edge avoidance for random & clustered, so not necessary.
 #8.  Percentage of Islands that are not removed (as decimal)
     islands.to.be.removed=0.2  #WARNING!!!!  TEMP. CHANGED,  may be better at .5######## 
 #9.  Repeats for habitat destruction
      #repeats is the number of time we repeat simulation  
     repeats=1  #WARNING!!!!  TEMP. CHANGED, may be better at 100########
 #10. Change Alpha (for natural landscape - alpha changes according to how much land you remove)
     alpha=seq(1,2,.25)
 #11. Change mu
     mu=1
 #12. Total number of repeats for the entire program
     number.of.bunch.of.repeats=1
#overall data output
setwd("M:/R Program/DATA")
#setwd("C:/Users/Ben/Dropbox/R code/R code/habitat loss versus degradation metapopulation") #machine specific
prop.area<-undisturbed.eigen<-eigen.patch.rm <-eigen.islands<-eigen.quality<-num.patch.rem<-alpha.count<-bunch.of.repeats<-landscape.id<-mean.nn<-var.nn<-0
out.data<-cbind(prop.area,undisturbed.eigen,eigen.patch.rm ,eigen.islands,eigen.quality,num.patch.rem,alpha.count,bunch.of.repeats,landscape.id,mean.nn,var.nn)
nice.out.data<-cbind(prop.area,undisturbed.eigen,eigen.patch.rm ,eigen.islands,eigen.quality,num.patch.rem,alpha.count,bunch.of.repeats)

  
#PROGRAM

###############################################################
###############################################################
###############################################################

#Unique label for each run
unique.lab<-format(Sys.time(), "%b %d, %H,%M") ;unique.lab 

#Bunch of repeats:
for (bunch.of.repeats in 1:number.of.bunch.of.repeats) {

#So let us call A[i] the area of patch i (pi).  Assume circular patches for now, centered at (x[i],y[i]).

#PATCH SIZE - Total area is 100
#A<-rep(0,num); dim(A)<-c(num)
#    #A) Equally Sized 
#        A=rep(1 ,num)  #This part may not be necessary if we are not adding any other distribution types to part B (ex. rpois, rbinom...)
#          
    #B) Log normal distribution of site sizes
        #Issue (Slight approx.  -->  When scaling to fit total area of 100, you disrupt log normal distribution)
        A=rlnorm(n=num,mean=0,sd=size.dist) #Higher sd is more clustered
        A<-(num*A)/(sum(A))  

#RADIUS - Since patches are circular, ri=radius of ith patch.
        r<-sqrt(A/pi);  landscape.original.r=r
    
#LOCATION - Assign xi and yi
x<-rep(0,num); dim(x)<-c(num)
y<-rep(0,num); dim(y)<-c(num)

#SPATIAL DISTRIBUTION OF PATCHES
  #A) Uniform distribution (Only works for num=100 right now - more complicated for alternative num and Tx, Ty values)
    if (spatial.dist=="u"){
    x<-rep(seq(from=5,to=95,by=10),10)
    y<-rep(seq(from=5,to=95,by=10),each=10)}
  #B) Random & Aggregated Distribution - Agregated is a small size value, random is a large size value
    if (spatial.dist!="u"){ 
          #First, Assign the number of patches to go in each block in the total spatial area
          #Total number of blocks is block.number
          block.number=Tx*Ty/100
          mean.num<-num/block.number 
          if (spatial.dist!="c"){blocks<-rnbinom(n=block.number,size=clustering,mu=mean.num)} #aggregated
          if (spatial.dist!="r"){blocks<-rpois(n=block.number,lambda=mean.num)} #random - this is equivalent to size = infinity
          #Make total patch count = num
          while (sum(blocks)<num) {
             block.add<-runif(n=1,min=1,max=block.number+1)
             blocks[block.add]<-blocks[block.add]+1
             }
          while (sum(blocks)>num) {
             block.rm<-runif(n=1,min=1,max=block.number+1)
             if(blocks[block.rm]>0){
                blocks[block.rm]<-blocks[block.rm]-1
                }
             }   
             
          #Now place the patches within each block.  (Approx:  If the patches are too big to fit in one block, randomly place the patch nearby.)  
          patch.rad<-rep(0,num); dim(patch.rad)<-c(num)
          block.counter<-1
          patch.counter<-1
          for (i in 0:9) {  
            for (j in 0:9) { 
              if (blocks[block.counter]>0) {
                for (k in 1:blocks[block.counter]) {
                fit<-0
                to.exit<-0
                      while (fit==0) {
                           if(to.exit<=3) {
                              print(to.exit)
                              tmpi<-i*10+sample(1:9,size=1)
                              tmpj<-j*10+sample(1:9,size=1)
                           }
                           else {
                              tmpi<-tmpi+rnorm(n=1,mean=0,sd=2)
                              tmpj<-tmpj+rnorm(n=1,mean=0,sd=2)
                           }
                           tmp.rad<-r[patch.counter]
                           dis<-((x-tmpi)^2+(y-tmpj)^2)^.5-patch.rad-tmp.rad
                           #Note:  There is some origin avoidance here
                           #Some edge avoidance:
                           edge.dis<-min(c(tmpi-tmp.rad,tmpj-tmp.rad,Tx-tmpi-tmp.rad,Ty-tmpj-tmp.rad))
                           if (min(dis)<=0) {to.exit<-to.exit+1}
                           else if (edge.dis<0) {to.exit<-to.exit+1}
                           else   {fit<-1}
                      }
                      x[patch.counter]<-tmpi
                      y[patch.counter]<-tmpj
                      patch.rad[patch.counter]<-r[patch.counter]
                      patch.counter<-patch.counter+1
                }
              }
            block.counter<-block.counter+1
          }
          } 
          r<-patch.rad
    }

landscape.id<- format(Sys.time(), "%b %d %H %M %S")
tmp.landscape<-data.frame(x,y,r) ; tmp.name<-paste("landscape",unique.lab,"-",bunch.of.repeats,".txt",sep="")
write.table(tmp.landscape,file=tmp.name,sep="\t",row.names=F)  

#DISTANCE among patches - d is distance between patch centres, sp is the distance between patch edges. 
d<-rep(0,num^2);dim(d)<-c(num,num)
sp<-rep(0,num^2);dim(sp)<-c(num,num)   
    for (i in 1:num) {
      for (j in 1:num)  {
        d[i,j]<-((x[i]-x[j])^2+(y[i]-y[j])^2)^.5
        if (j!=i) {sp[i,j]<-(((x[i]-x[j])^2+(y[i]-y[j])^2)^.5)-(r[i]+r[j])}
      }
    }

#DISTANCE METRICS!
        d2<-as.vector(d); d2<-subset(d2,d2>0) ;d2<-d2[order(d2)]
        min.dist<-d2[1];second.dist<-d2[3];var.dist<-var(d2);mean.dist<-mean(d2)
        #Index of dispersion randomly placed boxes.  Make non-overlapping.  Use sites that have center in box (or else you coud do total area)
          quadrant.counter<-1
          quad.size<-20
          num.of.quad<-10
          quad.reps=20
          dispersion.index.area.approx.pre=rep(0,quad.reps); dim(dispersion.index.area.approx.pre)=c(quad.reps); dispersion.index.points.pre=rep(0,quad.reps); dim(dispersion.index.points.pre)=c(quad.reps)
          for (k in 1:quad.reps){
              quad.x=rep((Tx+quad.size),num.of.quad); dim(quad.x)=c(num.of.quad); quad.y=rep((Ty+quad.size),num.of.quad); dim(quad.y)=c(num.of.quad)
              for (i in 1:(num.of.quad)){
                exit.strategy=0
                overlap=rep(-1,num.of.quad)                                                                                                                                                    
                while ((min(overlap)<0)&(exit.strategy<500)){ 
                  tmp.quad.x<-sample(0:(Tx-quad.size),1)
                  tmp.quad.y<-sample(0:(Ty-quad.size),1)
                  exit.strategy=exit.strategy+1
                  for (j in 1:num.of.quad) {
                  	overlap[j]<-(max((tmp.quad.x+quad.size-quad.x[j])*(tmp.quad.x-quad.x[j]-quad.size),(tmp.quad.y+quad.size-quad.y[j])*(tmp.quad.y-quad.y[j]-quad.size))) 
                  }
                quad.x[i]<-tmp.quad.x; quad.y[i]<-tmp.quad.y
             	  }
              }

              #Display:
              symbols((quad.x+((quad.size)/2)),(quad.y+((quad.size)/2)),fg="snow4",xlim=c(0,Tx),ylim=c(0,Ty), main=("Index of Dispersion Quadrants \n with patches"), rectangles= matrix(rep((quad.size),num.of.quad*2),ncol=2), inches=F); symbols(x, y,fg="slateblue1", bg="slateblue1",circles=r, inches=F, add=T)
              #Count Number of patches in each quadrant:
              quad.patch.points=rep(0,num.of.quad); dim(quad.patch.points)=c(num.of.quad)
              quad.patch.area.approx=rep(0,num.of.quad); dim(quad.patch.area.approx)=c(num.of.quad)
              for (i in 1:num.of.quad){
              	for (j in 1:num){
              		if ((max((x[j]-quad.x[i])*(x[j]-quad.x[i]-quad.size),(y[j]-quad.y[i])*(y[j]-quad.y[i]-quad.size)))<=0){quad.patch.points[i]=quad.patch.points[i]+1; quad.patch.area.approx[i]=quad.patch.area.approx[i]+r[j]}
              	}
              }
              var.quad.points=var(quad.patch.points); mean.quad.points=mean(quad.patch.points)
              dispersion.index.points.pre[k]=var.quad.points/mean.quad.points
              var.quad.area.approx=var(quad.patch.area.approx); mean.quad.area.approx=mean(quad.patch.area.approx)
              dispersion.index.area.approx.pre[k]=var.quad.area.approx/mean.quad.area.approx
          }
          dispersion.index.points=mean(dispersion.index.points.pre)
          dispersion.index.area.approx=mean(dispersion.index.area.approx.pre)
          dispersion.index.points; dispersion.index.area.approx
        # Save landscape locations and patch sizes in another file    
        landscape.summary<-data.frame(bunch.of.repeats, min.dist, second.dist, var.dist, mean.dist, num, Tx, Ty, size.dist, spatial.dist, mu, quad.size, num.of.quad, dispersion.index.points, dispersion.index.area.approx); landscape.summary.name<-paste("landscape summary", unique.lab,"-",bunch.of.repeats,".txt",sep="")
        write.table(landscape.summary, file=landscape.summary.name, sep="\t",row.names=T)

    
#RENAMING VARIABLES - Are we using d or sp this time?

if (metric.lab=="sp") {metric=sp; metric.sp=1}                                
else {metric=d; metric.sp=0}

#ALPHA repeats          
for (alpha.count in alpha) {

#NATURAL LANDSCAPE MATRIX & EIGENVALUE
m<-rep(0,num^2);dim(m)<-c(num,num)   
    for (i in 1:num) {
      for (j in 1:num)  {
      if (i==j) {m[i,j]<-0}
        else {
         m[i,j]<-mu^2*A[i]*A[j]*exp(-1*metric[i,j]/alpha.count)}
      }}
   sol<-eigen(m,symmetric=F)
   aa<-sol$values[order(sol$values,decreasing=T)]
   eigen.natural<-Re(aa[1])

###############################################################
count<-0
prop.area<-c();eigen.patch.rm=c(); eigen.islands=c(); eigen.quality=c(); num.patch.rem=c(); eigen.quality2=c()
total.area=sum(A)
new.m<-rep(0,num^2);dim(new.m)<-c(num,num)    #new m with sites removed
m.smallermu<-rep(0,num^2);dim(m.smallermu)<-c(num,num) #For lower patch quality 
m.islands<-rep(0,num^2);dim(m.islands)<-c(num,num)# For Island model
for (i in 1:(islands.to.be.removed*num)) {  
    for (j in 1:repeats){
        count<-count+1
        print(count)
        num.patch.rem[count]=i
        # PATCH REMOVAL
        to.rem<-sample(1:num,size=i,replace=F)
        A.rem<-A ;  A.rem[to.rem]<-0 # Set size of patches to remove to zero
            for (k in 1:num) {
               for (l in 1:num)  {
               if (k==l) {new.m[k,l]<-0}
                 else {
                  new.m[k,l]<-mu^2*A.rem[k]*A.rem[l]*exp(-1*metric[k,l]/alpha.count)}  # New matrix with removed sites removed
               }}
            sol<-eigen(new.m,symmetric=F)
            aa<-sol$values[order(sol$values,decreasing=T)]
            eigen.patch.rm[count]<-Re(aa[1])    

        # PATCH QUALITY
        prop.area[count]=(total.area-sum(A[to.rem]))/total.area
        eigen.quality[count]=eigen.natural*(prop.area[count]^2)        
        
        # ISLAND MODEL Reduce the area of each patch    
        if (metric.sp==1) {
            new.A=prop.area[count]*A
            new.r=sqrt(new.A/pi)        
            new.sp<-rep(0,num^2);dim(new.sp)<-c(num,num)     
            for (k in 1:num) {  #dont need to reset new2.m since k!=l always constant
               for (l in 1:num)  {
                 if (k!=l) { 
                 new.sp[k,l]<-(((x[k]-x[l])^2+(y[k]-y[l])^2)^.5)-(new.r[k]+new.r[l])
                 m.islands[k,l]<-mu^2*new.A[k]*new.A[l]*exp(-1*new.sp[k,l]/alpha.count)}
            } }
        sol<-eigen(m.islands,symmetric=F)
        aa<-sol$values[order(sol$values,decreasing=T)]
        eigen.islands[count]<-Re(aa[1])} 
    }
}
if (metric.sp==0) {eigen.islands=eigen.quality}




###############################################################
########################SAVE DATA##############################
###############################################################


#SUMMARY
#The natural state of the patches have an eigenvalue of eigen.natural
#Remove a patch - eigen.patch.rm
#Reduce area of patch like island - eigen.islands
#Total proportion of area removed for each trial is 1-prop.area
#alpha.count - current alpha value
#number.of.bunch.of.repeats - total # of repeats for given setup
t.undisturbed.eigen<-rep(eigen.natural,length(prop.area)) ;t.bunch.of.repeats<-rep(bunch.of.repeats,length(prop.area));t.alpha.count<-rep(alpha.count,length(prop.area))
tmp.out<-cbind(prop.area,t.undisturbed.eigen,eigen.patch.rm ,eigen.islands,eigen.quality,num.patch.rem,t.alpha.count,t.bunch.of.repeats,landscape.id,mean.nn,var.nn)
out.data<-rbind(out.data,tmp.out)
nice.tmp.out<-cbind(prop.area,t.undisturbed.eigen,eigen.patch.rm ,eigen.islands,eigen.quality,num.patch.rem,t.alpha.count,t.bunch.of.repeats)
nice.out.data<-rbind(nice.out.data,nice.tmp.out)

#IS THIS NECESSARY??? 
#M.summary.stats1=c();dim(M.summary.stats1)=c(number.of.bunch.of.repeats,length(row))
#M.summary.stats1[alpha.count,]<-row

summary.of.trial<-summary(cbind(prop.area,eigen.patch.rm,eigen.islands,eigen.quality))
tmp.name<-paste("Summary,alpha=",alpha.count,",",unique.lab,"-",bunch.of.repeats,".txt",sep="")
write.table(summary.of.trial,file=tmp.name,sep="\t",row.names=T)


}


#AGAIN -- WHAT WAS THIS FOR???
#summary.stats2=c()                                                                     
#summary.stats2[bunch.of.repeats]=matrix(paste(
}

summary.stats=data.frame(out.data)
summary.stats<-summary.stats[-1,]
nice.su.stats=data.frame(nice.out.data)
nice.su.stats=nice.su.stats[-1,]
#summary(summary.stats) # I'm pretty sure this is the remnants of some internal control to test that it was working properly

######SAVE OUTPUT
tmp.name<-paste("RESULTS",unique.lab,".txt",sep="")
write.table(summary.stats,file=tmp.name,sep="\t",row.names=T)
write.table(summary.stats,file=paste("nice",tmp.name),sep="\t",row.names=T)


 #Issues I need to bring up:
 #What was that summary stats doing?
 #What is mean.nn, var.nn supposed to be?  I forget.

print("DONE!!!")


#DISPLAY:

##### Subset into each alpha:
l.p.a=length(subset(nice.su.stats$alpha,nice.su.stats$alpha.count==alpha[1]))
prop.area.n=rep(NA,l.p.a*length(alpha));dim(prop.area.n)=c(l.p.a,length(alpha))
eigen.patch.rm.n=rep(NA,l.p.a*length(alpha));dim(eigen.patch.rm.n)=c(l.p.a,length(alpha))
eigen.islands.n=rep(NA,l.p.a*length(alpha));dim(eigen.islands.n)=c(l.p.a,length(alpha))
eigen.quality.n=rep(NA,l.p.a*length(alpha));dim(eigen.quality.n)=c(l.p.a,length(alpha))
for(j in 1:length(alpha))  {
      prop.area.n[,j]=(subset(nice.su.stats$prop.area,nice.su.stats$alpha==(alpha[j]))) 
      eigen.patch.rm.n[,j]=subset(nice.su.stats$eigen.patch.rm,nice.su.stats$alpha==(alpha[j]))
      eigen.islands.n[,j]=subset(nice.su.stats$eigen.islands,nice.su.stats$alpha==(alpha[j]))
      eigen.quality.n[,j]=subset(nice.su.stats$eigen.quality,nice.su.stats$alpha==(alpha[j]))
      ss<-subset(nice.su.stats,alpha.count==alpha[j]) 

      windows()
      plot(prop.area.n[,j],eigen.patch.rm.n[,j],main=(paste("Alpha=",alpha[j])))
      points(prop.area.n[,j],eigen.islands.n[,j],col="red")
      points(prop.area.n[,j],eigen.quality.n[,j],col="blue")
      plot.name1=paste("Point-plot(prop.area),alpha=",alpha[j],",",unique.lab,sep="")
      savePlot(filename=paste(plot.name1,".pdf",sep=""), type=c("pdf"))
      savePlot(filename=paste(plot.name1,".png",sep=""), type=c("png"))
      savePlot(filename=paste(plot.name1,".bmp",sep=""), type=c("bmp"))
      savePlot(filename=paste(plot.name1,".tiff",sep=""), type=c("tiff"))
      
      windows()
      break.size=5
      br.s=break.size/100
      num.breaks=islands.to.be.removed/br.s
      eigen.islands.subset=rep(NA,num.breaks*repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats);dim(eigen.islands.subset)=c(num.breaks,repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats)
      eigen.quality.subset=rep(NA,num.breaks*repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats);dim(eigen.quality.subset)=c(num.breaks,repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats)
      eigen.patch.rm.subset=rep(NA,num.breaks*repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats);dim(eigen.patch.rm.subset)=c(num.breaks,repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats)
      label.job=rep(NA,num.breaks);dim(label.job)=num.breaks
      for (i in 1:num.breaks){
          temp.di=subset(eigen.islands.n[,j],prop.area.n[,j]>=(1-islands.to.be.removed+(br.s*(i-1)))&prop.area.n[,j]<(1-islands.to.be.removed+(br.s*i)))
          eigen.islands.subset[i,1:length(temp.di)]=temp.di
          temp.di=subset(eigen.quality.n[,j],prop.area.n[,j]>=(1-islands.to.be.removed+(br.s*(i-1)))&prop.area.n[,j]<(1-islands.to.be.removed+(br.s*i)))
          eigen.quality.subset[i,1:length(temp.di)]=temp.di
          temp.di=subset(eigen.patch.rm.n[,j],prop.area.n[,j]>=(1-islands.to.be.removed+(br.s*(i-1)))&prop.area.n[,j]<(1-islands.to.be.removed+(br.s*i)))
          eigen.patch.rm.subset[i,1:length(temp.di)]=temp.di
          label.job[i]=paste((1-islands.to.be.removed+(br.s*(i-1))),"-",(1-islands.to.be.removed+(br.s*i)))
      }
      boxplot(t(eigen.islands.subset),col="red",names=label.job, main=(paste("Alpha=",alpha[j]))); boxplot(t(eigen.quality.subset),col="blue",add=T,names=label.job); boxplot(t(eigen.patch.rm.subset),add=T,names=label.job)
      plot.name2=paste("Barplot,alpha=",alpha[j],",",unique.lab,sep="")
      savePlot(filename=paste(plot.name2,".pdf",sep=""), type=c("pdf"))
      savePlot(filename=paste(plot.name2,".png",sep=""), type=c("png"))
      savePlot(filename=paste(plot.name2,".bmp",sep=""), type=c("bmp"))
      savePlot(filename=paste(plot.name2,".tiff",sep=""), type=c("tiff"))
      
}




windows()
plot(prop.area,eigen.patch.rm)
points(prop.area,eigen.islands,col="red")
points(prop.area,eigen.quality,col="blue")
plot.name1=paste("Point-plot(prop.area),",unique.lab,sep="")
savePlot(filename=paste(plot.name1,".pdf",sep=""), type=c("pdf"))
savePlot(filename=paste(plot.name1,".png",sep=""), type=c("png"))
savePlot(filename=paste(plot.name1,".bmp",sep=""), type=c("bmp"))
savePlot(filename=paste(plot.name1,".tiff",sep=""), type=c("tiff"))


windows()
break.size=5
br.s=break.size/100
num.breaks=islands.to.be.removed/br.s
eigen.islands.subset=rep(NA,num.breaks*repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats);dim(eigen.islands.subset)=c(num.breaks,repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats)
eigen.quality.subset=rep(NA,num.breaks*repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats);dim(eigen.quality.subset)=c(num.breaks,repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats)
eigen.patch.rm.subset=rep(NA,num.breaks*repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats);dim(eigen.patch.rm.subset)=c(num.breaks,repeats*(islands.to.be.removed*num)*number.of.bunch.of.repeats)
label.job=rep(NA,num.breaks);dim(label.job)=num.breaks
for (i in 1:num.breaks){
    temp.di=subset(eigen.islands,prop.area>=(1-islands.to.be.removed+(br.s*(i-1)))&prop.area<(1-islands.to.be.removed+(br.s*i)))
    eigen.islands.subset[i,1:length(temp.di)]=temp.di
    temp.di=subset(eigen.quality,prop.area>=(1-islands.to.be.removed+(br.s*(i-1)))&prop.area<(1-islands.to.be.removed+(br.s*i)))
    eigen.quality.subset[i,1:length(temp.di)]=temp.di
    temp.di=subset(eigen.patch.rm,prop.area>=(1-islands.to.be.removed+(br.s*(i-1)))&prop.area<(1-islands.to.be.removed+(br.s*i)))
    eigen.patch.rm.subset[i,1:length(temp.di)]=temp.di
    label.job[i]=paste((1-islands.to.be.removed+(br.s*(i-1))),"-",(1-islands.to.be.removed+(br.s*i)))
}
boxplot(t(eigen.islands.subset),col="red",names=label.job); boxplot(t(eigen.quality.subset),col="blue",add=T,names=label.job); boxplot(t(eigen.patch.rm.subset),add=T,names=label.job)
plot.name2=paste("Barplot",unique.lab,sep="")
savePlot(filename=paste(plot.name2,".pdf",sep=""), type=c("pdf"))
savePlot(filename=paste(plot.name2,".png",sep=""), type=c("png"))
savePlot(filename=paste(plot.name2,".bmp",sep=""), type=c("bmp"))
savePlot(filename=paste(plot.name2,".tiff",sep=""), type=c("tiff"))











#WHAT WAS THIS FOR??????? (Everything below this)

##new program
landscapes<-unique(summary.stats$landscape.id)
paste("landscape ",landscapes[1],".txt",sep="")


#TEST
tues=cbind(prop.area,eigen.patch.rm,eigen.islands,eigen.quality)
summary(tues)                     



tablet=0
for (i in 1:100){
    tablet=tablet+1
    print (tablet)  
    tbks=sample(1:2,1)
    if (tbks==1) {i=1}
}
