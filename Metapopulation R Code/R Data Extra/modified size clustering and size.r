
blocks<-rnbinom(n=100,size=.5,mu=1)

while (sum(blocks)<num) {
   block.add<-runif(n=1,min=1,max=100)
   blocks[block.add]<-blocks[block.add]+1
   }
while (sum(blocks)>num) {
   block.rm<-runif(n=1,min=1,max=100)
   if(blocks[block.rm]>0){
      blocks[block.rm]<-blocks[block.rm]-1
      }
   }
###########################################

   block<-1
site.counter<-1
for (i in 0:9) {  #each x
  for (j in 0:9) { #each y
    if (sites[block]>0) {
      for (k in 1:sites[block]) {
      fit<-0
      to.exit<-0
            while (fit==0) {
                 if(to.exit<=3) {
                    tmpi<-i*10+sample(1:9,size=1)
                    tmpj<-j*10+sample(1:9,size=1)
                 }
                 else {
                    tmpi<-tmpi+rnorm(n=1,mean=0,sd=2)
                    tmpj<-tmpj+rnorm(n=1,mean=0,sd=2)
                 }
                tmprad<-radii[site.counter]
                dis<-((loc.x-tmpi)^2+(loc.y-tmpj)^2)^.5-site.rad-tmprad
                print(dis)
                if (min(dis)>0) {fit<-1}
                else {to.exit<-1}
            }
            loc.x[site.counter]<-tmpi
            loc.y[site.counter]<-tmpj
            site.rad[site.counter]<-radii[site.counter]
            site.counter<-site.counter+1
     }
  }
  block<-block+1
}
}

      windows()
plot(loc.x,loc.y)
labs<-as.character(round(site.rad,1))
text(loc.x,loc.y,labels=labs)

dis.tmp<-c();dis.tmp<-rep(NA,100*100);dim(dis.tmp)<-c(100,100)
  for (i in 1:100) {
    for (j in 1:100) {
       dis.tmp[i,j]<-sqrt((loc.x[i]-loc.x[j])^2+(loc.y[i]-loc.y[j])^2)-site.rad[i]-site.rad[j]
        if (i==j) {dis.tmp[i,j]<-100}
     }}
dis.tmp[1:12,1:12]
min(dis.tmp)





