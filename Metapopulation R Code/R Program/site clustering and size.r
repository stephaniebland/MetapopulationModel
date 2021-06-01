?rpois
?rnbinom
aa<-rnbinom(n=10000,size=2,mu=1)
bb<-rnbinom(n=10000,size=.5,mu=1)

bb<-rpois(n=10000,lambda=1)
par(mfrow=c(2,1))
hist(aa)
hist(bb)
mean(aa)
mean(bb)

par(mfrow=c(2,1))
hist(aa,xlim=c(0,20),ylim=c(0,10000))
hist(bb,xlim=c(0,20),ylim=c(0,10000))
par(mfrow=c(1,1))

bb<-rpois(n=100,lambda=1)
sum(bb)

#a log normal distribution of site sizes, with the total area = 100
sizes<-rlnorm(n=100,mean=0,sd=1)
sizes<-sizes/(sum(sizes))*100
radii<-sqrt(sizes/pi)

#blocks are ordered from 1 to 100, each representing a 10 x 10 space in the landscape
#1 to 10 correspond with x=0,x=10,x=20, etc along the x
#11 is x=0,y=10 in bottom left corner, etc.
#start with Poisson

#sites<-rpois(n=100,lambda=1)

#then negative binomial
sites<-rnbinom(n=100,size=.5,mu=1)

tmp<-sum(sites)
loc.x<-rep(0,100)
loc.y<-rep(0,100)
site.rad<-rep(0,100)


#this code adds/reduces the number of sites so that there are exactly 100 sites
if (tmp < 100) {
  tmp2<-100-tmp
  while (tmp2!=0) {
     aa<-runif(n=1,min=1,max=100)
     sites[aa]<-sites[aa]+1
     tmp<-sum(sites)
     tmp2<-100-tmp
     print(tmp2)
  }
}
if (tmp>100) {
    tmp2<-tmp-100
  while (tmp2!=0) {
    aa<- runif(n=1,min=1,max=100)
    if(sites[aa]>0) {
      sites[aa]<-sites[aa]-1}
    tmp<-sum(sites)
    tmp2<-tmp-100
    print(tmp2)
  }
}

block<-1
site.counter<-1
for (i in 0:9) {  #each y
  for (j in 0:9) { #each x
    if (sites[block]>0) {
      for (k in 1:sites[block]) {
      fit<-0
      to.exit<-0
      while (fit==0) {
       if(to.exit==0) {
        tmpi<-i*10+sample(1:9,size=1)
        tmpj<-j*10+sample(1:9,size=1)
          } else {
          tmpi<-tmpi+rnorm(n=1,mean=0,sd=2)
          tmpj<-tmpj+rnorm(n=1,mean=0,sd=2) }
        tmprad<-radii[site.counter]
        print(tmpi)

        dis.x<-(loc.x-tmpi)^2
        dis.y<-(loc.y-tmpj)^2
        dis<-(dis.x+dis.y)^.5-site.rad
        if (min(dis)>tmprad) {fit<-1
          } else { to.exit<-1}
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


    
  

