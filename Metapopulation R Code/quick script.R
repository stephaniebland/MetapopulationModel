#setwd("//roger/t-bland/My Documents/Downloads")
location="C:\Users\Stephie!!!\Dropbox\RW516"
setwd("C:\Users\Stephie!!!\Dropbox\RW516\0.5c0.5DATA-1")
tmp<-read.table("RESULTSFeb 11, 18,32.txt",header=T,sep="\t")
setwd(paste(location,"))
final.results<-read.table("RESULTSFeb 11, 18,32.txt",header=T,sep="\t")
eigens<-tmp[,3:5]/tmp[,2]
tmp2<-data.frame(tmp,eigens)

a.1=  1
a.2="c"
a.3= 1

if(a.2=="u"){spat=1}
if(a.2=="r"){spat=2}  
if(a.2=="c"){
  if (a.1==0.5){spat=3}
  if (a.1==1){spat=4}
  }
  
if (a.3==0){size=1}
if (a.3==0.5){size=2}
if (a.3==1){size=3}


resul=rep(NA,3*20*length(seq(0,98,2))*3);dim(resul)=c(3,20,length(seq(0,98,2)),3)
listin=rep(0,3*20*3*2);dim(listin)=c(3,20,3,2)

for (l.alpha in 1:3){
  set1<-subset(tmp2,alpha.count==c(0.5,1,2)[l.alpha])
  for (l.rep in 1:20){
    set2<-subset(set1,bunch.of.repeats==l.rep)
    for (j in 1:length(seq(0,98,2))){
      i=((seq(0,98,2))/100)[j]
      set3<-subset(set2,set2$prop.area>=i&set2$prop.area<i+2)
      resul[l.alpha,l.rep,j,1]=median(set3$eigen.patch.rm.1)
      resul[l.alpha,l.rep,j,2]=median(set3$eigen.islands.1)
      resul[l.alpha,l.rep,j,3]=median(set3$eigen.quality.1)
      if (resul[l.alpha,l.rep,j,1]<resul[l.alpha,l.rep,j,2]){listin[l.alpha,l.rep,1,1]<-(i+.01)}
      if (resul[l.alpha,l.rep,j,1]<resul[l.alpha,l.rep,j,3]){listin[l.alpha,l.rep,1,2]<-(i+.01)}
      if ((resul[l.alpha,l.rep,j,1]>resul[l.alpha,l.rep,j,2])&(listin[l.alpha,l.rep,2,1]==0)){listin[l.alpha,l.rep,2,1]<-(i+.01)}
      if ((resul[l.alpha,l.rep,j,1]>resul[l.alpha,l.rep,j,3])&(listin[l.alpha,l.rep,2,2]==0)){listin[l.alpha,l.rep,2,2]<-(i+.01)}
  }}
  for (i in 1:2){for(j in 1:2){final.results[size,spat,l.alpha,1,i,j]=mean(listin[l.alpha,,i,j]);final.results[size,spat,l.alpha,2,i,j]=var(listin[l.alpha,,i,j])}}
}

final.results=rep(NA,3*4*3*2*3*2);dim(final.results)=c(3,4,3,2,3,2)
dir.create("C:/Users/Stephie!!!/Dropbox/RW516/FINAL");setwd("C:/Users/Stephie!!!/Dropbox/RW516/FINAL")
write.table(final.results,file="final results",sep="\t",row.names=T)


































list[l.alpha,,1,1]



x=resul[,6,,]
plot();points()



for(curve in 1:2){
for (j in 1:length(seq(0,98,2))){
i=((seq(0,98,2))/100)[j]
if (resul[l.alpha,l.rep,j,3]<resul[l.alpha,l.rep,j,curve]){=i+1}
}}

for (l.alpha in 1:3){
for (l.rep in 1:20){

while (j in 1:length(seq(0,98,2))){
}

}
}

final.results=rep(NA,3*4*3*2*3,2);dim(final.results)=c(3,4,3,2,3,2)








b=1:100

coe=matrix(NA,3,4)
eo=rep(NA,3*4*5);dim(eo)=c(3,4,5)
eo[1,3,2]=x

subset(tmp,tmp$prop.area>=0&tmp$prop.area<.02&tmp$bunch.of.repeats==1&tmp$alpha.count==1)

go=1:100
subset(go,go>=5&go<23)