
library(shape)
library(BayesFactor)

loadE1=function(){
  root="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/1dMemory/mrExpts/mr3/MR4S"
  subFiles=paste(root,sprintf("%02d",0:26),sep="")
  sizeE1=array(dim=c(27,3,100))
  for (s in 1:length(subFiles)){
    d=read.table(url(subFiles[s]))
    colnames(d)=c("sub","trl","type","stimNum","corResp","runCorrect","acc","size","resp")
    for (i in 0:2) sizeE1[s,i+1,]=d$size[d$type==i]
  }
  return(sizeE1)}

loadE2=function(){
  root="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/1dMemory/mrExpts/mr5/MR5S"
  subFiles=paste(root,sprintf("%02d",0:26),sep="")
  sizeE2=array(dim=c(27,3,100))
  for (s in 1:length(subFiles)){
    d=read.table(url(subFiles[s]))
    colnames(d)=c("sub","trl","type","stimNum","corResp","runCorrect","acc","size","resp")
    for (i in 0:2) sizeE2[s,i+1,]=d$size[d$type==i]}
  return(sizeE2)}

loadE3=function(){
  root="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/1dMemory/mrExpts/mr7/MR7S"
  subFiles=paste(root,sprintf("%02d",c(0:33,35,36)),sep="")
  sizeE3=array(dim=c(36,2,200))
  for (s in 1:length(subFiles)){
    d=read.table(url(subFiles[s]))
    colnames(d)=c("sub","trl","type","stimNum","corResp","runCorrect","acc","size","resp")
    for (i in 0:1) sizeE3[s,i+1,]=d$size[d$type==i]}
  return(sizeE3)}

loadE4=function(){
  root="https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/1dMemory/mrExpts/mr14/MR14AS"
  goodSub=15:50
  subFiles=paste(root,sprintf("%02d",goodSub),sep="")
  sizeE4=array(dim=c(36,3,120))
  for (s in 1:length(subFiles)){
    d=read.table(url(subFiles[s]))
    colnames(d)=c("sub","trl","type","stimNum","corResp","runCorrect","acc","size","resp")
    for (i in 0:2) sizeE4[s,i+1,]=d$size[d$type==i]}
  return(sizeE4)}

plot1=function(size,legendText,expNum=0){
  indCol=c(rgb(1,0,0,.3),rgb(0,.7,0,.3),rgb(0,0,1,.3))
  aveCol=c(rgb(1,0,0),rgb(0,.5,0),rgb(0,0,1))
  K=dim(size)[2]
  N=dim(size)[3]
  matplot(1:N,jitter(t(size[,1,])),typ='n',lwd=1,lty=1,col=indCol[1],
          xlab="Trial",ylab="Progressive Span")
  for (k in 1:K)
    matlines(1:N,jitter(t(size[,k,])),typ='s',lwd=1,lty=1,
             col=indCol[k])
  ave=apply(size,c(2,3),mean)
  for (k in 1:K)
    lines(1:N,ave[k,],col=aveCol[k],lwd=4)
  perfect=rep(1:20,1:20)[1:N]
  lines(1:N,perfect,)
  legend("topleft",
         legend=c(legendText,"Perfect"),
         col=c(aveCol,'black'),lwd=2,bg='white')
  mtext(side=3,adj=0,cex=1.3,paste("Expt.",expNum))
  }

plot2=function(size,legendText){
  indCol=c(rgb(.5,.5,0,.5),rgb(0,.5,.5,.3))
  aveCol=c(rgb(.3,.3,0),rgb(0,.3,.3))
  K=dim(size)[2]
  N=dim(size)[3]
  v=array(dim=c(dim(size)[1],K-1,N))
  for (k in 2:K) v[,k-1,]=size[,1,]-size[,k,]
  range=c(min(v),max(v))
  matplot(1:N,jitter(t(v[,1,])),typ='n',
        lwd=1,lty=1,col=indCol[1],ylim=range,
         xlab="Trial",ylab="Difference")
  for (k in 1:(K-1))
     matlines(1:N,jitter(t(v[,k,])),typ='s',lwd=1,lty=1,col=indCol[k])
  for (k in 1:(K-1))
    lines(1:N,apply(v[,k,],2,mean),col=aveCol[k],lwd=4)
  legend("topleft",legendText,col=aveCol,lwd=2,bg='white')
}

plot3=function(size,names){
  fillCol=c(rgb(.5,.5,0,1),rgb(0,.5,.5,1))
  K=dim(size)[2]
  N=dim(size)[3]
  v=array(dim=c(dim(size)[1],K-1,N))
  for (k in 2:K) v[,k-1,]=size[,1,]-size[,k,]
  range=c(min(v),max(v))
  if (K==2){
    a=table(v[,1,N])/length(v[,1,N])
    y=as.integer(names(a))
    plot(c(-.25,.25),c(min(y),max(y)),axes=F,ylab="Difference",
         xlab="",typ='n')
    abline(h=0,col='red')
    points(x=rep(0,length(a)),y=y,pch=19,cex=a*10,col=fillCol[1])
    axis(1,at=0,lab=names)
    }
  else{
    a=table(v[,1,N])/length(v[,1,N])
    b=table(v[,2,N])/length(v[,2,N])
    ya=as.integer(names(a))
    yb=as.integer(names(b))
    y=union(ya,yb)
    plot(c(-.5,.5),c(min(y),max(y)),axes=F,ylab="Difference",
         xlab="",typ='n')
    abline(h=0,col='red')
    points(x=rep(-.25,length(a)),y=ya,pch=19,cex=a*10,col=fillCol[1])
    points(x=rep(.25,length(b)),y=yb,pch=19,cex=b*10,col=fillCol[2])
    axis(1,at=c(-.25,.25),lab=names)
  }
  box()
  axis(2)
}



makeFig=function(sizeE1,sizeE2,sizeE3,sizeE4){
  png('datPlot.png',width=1200,height=1200,pointsize=20)
  par(mfrow=c(4,3),mar=c(4,4,2,2),mgp=c(2,1,0))

  plot1(sizeE1,c("Roman","Asomtavruli","Widgets"),1)
  plot2(sizeE1,c("Roman-Asomtavruli","Roman-Widgets"))
  plot3(sizeE1,c("Roman-Asomtavruli","Roman-Widgets"))

  plot1(sizeE2,c("Roman","Asomtavruli","Lines"),2)
  plot2(sizeE2,c("Roman-Asomtavruli","Roman-Lines"))
  plot3(sizeE2,c("Roman-Asomtavruli","Roman-Lines"))

  plot1(sizeE3,c("Roman","Asomtavruli"),3)
  plot2(sizeE3,c("Roman-Asomtavruli"))
  plot3(sizeE3,c("Roman-Asomtavruli"))

  plot1(sizeE4,c("Roman","Asomtavruli","Lines"),4)
  plot2(sizeE4,c("Roman-Asomtavruli","Roman-Lines"))
  plot3(sizeE4,c("Roman-Asomtavruli","Roman-Lines"))
  dev.off()}

bf=function(size){
  K=dim(size)[2]
  N=dim(size)[3]
  v=array(dim=c(dim(size)[1],K-1))
  bf=rep(0,K-1)
  for (k in 2:K){
    v[,k-1]=size[,1,N]-size[,k,N]
    bf[k-1]=ttestBF(v[,k-1])}
  return(bf)
}
