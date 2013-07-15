draw.rect <-
function(col='grey',lty=2,...){
  axis(3,-19.5:19.5,c(paste('D',0:9,sep=''),paste('E',0:9,sep=''),paste('F',0:9,sep=''),paste('G',0:9,sep='')),tick=F,line=-0.75)
  axis(4,seq(41.75,65.75,by=0.5),c(12:60),tick=F,las=1,line=-0.75)
  abline(v=-19:20,col=col,lty=lty,...)
  abline(h=seq(41.5,65,by=0.5),col=col,lty=lty,...)
  }
