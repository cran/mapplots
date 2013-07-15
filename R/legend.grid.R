legend.grid <-
function(x,y=NULL,breaks,col,digits=2,suffix='',type=1,pch=15,pt.cex=2.5,
                         bg='lightblue',...){
  ncol <- length(breaks)-1
  if(missing(col)) col=c('white',heat.colors(ncol-1)[(ncol-1):1])
  min <- signif(breaks[(ncol):1],digits)
  mid <- signif((breaks[(ncol):1]+breaks[(ncol + 1):2])/2, digits)
  max <- signif(breaks[(ncol+1):2],digits)
  if (type==1) legend <- paste(mid,suffix,sep='')
  if (type==2) legend <- paste(min,' - ',max,suffix,sep='')
  legend(x,y,legend=legend,col=col[ncol:1],pch=pch,pt.cex=pt.cex,bg=bg,...)
  }
