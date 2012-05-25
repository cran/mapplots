legend.grid <-
function(x,y=NULL,breaks,col,round=0,suffix='',type=1,pch=15,pt.cex=2.5,
                         bg='lightblue',...){
  ncol <- length(breaks)-1
  if(missing(col)) col=c('white',heat.colors(ncol-1)[(ncol-1):1])
  if (type==1) legend <- paste(round(breaks[(ncol+1):2],round),suffix,sep='')
  if (type==2) legend <- paste(round(breaks[(ncol):1],round),' < ',
                               round(breaks[(ncol+1):2],round),suffix,sep='')
  legend(x,y,legend=legend,col=col[ncol:1],pch=pch,pt.cex=pt.cex,bg=bg,...)
  }

