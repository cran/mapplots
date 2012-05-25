write.grid <-
function(grd,file){
  # this function exports the vms matrix as a csv file
  #
  #   grd       a matrix produced by make.matrix() or make.multimatrix()
  #   file      a filename
  lon=rep(rownames(grd),times=dim(grd)[2])
  lat=rep(colnames(grd),each=dim(grd)[1])
  value=as.numeric(grd)
  write.csv(na.omit(data.frame(lon,lat,value)),file,row.names=F)
  }

