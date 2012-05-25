geoTiff <-
function(grd,file){
  # this function exports the vms matrix as a geotiff file
  require(rgdal)
  minx <- min(as.numeric(rownames(grd)))
  maxx <- max(as.numeric(rownames(grd)))
  miny <- min(as.numeric(colnames(grd)))
  maxy <- max(as.numeric(colnames(grd)))
  byx <- as.numeric(rownames(grd))[2]-as.numeric(rownames(grd))[1]
  byy <- as.numeric(colnames(grd))[2]-as.numeric(colnames(grd))[1]
  grdT <- GridTopology(c(minx, miny), c(byx, byy),dim(grd))
  value <-c(grd[,dim(grd)[2]:1]) #for some reason need to inverse latitude 
  SGDF <- SpatialGridDataFrame(grdT, data=data.frame(value),proj4string = CRS("+proj=latlong +datum=WGS84"))
  SGDF$value <- ifelse(is.na(SGDF$value),-999,SGDF$value)
  writeGDAL(SGDF, file, drivername = "GTiff",mvFlag=-999)
  }

geoTiffRgb <-
function(grd,breaks,col,file){
  require(rgdal)
  ncol <- length(breaks)-1
  if(missing(col)) col <- c('white',heat.colors(ncol-1)[(ncol-1):1])
  grd <- ifelse(grd>max(breaks),max(breaks),grd)
  grd <- ifelse(grd<min(breaks),min(breaks),grd)
  minx <- min(as.numeric(rownames(grd)))
  maxx <- max(as.numeric(rownames(grd)))
  miny <- min(as.numeric(colnames(grd)))
  maxy <- max(as.numeric(colnames(grd)))
  byx <- as.numeric(rownames(grd))[2]-as.numeric(rownames(grd))[1]
  byy <- as.numeric(colnames(grd))[2]-as.numeric(colnames(grd))[1]
  grdT <- GridTopology(c(minx, miny), c(byx, byy),dim(grd))
  value <- c(grd[,dim(grd)[2]:1]) #for some reason need to inverse latitude 
  i <- findInterval(value,breaks,T)
  cols <- col[i]
  rgb <- data.frame(t(col2rgb(cols)))
  rgb[is.na(i),] <- -999
  SGDF <- SpatialGridDataFrame(grdT, data=rgb,proj4string = CRS("+proj=latlong +datum=WGS84"))
  writeGDAL(SGDF, file, drivername = "GTiff",mvFlag=-999)
}
