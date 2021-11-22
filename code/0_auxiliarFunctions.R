validPoints = function(lon, lat, land=FALSE) {
  
  coords = cbind(lon=lon, lat=lat)
  xind = which(complete.cases(coords))
  coords = coords[xind, ]
  
  land = maps::map(database="worldHires", fill=TRUE)
  land = maptools::map2SpatialPolygons(land, 
                                       IDs=sapply(strsplit(land$names, ":"), FUN="[", i=1), 
                                       proj4string=CRS("+proj=longlat"))
  sets = sp::SpatialPoints(cbind(lon=coords[, "lon"], lat=coords[, "lat"]), proj4string=CRS("+proj=longlat"))
  ind = is.na(over(sets, land))
  
  if(isTRUE(land)) ind = !ind
  ind = xind[which(ind)]
  
  return(ind)
}


getArcs <- function(distArea, threshold = 1){
  
  arcs <- which(distArea$arcs[,3] > 0)
  output <- NULL
  for(i in arcs) {
    c <- distArea$arcs[i, 1:2]
    r <- distArea$arcs[i, 3]
    v <- distArea$arcs[i, 4:5]
    theta <- distArea$arcs[i, 6]
    
    angles <- anglesArc(v, theta)
    seqang <- seq(angles[1], angles[2], length = 100)
    
    tempArc <- cbind(c[1] + r * cos(seqang),
                     c[2] + r * sin(seqang))
    
    output <- rbind(output, tempArc)
  }
  
  index <- sqrt(rowSums(abs(output[-1,] - output[-nrow(output),]))^2) > threshold
  output <- cbind(output, c(1, rep(NA, nrow(output) - 1)))
  output[-1, 3] <- output[1, 3] + cumsum(index)
  
  return(output)
}
