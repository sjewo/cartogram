# make i and k global variables so cran-check will not warn about no visible bindings
globalVariables(c('i','k'))

#' Calculate cartogram boundaries
#'
#' Construct a continuous area cartogram by a rubber sheet distortion algorithm (Dougenik et al. 1985)
#'
#' @param shp SpatialPolygonDataFrame.
#' @param weight Name of the weighting variable in shp.
#' @param itermax Maximum iterations for the cartogram transformation, if maxSizeError ist not reached.
#' @param maxSizeError Stop if meanSizeError is smaller than maxSizeError.
#' @param prepare Weighting values are adjusted to reach convergence much earlier. Possible methods are 
#' "adjust", adjust values to restrict the mass vector to the quantiles defined by threshold and 1-threshold (default),
#' "remove", remove features with values lower than quantile at threshold,
#' "none", don't adjust weighting values.
#' @param threshold Define threshold for data preperation. 
#' @return SpatialPolygonDataFrame with distorted polygon boundaries.
#' @export
#' @import sp rgeos foreach
#' @importFrom maptools checkPolygonsHoles
#' @importFrom utils globalVariables 
#' @importFrom stats quantile
#' @examples
#' 
#' library(maptools)
#' library(cartogram)
#' library(rgdal)
#' data(wrld_simpl)
#' 
#' afr <- spTransform(wrld_simpl[wrld_simpl$REGION==2 & wrld_simpl$POP2005 > 0,], 
#'                     CRS("+init=epsg:3395"))
#' par(mfcol=c(1,2))
#' plot(afr)
#' plot(cartogram(afr, "POP2005", 3))
#' 
#' \dontrun{
#' # do it parallel
#' library(doParallel)
#' 
#' #change to your number of CPU cores
#' cl<-makeCluster(3)
#' registerDoParallel(cl)
#' 
# cartogram will use all 3 cores
#' afrc_parallelDoParallel <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)
#' 
#' all.equal(rgeos::gArea(afrc, byid=T), rgeos::gArea(afrc_parallelDoParallel, byid=T))
#' 
#' stopCluster(cl)
#' }
#' @references Dougenik, Chrisman, Niemeyer (1985): An Algorithm To Construct Continuous Area Cartograms. In: Professional Geographer, 37(1), 75-81.
cartogram <- function(shp, weight, itermax=15, maxSizeError=1.0001, 
                      prepare="adjust", threshold=0.05) {

  # sum up total value
  value <- shp@data[,weight]
  
  # prepare data
  switch(prepare, 
         # remove missing and values below threshold
         "remove"={
           #maxValue <- quantile(value, probs=(1-threshold), na.rm=T)
           minValue <- quantile(value, probs=threshold, na.rm=T)
           shp <- shp[value > minValue | is.na(value),]
           value <- value[value > minValue | is.na(value)]
         },
         # Adjust ratio
         "adjust"={
           if(any(is.na(value)))
             stop("NA not allowed in weight vector")
           
           valueTotal <- sum(value, na.rm=T)
           
           # area for polygons and total area
           area <- gArea(shp, byid=T)
           area[area <0 ] <- 0
           areaTotal <- gArea(shp)
           
           # prepare force field calculations
           desired <- areaTotal*value/valueTotal
           ratio <- desired/area
           maxRatio <- quantile(ratio, probs=(1-threshold))
           minRatio <- quantile(ratio, probs=threshold)
           
           value[ratio > maxRatio] <- (maxRatio * area[ratio > maxRatio] * valueTotal)/areaTotal
           value[ratio < minRatio] <- (minRatio * area[ratio < minRatio] * valueTotal)/areaTotal
         },
         "none"={})
  

  
  valueTotal <- sum(value, na.rm=T)
  
  # keep row names
  rown <- rownames(shp@data)
  # identify multi polygons
  multipol <- sapply(seq_len(nrow(shp)), function(i) length(shp@polygons[[i]]@Polygons))
  ## save boundaries in lists  
  tmpcoords <- lapply(seq_len(nrow(shp)), function(i) lapply(seq_len(multipol[i]), function(j) shp@polygons[[i]]@Polygons[[j]]@coords))

  # set intial value for meanSizeError
  meanSizeError <- 100

  shp.iter <- shp

  # iterate until itermax is reached
  for(z in 1:itermax) {
    # break if mean Sizer Error is less than maxSizeError
    if(meanSizeError < maxSizeError) break

    # polygon centroids (centroids for multipart polygons)
    centroids <- coordinates(rgeos::gCentroid(shp.iter, byid=T))

    # area for polygons and total area
    area <- rgeos::gArea(shp.iter, byid=T)
    area[area <0 ] <- 0
    areaTotal <- rgeos::gArea(shp.iter)

    # prepare force field calculations
    desired <- areaTotal*value/valueTotal
    desired[desired==0] <- 0.01 # set minimum size to prevent inf values size Error
    radius <- sqrt(area/pi)
    mass <- sqrt(desired/pi) - sqrt(area/pi)
    
    sizeError <- apply(cbind(area,desired), 1, max)/apply(cbind(area,desired), 1, min)
    meanSizeError <- mean(sizeError, na.rm=T)
    forceReductionFactor <- 1/(1+meanSizeError)

    message(paste0("Mean size error for iteration ", z ,": ", meanSizeError))

    tmpcoords <- foreach(i=seq_along(shp.iter)) %:%
      foreach(k=seq_len(multipol[i])) %dopar% {

        newpts <- shp.iter@polygons[[i]]@Polygons[[k]]@coords

        #distance 
        for(j in  seq_len(nrow(centroids))) {

          # distance to centroid j        
          distance <- spDistsN1(newpts, centroids[j,])

          # calculate force vector        
          Fij <- mass[j] * radius[j] / distance
          Fbij <- mass[j] * (distance/radius[j])^2 * (4 - 3*(distance/radius[j]))
          Fij[distance <= radius[j]] <- Fbij[distance <= radius[j]]
          Fij <- Fij * forceReductionFactor / distance

          # calculate new border coordinates
          #newpts <- newpts + cbind(Fij,Fij) * t(apply(newpts, 1, function(x) { x - centroids[j,]}))
          newpts <- newpts + cbind(Fij,Fij) * (newpts - centroids[rep(j,nrow(newpts)),])    
        }

        # save final coordinates from this iteration to coordinate list
        newpts
      }
    
    # construct sp-object for area and centroid calculation
    shp.iter <- SpatialPolygons(lapply(seq_along(tmpcoords), function(x) maptools::checkPolygonsHoles(Polygons(lapply(tmpcoords[[x]], Polygon), rown[x]))),  proj4string = CRS(proj4string(shp)))

  }

  # construct final shape  
  shp.carto <- SpatialPolygons(lapply(seq_along(tmpcoords), function(x) maptools::checkPolygonsHoles(Polygons(lapply(tmpcoords[[x]], Polygon),rown[x]))), proj4string = CRS(proj4string(shp)))

  # add data
  shp.cartodf <- SpatialPolygonsDataFrame(shp.carto, shp@data)
  return(shp.cartodf)
}

