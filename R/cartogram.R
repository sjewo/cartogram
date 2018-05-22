# Copyright (C) 2016 Sebastian Jeworutzki
# Copyright (C) of 'checkPolygonsGEOS' from package maptools Roger Bivand and Edzer Pebesma

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

#' Calculate cartogram boundaries
#'
#' Construct a continuous area cartogram by a rubber sheet distortion algorithm (Dougenik et al. 1985)
#'
#' @param shp SpatialPolygonDataFrame or an sf object
#' @param weight Name of the weighting variable in shp
#' @param itermax Maximum iterations for the cartogram transformation, if maxSizeError ist not reached
#' @param maxSizeError Stop if meanSizeError is smaller than maxSizeError
#' @param prepare Weighting values are adjusted to reach convergence much earlier. Possible methods are 
#' "adjust", adjust values to restrict the mass vector to the quantiles defined by threshold and 1-threshold (default),
#' "remove", remove features with values lower than quantile at threshold,
#' "none", don't adjust weighting values
#' @param threshold Define threshold for data preparation
#' @return An object of the same class as shp
#' @export
#' @import sp
#' @import rgeos 
#' @importFrom methods is slot
#' @importFrom stats quantile
#' @importFrom sf st_area st_as_sf st_centroid st_coordinates st_distance st_geometry st_geometry<- st_point st_crs st_crs<-
#' @examples
#' 
#' library(maptools)
#' library(cartogram)
#' library(rgdal)
#' data(wrld_simpl)
#' 
#' # Remove uninhabited regions
#' afr <- spTransform(wrld_simpl[wrld_simpl$REGION==2 & wrld_simpl$POP2005 > 0,], 
#'                     CRS("+init=epsg:3395"))
#'
#' # Create cartogram
#' afr_carto <- cartogram(afr, "POP2005", 3)
#'
#' # Plot 
#' par(mfcol=c(1,2))
#' plot(afr, main="original")
#' plot(afr_carto, main="distorted (sp)")
#'
#' # Same with sf objects
#' library(sf)
#'
#' afr_sf = st_as_sf(afr)
#'
#' afr_sf_carto <- cartogram(afr_sf, "POP2005", 3)
#'
#' # Plot 
#' par(mfcol=c(1,3))
#' plot(afr, main="original")
#' plot(afr_carto, main="distorted (sp)")
#' plot(st_geometry(afr_sf_carto), main="distorted (sf)")
#' 
#' @references Dougenik, Chrisman, Niemeyer (1985): An Algorithm To Construct Continuous Area Cartograms. In: Professional Geographer, 37(1), 75-81.
cartogram <- function(shp, weight, itermax=15, maxSizeError=1.0001,
                      prepare="adjust", threshold=0.05) {
    UseMethod("cartogram")
}

#' @rdname cartogram
#' @export
cartogram.SpatialPolygonsDataFrame <- function(shp, weight, itermax=15, maxSizeError=1.0001,
                      prepare="adjust", threshold=0.05) {

  # prepare data
  value <- shp@data[,weight]

  switch(prepare, 
         # remove missing and values below threshold
         "remove"={
           #maxValue <- quantile(value, probs=(1-threshold), na.rm=T)
           minValue <- quantile(value, probs=threshold, na.rm=T)
           shp <- shp[value > minValue | !is.na(value),]
           value <- value[value > minValue | !is.na(value)]
         },
         # Adjust ratio
         "adjust"={
           if(any(is.na(value))) {
             warning("NA not allowed in weight vector. Features will be removed from Shape.")
             shp <- shp[!is.na(value),]
             value <- value[!is.na(value)]
           }

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

           # adjust values 
           value[ratio > maxRatio] <- (maxRatio * area[ratio > maxRatio] * valueTotal)/areaTotal
           value[ratio < minRatio] <- (minRatio * area[ratio < minRatio] * valueTotal)/areaTotal
         },
         "none"={})

  # sum up total value
  valueTotal <- sum(value, na.rm=T)

  # keep row names
  rown <- rownames(shp@data)
  # identify multi polygons
  multipol <- sapply(seq_len(nrow(shp)), function(i) length(shp@polygons[[i]]@Polygons))
  ## save boundaries in lists  
  tmpcoords <- lapply(seq_len(nrow(shp)), function(i) lapply(seq_len(multipol[i]), function(j) shp@polygons[[i]]@Polygons[[j]]@coords))

  # set meanSizeError
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

    for(i in seq_along(shp.iter)) {
      for(k in seq_len(multipol[i])) {

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
          newpts <- newpts + cbind(Fij,Fij) * (newpts - centroids[rep(j,nrow(newpts)),])    
        }

        # save final coordinates from this iteration to coordinate list
        tmpcoords[[i]][[k]] <- newpts
      }
    }
    
    # construct sp-object for area and centroid calculation
    shp.iter <- SpatialPolygons(lapply(seq_along(tmpcoords), function(x) checkPolygonsGEOS(Polygons(lapply(tmpcoords[[x]], Polygon), rown[x]))),  proj4string = CRS(proj4string(shp)))

  }

  # construct final shape  
  shp.carto <- SpatialPolygons(lapply(seq_along(tmpcoords), function(x) checkPolygonsGEOS(Polygons(lapply(tmpcoords[[x]], Polygon),rown[x]))), proj4string = CRS(proj4string(shp)))

  # add data
  shp.cartodf <- SpatialPolygonsDataFrame(shp.carto, shp@data)
  return(shp.cartodf)
}

#' @rdname cartogram
#' @export
cartogram.sf <- function(shp, weight, itermax=15, maxSizeError=1.0001,
                      prepare="adjust", threshold=0.05) {

  # prepare data
  value <- shp[[weight]]

  switch(prepare, 
         # remove missing and values below threshold
         "remove"={
           #maxValue <- quantile(value, probs=(1-threshold), na.rm=T)
           minValue <- quantile(value, probs=threshold, na.rm=T)
           shp <- shp[value > minValue | !is.na(value),]
           value <- value[value > minValue | !is.na(value)]
         },
         # Adjust ratio
         "adjust"={
           if(any(is.na(value))) {
             warning("NA not allowed in weight vector. Features will be removed from Shape.")
             shp <- shp[!is.na(value),]
             value <- value[!is.na(value)]
           }

           valueTotal <- sum(value, na.rm=T)

           # area for polygons and total area
           area <- as.numeric(st_area(shp))
           areaTotal <- sum(area)
           area[area <0 ] <- 0

           # prepare force field calculations
           desired <- areaTotal*value/valueTotal
           ratio <- desired/area
           maxRatio <- quantile(ratio, probs=(1-threshold))
           minRatio <- quantile(ratio, probs=threshold)

           # adjust values 
           value[ratio > maxRatio] <- (maxRatio * area[ratio > maxRatio] * valueTotal)/areaTotal
           value[ratio < minRatio] <- (minRatio * area[ratio < minRatio] * valueTotal)/areaTotal
         },
         "none"={})

  # sum up total value
  valueTotal <- sum(value, na.rm=T)

  # set meanSizeError
  meanSizeError <- 100

  shp.iter <- shp

  # iterate until itermax is reached
  for(z in 1:itermax) {
    # break if mean Sizer Error is less than maxSizeError
    if(meanSizeError < maxSizeError) break

    # polygon centroids (centroids for multipart polygons)
    centroids_sf <- st_centroid(st_geometry(shp.iter))
    st_crs(centroids_sf) <- st_crs(NULL)
    centroids <- do.call(rbind, centroids_sf)

    # area for polygons and total area
    area <- as.numeric(st_area(shp.iter))
    area[area <0 ] <- 0
    areaTotal <- as.numeric(sum(st_area(shp.iter)))

    # prepare force field calculations
    desired <- areaTotal*value/valueTotal
    desired[desired==0] <- 0.01 # set minimum size to prevent inf values size Error
    radius <- sqrt(area/pi)
    mass <- sqrt(desired/pi) - sqrt(area/pi)

    sizeError <- apply(cbind(area,desired), 1, max)/apply(cbind(area,desired), 1, min)
    meanSizeError <- mean(sizeError, na.rm=T)
    forceReductionFactor <- 1/(1+meanSizeError)

    message(paste0("Mean size error for iteration ", z ,": ", meanSizeError))

    for(i in seq_len(nrow(shp.iter))) {
      pts <- st_coordinates(st_geometry(shp.iter)[[i]])
      idx <- unique(pts[, c("L1", "L2", "L3")])

      for(k in seq_len(nrow(idx))) {

        newpts <- pts[pts[,"L1"]==idx[k, "L1"] & pts[, "L2"]==idx[k, "L2"], c("X","Y")]
        newpts_sf <- st_as_sf(data.frame(newpts), coords=c("X","Y"))
        distances <- st_distance(newpts_sf, centroids_sf, by_element=FALSE)

        #distance 
        for(j in  seq_len(nrow(centroids))) {

          distance <- distances[, j]
          
          # calculate force vector        
          Fij <- mass[j] * radius[j] / distance
          Fbij <- mass[j] * (distance/radius[j])^2 * (4 - 3*(distance/radius[j]))
          Fij[distance <= radius[j]] <- Fbij[distance <= radius[j]]
          Fij <- Fij * forceReductionFactor / distance

          # calculate new border coordinates
          newpts <- newpts + cbind(X1=Fij, X2=Fij) * (newpts - centroids[rep(j,nrow(newpts)),])    
        }

        # save final coordinates from this iteration to coordinate list
        st_geometry(shp.iter)[[i]][[idx[k, "L2"]]][[idx[k, "L1"]]] <- newpts
      }
    }
  }

  # 
  return(shp.iter)
}


#' Check polygons
#' 
#' Code from \code{maptools 0.8-39}.
#' Copyright: Roger Bivand and Edzer Pebesma.
#'
#' @param obj Polygons object
#' @param properly use \code{\link[rgeos]{gContainsProperly}} rather than \code{\link[rgeos]{gContains}}
#' @param useSTRtree use \pkg{rgeos} STRtree in checking holes, which is much faster, but uses a lot of memory and does not release it on completion
#' @param force ignore errors from \code{\link[rgeos]{createPolygonsComment}}
#' @import rgeos 
checkPolygonsGEOS <- function(obj, properly=TRUE, force=TRUE, useSTRtree=FALSE) {
  if (!is(obj, "Polygons")) 
    stop("not a Polygons object")
  if (!requireNamespace("rgeos", quietly = TRUE))
    stop("package rgeos required for checkPolygonsGEOS")
  comm <- try(rgeos::createPolygonsComment(obj), silent=TRUE)
  if (class(comm) != "try-error" && !force) {
    comment(obj) <- comm
    return(obj)
  }
  pls <- slot(obj, "Polygons")
  IDs <- slot(obj, "ID")
  n <- length(pls)
  if (n < 1) stop("Polygon list of zero length")
  uniqs <- rep(TRUE, n)
  if (n > 1) {
    if (useSTRtree) tree1 <- rgeos::gUnarySTRtreeQuery(obj)
    SP <- SpatialPolygons(lapply(1:n, function(i) 
                                 Polygons(list(pls[[i]]), ID=i)))
    for (i in 1:(n-1)) {
      if (useSTRtree) {
        if (!is.null(tree1[[i]])) {
          res <- try(rgeos::gEquals(SP[i,], SP[tree1[[i]],], byid=TRUE),
                     silent=TRUE)
          if (class(res) == "try-error") {
            warning("Polygons object ", IDs, ", Polygon ",
                    i, ": ", res)
            next
          }
          if (any(res)) {
            uniqs[as.integer(rownames(res)[res])] <- FALSE
          }
        }
      } else {
        res <- try(rgeos::gEquals(SP[i,], SP[uniqs,], byid=TRUE), silent=TRUE)
        if (class(res) == "try-error") {
          warning("Polygons object ", IDs, ", Polygon ",
                  i, ": ", res)
          next
        }
        res[i] <- FALSE
        if (any(res)) {
          wres <- which(res)
          uniqs[wres[wres > i]] <- FALSE
        }
      }
    }
  }
  if (any(!uniqs)) warning(paste("Duplicate Polygon objects dropped:",
                                 paste(wres, collapse=" ")))
  pls <- pls[uniqs]
  #    IDs <- IDs[uniqs]
  n <- length(pls)
  if (n < 1) stop("Polygon list of zero length")
  if (n == 1) {
    oobj <- Polygons(pls, ID=IDs)
    comment(oobj) <- rgeos::createPolygonsComment(oobj)
    return(oobj)
  }
  areas <- sapply(pls, slot, "area")
  pls <- pls[order(areas, decreasing=TRUE)]
  oholes <- sapply(pls, function(x) slot(x, "hole"))
  holes <- rep(FALSE, n)
  SP <- SpatialPolygons(lapply(1:n, function(i) 
                               Polygons(list(pls[[i]]), ID=i)))
  if (useSTRtree) tree2 <- rgeos::gUnarySTRtreeQuery(SP)
  for (i in 1:(n-1)) {
    if (useSTRtree) {
      if (!is.null(tree2[[i]])) {
        if (properly) res <- rgeos::gContainsProperly(SP[i,], SP[tree2[[i]],],
                                                      byid=TRUE)
      else res <- rgeos::gContains(SP[i,], SP[tree2[[i]],], byid=TRUE)
      } else {
        res <- FALSE
      }
    } else {
      if (properly) res <- rgeos::gContainsProperly(SP[i,], SP[-(1:i),],
                                                    byid=TRUE)
    else res <- rgeos::gContains(SP[i,], SP[-(1:i),], byid=TRUE)
    }
    wres <- which(res)
    if (length(wres) > 0L) {
      nres <- as.integer(rownames(res))
      holes[nres[wres]] <- ! holes[nres[wres]]
    }
  }
  for (i in 1:n) {
    if (oholes[i] != holes[i])
      pls[[i]] <- Polygon(slot(pls[[i]], "coords"), hole=holes[i])
  }
  oobj <- Polygons(pls, ID=IDs)
  comment(oobj) <- rgeos::createPolygonsComment(oobj)
  oobj    
}
# Code from maptools 0.8-39


