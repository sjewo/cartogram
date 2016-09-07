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
#' @param shp SpatialPolygonDataFrame
#' @param weight Name of the weighting variable in shp
#' @param itermax Maximum iterations for the cartogram transformation, if maxSizeError ist not reached
#' @param maxSizeError Stop if meanSizeError is smaller than maxSizeError
#' @return SpatialPolygonDataFrame with distorted polygon boundaries
#' @export
#' @import sp
#' @import rgeos
#' @importFrom methods is slot
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
#' @references Dougenik, Chrisman, Niemeyer (1985): An Algorithm To Construct Continuous Area Cartograms. In: Professional Geographer, 37(1), 75-81.
cartogram <- function(shp, weight, itermax=15, maxSizeError=1.0001) {

  # keep row names
  rown <- rownames(shp@data)
  # identify multi polygons
  multipol <- sapply(seq_len(nrow(shp)), function(i) length(shp@polygons[[i]]@Polygons))
  ## save boundaries in lists  
  tmpcoords <- lapply(seq_len(nrow(shp)), function(i) lapply(seq_len(multipol[i]), function(j) shp@polygons[[i]]@Polygons[[j]]@coords))

  # sum up total value
  value <- shp@data[,weight]
  
  if(any(is.na(value)))
     stop("NA not allowed in weight vector")

  valueTotal <- sum(value, na.rm=T)

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
        #newpts <- newpts + cbind(Fij,Fij) * t(apply(newpts, 1, function(x) { x - centroids[j,]}))
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


# Code from maptools 0.8-39
# Copyright: Roger Bivand and Edzer Pebesma
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


