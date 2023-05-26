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

#' @title Calculate Contiguous Cartogram Boundaries
#' @description Construct a continuous area cartogram by a rubber sheet distortion algorithm (Dougenik et al. 1985)
#'
#' @name cartogram_cont
#' @param x a polygon or multiplogyon sf object
#' @param weight Name of the weighting variable in x
#' @param itermax Maximum iterations for the cartogram transformation, if maxSizeError ist not reached
#' @param maxSizeError Stop if meanSizeError is smaller than maxSizeError
#' @param prepare Weighting values are adjusted to reach convergence much earlier. Possible methods are 
#' "adjust", adjust values to restrict the mass vector to the quantiles defined by threshold and 1-threshold (default),
#' "remove", remove features with values lower than quantile at threshold,
#' "none", don't adjust weighting values
#' @param threshold Define threshold for data preparation
#' @param verbose print meanSizeError on each iteration
#' @return An object of the same class as x
#' @export
#' @importFrom methods is slot
#' @importFrom stats quantile
#' @importFrom sf st_area st_as_sf st_centroid st_coordinates st_distance st_geometry st_geometry<- st_point st_crs st_crs<-
#' @examples
#'library(sf)
#'library(cartogram)
#'
# Load North Carolina SIDS data
#'nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#'
#'# transform to NAD83 / UTM zone 16N
#'nc_utm <- st_transform(nc, 26916)
#'
#'# Create cartogram
#'nc_utm_carto <- cartogram_cont(nc_utm, weight = "BIR74", itermax = 5)
#'
#'# Plot 
#'par(mfrow=c(2,1))
#'plot(nc[,"BIR74"], main="original", key.pos = NULL, reset = FALSE)
#'plot(nc_utm_carto[,"BIR74"], main="distorted", key.pos = NULL, reset = FALSE)
#' 
#' @references Dougenik, J. A., Chrisman, N. R., & Niemeyer, D. R. (1985). An Algorithm To Construct Continuous Area Cartograms. In The Professional Geographer, 37(1), 75-81.
cartogram_cont <- function(x, weight, itermax=15, maxSizeError=1.0001,
                      prepare="adjust", threshold=0.05, verbose = FALSE) {
  UseMethod("cartogram_cont")
}

#' @title Calculate Contiguous Cartogram Boundaries
#' @description This function has been renamed: Please use cartogram_cont() instead of cartogram().
#'
#' @export
#' @param shp SpatialPolygonDataFrame or an sf object
#' @inheritDotParams cartogram_cont -x
#' @keywords internal
cartogram <- function(shp, ...) {
  message("\nPlease use cartogram_cont() instead of cartogram().\n")
  cartogram_cont(x=shp, ...)
}

#' @rdname cartogram_cont
#' @importFrom sf st_as_sf
#' @export
cartogram_cont.SpatialPolygonsDataFrame <- function(x, weight, itermax=15, maxSizeError=1.0001,
                      prepare="adjust", threshold=0.05, verbose = FALSE) {
  as(cartogram_cont.sf(sf::st_as_sf(x), weight, itermax=itermax, maxSizeError=maxSizeError,
                    prepare=prepare, threshold=threshold, verbose=verbose), 'Spatial')

}

#' @rdname cartogram_cont
#' @importFrom sf st_area st_geometry st_geometry_type st_centroid st_crs st_coordinates st_buffer st_is_longlat
#' @export
cartogram_cont.sf <- function(x, weight, itermax = 15, maxSizeError = 1.0001,
                              prepare = "adjust", threshold = 0.05, verbose = FALSE) {

  if (isTRUE(sf::st_is_longlat(x))) {
    stop('Using an unprojected map. This function does not give correct centroids and distances for longitude/latitude data:\nUse "st_transform()" to transform coordinates to another projection.', call. = F)
  }
  # prepare data
  value <- x[[weight]]
  
  switch(prepare,
         # remove missing and values below threshold
         "remove" = {
           #maxValue <- quantile(value, probs=(1-threshold), na.rm=T)
           minValue <- quantile(value, probs = threshold, na.rm = T)
           x <- x[value > minValue | !is.na(value), ]
           value <- value[value > minValue | !is.na(value)]
         },
         # Adjust ratio
         "adjust" = {
           if (any(is.na(value))) {
             warning("NA not allowed in weight vector. Features will be removed from Shape.")
             x <- x[!is.na(value), ]
             value <- value[!is.na(value)]
           }
           
           # area for polygons and total area
           area <- as.numeric(st_area(x))
           areaTotal <- sum(area)
           area[area < 0] <- 0
           
           # sum up total value
           valueTotal <- sum(value, na.rm = TRUE)
           
           # prepare force field calculations
           desired <- areaTotal * value / valueTotal
           ratio <- desired / area
           maxRatio <- quantile(ratio, probs = (1 - threshold))
           minRatio <- quantile(ratio, probs = threshold)
           
           # adjust values
           value[ratio > maxRatio] <- (maxRatio * area[ratio > maxRatio] * valueTotal) / areaTotal
           value[ratio < minRatio] <- (minRatio * area[ratio < minRatio] * valueTotal) / areaTotal
         },
         "none" = {
         })
  
  
  # sum up total value
  valueTotal <- sum(value, na.rm = TRUE)
  
  # set meanSizeError
  meanSizeError <- 100
  
  x.iter <- x
  
  # iterate until itermax is reached
  for (z in 1:itermax) {
    # break if mean Sizer Error is less than maxSizeError
    if (meanSizeError < maxSizeError) break
    
    # geometry
    x.iter_geom <- sf::st_geometry(x.iter)
    
    # polygon centroids (centroids for multipart polygons)
    centroids_sf <- sf::st_centroid(x.iter_geom)
    st_crs(centroids_sf) <- sf::st_crs(NULL)
    centroids <- do.call(rbind, centroids_sf)
    
    # area for polygons and total area
    area <- as.numeric(sf::st_area(x.iter))
    areaTotal <- as.numeric(sum(area))
    area[area < 0] <- 0
    
    # prepare force field calculations
    desired <- areaTotal * value / valueTotal
    desired[desired == 0] <- 0.01 # set minimum size to prevent inf values size Error
    radius <- sqrt(area / pi)
    mass <- sqrt(desired / pi) - sqrt(area / pi)
    
    sizeError <- apply(cbind(area, desired), 1, max) / apply(cbind(area, desired), 1, min)
    meanSizeError <- mean(sizeError, na.rm = TRUE)
    forceReductionFactor <- 1 / (1 + meanSizeError)
    
    if(verbose)
      message(paste0("Mean size error for iteration ", z , ": ", meanSizeError))
    
    for (i in seq_len(nrow(x.iter))) {
      pts <- sf::st_coordinates(x.iter_geom[[i]])
      idx <- unique(pts[, colnames(pts) %in% c("L1", "L2", "L3")])

      for (k in seq_len(nrow(idx))) {
        newpts <- pts[pts[, "L1"] == idx[k, "L1"] & pts[, "L2"] == idx[k, "L2"], c("X", "Y")]
        
        distances <- apply(centroids, 1, function(pt) {
          ptm <- matrix(pt, nrow=nrow(newpts), ncol=2, byrow=T)
          sqrt(rowSums((newpts - ptm)^2))
        })
        
        #dold <- spDists(newpts, centroids)
        #all.equal(distances, dold)
        
        #distance
        for (j in  seq_len(nrow(centroids))) {
          distance <- distances[, j]
          
          # calculate force vector
          Fij <- mass[j] * radius[j] / distance
          Fbij <- mass[j] * (distance / radius[j]) ^ 2 * (4 - 3 * (distance / radius[j]))
          Fij[distance <= radius[j]] <- Fbij[distance <= radius[j]]
          Fij <- Fij * forceReductionFactor / distance
          
          # calculate new border coordinates
          newpts <- newpts + cbind(X1 = Fij, X2 = Fij) * (newpts - centroids[rep(j, nrow(newpts)), ])
        }
        
        # save final coordinates from this iteration to coordinate list
        if (sf::st_geometry_type(sf::st_geometry(x.iter)[[i]]) == "POLYGON"){
          sf::st_geometry(x.iter)[[i]][[idx[k, "L1"]]] <- newpts
        } else {
          sf::st_geometry(x.iter)[[i]][[idx[k, "L2"]]][[idx[k, "L1"]]] <- newpts
        }
      }
    }
  }
  
  # return and try to fix self-intersections
  return(sf::st_buffer(x.iter, 0))
}
