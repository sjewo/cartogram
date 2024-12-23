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
#' @param n_cpu Number of cores to use. Defaults to "respect_future_plan". Available options are:
#' * "respect_future_plan" - By default, the function will run on a single core, unless the user specifies the number of cores using \code{\link[future]{plan}}
#' * "auto" - Use all available cores except one
#' * a `numeric` value - Use the specified number of cores
#' @param show_progress A `logical` value. If TRUE, show progress bar. Defaults to TRUE.
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
                      prepare="adjust", threshold=0.05, verbose = FALSE,
                      n_cpu="respect_future_plan", show_progress=TRUE) {
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
                      prepare="adjust", threshold=0.05, verbose = FALSE,
                      n_cpu="respect_future_plan", show_progress=TRUE) {
  as(cartogram_cont.sf(sf::st_as_sf(x), weight, itermax=itermax, maxSizeError=maxSizeError,
                    prepare=prepare, threshold=threshold, verbose=verbose, n_cpu=n_cpu, show_progress=show_progress), 'Spatial')

}

#' @rdname cartogram_cont
#' @importFrom sf st_area st_geometry st_geometry_type st_centroid st_crs st_coordinates st_buffer st_is_longlat
#' @export
cartogram_cont.sf <- function(x, weight, itermax = 15, maxSizeError = 1.0001,
                              prepare = "adjust", threshold = 0.05, verbose = FALSE, n_cpu="respect_future_plan", show_progress=TRUE) {

  if (isTRUE(sf::st_is_longlat(x))) {
    stop('Using an unprojected map. This function does not give correct centroids and distances for longitude/latitude data:\nUse "st_transform()" to transform coordinates to another projection.', call. = F)
  }
  
  # Check n_cpu parameter and set up parallel processing
  if(length(n_cpu) > 1) {
    stop('Invalid value for `n_cpu`. Use "respect_future_plan", "auto", or a numeric value.', call. = FALSE)
  }

  # Determine if we should use multithreading
  if (is.numeric(n_cpu) & n_cpu == 1) {
    multithreadded <- FALSE
  } else if (is.numeric(n_cpu) & n_cpu > 1) {
    cartogram_assert_package(c("future", "future.apply"))
    future::plan(future::multisession, workers = n_cpu)
    multithreadded <- TRUE
  } else if (n_cpu == "auto") {
    cartogram_assert_package("parallelly")
    n_cpu <- max(parallelly::availableCores() - 1, 1)
    if (n_cpu == 1) {
      multithreadded <- FALSE
    } else if (n_cpu > 1) {
      cartogram_assert_package(c("future", "future.apply"))
      future::plan(future::multisession, workers = n_cpu)
      multithreadded <- TRUE
    }
  } else if (n_cpu == "respect_future_plan") {
    if (rlang::is_installed("future")) {
      if (is(future::plan(), "sequential")) {
        multithreadded <- FALSE
      } else {
        multithreadded <- TRUE
      }
    } else {
      multithreadded <- FALSE
    }
  } else {
    stop('Invalid value for `n_cpu`. Use "respect_future_plan", "auto", or a numeric value.', call. = FALSE)
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
    
    # Process polygons either in parallel or sequentially
    if (multithreadded) {
      if (show_progress) {
        cartogram_assert_package("progressr")
        progressr::handlers(global = TRUE)
        progressr::handlers("progress")
        p <- progressr::progressor(along = seq_len(nrow(x.iter)))
      } else {
        p <- function(...) NULL
      }
      
      x.iter_geom <- future.apply::future_lapply(
        seq_len(nrow(x.iter)),
        function(i) {
          if (show_progress) p(sprintf("Processing polygon %d in iteration %d", i, z))
          process_polygon(x.iter_geom[[i]], centroids, mass, radius, forceReductionFactor)
        },
        future.seed = TRUE
      )
    } else {
      if (show_progress) {
        pb <- utils::txtProgressBar(min = 0, max = nrow(x.iter), style = 3)
      }
      
      x.iter_geom <- lapply(
        seq_len(nrow(x.iter)),
        function(i) {
          if (show_progress) utils::setTxtProgressBar(pb, i)
          process_polygon(x.iter_geom[[i]], centroids, mass, radius, forceReductionFactor)
        }
      )
      
      if (show_progress) close(pb)
    }
    
    sf::st_geometry(x.iter) <- do.call(sf::st_sfc, x.iter_geom)
  }
  
  # Clean up parallel workers if they were created within this function
  if ((is.numeric(n_cpu) & n_cpu > 1) || n_cpu == "auto") {
    future::plan(future::sequential)
  }
  
  return(sf::st_buffer(x.iter, 0))
}

#' @keywords internal
process_polygon <- function(poly_geom, centroids, mass, radius, forceReductionFactor) {
  pts <- sf::st_coordinates(poly_geom)
  idx <- unique(pts[, colnames(pts) %in% c("L1", "L2", "L3")])
  
  for (k in seq_len(nrow(idx))) {
    newpts <- pts[pts[, "L1"] == idx[k, "L1"] & pts[, "L2"] == idx[k, "L2"], c("X", "Y")]
    
    distances <- apply(centroids, 1, function(pt) {
      ptm <- matrix(pt, nrow=nrow(newpts), ncol=2, byrow=T)
      sqrt(rowSums((newpts - ptm)^2))
    })
    
    for (j in seq_len(nrow(centroids))) {
      distance <- distances[, j]
      
      Fij <- mass[j] * radius[j] / distance
      Fbij <- mass[j] * (distance / radius[j]) ^ 2 * (4 - 3 * (distance / radius[j]))
      Fij[distance <= radius[j]] <- Fbij[distance <= radius[j]]
      Fij <- Fij * forceReductionFactor / distance
      
      newpts <- newpts + cbind(X1 = Fij, X2 = Fij) * (newpts - centroids[rep(j, nrow(newpts)), ])
    }
    
    if (sf::st_geometry_type(poly_geom) == "POLYGON") {
      poly_geom[[idx[k, "L1"]]] <- newpts
    } else {
      poly_geom[[idx[k, "L2"]]][[idx[k, "L1"]]] <- newpts
    }
  }
  return(poly_geom)
}
