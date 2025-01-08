# Copyright (C) 2016 Sebastian Jeworutzki
# Copyright (C) of 'nc_cartogram' Timothee Giraud and Nicolas Lambert
#
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


#' @title Calculate Non-Contiguous Cartogram Boundaries
#' @description Construct a non-contiguous area cartogram (Olson 1976).
#'
#' @name cartogram_ncont
#' @param x a polygon or multiplogyon sf object
#' @param weight Name of the weighting variable in x
#' @param k Factor expansion for the unit with the greater value
#' @param inplace If TRUE, each polygon is modified in its original place, 
#' if FALSE multi-polygons are centered on their initial centroid
#' @param n_cpu Number of cores to use. Defaults to "respect_future_plan". Available options are:
#' * "respect_future_plan" - By default, the function will run on a single core, unless the user specifies the number of cores using \code{\link[future]{plan}} (e.g. `future::plan(future::multisession, workers = 4)`) before running the `cartogram_ncont` function.
#' * "auto" - Use all except available cores (identified with \code{\link[parallelly]{availableCores}}) except 1, to keep the system responsive.
#' * a `numeric` value - Use the specified number of cores. In this case `cartogram_ncont` will use set the specified number of cores internally with `future::plan(future::multisession, workers = n_cpu)` and revert that back by switching the plan back to whichever plan might have been set before by the user. If only 1 core is set, the function will not require `future` and `future.apply` and will run on a single core.
#' @param show_progress A `logical` value. If TRUE, show progress bar. Defaults to TRUE.
#' @return An object of the same class as x with resized polygon boundaries
#' @export
#' @importFrom methods is slot as
#' @examples
#'# ========= Basic example =========
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
#'nc_utm_carto <- cartogram_ncont(nc_utm, weight = "BIR74")
#'
#'# Plot 
#'par(mfrow=c(2,1))
#'plot(nc[,"BIR74"], main="original", key.pos = NULL, reset = FALSE)
#'plot(st_geometry(nc_utm), main="distorted", reset = FALSE)
#'plot(nc_utm_carto[,"BIR74"], add =TRUE)
#'
#' 
#'# ========= Advanced example 1 =========
#'# Faster cartogram using multiple CPU cores
#'# using n_cpu parameter
#'library(sf)
#'library(cartogram)
#'
# Load North Carolina SIDS data
#'nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#'
#'# transform to NAD83 / UTM zone 16N
#'nc_utm <- st_transform(nc, 26916)
#'
#'# Create cartogram using 2 CPU cores on local machine
#'nc_utm_carto <- cartogram_ncont(nc_utm, weight = "BIR74", n_cpu = 2)
#'
#'# Plot 
#'par(mfrow=c(2,1))
#'plot(nc[,"BIR74"], main="original", key.pos = NULL, reset = FALSE)
#'plot(st_geometry(nc_utm), main="distorted", reset = FALSE)
#'plot(nc_utm_carto[,"BIR74"], add =TRUE)
#' 
#' 
#'# ========= Advanced example 2 =========
#'# Faster cartogram using multiple CPU cores
#'# using future package plan
#'library(sf)
#'library(cartogram)
#'library(future)
#'
# Load North Carolina SIDS data
#'nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#'
#'# transform to NAD83 / UTM zone 16N
#'nc_utm <- st_transform(nc, 26916)
#
#'# Set the future plan with 2 CPU local cores
#'# You can of course use any other plans, not just multisession
#'future::plan(future::multisession, workers = 2)
#' 
#'# Create cartogram with multiple CPU cores
#'# The cartogram_cont() will respect the plan set above
#'nc_utm_carto <- cartogram_ncont(nc_utm, weight = "BIR74")
#' 
#'# Shutdown the R processes that were created by the future plan
#'future::plan(future::sequential) 
#' 
#'# Plot 
#'par(mfrow=c(2,1))
#'plot(nc[,"BIR74"], main="original", key.pos = NULL, reset = FALSE)
#'plot(st_geometry(nc_utm), main="distorted", reset = FALSE)
#'plot(nc_utm_carto[,"BIR74"], add =TRUE)
#' 
#' 
#' @references Olson, J. M. (1976). Noncontiguous Area Cartograms. In The Professional Geographer, 28(4), 371-380.
cartogram_ncont <- function(
  x,
  weight,
  k = 1,
  inplace = TRUE,
  n_cpu = "respect_future_plan",
  show_progress = TRUE
){
  UseMethod("cartogram_ncont")
}

#' @title Calculate Non-Contiguous Cartogram Boundaries
#' @description This function has been renamed: Please use cartogram_ncont() instead of nc_cartogram().
#'
#' @export
#' @param shp SpatialPolygonDataFrame or an sf object
#' @inheritDotParams cartogram_ncont -x
#' @keywords internal
nc_cartogram <- function(shp, ...) {
  message("\nPlease use cartogram_ncont() instead of nc_cartogram().\n", call. = F)
  cartogram_ncont(x=shp, ...)
}

#' @rdname cartogram_ncont
#' @importFrom sf st_as_sf
#' @export
cartogram_ncont.SpatialPolygonsDataFrame <- function(
  x,
  weight,
  k = 1,
  inplace = TRUE,
  n_cpu = "respect_future_plan",
  show_progress = TRUE
){
  as(cartogram_ncont.sf(sf::st_as_sf(x), weight, k = k, inplace = inplace, n_cpu = n_cpu, show_progress = show_progress), 'Spatial')
}


#' @rdname cartogram_ncont
#' @importFrom sf st_geometry st_area st_buffer st_is_longlat
#' @export
cartogram_ncont.sf <- function(
  x,
  weight,
  k = 1,
  inplace = TRUE,
  n_cpu = "respect_future_plan",
  show_progress = TRUE
) {
  
  if (isTRUE(sf::st_is_longlat(x))) {
    stop('Using an unprojected map. This function does not give correct centroids and distances for longitude/latitude data:\nUse "st_transform()" to transform coordinates to another projection.', call. = F)
  }

  if(length(n_cpu) > 1) {
    stop('Invalid value for `n_cpu`. Use "respect_future_plan", "auto", or a numeric value.', call. = FALSE)
  }

  if (is.numeric(n_cpu) & n_cpu == 1) {
    multithreadded <- FALSE
  } else if (is.numeric(n_cpu) & n_cpu > 1) {
    cartogram_assert_package(c("future", "future.apply"))
    original_plan <- future::plan(future::multisession, workers = n_cpu)
    on.exit(future::plan(original_plan), add = TRUE)
    multithreadded <- TRUE
  } else if (n_cpu == "auto") {
    cartogram_assert_package("parallelly")
    n_cpu <- max(parallelly::availableCores() - 1, 1)
    if (n_cpu == 1) {
      multithreadded <- FALSE
    } else if (n_cpu > 1) {
      cartogram_assert_package(c("future", "future.apply"))
      original_plan <- future::plan(future::multisession, workers = n_cpu)
      on.exit(future::plan(original_plan), add = TRUE)
      multithreadded <- TRUE
    }
  } else if (n_cpu == "respect_future_plan") {
    if (rlang::is_installed("future")) {
      if ( is(future::plan(), "sequential") ) {
        multithreadded <- FALSE
      } else {
        multithreadded <- TRUE
      }
    } else {
      # if future is not installed, there is definetly no multithreading plan active, so just fallback to single core code
      multithreadded <- FALSE
    }
  } else if (n_cpu != "respect_future_plan") {
    stop('Invalid value for `n_cpu`. Use "respect_future_plan", "auto", or a numeric value.', call. = FALSE)
  }

  var <- weight
  spdf <- x[!is.na(x[, var, drop=T]),]
  
  # size
  surf <- as.numeric(sf::st_area(spdf, by_element=T))
  v <- spdf[, var, drop=T] 
  mv <- max(v)
  ms <- surf[v==mv]
  wArea <- k * v * (ms / mv)
  spdf$r <- as.numeric(sqrt( wArea/ surf))
  spdf$r[spdf$r == 0] <- 0.001 # don't shrink polygons to zero area
  n <- nrow(spdf)
  crs <- st_crs(spdf) # save crs
  
  if (multithreadded == TRUE) {
    cartogram_assert_package(c("future.apply"))
    # handle show_progress
    if (show_progress) {
      cartogram_assert_package("progressr")
      progressr::handlers(global = TRUE)
      progressr::handlers("progress")
      p <- progressr::progressor(along = seq_len(nrow(spdf)))
    } else {
      p <- function(...) NULL # don't show progress
    }
    
    spdf_geometry_list <- future.apply::future_lapply(
      X = seq_len(nrow(spdf)),
      FUN = function(i) {
        p(sprintf("Processing polygon %d", i))
        rescalePoly.sf(
          spdf[i, ],
          r = spdf$r[i],
          inplace = inplace
        )
      },
      future.seed = TRUE
    )
  } else if (multithreadded == FALSE) {
    if (show_progress) {
      pb <- utils::txtProgressBar(min = 0, max = nrow(spdf), style = 3)
    }
    spdf_geometry_list <- lapply(
      X = seq_len(nrow(spdf)),
      FUN = function(i) {
        if (show_progress) {
          utils::setTxtProgressBar(pb, i)
        }
        rescalePoly.sf(
          spdf[i, ],
          r = spdf$r[i],
          inplace = inplace
        )
      }
    )
    
    if (show_progress) {
      close(pb)
    }
  }
  spdf$geometry <- do.call(c, spdf_geometry_list)
  st_crs(spdf) <- crs # restore crs
  spdf$r <- NULL
  sf::st_buffer(spdf, 0)
}

#' @importFrom sf st_geometry st_centroid st_cast st_union
#' @keywords internal
rescalePoly.sf <- function(p, r = 1, inplace = T){
  
  co <- sf::st_geometry(p)
  
  if(inplace) {
    cntr <- sf::st_centroid(co)
    ps <- (co - cntr) * r + cntr
  } else {
    cop <- sf::st_cast(co, "POLYGON")
    cntrd = sf::st_centroid(cop) 
    ps <- sf::st_union((cop - cntrd) * r + cntrd)
  }
  
  return(ps)
}
