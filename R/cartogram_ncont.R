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
#' @return An object of the same class as x with resized polygon boundaries
#' @export
#' @importFrom methods is slot as
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
#'nc_utm_carto <- cartogram_ncont(nc_utm, weight = "BIR74")
#'
#'# Plot 
#'par(mfrow=c(2,1))
#'plot(nc[,"BIR74"], main="original", key.pos = NULL, reset = FALSE)
#'plot(st_geometry(nc_utm), main="distorted", reset = FALSE)
#'plot(nc_utm_carto[,"BIR74"], add =TRUE)
#'
#' @references Olson, J. M. (1976). Noncontiguous Area Cartograms. In The Professional Geographer, 28(4), 371-380.
cartogram_ncont <- function(x, weight, k = 1, inplace = TRUE){
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
cartogram_ncont.SpatialPolygonsDataFrame <- function(x, weight, k = 1, inplace = TRUE){
  as(cartogram_ncont.sf(sf::st_as_sf(x), weight, k = k, inplace = inplace), 'Spatial')
}


#' @rdname cartogram_ncont
#' @importFrom sf st_geometry st_area st_buffer st_is_longlat
#' @export
cartogram_ncont.sf <- function(x, weight, k = 1, inplace = TRUE){
  
  if (isTRUE(sf::st_is_longlat(x))) {
    stop('Using an unprojected map. This function does not give correct centroids and distances for longitude/latitude data:\nUse "st_transform()" to transform coordinates to another projection.', call. = F)
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
  for(i in 1:n){
    sf::st_geometry(spdf)[i] <- rescalePoly.sf(spdf[i, ], 
                                         inplace = inplace, 
                                         r = spdf[i,]$r)
  } 
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
