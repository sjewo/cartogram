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
#' @param shp SpatialPolygonDataFrame or an sf object
#' @param weight Name of the weighting variable in shp
#' @param k Factor expansion for the unit with the greater value
#' @param inplace If TRUE, each polygon is modified in its original place, 
#' if FALSE multi-polygons are centered on their initial centroid
#' @return An object of the same class as shp with resized polygon boundaries
#' @export
#' @import sp
#' @import rgeos
#' @importFrom methods is slot as
#' @examples
#' library(maptools)
#' library(cartogram)
#' library(rgdal)
#' data(wrld_simpl)
#' 
#' # Remove uninhabited regions
#' afr <- spTransform(wrld_simpl[wrld_simpl$REGION==2 & wrld_simpl$POP2005 > 0,],
#'                    CRS("+init=epsg:3395"))
#'
#' # Create cartogram
#' afr_nc <- nc_cartogram(afr, "POP2005")
#'
#' # Plot
#' plot(afr)
#' plot(afr_nc, add = TRUE, col = 'red')
#'
#' # Same with sf objects
#' library(sf)
#'
#' afr_sf = st_as_sf(afr)
#'
#' afr_sf_nc <- nc_cartogram(afr_sf, "POP2005")
#'
#' plot(st_geometry(afr_sf))
#' plot(st_geometry(afr_sf_nc), add = TRUE, col = 'red')
#'
#' @references Olson, J. M. (1976), Noncontiguous Area Cartograms. The Professional Geographer, 28: 371–380. doi:10.1111/j.0033-0124.1976.00371.x
nc_cartogram <- function(shp, weight, k = 1, inplace = T){
  UseMethod("nc_cartogram")
}

#' @rdname nc_cartogram
#' @export
nc_cartogram.SpatialPolygonsDataFrame <- function(shp, weight, k = 1, inplace = T){

  var <- weight
  spdf <- shp[!is.na(shp@data[,var]),]
  
  # size
  surf <- rgeos::gArea(spgeom = spdf, byid = T)
  v <- spdf@data[, var] 
  mv <- max(v)
  ms <- surf[v==mv]
  wArea <- k * v * (ms / mv)
  spdf$r <- sqrt( wArea/ surf)
  n <- nrow(spdf)
  for(i in 1:n){
    x <- rescalePoly(spdf[i, ], inplace = inplace, r = spdf[i,]$r)
    spdf@polygons[[i]] <- checkPolygonsGEOS(x@polygons[[1]])
  } 
  spdf@data <- spdf@data[, 1:(ncol(spdf)-1)]
  rp <- rank(-v, ties.method = "random")
  num <- integer(n)
  for (i in 1:n){
    num[i] <- which(rp == i)
  }
  spdf <- spdf[num, ]
  spdf@plotOrder <- 1:n
  return(spdf)
}

#' @rdname nc_cartogram
#' @export
nc_cartogram.sf <- function(shp, weight, k = 1, inplace = T){
  st_as_sf(nc_cartogram.SpatialPolygonsDataFrame(as(shp, "Spatial"),
                                        weight=weight, k=k, inplace=T))
}

rescalePoly <- function(spdf, inplace = TRUE, r = 1){
  nsubpolygon <- length(spdf@polygons[[1]]@Polygons)
  x <- logical(0)
  for (i in 1:nsubpolygon){
    hole <- spdf@polygons[[1]]@Polygons[[i]]@hole
    x <- c(x,hole)
  }
  if (inplace==FALSE){
    consp <- rgeos::gConvexHull(spdf)
    centerM <- sp::coordinates(consp)
  }
  for (i in 1:nsubpolygon){
    if(inplace){
      if(x[i]==FALSE){
        center <- spdf@polygons[[1]]@Polygons[[i]]@labpt
      }
    }else{
      center <- centerM
    }
    spdf@polygons[[1]]@Polygons[[i]]@coords <- 
      rescale(spdf@polygons[[1]]@Polygons[[i]]@coords, center, r)
  }  
  return(spdf)
}

rescale <- function(vertices, center, r){
  p <- vertices
  p2 <- p
  p2[,1] <- (1 - r) * center[1] + r * p[,1] 
  p2[,2] <- (1 - r) * center[2] + r * p[,2] 
  p2
}




