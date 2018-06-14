#' @title Calculate non-overlapping circles Cartogram
#' @description Construct a cartogram which represents each geographic region 
#' as non-overlapping circles (Dorling 1996).
#' @name d_cartogram
#' @param x SpatialPolygonDataFrame or an sf object
#' @param var Name of the weighting variable in shp
#' @param k Share of the shp bounding box filled with the greatest circle
#' @param weights An optional vector of numeric weights (0 to 1 inclusive) to 
#' apply to the distance each circle moves during pair-repulsion. A weight of 0 
#' prevents any movement. A weight of 1 gives the default movement distance. A 
#' single value can be supplied for uniform weights. A vector with length less 
#' than the number of circles will be silently extended by repeating the final 
#' value. Any values outside the range [0, 1] will be clamped to 0 or 1.
#' @param itermax The maximum number of iterations. 
#' @return An object of the same class as shp with non overlaping proportional 
#' circles. 
#' @export
#' @references Daniel, D. (1996). Area Cartograms: Their Use and Creation. In Concepts and Techniques in Modern Geography (CATMOG), 59.
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
#' afr_carto <- d_cartogram(afr, "POP2005")
#' 
#' # Plot 
#' par(mfcol=c(1,2))
#' plot(afr, main="original")
#' 
#' plot(afr, main="distorted (sp)")
#' plot(afr_carto, col = "red", add=TRUE)
#' 
#' # Same with sf objects
#' library(sf)
#' 
#' afr_sf = st_as_sf(afr)
#' 
#' afr_sf_carto <- d_cartogram(afr_sf, "POP2005")
#' 
#' # Plot 
#' par(mfcol=c(1,3))
#' plot(afr, main="original")
#' plot(afr_carto, main="distorted (sp)")
#' plot(st_geometry(afr_sf_carto), main="distorted (sf)")
d_cartogram <- function(x, var, k = 5, weights = 1, itermax= 1000){
  if(methods::is(x, "Spatial")){
    x <- sf::st_as_sf(x)
    sp <- TRUE
  }else{
    sp <- FALSE
  }
  if (sf::st_is_longlat(x)) {
    warning("Using an unprojected map. Converting to equal area is recommended", call. = F)
  }
  x <- x[x[[var]]>0,]
  k = k/100
  dat.init <- data.frame(sf::st_coordinates(sf::st_centroid(sf::st_geometry(x))), v =x[[var]])
  surf <- (max(dat.init[,1])-min(dat.init[,1])) *  (max(dat.init[,2])-min(dat.init[,2]))
  dat.init$v <- dat.init$v * (k * surf) / max(dat.init$v)
  res <- packcircles::circleRepelLayout(x = dat.init, xysizecols = 1:3,
                                        wrap = FALSE, sizetype = "area",
                                        maxiter = itermax, weights = weights)
  . <- sf::st_buffer(sf::st_as_sf(res$layout,
                          coords =c('x', 'y'),
                          crs = sf::st_crs(x)),
                 dist = res$layout$radius)
  
  sf::st_geometry(x) <- sf::st_geometry(.)

  if(sp){
    x <- methods::as(x, "Spatial")
  }
  return(x) 
}
