#' @title Calculate Non-Overlapping Circles Cartogram
#' @description Construct a cartogram which represents each geographic region 
#' as non-overlapping circles (Dorling 1996).
#' @name cartogram_dorling
#' @param x SpatialPolygonsDataFrame, SpatialPointsDataFrame or an sf object
#' @param weight Name of the weighting variable in x
#' @param k Share of the bounding box of x filled by the larger circle
#' @param m_weight Circles' movements weights. An optional vector of numeric weights 
#' (0 to 1 inclusive) to 
#' apply to the distance each circle moves during pair-repulsion. A weight of 0 
#' prevents any movement. A weight of 1 gives the default movement distance. A 
#' single value can be supplied for uniform weights. A vector with length less 
#' than the number of circles will be silently extended by repeating the final 
#' value. Any values outside the range [0, 1] will be clamped to 0 or 1.
#' @param itermax Maximum iterations for the cartogram transformation. 
#' @return Non overlaping proportional circles of the same class as x.
#' @export
#' @references Dorling, D. (1996). Area Cartograms: Their Use and Creation. In Concepts and Techniques in Modern Geography (CATMOG), 59.
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
#' afr_carto <- cartogram_dorling(afr, "POP2005")
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
#' afr_sf_carto <- cartogram_dorling(afr_sf, "POP2005")
#' 
#' # Plot 
#' par(mfcol=c(1,3))
#' plot(afr, main="original")
#' plot(afr_carto, main="distorted (sp)")
#' plot(st_geometry(afr_sf_carto), main="distorted (sf)")
cartogram_dorling <- function(x, weight, k = 5, m_weight = 1, itermax= 1000) {
  UseMethod("cartogram_dorling")
}

#' @rdname cartogram_dorling
#' @export
cartogram_dorling.sf <- function(x, weight, k = 5, m_weight = 1, itermax= 1000){
  # proj or unproj
  if (sf::st_is_longlat(x)) {
    warning("Using an unprojected map. Converting to equal area is recommended", call. = FALSE)
  }
  # no 0 values
  x <- x[x[[weight]]>0,]
  # data prep
  dat.init <- data.frame(sf::st_coordinates(sf::st_centroid(sf::st_geometry(x))),
                         v = x[[weight]])
  surf <- (max(dat.init[,1]) - min(dat.init[,1])) *  (max(dat.init[,2]) - min(dat.init[,2]))
  dat.init$v <- dat.init$v * (surf * k / 100) / max(dat.init$v)
  # circles layout and radiuses
  res <- packcircles::circleRepelLayout(x = dat.init, xysizecols = 1:3,
                                        wrap = FALSE, sizetype = "area",
                                        maxiter = itermax, weights = m_weight)
  # sf object creation
  . <- sf::st_buffer(sf::st_as_sf(res$layout,
                                  coords =c('x', 'y'),
                                  crs = sf::st_crs(x)),
                     dist = res$layout$radius)
  sf::st_geometry(x) <- sf::st_geometry(.)
  return(x) 
}

#' @rdname cartogram_dorling
#' @export
cartogram_dorling.SpatialPolygonsDataFrame <- function(x, weight, k = 5, m_weight = 1, itermax = 1000){
  as(cartogram_dorling.sf(st_as_sf(x), weight = weight, k = k, m_weight = m_weight, itermax = itermax), "Spatial")
}
