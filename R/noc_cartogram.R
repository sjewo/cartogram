#' @title Calculate Non-Overlapping Circles Cartogram
#' @description Construct a cartogram which represents each geographic region 
#' as non-overlapping circles (Dorling 1996).
#' @name noc_cartogram
#' @param shp SpatialPolygonsDataFrame, SpatialPointsDataFrame or an sf object
#' @param weight Name of the weighting variable in shp
#' @param k Share of the shp bounding box filled by the larger circle
#' @param m_weight Circles' movements weights. An optional vector of numeric weights 
#' (0 to 1 inclusive) to 
#' apply to the distance each circle moves during pair-repulsion. A weight of 0 
#' prevents any movement. A weight of 1 gives the default movement distance. A 
#' single value can be supplied for uniform weights. A vector with length less 
#' than the number of circles will be silently extended by repeating the final 
#' value. Any values outside the range [0, 1] will be clamped to 0 or 1.
#' @param itermax Maximum iterations for the cartogram transformation. 
#' @return Non overlaping proportional circles of the same class as shp.
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
#' afr_carto <- noc_cartogram(afr, "POP2005")
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
#' afr_sf_carto <- noc_cartogram(afr_sf, "POP2005")
#' 
#' # Plot 
#' par(mfcol=c(1,3))
#' plot(afr, main="original")
#' plot(afr_carto, main="distorted (sp)")
#' plot(st_geometry(afr_sf_carto), main="distorted (sf)")

noc_cartogram <- function(shp, weight, k = 5, m_weight = 1, itermax= 1000) {
  UseMethod("noc_cartogram")
}

#' @rdname noc_cartogram
#' @export
noc_cartogram.sf <- function(shp, weight, k = 5, m_weight = 1, itermax= 1000){
  # proj or unproj
  if (sf::st_is_longlat(shp)) {
    warning("Using an unprojected map. Converting to equal area is recommended", call. = FALSE)
  }
  # no 0 values
  shp <- shp[shp[[weight]]>0,]
  # data prep
  dat.init <- data.frame(sf::st_coordinates(sf::st_centroid(sf::st_geometry(shp))),
                         v = shp[[weight]])
  surf <- (max(dat.init[,1]) - min(dat.init[,1])) *  (max(dat.init[,2]) - min(dat.init[,2]))
  dat.init$v <- dat.init$v * (surf * k / 100) / max(dat.init$v)
  # circles layout and radiuses
  res <- packcircles::circleRepelLayout(x = dat.init, xysizecols = 1:3,
                                        wrap = FALSE, sizetype = "area",
                                        maxiter = itermax, weights = m_weight)
  # sf object creation
  . <- sf::st_buffer(sf::st_as_sf(res$layout,
                                  coords =c('x', 'y'),
                                  crs = sf::st_crs(shp)),
                     dist = res$layout$radius)
  sf::st_geometry(shp) <- sf::st_geometry(.)
  return(shp) 
}

#' @rdname noc_cartogram
#' @export
noc_cartogram.SpatialPolygonsDataFrame <- function(shp, weight, k = 5, m_weight = 1, itermax = 1000){
  as(noc_cartogram.sf(st_as_sf(shp), weight = weight, k = k, m_weight = m_weight, itermax = itermax), "Spatial")
}
