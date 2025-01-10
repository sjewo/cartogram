test_that("cartogram_cont matches expected area", {
  # Load North Carolina SIDS data
  nc = sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
  # transform to NAD83 / UTM zone 16N
  nc_utm <- sf::st_transform(nc, 26916)
  
  # Create cartogram
  nc_utm_carto <- cartogram_cont(nc_utm, weight = "BIR74", itermax = 5)
  cartogram_area <- as.integer((sum(nc_utm_carto |> st_area()))/1000)
  expect_equal(cartogram_area, 118877899, tolerance = 500)
})

test_that("cartogram_cont has crs", {
  # Load North Carolina SIDS data
  nc = sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
  # transform to NAD83 / UTM zone 16N
  nc_utm <- sf::st_transform(nc, 26916)
  
  # Create cartogram
  nc_utm_carto <- cartogram_cont(nc_utm, weight = "BIR74", itermax = 5)
  expect_false(is.na(sf::st_crs(nc_utm_carto)$wkt))
})