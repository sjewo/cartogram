library(cartogram)
library(maptools)
library(rgeos)

context("Testing algorithms")

data(wrld_simpl)

afr <- wrld_simpl[wrld_simpl$REGION==2,]
afr <- spTransform(afr, CRS("+init=epsg:3395"))
afr <- afr[afr$POP2005>0,]

area <- c(1753760971468.07, 855242566445.558, 204337093948.932, 226523528763.156, 2076503387171.31, 154407691766.395, 600924323021.175, 664581895712.136, 1198090163.46008, 341283912356.329, 1868095177.87217, 40383893377.7221, 1853037665993.41, 16501277608.724, 142208057973.834, 1839507795660.51, 11429712776.8728, 99814653500.5888, 202602497515.944, 282606914499.876, 475240123334.742, 972813289675.399, 115621711677.856, 855012835434.764, 665390878091.788, 536008769486.178, 755322640468.938, 2003536703.95958, 322395068613.557, 858285292564.179, 235040502458.512, 470891532375.029, 2702103418092.9, 26642305518.6588, 5669949098.75414, 92588825262.4727, 333615709.614382, 1614554640100.5, 53826442476.7646, 215951716479.588, 118439033496.638, 133672180169.872, 533268895019.685, 1836012213237.49, 117381008316.694, 838731620.414395, 264745424146.373, 1198851866222.3, 601825145491.528, 196983587661.161, 396835048493.781, 28522780722.8323, 585526589601.891, 427118740719.371, 227889639.754759, 90805906062.3921)
names(area) <- c( "DZA", "AGO", "BEN", "COG", "COD", "BDI", "CMR", "TCD", "COM", "CAF", "CPV", "DJI", "EGY", "GNQ", "ERI", "ETH", "GMB", "GAB", "GHA", "GIN", "CIV", "KEN", "LBR", "LBY", "MDG", "MLI", "MAR", "MUS", "MRT", "MOZ", "MWI", "NER", "NGA", "GNB", "REU", "RWA", "SYC", "ZAF", "LSO", "BWA", "SEN", "SLE", "SOM", "SDN", "TGO", "STP", "TUN", "TZA", "UGA", "BFA", "NAM", "SWZ", "ZMB", "ZWE", "SHN", "ESH")

test_that("Check area against reference", {
            registerDoSEQ()
            afrc_seriell <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)
            expect_true(all.equal(area, rgeos::gArea(afrc_seriell, byid=T)))
})

test_that("Check area against reference (doParallel backend)", {
            skip_on_cran()
            # Windows
            library(doParallel)
            cl<-makeCluster(3)
            registerDoParallel(cl)

            afrc_parallelDoParallel <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)
            stopCluster(cl)

            expect_true(all.equal(area, rgeos::gArea(afrc_parallelDoParallel, byid=T)))

            detach("package:doParallel", unload=TRUE)
})


test_that("Check area against reference (doMC backend)", {
            skip_on_cran()
            skip_on_os("windows")

            # Linux and Mac
            library(doMC)
            registerDoMC(3)
            afrc_parallelDoMC <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)

            expect_true(all.equal(area, rgeos::gArea(afrc_parallelDoMC, byid=T)))
            detach("package:doMC", unload=TRUE)

})
