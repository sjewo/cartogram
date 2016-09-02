# cartogram
[![Build Status](https://travis-ci.org/sjewo/cartogram.svg?branch=master)](https://travis-ci.org/sjewo/cartogram)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/cartogram)](https://cran.r-project.org/package=cartogram)

## Create Cartograms with R

Construct a continuous area cartogram by a rubber sheet distortion algorithm (Dougenik et al. 1985).

## News
* [0.0.2] parallelization of cartogram algorithm


## Example
```
library(cartogram)
library(tmap)
library(maptools)

data(wrld_simpl)

afr <- wrld_simpl[wrld_simpl$REGION==2,]
afr <- spTransform(afr, CRS("+init=epsg:3395"))

# construct cartogram
afrc <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)

# plot it
tm_shape(afrc) + tm_fill("POP2005", style="jenks") + tm_borders() + tm_layout(frame=F)

# do it parallel
library(doParallel)

#change to your number of CPU cores
cl<-makeCluster(3)
registerDoParallel(cl)

# cartogram will use all 3 cores
afrc_parallelDoParallel <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)

all.equal(rgeos::gArea(afrc, byid=T), rgeos::gArea(afrc_parallelDoParallel, byid=T))

stopCluster(cl)
```

![Cartogram](http://www.methoden.ruhr-uni-bochum.de/files/cartogram.jpg)

## References
* Dougenik, Chrisman, Niemeyer (1985): An Algorithm To Construct Continuous Area Cartograms. In: Professional Geographer, 37(1), 75-81.

