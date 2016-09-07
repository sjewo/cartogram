# cartogram
[![Build Status](https://travis-ci.org/sjewo/cartogram.svg?branch=master)](https://travis-ci.org/sjewo/cartogram)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/cartogram)](https://cran.r-project.org/package=cartogram)

## Create Cartograms with R

Construct a continuous area cartogram by a rubber sheet distortion algorithm (Dougenik et al. 1985).


## Example
```
library(cartogram)
library(tmap)
library(maptools)

data(wrld_simpl)

afr <- wrld_simpl[wrld_simpl$REGION==2,]
afr <- spTransform(afr, CRS("+init=epsg:3395"))

# keep only countries with population > 0
# automatic data cleaning is planend for next release
# one needs to do this manually at the moment
afr <- afr[afr$POP2005>0,]

# construct cartogram
afrc <- cartogram(afr[afr$POP2005>0,], "POP2005", itermax=5)

# plot it
tm_shape(afrc) + tm_fill("POP2005", style="jenks") + tm_borders() + tm_layout(frame=F)
```

![Cartogram](http://www.methoden.ruhr-uni-bochum.de/files/cartogram.jpg)
```
# Olson cartogram (thanks to @rCarto and @neocarto)
afrnc <- nc_cartogram(afr, "POP2005")

tm_shape(afr) + tm_borders() + 
  tm_shape(afrnc) + tm_fill("POP2005", style="jenks") + tm_borders() + tm_layout(frame=F)
```
![Cartogram Olson](http://www.methoden.ruhr-uni-bochum.de/files/cartogram_nc.png)


## References
* Dougenik, Chrisman, Niemeyer (1985): An Algorithm To Construct Continuous Area Cartograms. In: Professional Geographer, 37(1), 75-81.

