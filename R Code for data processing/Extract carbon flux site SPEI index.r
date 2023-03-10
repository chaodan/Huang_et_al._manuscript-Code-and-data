#Extract carbon flux site SPEI index
library(sp)
library(raster)
library(rgdal)
library(ncdf4)
library(Rcpp)
library(dplyr)
library(spatial)
setwd("H:/netcdf")
IPCC <- read.table("H:\\netcdf\\carbonflux_site.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
lon.pts=IPCC$POINT_X
lat.pts=IPCC$POINT_Y
extract.pts <- cbind(lon.pts,lat.pts)
fname <- "H:/netcdf/spei01.nc"
r <- brick(fname)
r
plot(r)
ext <- extract(r,extract.pts,method="bilinear")
write.csv(ext, "SPEI.csv")