#Calculation of global terrestrial ecosystem VPD in the 21st century based on simulation results of IPCC CMIP5 and CMIP6 earth system models
#Extract maximum air temperature from IPCC CMIP5 and CMIP6 Earth system models
library(sp)
library(raster)
library(rgdal)
library(ncdf4)
library(Rcpp)
library(dplyr)
library(spatial)
library(readr)
setwd("H:/netcdf")
shp = readOGR("global_VPD_news.shp")
fname <- "H:/netcdf/tasmax_Amon_CESM2-WACCM_ssp585_r1i1p1f1_gn_225101-229912.nc"
r <- brick(fname)
r
plot(r)
extent(r)
r = rotate(r)
crs(shp)
crs(r)
shp = spTransform(shp, crs(r))
d <- extract(r, shp)
head(d)
Tmax <- d - 273.15
#Extract minimum temperature from IPCC CMIP5 and CMIP6 Earth system models
shp = readOGR("global_VPD_news.shp")
fname <- "H:/netcdf/tasmin_Amon_CESM2-WACCM_ssp585_r1i1p1f1_gn_225101-229912.nc"
r <- brick(fname)
r
plot(r)
extent(r)
r = rotate(r)
crs(shp)
crs(r)
shp = spTransform(shp, crs(r))
d <- extract(r, shp)
head(d)
Tmin <- d - 273.15
#Extract relative humidity from IPCC CMIP5 and CMIP6 Earth system models
shp = readOGR("global_VPD_news.shp")
fname <- "H:/netcdf/hurs_Amon_CESM2-WACCM_ssp585_r1i1p1f1_gn_225101-229912_2.nc"
r <- brick(fname)
r
plot(r)
extent(r)
r = rotate(r)
crs(shp)
crs(r)
shp = spTransform(shp, crs(r))
d <- extract(r, shp)
head(d)
Rh <- d 
#Calculation VPD
esmin <- 0.611 * exp((17.3 * Tmin) / (Tmin + 237.3))
esmax <- 0.611 * exp((17.3 * Tmax) / (Tmax + 237.3))
es <- 0.5 * (esmin + esmax)
ea <- Rh * es / 100
VPD <- es - ea
write.csv(VPD, "VPD_Amon_CESM2-WACCM_ssp585_r1i1p1f1_gn_225101-229912.csv")