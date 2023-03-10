##Extract ESA CCI soil moisture data
library(sp)
library(raster)
library(rgdal)
library(ncdf4)
library(Rcpp)
library(dplyr)
library(spatial)
setwd("D:/sm")
IPCC <- read.table("D:\\sm\\SIFGPP_monthly_提取土壤湿度样点.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
lon.pts=IPCC$LONG
lat.pts=IPCC$LAT
extract.pts <- cbind(lon.pts,lat.pts)
fname <- "D:/sm/C3S-SOILMOISTURE-L3S-SSMV-COMBINED-MONTHLY-20191201000000-TCDR-v202012.0.0.nc"
r <- brick(fname)
r
plot(r)
ext <- extract(r,extract.pts,method="bilinear")
write.csv(ext, "SSMV-COMBINED-MONTHLY-20191201000000.csv")