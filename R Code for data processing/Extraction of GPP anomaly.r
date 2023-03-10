## According to the VPD anomaly (above the 95th percentile VPD) to extract GPP anomaly in the corresponding period
## The input data are VPD and GPP anomalies calculated based on carbon flux observations, SIFGPP products and simulation results from earth system model 
library(sp)
library(raster)
library(rgdal)
library(ncdf4)
library(Rcpp)
library(dplyr)
library(spatial)
library(tidyverse)
library(Rmisc)
library(readr)
setwd("G:/netcdf")
d.vpd <- read_delim("VPD_anomoly.csv",delim = ",")
d.gpp <- read_delim("GPP_anomoly.csv",delim = ",")
# According to the VPD anomaly to extract the GPP anomaly in the corresponding perio
head(d.vpd)
ind.v <- vector()
vpd.max.v <- vector()
gpp.max.v <- vector()
if(all(d.vpd[,1] == d.gpp[,1]) == 1 ){
  for (i in seq(nrow(d.vpd))){
    # i <- 1
    # print(i)
    
    d.use <- as.vector(unlist(d.vpd[i,2:ncol(d.vpd)]))
    #d.use
    vpd.max.v <- append(vpd.max.v,max(d.use)) 
    ind.tmp <- which(d.use == max(d.use))[1] # The time of VPD anomaly occurs
    ind.v <- append(ind.v,ind.tmp)
    
    if(max(d.use) != 0){
      gpp.max.v <- append(gpp.max.v,as.vector(unlist(d.gpp[i,ind.tmp+1]))) 
    }else if(max(d.use) == 0){
      max.gpp <- max(d.gpp[i,2:ncol(d.gpp)])
      if (max.gpp == 0){
        gpp.max.v <- append(gpp.max.v,min(d.gpp[i,2:ncol(d.gpp)]))
      }else{
        gpp.max.v <- append(gpp.max.v,max.gpp) 
        }
    }else{gpp.max.v <- "na"}

  }
}else{
  print("报错：Vpd and gpp anomaly occurrence time is inconsistent")
}

#vpd.max.v
#ind.v
#gpp.max.v

# Generate and save CSV file
d.final <- data.frame(vpd=vpd.max.v,gpp=gpp.max.v,stringsAsFactors = F) %>% as_tibble()
write_delim(d.final,"dat.csv",delim = ",")


