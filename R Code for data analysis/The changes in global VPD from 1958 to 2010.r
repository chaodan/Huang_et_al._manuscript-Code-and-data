##The changes in VPD of global terrestrial ecosystems from 1958 to 2010
library(dplyr)
library(ggplot2)
library(plotluck)
library(psych)
library(ggthemes)
library(tidyverse)
library(Rmisc)
library(readr)
library(ggsci)
library(gridExtra)
library(ggpmisc)
#Calculate 95% confidence interval of VPD
VPD <- read_csv("E:\\RP\\carbon fluex\\GlobalTerraClimate_VPD_95%CI.csv")
glimpse(VPD , width = 40)
MeanCI(VPD$VPD, conf.level=0.95)
#The changes in VPD of global terrestrial ecosystems from 1958 to 2010
GlobalTerraClimate_VPD <- read_csv("E:\\RP\\carbon fluex\\GlobalTerraClimate_vpd1958_2010.csv")
glimpse(GlobalTerraClimate_VPD , width = 40)
p1 <- p + stat_smooth(color = "blue", fill = "red",method = "lm",formula=y~x, alpha=0.15) +  ylim(0.85, 0.95)  + xlab("Year") + ylab("VPD") + theme(axis.title.x =element_text(size=14, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=14,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=16,face="bold", color = "black", hjust=0.5))
p1
p2 <- p1 + stat_poly_eq(parse=T, aes(label = ..rr.label..), formula=y~x, label.y = 0.1, label.x = 0.7) + stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x, label.y = 0.15, label.x = 0.9)
p2
ggsave("GlobalTerraClimate_vpd1958_2010change.tiff",width=310, height=110, units="mm", dpi=800)



