#The relationships between GPP, VPD, and soil moisture
install.packages("ppcor")
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
library(Hmisc)
library(survival)
library(Formula)
library(ppcor)
#The relationships between SIFGPP, VPD, and ESA CCI soil moisture
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\GLOBALSIF_GPP&VPD&SM_20211002.csv")
glimpse(Global_GPP , width = 40)
GPP_cor <- rcorr(as.matrix(Global_GPP), type = 'spearman')
GPP_cor$r  
GPP_cor$P  
pcor.test(x = Global_GPP$VPD, y = Global_GPP$SIFGPP, z = Global_GPP$SM, method = 'spearman')
pcor.test(x = Global_GPP$SM, y = Global_GPP$SIFGPP, z = Global_GPP$VPD, method = 'spearman')
pcor.test(x = Global_GPP$VPD, y = Global_GPP$SIFGPP, z = Global_GPP$SM, method = 'spearman')
pcor.test(x = Global_GPP$SM, y = Global_GPP$SIFGPP, z = Global_GPP$VPD, method = 'spearman')
plot_dat <- read.delim("E:\\RP\\carbon fluex\\SIFGPP&VPD&SM_cor_result.txt", stringsAsFactors = FALSE)
plot_dat$partial_correlation_control <- factor(plot_dat$partial_correlation_control, levels = c('Zero-order', 'VPD', 'SM'))
plot_dat$variable1 <- factor(plot_dat$variable1, levels = c('VPD', 'SM'))
PP <- ggplot(plot_dat, aes(x = partial_correlation_control, y = variable1)) + geom_tile(aes(fill = spearman))+ scale_fill_gradientn(colors = c('blue', 'grey95', 'red'), limit = c(-1, 1)) +  geom_text(aes(label = sig), size = 5) + facet_grid(~variable2) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size=14, face="bold", color = "black")) + labs(x = '\nPartial correlation control', y = '', fill = "(Partial)\ncorrelations\n\nSpearman's r") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))  + theme(axis.title.x =element_text(size=16, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=16,face="bold", color = "black", hjust=0.5)) + theme(strip.text = element_text(size=16,face="bold", color = "black"))
PP
ggsave("partial_correlation_SIFGPP&VPD&SM.tiff",width=160, height=140, units="mm", dpi=800) 

#The relationships between carbon fluxex GPP, VPD, and soil moisture
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\Carbonfluxes_GPP&VPD&SWC20221003.csv")
glimpse(Global_GPP , width = 40)
GPP_cor <- rcorr(as.matrix(Global_GPP), type = 'spearman')
GPP_cor$r  
GPP_cor$P  
pcor.test(x = Global_GPP$VPD_kPa, y = Global_GPP$GPP, z = Global_GPP$SWC, method = 'spearman')
pcor.test(x = Global_GPP$SWC, y = Global_GPP$GPP, z = Global_GPP$VPD_kPa, method = 'spearman')
plot_dat <- read.delim("E:\\RP\\carbon fluex\\carbonfluxGPP&VPD&SM_cor_result.txt", stringsAsFactors = FALSE)
plot_dat$partial_correlation_control <- factor(plot_dat$partial_correlation_control, levels = c('Zero-order', 'VPD', 'SWC'))
plot_dat$variable1 <- factor(plot_dat$variable1, levels = c('VPD', 'SWC'))
PP <- ggplot(plot_dat, aes(x = partial_correlation_control, y = variable1)) + geom_tile(aes(fill = spearman))+ scale_fill_gradientn(colors = c('blue', 'grey95', 'red'), limit = c(-1, 1)) +  geom_text(aes(label = sig), size = 5) + facet_grid(~variable2) + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_text(size=14, face="bold", color = "black")) + labs(x = '\nPartial correlation control', y = '', fill = "(Partial)\ncorrelations\n\nSpearman's r") + scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))  + theme(axis.title.x =element_text(size=16, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=16,face="bold", color = "black", hjust=0.5)) + theme(strip.text = element_text(size=16,face="bold", color = "black"))
PP
ggsave("partial_correlation_GPP&VPD&SM.tiff",width=160, height=140, units="mm", dpi=800) 
