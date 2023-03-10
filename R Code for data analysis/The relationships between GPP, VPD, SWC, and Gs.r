##The relationships between GPP, VPD, SWC, and Gs among different moisture categories
library(RColorBrewer)
library(ggpubr)
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
hztemp <- read.csv("E:\\RP\\carbon fluex\\Carbonfluxes_GS and GPP20220424.csv")
glimpse(hztemp , width = 40)
#extreme drought conditions
mydata <- hztemp %>% filter(Drough_cat == "ED")
glimpse(mydata , width = 40)
p1 <- ggplot(mydata,aes(x=VPD_kPa_LOG,y=GPP_LOG, color=SWC))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=VPD_kPa_LOG,y=GPP_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("GPP_LOG") + xlab("VPD_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between GPP and VPD of terrestrial ecosystems under extreme drought conditions.tiff",width=180, height=160, units="mm", dpi=800)

p1 <- ggplot(mydata,aes(x=SWC_LOG,y=GPP_LOG, color=VPD_kPa))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=SWC_LOG,y=GPP_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("GPP_LOG") + xlab("SWC_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between GPP and SWC of terrestrial ecosystems under extreme drought conditions.tiff",width=180, height=160, units="mm", dpi=800)

p1 <- ggplot(mydata,aes(x=VPD_kPa_LOG,y=Gs_mol_LOG, color=SWC))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=VPD_kPa_LOG,y=Gs_mol_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("Gs_LOG") + xlab("VPD_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between Ga and VPD of terrestrial ecosystems under extreme drought conditions.tiff",width=180, height=160, units="mm", dpi=800)

p1 <- ggplot(mydata,aes(x=SWC_LOG,y=Gs_mol_LOG, color=VPD_kPa))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=SWC_LOG,y=Gs_mol_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("Gs_LOG") + xlab("SWC_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between Ga and SWC of terrestrial ecosystems under extreme drought conditions.tiff",width=180, height=160, units="mm", dpi=800)


#extreme drought conditions
mydata <- hztemp %>% filter(Drough_cat == "EH")
glimpse(mydata , width = 40)
p1 <- ggplot(mydata,aes(x=VPD_kPa_LOG,y=GPP_LOG, color=SWC))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=VPD_kPa_LOG,y=GPP_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("GPP_LOG") + xlab("VPD_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between GPP and VPD of terrestrial ecosystems under extreme wet conditions.tiff",width=180, height=160, units="mm", dpi=800)

p1 <- ggplot(mydata,aes(x=SWC_LOG,y=GPP_LOG, color=VPD_kPa))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=SWC_LOG,y=GPP_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("GPP_LOG") + xlab("SWC_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between GPP and SWC of terrestrial ecosystems under extreme wet conditions.tiff",width=180, height=160, units="mm", dpi=800)


p1 <- ggplot(mydata,aes(x=VPD_kPa_LOG,y=Gs_mol_LOG, color=SWC))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=VPD_kPa_LOG,y=Gs_mol_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("Gs_LOG") + xlab("VPD_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between Gs and VPD of terrestrial ecosystems under extreme wet conditions.tiff",width=180, height=160, units="mm", dpi=800)

p1 <- ggplot(mydata,aes(x=SWC_LOG,y=Gs_mol_LOG, color=VPD_kPa))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=SWC_LOG,y=Gs_mol_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("Gs_LOG") + xlab("SWC_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between Gs and SWC of terrestrial ecosystems under extreme wet conditions.tiff",width=180, height=160, units="mm", dpi=800)



#normal conditions
mydata <- hztemp %>% filter(Drough_cat == "NO")
glimpse(mydata , width = 40)
p1 <- ggplot(mydata,aes(x=VPD_kPa_LOG,y=GPP_LOG, color=SWC))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=VPD_kPa_LOG,y=GPP_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("GPP_LOG") + xlab("VPD_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between GPP and VPD of terrestrial ecosystems under normal conditions.tiff",width=180, height=160, units="mm", dpi=800)

p1 <- ggplot(mydata,aes(x=SWC_LOG,y=GPP_LOG, color=VPD_kPa))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=SWC_LOG,y=GPP_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("GPP_LOG") + xlab("SWC_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between GPP and SWC of terrestrial ecosystems under normal conditions.tiff",width=180, height=160, units="mm", dpi=800)


p1 <- ggplot(mydata,aes(x=VPD_kPa_LOG,y=Gs_mol_LOG, color=SWC))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=VPD_kPa_LOG,y=Gs_mol_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("Gs_LOG") + xlab("VPD_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between Gs and VPD of terrestrial ecosystems under normal conditions.tiff",width=180, height=160, units="mm", dpi=800)


p1 <- ggplot(mydata,aes(x=SWC_LOG,y=Gs_mol_LOG, color=VPD_kPa))+ geom_point(size=2,alpha = 0.9) + geom_smooth(aes(x=SWC_LOG,y=Gs_mol_LOG),method=lm,level=0.95,color="gray4", alpha = 0.45, size = 1.1, span = 1.3) + ylab("Gs_LOG") + xlab("SWC_LOG") +  theme_bw() + stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.02) +  theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5)) +  theme(legend.text=element_text(face="bold", color = "black"))
p1 
p1 + gradient_color("RdYlBu")
ggsave("The relationships between Gs and SWC of terrestrial ecosystems under normal conditions.tiff",width=180, height=160, units="mm", dpi=800)











