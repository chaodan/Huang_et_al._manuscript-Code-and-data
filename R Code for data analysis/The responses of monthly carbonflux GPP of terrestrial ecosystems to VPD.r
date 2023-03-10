#The responses of GLOBAL monthly GPP of terrestrial ecosystems to VPD 
#GPP and VPD were obtained from the FLUXNET2015 dataset
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
p1 <- ggplot(Global_GPP,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p1
p1_npg <- p1 + scale_color_ucscgb() + scale_fill_npg()
p1_npg
ggsave("CARBONfluxesGPP_VPD_global.tiff",width=180, height=160, units="mm", dpi=400) 



#The responses of GPP of grass ecosystems to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'GRA')
glimpse(GRA , width = 40)
p2 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p2
p2_npg <- p2 + scale_color_ucscgb() + scale_fill_npg()
p2_npg
ggsave("CARBONfluxesGPP_VPD_GRA.tiff",width=180, height=160, units="mm", dpi=400) 



#The responses of  GPP of crop ecosystems to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'CRO')
glimpse(GRA , width = 40)
p3 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p3
p3_npg <- p3 + scale_color_ucscgb() + scale_fill_npg()
p3_npg
ggsave("CARBONfluxesGPP_VPD_CRO.tiff",width=180, height=160, units="mm", dpi=400) 



#The responses of  GPP of Closed Shrublands to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'CSH')
glimpse(GRA , width = 40)
p4 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p4
p4_npg <- p4 + scale_color_ucscgb() + scale_fill_npg()
p4_npg
ggsave("CARBONfluxesGPP_VPD_CSH.tiff",width=180, height=160, units="mm", dpi=400) 



#The responses of  GPP of deciduous broadleaf forest to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'DBF')
glimpse(GRA , width = 40)
p5 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p5
p5_npg <- p4 + scale_color_ucscgb() + scale_fill_npg()
p5_npg
ggsave("CARBONfluxesGPP_VPD_DBF.tiff",width=180, height=160, units="mm", dpi=400) 






#The responses of  GPP of Evergreen Broadleaf Forest to VPD   
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'EBF')
glimpse(GRA , width = 40)
p6 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p6
p6_npg <- p6 + scale_color_ucscgb() + scale_fill_npg()
p6_npg
ggsave("CARBONfluxesGPP_VPD_EBF.tiff",width=180, height=160, units="mm", dpi=400)




#The responses of  GPP of Evergreen Needleleaf Forest to VPD  
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'ENF')
glimpse(GRA , width = 40)
p7 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p7
p7_npg <- p7 + scale_color_ucscgb() + scale_fill_npg()
p7_npg
ggsave("CARBONfluxesGPP_VPD_ENF.tiff",width=180, height=160, units="mm", dpi=400)





#The responses of  GPP of Mixed Forest to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'MF')
glimpse(GRA , width = 40)
p8 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p8
p8_npg <- p8 + scale_color_ucscgb() + scale_fill_npg()
p8_npg
ggsave("CARBONfluxesGPP_VPD_MF.tiff",width=180, height=160, units="mm", dpi=400)




#The responses of  GPP of woody savannas to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'WSA')
glimpse(GRA , width = 40)
p9 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p9
p9_npg <- p9 + scale_color_ucscgb() + scale_fill_npg()
p9_npg
ggsave("CARBONfluxesGPP_VPD_WSA.tiff",width=180, height=160, units="mm", dpi=400)



#The responses of  GPP of savannas to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'SAV')
glimpse(GRA , width = 40)
p10 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p10
p10_npg <- p10 + scale_color_ucscgb() + scale_fill_npg()
p10_npg
ggsave("CARBONfluxesGPP_VPD_SAV.tiff",width=180, height=160, units="mm", dpi=400)





#The responses of  GPP of Open Shrublands to VPD
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
Global_GPP <- read_csv("E:\\RP\\carbon fluex\\carbonflxues_months20220303.csv")
glimpse(Global_GPP , width = 40)
GRA <- filter(Global_GPP,IGBP == 'OSH')
glimpse(GRA , width = 40)
p11 <- ggplot(GRA,aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD_kPa,y=GPP_NT_VUT_MEAN),alpha=0.5,size=1.1,span=1) + ylab("GPP") + xlab("VPD") + theme_bw() + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p11
p11_npg <- p11 + scale_color_ucscgb() + scale_fill_npg()
p11_npg
ggsave("CARBONfluxesGPP_VPD_OSH.tiff",width=180, height=160, units="mm", dpi=400)







