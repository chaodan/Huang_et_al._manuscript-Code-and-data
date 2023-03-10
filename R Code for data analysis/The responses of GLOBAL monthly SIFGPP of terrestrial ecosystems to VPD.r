#The responses of GLOBAL monthly SIFGPP of terrestrial ecosystems to VPD 
#GPP was estimated based on OCO-2 SIF products
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
LAT=hztemp$LAT
LONG=hztemp$LONG
IGBP=hztemp$IGBP
Climate=hztemp$Climate
year=hztemp$year
month=hztemp$month
SIFGPP=hztemp$SIFGPP
VPD=hztemp$VPD
SPEI=hztemp$SPEI
Drought=hztemp$Drought
CLASS=hztemp$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
#The responses of GLOBAL SIFGPP of terrestrial ecosystems to VPD 
p1 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p1
p1_npg <- p1 + scale_color_ucscgb() + scale_fill_npg()
p1_npg <- p1 + scale_color_npg() + scale_fill_npg()
p1_npg
ggsave("Global_SIFGPP_VPD20220307.tiff",width=180, height=160, units="mm", dpi=400) 


#The responses of GPP of Evergreen Needleleaf Forest to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
GRA <- filter(hztemp,IGBP == 'ENF')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p2 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p2
p2_npg <- p2 + scale_color_ucscgb() + scale_fill_npg()
p2_npg
ggsave("SIFGPP_VPD_ENF20220307.tiff",width=180, height=160, units="mm", dpi=400) 


#The responses of GPP of Evergreen Broadleaf Forest to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
GRA <- filter(hztemp,IGBP == 'EBF')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p3 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p3
p3_npg <- p3 + scale_color_ucscgb() + scale_fill_npg()
p3_npg
ggsave("SIFGPP_VPD_EBF20220307.tiff",width=180, height=160, units="mm", dpi=400) 



#The responses of GPP of Deciduous Needleleaf Forest to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
GRA <- filter(hztemp,IGBP =='DNF')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p4 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p4
p4_npg <- p4 + scale_color_ucscgb() + scale_fill_npg()
p4_npg
ggsave("SIFGPP_VPD_DNF20220307.tiff",width=180, height=160, units="mm", dpi=400) 




#The responses of GPP of Deciduous Broadleaf Forest to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
GRA <- filter(hztemp,IGBP == 'DBF')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p5 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p5
p5_npg <- p5 + scale_color_ucscgb() + scale_fill_npg()
p5_npg
ggsave("SIFGPP_VPD_DBF20220307.tiff",width=180, height=160, units="mm", dpi=400) 




#The responses of GPP of Mixed Forest to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
GRA <- filter(hztemp,IGBP == 'MF')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p6 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p6
p6_npg <- p6 + scale_color_ucscgb() + scale_fill_npg()
p6_npg
ggsave("SIFGPP_VPD_MF20220307.tiff",width=180, height=160, units="mm", dpi=400) 




#The responses of GPP of Closed Shrubland to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
GRA <- filter(hztemp,IGBP == 'CSH')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p7 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p7
p7_npg <- p7 + scale_color_ucscgb() + scale_fill_npg()
p7_npg
ggsave("SIFGPP_VPD_CSH20220307.tiff",width=180, height=160, units="mm", dpi=400)




#The responses of GPP of Open Shrubland to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GRA <- filter(hztemp,IGBP == 'OSH')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p8 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p8
p8_npg <- p8 + scale_color_ucscgb() + scale_fill_npg()
p8_npg
ggsave("SIFGPP_VPD_OSH20220307.tiff",width=180, height=160, units="mm", dpi=400)





#The responses of GPP of woody savannas to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GRA <- filter(hztemp,IGBP == 'WSA')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p9 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p9
p9_npg <- p9 + scale_color_ucscgb() + scale_fill_npg()
p9_npg
ggsave("SIFGPP_VPD_WSA20220307.tiff",width=180, height=160, units="mm", dpi=400)




#The responses of GPP of savannas to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GRA <- filter(hztemp,IGBP == 'SAV')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p10 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p10
p10_npg <- p10 + scale_color_ucscgb() + scale_fill_npg()
p10_npg
ggsave("SIFGPP_VPD_SAV20220307.tiff",width=180, height=160, units="mm", dpi=400)





#The responses of GPP of Grassland to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GRA <- filter(hztemp,IGBP == 'GRA')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p11 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p11
p11_npg <- p11 + scale_color_ucscgb() + scale_fill_npg()
p11_npg
ggsave("SIFGPP_VPD_GRA20220307.tiff",width=180, height=160, units="mm", dpi=400)





#The responses of GPP of Cropland to VPD
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
hztemp <- read.table("E:\\RP\\carbon fluex\\GLOBALSIF_months20200303.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
GRA <- filter(hztemp,IGBP == 'CRO')
LAT=GRA$LAT
LONG=GRA$LONG
IGBP=GRA$IGBP
Climate=GRA$Climate
year=GRA$year
month=GRA$month
SIFGPP=GRA$SIFGPP
VPD=GRA$VPD
SPEI=GRA$SPEI
Drought=GRA$Drought
CLASS=GRA$CLASS
hz=data.frame(LAT,LONG,IGBP,Climate,year,month,SIFGPP,VPD,SPEI,Drought,CLASS)
p12 <- ggplot(hz,aes(x=VPD,y=SIFGPP))+ geom_point(aes(color=factor(CLASS)), alpha = 0.3) + geom_smooth(aes(x=VPD,y=SIFGPP),alpha=0.5,size=1.1,span=1)+ ylab("SIFGPP") + xlab("VPD") + theme_bw()  + theme(axis.title.x =element_text(size=22, face="bold", color = "black", hjust=0.5), axis.title.y=element_text(size=22,face="bold", color = "black", hjust=0.5),axis.text=element_text(size=24,face="bold", color = "black", hjust=0.5))
p12
p12_npg <- p12 + scale_color_ucscgb() + scale_fill_npg()
p12_npg
ggsave("SIFGPP_VPD_CRO20220307.tiff",width=180, height=160, units="mm", dpi=400)






