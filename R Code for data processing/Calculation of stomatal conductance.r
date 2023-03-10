##Calculation of stomatal conductance based on carbon flux observation data
##The input data are carbon flux observations
library(dplyr)
library(bigleaf)
hztemp <- read.table("E:\\RP\\carbon fluex\\AU-Dry_Gs.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
Fluxes_site=hztemp$Fluxes_site
LAT=hztemp$LAT
LONG=hztemp$LONG
IGBP=hztemp$IGBP
year=hztemp$YEAR
month=hztemp$MONTH
Tair=hztemp$TA_F
Tair_qc=hztemp$TA_F_QC
VPD=hztemp$VPD_F
VPD_qc=hztemp$VPD_F_QC
pressure=hztemp$PA_F
precip=hztemp$P_F
precip_qc=hztemp$P_F_QC
ustar=hztemp$USTAR
wind=hztemp$WS_F
wind_qc=hztemp$WS_F_QC
Ca=hztemp$CO2_F_MDS
Ca_qc=hztemp$CO2_F_MDS_QC
LW_up=hztemp$LW_OUT
Rn=hztemp$NETRAD
LE=hztemp$LE_F_MDS
LE_qc=hztemp$LE_F_MDS_QC
H=hztemp$H_F_MDS
H_qc=hztemp$H_F_MDS_QC
G=hztemp$G_F_MDS
G_qc=hztemp$G_F_MDS_QC
NEE=hztemp$NEE_VUT_USTAR50
NEE_qc=hztemp$NEE_VUT_USTAR50_QC
GPP=hztemp$GPP_NT_VUT_USTAR50
Reco=hztemp$RECO_NT_VUT_USTAR50
hz=data.frame(Fluxes_site,LAT,LONG,IGBP,year,month,Tair,Tair_qc,VPD,VPD_qc,pressure,precip,precip_qc,ustar,wind,wind_qc,Ca,Ca_qc,LW_up,Rn,LE,H,H_qc,G,G_qc,NEE,NEE_qc,GPP,Reco)
#Calculate aerodynamic conductance
p1 <- aerodynamic.conductance(hz,Rb_model="Thom_1972")
write.csv(p1, "AT-Neu_month_Ga")
#Read again the carbon flux observations with aerodynamic conductance added
hztemp <- read.table("E:\\RP\\carbon fluex\\AU-Dry_Gs.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 
Fluxes_site=hztemp$Fluxes_site
LAT=hztemp$LAT
LONG=hztemp$LONG
IGBP=hztemp$IGBP
year=hztemp$YEAR
month=hztemp$MONTH
Tair=hztemp$TA_F
Tair_qc=hztemp$TA_F_QC
VPD=hztemp$VPD_F
VPD_qc=hztemp$VPD_F_QC
pressure=hztemp$PA_F
precip=hztemp$P_F
precip_qc=hztemp$P_F_QC
ustar=hztemp$USTAR
wind=hztemp$WS_F
wind_qc=hztemp$WS_F_QC
Ca=hztemp$CO2_F_MDS
Ca_qc=hztemp$CO2_F_MDS_QC
LW_up=hztemp$LW_OUT
Rn=hztemp$NETRAD
LE=hztemp$LE_F_MDS
LE_qc=hztemp$LE_F_MDS_QC
H=hztemp$H_F_MDS
H_qc=hztemp$H_F_MDS_QC
G=hztemp$G_F_MDS
G_qc=hztemp$G_F_MDS_QC
NEE=hztemp$NEE_VUT_USTAR50
NEE_qc=hztemp$NEE_VUT_USTAR50_QC
GPP=hztemp$GPP_NT_VUT_USTAR50
GPP_qc=hztemp$NEE_VUT_USTAR50_QC
Reco=hztemp$RECO_NT_VUT_USTAR50
Ga=hztemp$Ga_h
hz=data.frame(Fluxes_site,LAT,LONG,IGBP,year,month,Tair,Tair_qc,VPD,VPD_qc,pressure,precip,precip_qc,ustar,wind,wind_qc,Ca,Ca_qc,LW_up,Rn,LE,H,H_qc,G,G_qc,NEE,NEE_qc,GPP,Reco,Ga)
# calculate Gs from the the inverted PM equation (now Rn, and Ga are needed),
Gs_PM <- surface.conductance(hz,Tair="Tair",pressure="pressure",Rn="Rn",G="G",S=NULL,VPD="VPD",Ga=Ga,formulation="Penman-Monteith")
Gs_PM
write.csv(Gs_PM, "AU-Dry_month_Gs_PM")