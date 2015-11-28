library(HH)
library(leaps)
library(dplyr)
library(tidyr)

#import data -- Martin
wdi_data<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Data.csv',header=T, sep=",", fill = TRUE)
wdi_countries<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Country.csv',header=T, sep=",", fill = TRUE)

#import data -- Patrick
#wdi_data<-read.csv('C:/Users/patrickflavel/OneDrive/IIT Uni/1st Year/1st Semester/MATH 484 - Regression & Forecasting/Project/Data/WDI_Data.csv',header=T, sep=",", fill = TRUE)
#wdi_countries<-read.csv('C:/Users/patrickflavel/OneDrive/IIT Uni/1st Year/1st Semester/MATH 484 - Regression & Forecasting/Project/Data/WDI_Country.csv',header=T, sep=",", fill = TRUE)

indicator_codes<-data.frame(Code=c("EG.ELC.ACCS.ZS", "SE.PRM.GINT.FE.ZS","SE.PRM.ENRL.TC.ZS","SH.IMM.IDPT", "SP.DYN.LE00.IN","SH.XPD.TOTL.ZS","SH.STA.ACSN","NY.GDP.PCAP.CD","SP.RUR.TOTL.ZS","SP.POP.GROW", "SH.H2O.SAFE.RU.ZS","SH.H2O.SAFE.UR.ZS","SP.POP.TOTL", "EN.ATM.CO2E.PC","SH.IMM.MEAS", "SP.ADO.TFRT","SH.TBS.INCD","SL.UEM.TOTL.ZS","SE.PRM.CMPT.ZS","FP.CPI.TOTL.ZG"),
                            Name=c("ELECT","GROSS_INT", "PUP_TEA","DPT_IMM","LIFE_EXP", "HEALTH_EXP", "SANIT", "GDP", "RURAL", "POP_GROWTH","WATER_RUR", "WATER_URB","POP", "CO2", "MEAS_IMM","ADOL_FERT","TB","UNEMP","PRIM_COMP","INFL"),
                            Year=c("X2012","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011")
                            )

#obtain all countries; intialize all_indicators with only country code and country name
all_indicators<-unique(wdi_data[c("Country.Code","Country.Name")])

#add region, filtering out those who are not countries (i.e. do not belong to a region)
all_indicators = merge(all_indicators,subset(wdi_countries[c("Country.Code","Region")],nchar(as.character(Region)) > 0))

#add indicators
for(i in 1:nrow(indicator_codes)) {
  
    indicator <- subset(wdi_data,as.character(Indicator.Code) == as.character(indicator_codes[i,"Code"]), select = c("Country.Code", as.character(indicator_codes[i,"Year"])))
    colnames(indicator) <- c("Country.Code",as.character(indicator_codes[i,"Name"]))
    all_indicators = merge(all_indicators,indicator)
  
}

#Get the number of NA values per indicator
colSums(is.na(all_indicators))

#keep indicators with NA values < 30
all_indicators <- all_indicators[,which(colSums(is.na(all_indicators)) < 30)]

#get eliminated countries
all_indicators[which(rowSums(is.na(all_indicators)) > 0),]

#keep countries with no NA values
rowSums(is.na(all_indicators))
all_indicators <- all_indicators[which(rowSums(is.na(all_indicators)) < 1),]

#How many countries have complete data?
nrow(all_indicators[complete.cases(all_indicators),])

#statistics of the indicators
summary(all_indicators)

#generate squared terms
Access_electricity.2<-all_indicators$ELEC*all_indicators$ELEC
Immunizations_DPT.2<-all_indicators$DPT_IMM*all_indicators$DPT_IMM
Health_expenditure.2<-all_indicators$HEALTH_EXP*all_indicators$HEALTH_EXP
Sanitation.2<-all_indicators$SANIT*all_indicators$SANIT
GDP.2<-all_indicators$GDP*all_indicators$GDP
Rural.2<-all_indicators$RURAL*all_indicators$RURAL
Population_Grow.2<-all_indicators$POP_GROWTH*all_indicators$POP_GROWTH
Water_Access_Rural.2<-all_indicators$WATER_RUR*all_indicators$WATER_RUR
Water_Access_Urban.2<-all_indicators$WATER_URB*all_indicators$WATER_URB
Population.2<-all_indicators$POP*all_indicators$POP
CO_Emissions.2<-all_indicators$CO2*all_indicators$CO2
Immunizations_measless.2<-all_indicators$MEAS_IMM*all_indicators$MEAS_IMM
Adolescent_Fertitlity_rate.2<-all_indicators$ADOL_FERT*all_indicators$ADOL_FERT
Tuberculosis_incidence.2<-all_indicators$TB*all_indicators$TB

#add squared terms to all_indicators
squared_terms<-data.frame(Country.Code=all_indicators$Country.Code,Access_electricity.2,Immunizations_DPT.2,Health_expenditure.2,Sanitation.2,GDP.2,Rural.2,Population_Grow.2,Water_Access_Rural.2,Water_Access_Urban.2,Population.2,CO_Emissions.2,Immunizations_measless.2,Adolescent_Fertitlity_rate.2,Tuberculosis_incidence.2)
all_indicators_with_squared<-merge(all_indicators,squared_terms)



