library(HH)
library(leaps)
library(dplyr)
library(tidyr)

#import data
wdi_data<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Data.csv',header=T, sep=",", fill = TRUE)
wdi_countries<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Country.csv',header=T, sep=",", fill = TRUE)

indicator_codes<-data.frame(Code=c("EG.ELC.ACCS.ZS", "SE.PRM.GINT.FE.ZS","SE.PRM.ENRL.TC.ZS","SH.IMM.IDPT", "SP.DYN.LE00.IN","SH.XPD.PCAP","SH.STA.ACSN","NY.GDP.PCAP.CD","SP.RUR.TOTL.ZS","SP.POP.GROW", "SH.H2O.SAFE.RU.ZS","SH.H2O.SAFE.UR.ZS","SP.POP.TOTL", "EN.ATM.CO2E.PC","SH.IMM.MEAS", "SP.ADO.TFRT","SH.TBS.INCD","SL.UEM.TOTL.ZS","SE.PRM.CMPT.ZS","FP.CPI.TOTL.ZG"),
                            Name=c("Access_electricity","Gross_intake_ratio", "Pupil_teacher_ratio","Immunizations_DPT","Life_expectancy", "Health_expenditure", "Sanitation", "GDP", "Rural", "Population_Grow","Water_Access_Rural", "Water_Access_Urban","Population", "CO_Emissions", "Immunizations_measless","Adolescent_Fertitlity_rate","Tuberculosis_incidence","Unemployment","Primary","Inflation"),
                            Year=c("X2012","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011","X2011")
                            )

#obtain all countries
countries<-unique(wdi_data[c("Country.Code","Country.Name")])
#intialize all_indicators with only country code
all_indicators = data.frame(Country.Code=countries$Country.Code, Country.Name=countries$Country.Name)

#add region, filtering those who are not countries (do not belong to a region)
all_indicators = merge(all_indicators,subset(wdi_countries[c("Country.Code","Region")],nchar(as.character(Region)) > 0))
all_indicators$Region = as.factor(all_indicators$Region)

#add indicators
for(i in 1:nrow(indicator_codes)) {
  
    indicator <- subset(wdi_data,as.character(Indicator.Code) == as.character(indicator_codes[i,"Code"]), select = c("Country.Code", as.character(indicator_codes[i,"Year"])))
    colnames(indicator) <- c("Country.Code",paste(as.character(indicator_codes[i,"Name"]), indicator_codes[i,"Year"]))
    all_indicators = merge(all_indicators,indicator)   
  
}

#Get the number of NA values per indicator
colSums(is.na(all_indicators))

#keep indicators with na values < 40
all_indicators <- all_indicators[,which(colSums(is.na(all_indicators)) < 40)]

#keep countries with no na values
rowSums(is.na(all_indicators))
all_indicators <- all_indicators[which(rowSums(is.na(all_indicators)) < 1),]


#How many countries are complete?
nrow(all_indicators[complete.cases(all_indicators),])
