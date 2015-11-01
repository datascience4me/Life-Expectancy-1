wdi_data<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Data.csv',header=T, sep=",", fill = TRUE)
wdi_countries<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Country.csv',header=T, sep=",", fill = TRUE)

#indicator_codes<-data.frame(Code=c("SH.IMM.IDPT", "SP.DYN.LE00.IN","SH.XPD.PCAP","SH.STA.ACSN","NY.GDP.PCAP.CD","SL.UEM.TOTL.ZS","SP.RUR.TOTL.ZS","SE.PRM.CMPT.ZS","SH.DYN.MORT"),
#                            Name=c("Immunizations","Life_expectancy", "Health_expenditure", "Sanitation", "GDP", "Unemployment", "Rural", "Primary", "Mortality")       
#                            )

indicator_codes<-c("SH.IMM.IDPT", "SP.DYN.LE00.IN","SH.XPD.PCAP","SH.STA.ACSN","NY.GDP.PCAP.CD","SL.UEM.TOTL.ZS","SP.RUR.TOTL.ZS","SE.PRM.CMPT.ZS","SH.DYN.MORT")

#obtain all countries
countries<-unique(wdi_data[c("Country.Code")])
#intialize all_indicators with only country code
all_indicators = data.frame(Country.Code=countries$Country.Code)

#add region, filtering those who are not countries (do not belong to a region)
all_indicators = merge(all_indicators,subset(wdi_countries[c("Country.Code","Region")],nchar(as.character(Region)) > 0))

#for(i in 1:nrow(indicator_codes)) {
  
# indicator_code <- indicator_codes[i,]
# print(indicator_code[1])
# indicator <- subset(wdi_data,as.character(Indicator.Code) == indicator_code[1], select = c("Country.Code", "X2013"))
# print(indicator)
# colnames(indicator) <- c("Country.Code",indicator_code[2])
  
# all_indicators = merge(all_indicators,indicator)
  
#}


#add indicators
for(indicator_code in indicator_codes){
  
  indicator <- subset(wdi_data,as.character(Indicator.Code) == indicator_code, select = c("Country.Code", "X2013"))
  #for (i in which(sapply(indicator, is.numeric))) {
  #  indicator[is.na(indicator[, i]), i] <- mean(indicator[, i],  na.rm = TRUE)
  #}
  colnames(indicator) <- c("Country.Code",indicator_code)
  all_indicators = merge(all_indicators,indicator)
}

#How many countries are complete?
nrow(all_indicators[!complete.cases(all_indicators),])
#Thus, we cannot eliminate those observations. We have to estimate their values

#option 1: replace with the mean
for (i in which(sapply(all_indicators, is.numeric))) {
  all_indicators[is.na(all_indicators[, i]), i] <- mean(all_indicators[, i],  na.rm = TRUE)
}

#option 2: fit model to estimate the values
#attach(all_indicators)
#lm.imp.1 <- lm (earnings ~ male + over65 + white + immig + educ_r + R code
#                workmos + workhrs.top + any.ssi + any.welfare + any.charity,
#                data=SIS, subset=earnings>0)
#dettach(all_indicators)


attach(all_indicators)
fit<-lm(SP.DYN.LE00.IN~SH.IMM.IDPT+SH.XPD.PCAP+SH.STA.ACSN+NY.GDP.PCAP.CD+SL.UEM.TOTL.ZS+SP.RUR.TOTL.ZS+SE.PRM.CMPT.ZS+SH.DYN.MORT, data=allIndicators)
summary(fit)
detach(all_indicators)