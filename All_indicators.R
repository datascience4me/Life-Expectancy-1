library(HH)
library(leaps)
library(dplyr)
library(tidyr)

#import data
wdi_data<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Data.csv',header=T, sep=",", fill = TRUE)
wdi_countries<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Country.csv',header=T, sep=",", fill = TRUE)

indicator_codes<-data.frame(Code=c("SH.IMM.IDPT", "SP.DYN.LE00.IN","SH.XPD.PCAP","SH.STA.ACSN","NY.GDP.PCAP.CD","SP.RUR.TOTL.ZS","SH.DYN.MORT","SP.POP.GROW", "SH.H2O.SAFE.RU.ZS","SH.H2O.SAFE.UR.ZS","SP.POP.TOTL", "EN.ATM.CO2E.PC","SH.IMM.MEAS", "SP.ADO.TFRT","SH.TBS.INCD","SL.UEM.TOTL.ZS","SE.PRM.CMPT.ZS"),
                            Name=c("Immunizations_DPT","Life_expectancy", "Health_expenditure", "Sanitation", "GDP", "Rural", "Mortality", "Population_Grow","Water_Access_Rural", "Water_Access_Urban","Population", "CO_Emissions", "Immunizations_measless","Adolescent_Fertitlity_rate","Tuberculosis_incidence","Unemployment","Primary" )       
                            )

#obtain all countries
countries<-unique(wdi_data[c("Country.Code","Country.Name")])
#intialize all_indicators with only country code
all_indicators = data.frame(Country.Code=countries$Country.Code, Country.Name=countries$Country.Name)

#add region, filtering those who are not countries (do not belong to a region)
all_indicators = merge(all_indicators,subset(wdi_countries[c("Country.Code","Region")],nchar(as.character(Region)) > 0))
all_indicators$Region = as.factor(all_indicators$Region)

year <- c("X2011")

#add indicators
for(i in 1:nrow(indicator_codes)) {
  for(j in 1:length(year)){
    indicator <- subset(wdi_data,as.character(Indicator.Code) == as.character(indicator_codes[i,"Code"]), select = c("Country.Code", year[j]))
    colnames(indicator) <- c("Country.Code",paste(as.character(indicator_codes[i,"Name"]), year[j]))
    all_indicators = merge(all_indicators,indicator)   
  }
}

#Get the number of NA values per indicator
colSums(is.na(all_indicators))

#keep indicators with na values < 40
all_indicators <- all_indicators[,which(colSums(is.na(all_indicators)) < 40)]

#keep countries with no more than 1 na

all_indicators <- filter(all_indicators,rowSums(is.na(all_indicators))<2)


rowSums(is.na(all_indicators))


#How many countries are complete?
nrow(all_indicators[!complete.cases(all_indicators),])

#Thus, we cannot eliminate those observations. We have to estimate their values

#option 1: replace with the mean
for (i in which(sapply(all_indicators, is.numeric))) {
 all_indicators[is.na(all_indicators[, i]), i] <- mean(all_indicators[, i],  na.rm = TRUE)
}

#option 2: fit model to estimate the values
#do this for every variable. Not possible because other predictor variables are also na
# attach(all_indicators)
# lm.imp.1 <- lm (GDP ~ Unemployment, data=all_indicators)
# pred.1 <- predict(lm.imp.1,all_indicators,interval='confidence')
# detach(all_indicators)

#plotting Y against all X's
attach(all_indicators)
plot(Immunizations, Life_expectancy, xlab='Immunizations',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Immunizations')
plot(Sanitation, Life_expectancy, xlab='Sanitation',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
plot(Mortality, Life_expectancy, xlab='Mortality',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
plot(Rural, Life_expectancy, xlab='Mortality',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
plot(GDP, Life_expectancy, xlab='GDP',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
plot(GDP, Unemployment, xlab='GDP',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
detach(all_indicators)

#fit model
attach(all_indicators)
fit<-lm(Life_expectancy~Immunizations+Health_expenditure+Sanitation+GDP+Unemployment+Rural+Primary+Mortality+Population_Grow+Water_Access_Rural+Water_Access_Urban+Population+Region, data=all_indicators)
summary(fit)
detach(all_indicators)