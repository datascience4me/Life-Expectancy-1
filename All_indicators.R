#import data
wdi_data<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Data.csv',header=T, sep=",", fill = TRUE)
wdi_countries<-read.csv('D:/dropbox/Dropbox/IIT/Applied Statistics/Project/WDI_csv/WDI_Country.csv',header=T, sep=",", fill = TRUE)

indicator_codes<-data.frame(Code=c("SH.IMM.IDPT", "SP.DYN.LE00.IN","SH.XPD.PCAP","SH.STA.ACSN","NY.GDP.PCAP.CD","SL.UEM.TOTL.ZS","SP.RUR.TOTL.ZS","SE.PRM.CMPT.ZS","SH.DYN.MORT"),
                            Name=c("Immunizations","Life_expectancy", "Health_expenditure", "Sanitation", "GDP", "Unemployment", "Rural", "Primary", "Mortality")       
                            )

#obtain all countries
countries<-unique(wdi_data[c("Country.Code")])
#intialize all_indicators with only country code
all_indicators = data.frame(Country.Code=countries$Country.Code)

#add region, filtering those who are not countries (do not belong to a region)
all_indicators = merge(all_indicators,subset(wdi_countries[c("Country.Code","Region")],nchar(as.character(Region)) > 0))

#add indicators
for(i in 1:nrow(indicator_codes)) {
 indicator <- subset(wdi_data,as.character(Indicator.Code) == as.character(indicator_codes[i,"Code"]), select = c("Country.Code", "X2013"))
 colnames(indicator) <- c("Country.Code",as.character(indicator_codes[i,"Name"]))
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

attach(all_indicators)
fit<-lm(Life_expectancy~Immunizations+Health_expenditure+Sanitation+GDP+Unemployment+Rural+Primary+Mortality, data=allIndicators)
summary(fit)
detach(all_indicators)