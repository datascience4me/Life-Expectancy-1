#Assumptions 1 and 2--Plot residuals against model output and all independent variables
Life_expectancy.res<-fit.forward.bic.2$residuals
Life_expectancy.hat<-fit.forward.bic.2$fitted.values

#model output against residuals
plot(Life_expectancy.hat, Life_expectancy.res, xlab = expression(hat(y)), ylab = "Residuals")

#outliers removed
all_indicatos_after_outliers_removed<-all_indicators_with_squared[which(abs(press)<qt(1-0.005,n-k-2)),]

#model output against independent variables
par(mfrow=c(3,4))
plot(all_indicatos_after_outliers_removed$Sanitation.2, Life_expectancy.res, xlab = "Sanitation^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Tuberculosis_incidence, Life_expectancy.res, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Rural, Life_expectancy.res, xlab = "Rural Population", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Adolescent_Fertitlity_rate, Life_expectancy.res, xlab = "Adolescent Fertitlity Rate", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Water_Access_Rural.2, Life_expectancy.res, xlab = "Water Access Rural^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, xlab = "CO Emissions", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Immunizations_DPT, Life_expectancy.res, xlab = "Immunizations DPT", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Immunizations_DPT.2, Life_expectancy.res, xlab = "Immunizations DPT^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Region, Life_expectancy.res, xlab = "Region", ylab = "Residuals")

#Assumption 3--QQ Plot
qqnorm(fit.forward.bic.2$residuals)
qqline(fit.forward.bic.2$residuals)

#Residual plots of interest
par(mfrow=c(2,2))
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, xlab = "CO Emissions", ylab = "Residuals")

#Box-Cox transformation; lambda != 0)
library(MASS)
trans<-boxcox(fit.forward.bic.2,lambda=seq(-2,2,0.1),plotit=T)
lambda1<-trans$x[which.max(trans$y)]
y.trans1<-((all_indicatos_after_outliers_removed$Life_expectancy^lambda1-1)/lambda1)
fit.forward.bic.3<-lm(formula = y.trans1 ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2, data = all_indicatos_after_outliers_removed)
plot(y=fit.forward.bic.3$residuals,x=fit.forward.bic.3$fitted.values,xlab=expression(hat(y)),ylab='residuals')

#Residual plots of interest--comparison of pre and post transformation
par(mfrow=c(3,2))
#post
plot(all_indicatos_after_outliers_removed$GDP, fit.forward.bic.3$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, fit.forward.bic.3$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, fit.forward.bic.3$residuals, main = "Post Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")

#Box-Cox transformation (lambda = 0)
lambda2<-0
y.trans2<-log(all_indicatos_after_outliers_removed$Life_expectancy)
fit.forward.bic.4<-lm(formula = y.trans2 ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2, data = all_indicatos_after_outliers_removed)
plot(y=fit.forward.bic.4$residuals,x=fit.forward.bic.4$fitted.values,xlab=expression(hat(y)),ylab='residuals')

#Residual plots of interest--comparison of pre and post transformation 2
par(mfrow=c(3,2))
#post
plot(all_indicatos_after_outliers_removed$GDP, fit.forward.bic.4$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, fit.forward.bic.4$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, fit.forward.bic.4$residuals, main = "Post Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")

#Box-Cox transformation extension (inclusion of geometric mean of y; lambda != 0)
lambda3<-trans$x[which.max(trans$y)]
geo_mean<-exp(mean(log(all_indicatos_after_outliers_removed$Life_expectancy)))
y.trans3<-((all_indicatos_after_outliers_removed$Life_expectancy^lambda3-1)/(lambda3*(geo_mean^(lambda3-1))))
fit.forward.bic.5<-lm(formula = y.trans3 ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2, data = all_indicatos_after_outliers_removed)
plot(y=fit.forward.bic.5$residuals,x=fit.forward.bic.5$fitted.values,xlab=expression(hat(y)),ylab='residuals')

#Residual plots of interest--comparison of pre and post transformation 3
par(mfrow=c(3,2))
#post
plot(all_indicatos_after_outliers_removed$GDP, fit.forward.bic.5$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, fit.forward.bic.5$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, fit.forward.bic.5$residuals, main = "Post Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")

#Box-Cox transformation extension (inclusion of geometric mean of y; lambda = 0)
lambda4<-0
geo_mean<-exp(mean(log(all_indicatos_after_outliers_removed$Life_expectancy)))
y.trans4<-geo_mean*log(all_indicatos_after_outliers_removed$Life_expectancy)
fit.forward.bic.6<-lm(formula = y.trans4 ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2, data = all_indicatos_after_outliers_removed)
plot(y=fit.forward.bic.6$residuals,x=fit.forward.bic.6$fitted.values,xlab=expression(hat(y)),ylab='residuals')

#Residual plots of interest--comparison of pre and post transformation 4
par(mfrow=c(3,2))
#post
plot(all_indicatos_after_outliers_removed$GDP, fit.forward.bic.6$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, fit.forward.bic.6$residuals, main = "Post Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, fit.forward.bic.6$residuals, main = "Post Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, main = "Pre Box-Cox Transformation", xlab = "CO Emissions", ylab = "Residuals")

#Higher order transformations of GDP and CO Emissions required
#Log transformation
GDP.log<-log(all_indicatos_after_outliers_removed$GDP)
GDP.2.log<-log(all_indicatos_after_outliers_removed$GDP.2)
CO_Emissions.log<-log(all_indicatos_after_outliers_removed$CO_Emissions)
fit.forward.bic.7<-lm(formula = Life_expectancy ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP.log + CO_Emissions.log + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2.log, data = all_indicatos_after_outliers_removed)

#Residual plots of interest--comparison of pre and post log transformation
par(mfrow=c(3,2))
#post
plot(GDP.log, fit.forward.bic.7$residuals, main = "Post Log Transformation", xlab = "ln(GDP per capita)", ylab = "Residuals")
plot(GDP.2.log, fit.forward.bic.7$residuals, main = "Post Log Transformation", xlab = "ln(GDP per capita^2)", ylab = "Residuals")
plot(CO_Emissions.log, fit.forward.bic.7$residuals, main = "Post Log Transformation", xlab = "ln(CO Emissions)", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, main = "Pre Log Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, main = "Pre Log Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, main = "Pre Log Transformation", xlab = "CO Emissions", ylab = "Residuals")

#Box-Cox and Log Transformation
fit.forward.bic.8<-lm(formula = y.trans1 ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP.log + CO_Emissions.log + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2.log, data = all_indicatos_after_outliers_removed)

#Residual plots of interest--comparison of pre and post box-cox & log transformation
par(mfrow=c(3,2))
#post
plot(GDP.log, fit.forward.bic.8$residuals, main = "Post Log Transformation", xlab = "ln(GDP per capita)", ylab = "Residuals")
plot(GDP.2.log, fit.forward.bic.8$residuals, main = "Post Log Transformation", xlab = "ln(GDP per capita^2)", ylab = "Residuals")
plot(CO_Emissions.log, fit.forward.bic.8$residuals, main = "Post Log Transformation", xlab = "ln(CO Emissions)", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, main = "Pre Log Transformation", xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, main = "Pre Log Transformation", xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions, Life_expectancy.res, main = "Pre Log Transformation", xlab = "CO Emissions", ylab = "Residuals")

