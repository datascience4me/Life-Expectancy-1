#Assumptions 1 and 2--Plot residuals against model output and all independent variables
Life_expectancy.res<-resid(fit.forward.bic.2)
Life_expectancy.hat<-fit.forward.bic.2$fitted.values

#model output against residuals
plot(Life_expectancy.hat, Life_expectancy.res, main = "Residuals vs. Model Output", xlab = "Model Output", ylab = "Residuals")

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
plot(all_indicatos_after_outliers_removed$CO_Emissions.2, Life_expectancy.res, xlab = "CO Emissions^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Immunizations_DPT, Life_expectancy.res, xlab = "Immunizations DPT", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Immunizations_DPT.2, Life_expectancy.res, xlab = "Immunizations DPT^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$Region, Life_expectancy.res, xlab = "Region", ylab = "Residuals")

#Assumption 3--QQ Plot
qqnorm(fit.forward.bic.2$residuals)
qqline(fit.forward.bic.2$residuals)

#Residual plots of interest
par(mfrow=c(2,2))
plot(all_indicatos_after_outliers_removed$Tuberculosis_incidence, Life_expectancy.res, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions.2, Life_expectancy.res, xlab = "CO Emissions^2", ylab = "Residuals")

#Box-Cox transformation
library(MASS)
trans<-boxcox(fit.forward.bic.2,lambda=seq(-2,2,0.1),plotit=T)
lambda1<-trans$x[which.max(trans$y)]
y.trans1<-((all_indicatos_after_outliers_removed$Life_expectancy^lambda1-1)/lambda1)
fit.forward.bic.3<-lm(formula = y.trans ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2, data = all_indicatos_after_outliers_removed)
plot(y=fit.forward.bic.3$residuals,x=fit.forward.bic.3$fitted.values,xlab=expression(hat(y)),ylab='residuals')

#Residual plots of interest--comparison of pre and post transformation
par(mfrow=c(2,2))
#post
plot(all_indicatos_after_outliers_removed$Tuberculosis_incidence, fit.forward.bic.3$residuals, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP<70000, fit.forward.bic.3$residuals[which(all_indicatos_after_outliers_removed$GDP<70000)], xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, fit.forward.bic.3$residuals, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions.2, fit.forward.bic.3$residuals, xlab = "CO Emissions^2", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$Tuberculosis_incidence, Life_expectancy.res, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP<70000, Life_expectancy.res, xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions.2, Life_expectancy.res, xlab = "CO Emissions^2", ylab = "Residuals")

#Box-Cox transformation (lambda = 0)
lambda2<-0
y.trans2<-log(all_indicatos_after_outliers_removed$Life_expectancy)
fit.forward.bic.4<-lm(formula = y.trans2 ~ Sanitation.2 + Region + Tuberculosis_incidence + 
                        Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
                        GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
                        GDP.2, data = all_indicatos_after_outliers_removed)
plot(y=fit.forward.bic.4$residuals,x=fit.forward.bic.4$fitted.values,xlab=expression(hat(y)),ylab='residuals')

#Residual plots of interest--comparison of pre and post transformation 2
par(mfrow=c(2,2))
#post
plot(all_indicatos_after_outliers_removed$Tuberculosis_incidence, fit.forward.bic.4$residuals, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP, fit.forward.bic.4$residuals, xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, fit.forward.bic.4$residuals, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions.2, fit.forward.bic.4$residuals, xlab = "CO Emissions^2", ylab = "Residuals")
#pre
plot(all_indicatos_after_outliers_removed$Tuberculosis_incidence, Life_expectancy.res, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP, Life_expectancy.res, xlab = "GDP per capita", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$GDP.2, Life_expectancy.res, xlab = "GDP per capita^2", ylab = "Residuals")
plot(all_indicatos_after_outliers_removed$CO_Emissions.2, Life_expectancy.res, xlab = "CO Emissions^2", ylab = "Residuals")
