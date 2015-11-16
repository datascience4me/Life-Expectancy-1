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