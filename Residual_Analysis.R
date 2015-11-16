#Assumptions 1 and 2--Plot residuals against model output and all independent variables

Life_expectancy.res = resid(fit.forward.bic)
#how to include the region dummy variables in this formula?
Life_expectancy.hat = c(50.60 + 0.0004*Sanitation.2 - 0.011*Tuberculosis_incidence - 0.044*Rural - 0.032*Adolescent_Fertitlity_rate + 0.0003*Water_Access_Rural.2 + 0.0001*GDP - 0.000000001*GDP.2 - 0.165*CO_Emissions.2 + 0.51*Immunizations_DPT - 0.003*Immunizations_DPT.2 - 2.36*RegionEurope & Central Asia - 0.74*RegionLatin America & Caribbean - 3*RegionMiddle East & North Africa - 2.5*RegionNorth America - 1.3*RegionSouth Asia - 7.40*RegionSub-Saharan Africa)

#model output against residuals
plot(Life_expectancy.hat, Life_expectancy.res, main = "Residuals vs. Model1 Output", xlab = "Model1 Output", ylab = "Residuals")

#model output against independent variables
par(mfrow=c(4,4))
plot(Sanitation.2, Life_expectancy.res, xlab = "Sanitation^2", ylab = "Residuals")
plot(Tuberculosis_incidence, Life_expectancy.res, xlab = "Tuberculosis Incidence", ylab = "Residuals")
plot(Rural, Life_expectancy.res, xlab = "Rural Population", ylab = "Residuals")
plot(Adolescent_Fertitlity_rate, Life_expectancy.res, xlab = "Adolescent Fertitlity Rate", ylab = "Residuals")
plot(Water_Access_Rural.2, Life_expectancy.res, xlab = "Water Access Rural^2", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP per capita", ylab = "Residuals")
plot(GDP.2, Life_expectancy.res, xlab = "GDP per capita^2", ylab = "Residuals")
plot(CO_Emissions.2, Life_expectancy.res, xlab = "CO_Emissions.2", ylab = "Residuals")
plot(Immunizations_DPT, Life_expectancy.res, xlab = "Immunizations_DPT", ylab = "Residuals")
plot(Immunizations_DPT.2, Life_expectancy.res, xlab = "Immunizations_DPT.2", ylab = "Residuals")

#how to include regions?
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")
plot(GDP, Life_expectancy.res, xlab = "GDP", ylab = "Residuals")


#Assumption 3--QQ Plot
qqnorm(fit.forward.bic$residuals)
qqline(fit.forward.bic$residuals)