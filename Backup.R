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


#delete outliers
fit.full.2<-lm(formula_full,data=all_indicators_with_squared[which(abs(press)<qt(1-0.005,n-k-2)),])
fit0.2<-lm(Life_expectancy~1,data=all_indicators_with_squared[which(abs(press)<qt(1-0.005,n-k-2)),])
scope<-list(upper=formula_full, lower=Life_expectancy~1)

#Stepwise Regression forward.
#using BIC
fit.forward.bic.2<-step(fit0.2,direction='forward',scope=scope,  k=log(nrow(all_indicators_with_squared[which(abs(press)<qt(1-0.005,n-k-2)),])))