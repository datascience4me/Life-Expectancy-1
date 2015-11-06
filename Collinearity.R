install.packages("dplyr")
install.packages("tidyr")
install.packages("HH")
install.packages("leaps")


library(HH)
library(leaps)
library(dplyr)
library(tidyr)
#Get Data
tbl_df(all_indicators)

#Get collinearity matrix
collinearity_matrix<-cor(select(all_indicators,-Life_expectancy,-Region,-Country.Code))
collinearity_matrix

#get vif values
fit<-lm(Life_expectancy~Immunizations+Health_expenditure+Sanitation+GDP+Unemployment+Rural+Primary+Mortality+Population_Grow+Water_Access_Rural+Water_Access_Urban+Population+Region, data=all_indicators)
vif<-vif(fit)

#Average vif is >> 1?
mean(vif)
#max vif > 10?
max(vif) > 10

#Exhaustive search
best.fit<-regsubsets(x=select(all_indicators,-Region,-Life_expectancy,-Country.Code), y=all_indicators$Life_expectancy,data=all_indicators,nbest=10,nvmax=12)
best.fit.results<-summary(best.fit)
plot(rowSums(best.fit.results$which), best.fit.results$cp, xlab='model size p+1', ylab='Mellow Cp')
model.size<-unique(rowSums(best.fit.results$which))
min.cp<-numeric(12)
for (i in 1:12) {
  min.cp[i]<-min(best.fit.results$cp[(rowSums(best.fit.results$which))==(i+1)])
}
lines(model.size,min.cp)

#Stepwise Regression
fit0<-lm(Life_expectancy~1,data=all_indicators)
scope<-list(upper=Life_expectacy~Immunizations+Health_expenditure+Sanitation+GDP+Unemployment+Rural+Primary+Mortality+Population_Grow+Water_Access_Rural+Water_Access_Urban+Population+Region, lower=Sales~1)
fit.forward<-step(fit0,direction='forward',scope=scope)
fit.backward<-step(fit,direction='backward',scope=scope)
fit.both<-step(fit,direction='both',scope=scope)
