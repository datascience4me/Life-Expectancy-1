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
collinearity_matrix<-cor(select(all_indicators,-Region,-Country.Code,-Country.Name))
collinearity_matrix
#which are the pairs of indicators with corr > 0.9
for(i in (which(collinearity_matrix > 0.9))){
  print(i)
  k<-arrayInd(i,dim(collinearity_matrix))
  print(rownames(collinearity_matrix)[k[,1]])
  print(colnames(collinearity_matrix)[k[,2]])
}

#get vif values
attach(all_indicators)
fit<-lm(Life_expectancy~., data=select(all_indicators,-Life_expectancy,-Country.Code,-Country.Name))
vif<-vif(fit)
detach(all_indicators)
#Average vif is >> 1?
mean(vif)
#max vif > 10?
max(vif) > 10

#model selection
formula_full<-as.formula(paste(paste("Life_expectancy ~ (",paste(colnames(select(all_indicators,-Life_expectancy, -Region,-Country.Code, -Country.Name)),collapse="+"),")^2+Region+", sep=""),paste(colnames(select(squared_terms,-Country.Code)),collapse="+"),sep=""))
scope<-list(upper=formula_full, lower=Life_expectancy~1)
fit.full<-lm(formula_full,data=all_indicators_with_squared)
fit0<-lm(Life_expectancy~1,data=all_indicators)

#Stepwise Regression forward.
#using AIC
fit.forward.aic<-step(fit0,direction='forward',scope=scope)
#using BIC
fit.forward.bic<-step(fit0,direction='forward',scope=scope,  k=log(nrow(all_indicators)))
#using cp
fit.forward.cp<-step(fit0,direction='forward',scope=scope, scale=(summary(fit.full)$sigma)^2)


#Backward Elimination
#using AIC
fit.backward.aic<-step(fit.full,direction='backward',scope=scope)
#using BIC
fit.backward.bic<-step(fit.full,direction='backward',scope=scope, k=log(nrow(all_indicators)))
#using CP
fit.backward.cp<-step(fit.full,direction='backward',scope=scope, scale=(summary(fit.full)$sigma)^2)

#both directions
#using aic
fit.both.aic<-step(fit.full,direction='both',scope=scope)
#using bic
fit.both.bic<-step(fit.full,direction='backward',scope=scope, k=log(nrow(all_indicators)))
#using CP
fit.both.cp<-step(fit.full,direction='backward',scope=scope, scale=(summary(fit.full)$sigma)^2)

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