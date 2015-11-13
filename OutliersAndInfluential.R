#leverage values. outlier with respect to x?
k<-16
n<-170
leverageValues<-as.data.frame(hatvalues(fit.forward.bic))
leverageValues>((2*(k+1))/n)
#what countries are outliers wrt x
all_indicators_with_squared[which(leverageValues>((2*(k+1))/n)),c("Country.Name")]

#PRESS residual, Outlier with respect to y?
press<-rstudent(fit.forward.bic)
qt(1-0.025,n-k-2)
qt(1-0.005,n-k-2)
abs(press)>qt(1-0.025,n-k-2)
abs(press)>qt(1-0.005,n-k-2)
all_indicators_with_squared[which(abs(press)>qt(1-0.025,n-k-2)),c("Country.Name")]
all_indicators_with_squared[which(abs(press)>qt(1-0.005,n-k-2)),c("Country.Name")]
all_indicators_with_squared[which(abs(press)>qt(1-0.005,n-k-2)),c("Country.Name","Life_expectancy")]
#cooks distance
cooks<-cooks.distance(fit.forward.bic)
qf(.2, df1=k+1, df2=n-k-1)
qf(.5, df1=k+1, df2=n-k-1)
which(cooks>qf(.5, df1=k+1, df2=n-k-1)) #no influential observations
length(which(cooks<qf(.2, df1=k+1, df2=n-k-1))) #no influential observations!




fit.forward.bic.2<-lm(formula = Life_expectancy ~ Sanitation.2 + Region + Tuberculosis_incidence + 
     Rural + Adolescent_Fertitlity_rate + Water_Access_Rural.2 + 
     GDP + CO_Emissions + Immunizations_DPT + Immunizations_DPT.2 + 
     GDP.2, data = all_indicators_with_squared[which(abs(press)<qt(1-0.005,n-k-2)),])
