#leverage values. outlier with respect to x?
k<-16
n<-170
leverageValues<-as.data.frame(hatvalues(fit.forward.bic))
leverageValues>((2*(k+1))/n)
#what countries are outliers wrt x
all_indicators_with_squared[which(leverageValues>((2*(k+1))/n)),c("Country.Name")]

#PRESS residual, Outlier with respect to y?

#cooks distance
cooks<-cooks.distance(fit.forward.bic)
qf(.2, df1=k+1, df2=n-k-1)
qf(.5, df1=k+1, df2=n-k-1)
cooks>qf(.5, df1=k+1, df2=n-k-1) #no influential observations
cooks<qf(.2, df1=k+1, df2=n-k-1) #no influential observations!