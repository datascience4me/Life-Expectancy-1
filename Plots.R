plot(all_indicators_with_squared$Sanitation, all_indicators_with_squared$Life_expectancy, xlab='Sanitation',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
plot(all_indicators_with_squared$Water_Access_Rural, all_indicators_with_squared$Life_expectancy, xlab='Water Access Rural',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')
plot(all_indicators_with_squared$GDP, all_indicators_with_squared$Life_expectancy, xlab='GDP',ylab='Life Expectancy',type='p', main='scatter plot of Life Expectancy Vs. Sanitation')


par(mfrow=c(3,4))
attach(all_indicators)

plot(ELECT,LIFE_EXP,xlab='ELECT',ylab='ELECT',type='p')
plot(DPT_IMM,LIFE_EXP,xlab='DPT_IMM',ylab='Life LIFE_EXP',type='p')
plot(HEALTH_EXP,LIFE_EXP,xlab='HEALTH_EXP',ylab='LIFE_EXP',type='p')
plot(SANIT,LIFE_EXP,xlab='SANIT',ylab='LIFE_EXP',type='p')



detach(all_indicators)