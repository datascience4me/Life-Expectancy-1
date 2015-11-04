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
fit<-lm(Life_expectancy~Immunizations+Health_expenditure+Sanitation+GDP+Unemployment+Rural+Primary+Mortality+Population_Grow+Water_Access_Rural+Water_Access_Urban+Population+Region, data=allIndicators)
vif<-vif(fit)

#Average vif is >> 1?
mean(vif)
#max vif > 10?
max(vif) > 10

