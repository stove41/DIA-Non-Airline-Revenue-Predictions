library(car)
library(MASS)
library(leaps)
library(e1071)

den_data = readxl::read_xlsx('DEN 2012 - Jun 17-1.xlsx')
#Month and year column gets read in wrong so I fix it here.
den_data$`Month and Year` = strptime(den_data$`Month and Year`, '%Y-%m-%d')
months = array(NA, dim = length(den_data$`Month and Year`))
years = array(NA, dim = length(den_data$`Month and Year`))
days = array('1', dim = length(den_data$`Month and Year`))
for(i in 1:length(months)){
    months[i] = as.character(den_data$`Month and Year`[i]$mon + 1)
    years[i] = as.character(den_data$`Month and Year`[i]$mday)
}
real_dates = cbind(months, days, years)
date_strings = array(NA, dim = length(real_dates[,1]))
for(i in 1:length(real_dates[,1])){
    date_strings[i] = paste(real_dates[i,1], real_dates[i,2], real_dates[i,3], sep = '-')
}

date_strings = as.Date(date_strings, '%m-%d-%y')
den_data$`Month and Year` = date_strings

#change string vars to factors, and parking to numeric
den_data$Month = factor(den_data$Month, levels = month.abb)
den_data$`Cannabis?` = as.factor(den_data$`Cannabis?`) 
den_data$Parking = as.numeric(den_data$Parking)
str(den_data)

#there is a big outlier in the concession data. -3Mil. It appears to be a mistaken negative as 3Mil would fit with the data so I will use absolute value.
which(den_data$Concession == min(den_data$Concession))
den_data[18, 8] = abs(den_data[18,8])

#Parking NA to mean of Parking
str(den_data)
which(is.na(den_data$Parking))
den_data[41, 9] = mean(den_data$Parking, na.rm =TRUE)


#Create peak season indicator variable for summer months.
plot(den_data$Month, den_data$Concession, main = 'Month vs. Concession', xlab = 'Months', ylab = 'Concession ($)')
peak_months = c('May', 'Jun', 'Jul', 'Aug')
for(month in unique(den_data$Month)) {
  den_data['peak'] = ifelse(den_data$Month== 'May' | den_data$Month == 'Jun' | den_data$Month == 'Jul' | den_data$Month == 'Aug' | den_data$Month == 'Sep',1,0)
}
str(den_data)
den_data = na.omit(den_data)


#Remove first 3 rows to get rid of NAs in UMCSENTLag columns.
first_data = na.omit(den_data)
str(first_data)


plot(den_data$`Month and Year`, den_data$`Rental Car`)
######################################################################################################
#First try model with original vars.
first_try = lm(`Rental Car` ~ ., data = first_data)
summary(first_try)
first_resids = residuals.lm(first_try)
plot(first_data$`Rental Car`, first_resids)
qqnorm(first_resids)
vif(first_try)
#adj_r2 = 0.82
#Significant vars:
#Month and year, Month, Concession, Parking, Cannabis?


#Use best subset method to find the best independent variables.
first_sub_full = regsubsets(`Rental Car` ~ ., data = first_data, nvmax = 27)
first_summary = summary(first_sub_full)
#Print adj r^2 value of each model.
first_summary$adjr2
first_summary$cp
#Pick subset with highest adj_r2
which(first_summary$adjr2 == max(first_summary$adjr2))
which(first_summary$cp == min(first_summary$cp))
first_summary

coef(first_sub_full, 17)
first_summary$adjr2[17]
coef(first_sub_full, 11)
first_summary$adjr2[11]

###########################################################################
#Best CP model with no transformations added:
initial_cols = c('Rental Car', 'Month', 'Enplaned', 'Deplaned', 'Originating', 'Parking', 'peak', 'UMCSENTLag1')
initial_best_data = first_data[initial_cols]
initial_max_model = lm(`Rental Car`~ ., data = initial_best_data)
summary(initial_max_model)
vif(initial_max_model)
initial_max_resids = residuals.lm(initial_max_model)
plot(initial_best_data$`Rental Car`, initial_max_resids)
qqnorm(initial_max_resids)
skewness(initial_max_resids)
kurtosis(initial_max_resids)
#Adj_r2 = 0.80
#Sig vars:
#Month, Originating



#Best adj_r2 model with no transformations added.
initial_cols = c('Ground', 'Month and Year', 'Month', 'Deplaned', 'Transfer',  'Concession', 'Parking', 'Rental Car', 'UMCSENTLag2', 'UMCSENTLag1', 'Destination')
initial_best_data = first_data[initial_cols]
initial_max_model = lm(`Rental Car` ~ ., data = initial_best_data)
summary(initial_max_model)
vif(initial_max_model)
initial_max_resids = residuals.lm(initial_max_model)
plot(initial_best_data$`Rental Car`, initial_max_resids)
qqnorm(initial_max_resids)
skewness(initial_max_resids)
kurtosis(initial_max_resids)
#Adj_r2 = 0.81
#Sig vars:
#Month, Deplaned, Transfer, Concession, Parking
#High VIF: Month and year, Deplaned, Transfer, Destination

#adj_r2 = .82
#Sig Vars:Monthand year, Rental Car
#High VIF:
#Month and year, Enplaned, Deplaned, Transfer, Originating



wanted_cols = c('Rental Car', 'Month', 'Deplaned', 'Transfer')
best_data = first_data[wanted_cols]
best_model = lm(`Rental Car` ~ ., data = best_data)
summary(best_model)
vif(best_model)
best_resids = residuals.lm(best_model)
plot(initial_best_data$`Rental Car`, best_resids)
qqnorm(best_resids)
skewness(best_resids)
kurtosis(best_resids)



test_data = den_data$`Rental Car`[60:63]
actual_values = test_data
actual_values
dates = den_data$`Month and Year`[60:63]
dates

predictions = predict.lm(best_model)
predictions = predictions[60:63]
predictions


plot(dates, actual_values, cex = 1.1, pch = 19, col = 'blue', main = 'Predictions vs. Actual', ylab = 'Values', xlab = 'Date')
points(dates, predictions, cex = 1.1, pch = 19, col = 'red')
legend('topleft', legend=c("Actual", "Lin Reg"), col=c('blue', "red"), pch = 19, cex=0.8)

print('Rental Car predictions')
print(predictions)



