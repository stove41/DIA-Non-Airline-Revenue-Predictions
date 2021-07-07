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
str(den_data)


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

######################################################################################################
#First try model with original vars.
first_try = lm(Concession ~ ., data = first_data)
summary(first_try)
first_resids = residuals.lm(first_try)
plot(first_data$Concession, first_resids)
qqnorm(first_resids)
vif(first_try)
#adj_r2 = 0.75
#Significant vars:
#Month and year, Month


#Use best subset method to find the best independent variables.
first_sub_full = regsubsets(Concession ~ ., data = first_data, nvmax = 27)
first_summary = summary(first_sub_full)
#Print adj r^2 value of each model.
first_summary$adjr2
first_summary$cp
#Pick subset with highest adj_r2
which(first_summary$adjr2 == max(first_summary$adjr2))
which(first_summary$cp == min(first_summary$cp))
first_summary

plot(first_summary$adjr2, type = 'l', lwd = 3, main = 'Adjusted R^2', ylab = 'adj r^2')
points(11, first_summary$adjr2[11], cex =2, pch = 19, col = 'red')
plot(first_summary$cp, type = 'l', lwd = 3, main = 'Cp', ylab = 'Cp' )
points(6, first_summary$cp[6], cex =2, pch = 19, col = 'red')
coef(first_sub_full, 11)
first_summary$adjr2[11]
coef(first_sub_full, 6)
first_summary$adjr2[6]
###########################################################################
#Best CP model with no transformations added:
initial_cols = c('Concession', 'Month and Year', 'Month', 'Deplaned', 'UMCSENTLag3')
initial_best_data = first_data[initial_cols]
initial_max_model = lm(Concession ~ ., data = initial_best_data)
summary(initial_max_model)
vif(initial_max_model)
initial_max_resids = residuals.lm(initial_max_model)
plot(initial_best_data$Concession, initial_max_resids)
qqnorm(initial_max_resids)
skewness(initial_max_resids)
kurtosis(initial_max_resids)
#Adj_r2 = 0.76
#Sig vars:
#Month and year, Month, Deplaned
#High VIF: None.


#Best adj_r2 model with no transformations added.
initial_cols = c('Concession', 'Month and Year', 'Month', 'Deplaned', 'Ground','UMCSENTLag1', 'UMCSENTLag2', 'UMCSENTLag3')
initial_best_data = first_data[initial_cols]
initial_max_model = lm(Concession ~ ., data = initial_best_data)
summary(initial_max_model)
vif(initial_max_model)
initial_max_resids = residuals.lm(initial_max_model)
plot(initial_best_data$Concession, initial_max_resids)
qqnorm(initial_max_resids)
skewness(initial_max_resids)
kurtosis(initial_max_resids)
#Adj_r2 = 0.74
#Sig vars:
#Month and year, Month, Deplaned.
#High VIF: None.
#########################################################################################################################
#Neither model is great but at least all assumptions are met. We will try to transform the data to get a better adj_r2 value.

#########################################################################################################################

#Create function to do the specified transform on the selected data.
#Transforms are as follows:
# -2 = 1/x^-2
# -1 = 1/x
# -.5 = 1/sqrt(x)
# 0 = ln(x)
# .5 = sqrt(x)
# 1 = x
# 2 = x^2
#It then prints out the initial r value, and the r value after transformation.
transform = function(dep_var, ind_var, xform){
    print('Initial Correlation')
    print(cor(dep_var, ind_var))
    print(paste('Current transform: ', as.character(xform)))
    ind_var_xform = bcPower(ind_var, xform)
    print('Cor with X transformed')
    print(cor(dep_var, ind_var_xform))
}

#select transformations we want to try on each variable.
xforms = c(-2, -1, 0, 0.5, 2, 3)

#Variable transformations. Run each loop by itself and look through new r values.
#Ill change the function eventually to spit out transformation with highest r value.
#Ground
plot(den_data$Ground, den_data$Concession)
cor(den_data$Ground, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$Ground, i)
}
#All transformations had a lower r value.

#Parking
plot(den_data$Parking, den_data$Concession)
cor(den_data$Parking, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$Parking, i)
}
#All transformations had a lower r value.

#Rental Car
plot(den_data$`Rental Car`, den_data$Concession)
cor(den_data$`Rental Car`, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$`Rental Car`, i)
}
#All transformations had a lower r vlaue.

#Deplaned
plot(den_data$Deplaned, den_data$Concession)
cor(den_data$Deplaned, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$Deplaned, i)
}

#deplan^2 ^3 transformations had the best r value. Adding to dataset.
deplan_2 = bcPower(den_data$Deplaned, 2)
den_data['Deplan_2'] = deplan_2
deplan_3 = bcPower(den_data$Deplaned, 3)
den_data['Deplan_3'] = deplan_2


#Enplaned
plot(den_data$Enplaned, den_data$Concession)
cor(den_data$Enplaned, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$Enplaned, i)
}
#enplan^2 transformation had the best r value. Adding to dataset.
enplan_2 = bcPower(den_data$Enplaned, 2)
den_data['Enplan_2'] = enplan_2
enplan_3 = bcPower(den_data$Enplaned, 3)
den_data['Enplan_3'] = enplan_3


#Destination
plot(den_data$Destination, den_data$Concession)
cor(den_data$Destination, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$Destination, i)
}
#Destination^2 transformation had the best r value. Adding to dataset.
dest_2 = bcPower(den_data$Destination, 2)
den_data['Dest_2'] = dest_2
dest_3 = bcPower(den_data$Destination, 3)
den_data['Dest_3'] = dest_3


#Originating
plot(den_data$Concession, den_data$Originating)
cor(den_data$Concession, den_data$Originating)
for(i in xforms){
    transform(den_data$Concession, den_data$Originating, i)
}
#Originating^2 transformation had the best r value. Adding to dataset.
orig_2 = bcPower(den_data$Originating, 2)
den_data['Orig_2'] = orig_2
orig_3 = bcPower(den_data$Originating, 3)
den_data['Orig_3'] = orig_3


#Transfer
plot(den_data$Concession, den_data$Transfer)
cor(den_data$Concession, den_data$Transfer)
for(i in xforms){
    transform(den_data$Concession, den_data$Transfer, i)
}
#None of the transformations had a high r2


#Origin + Destin
plot(den_data$Concession, den_data$`Origin + Destin`)
cor(den_data$Concession, den_data$`Origin + Destin`)
for(i in xforms){
    transform(den_data$Concession, den_data$`Origin + Destin`, i)
}
#Originating^2 and ^3 transformation had the best r values. Adding to dataset.
orig_dest_2 = bcPower(den_data$`Origin + Destin`, 2)
den_data['Orig_dest_2'] = orig_dest_2
orig_dest_3 = bcPower(den_data$`Origin + Destin`, 3)
den_data['Orig_dest_3'] = orig_dest_3


#Month and year. time stamp, wont transform.
plot(den_data$`Month and Year`, den_data$Concession, type = 'l', main = 'Concession Income', xlab = 'Date', ylab = 'Concession ($)', lwd = 3)
cor(den_data$`Month and Year`, sqrt(den_data$Concession))

#UMCSENT
plot(den_data$Concession, den_data$UMCSENT)
cor(den_data$Concession, den_data$UMCSENT)
for(i in xforms){
    transform(den_data$Concession, den_data$UMCSENT, i)
}
#UMCSENT^-2 transformation had the best r value. Adding to dataset.
umcsent_inv2 = bcPower(den_data$UMCSENT, -2)
den_data['UMCSENT_inv2'] = umcsent_inv2

#Create new data columns
#Total passengers
den_data['total'] = den_data$Transfer + den_data$`Origin + Destin`
plot(den_data$total, den_data$Concession)
cor(den_data$total, den_data$Concession)
for(i in xforms){
    transform(den_data$Concession, den_data$total, i)
}
#Total^2 and^3 had the best r values. Adding to dataset.
total_2 = bcPower(den_data$total, 2)
den_data['total_2'] = total_2
total_3 = bcPower(den_data$total, 3)
den_data['total_3'] = total_3
str(den_data)



#######################################################################
#Try new model with additional variables.
second_data = na.omit(den_data)
str(second_data)
second_try = lm(Concession ~ ., data = second_data)
summary(second_try)
vif(second_try)
#Adj_r2 = 0.74
#Sig vars: None

############################################################
#Use best subsets to determine best variables.
second_sub_full = regsubsets(Concession ~ ., data = second_data, nvmax = 32)
second_summary = summary(second_sub_full)
#Print adj r^2 value of each model.
second_summary$adjr2
second_summary$cp
#Pick subset with highest adj_r2
which(second_summary$adjr2 == max(second_summary$adjr2))
which(second_summary$cp == min(second_summary$cp))
second_summary


plot(second_summary$adjr2, type = 'l')
points(16, second_summary$adjr2[16])
plot(second_summary$cp, type = 'l')
points(7, second_summary$cp[7])
coef(second_sub_full, 16)
second_summary$adjr2[16]
coef(second_sub_full, 7)
second_summary$adjr2[7]
#Best adj_r2 model used:
#Month and year, Month, Enplaned, Deplaned, Transfer, Originating, Rental Car, Ground, UMCSENT_all, Deplan_2, Enplan_2, Enplan_3, Dest_3, Orig_2.
#Adj_r2 = .78

#Adj_r2 wanted cols
wanted_cols = c('Month and Year', 'Month', 'Concession', 'Ground', 'UMCSENTLag2', 'UMCSENTLag3', 'UMCSENTLag1', 'Deplan_2', 'Enplan_2', 'Dest_2', 'Dest_3', 'UMCSENT_inv2', 'total_2', 'Origin + Destin')
#CP Wanted cols
wanted_cols = c('Month and Year', 'Month', 'Concession', 'Dest_2', 'Dest_3', 'peak')
second_data = den_data[wanted_cols]
second_data = na.omit(second_data)
str(second_data)
second_max_model = lm(Concession ~ ., data = second_data)
summary(second_max_model)
second_max_resids = residuals.lm(second_max_model)
plot(second_data$Concession, second_max_resids)
qqnorm(second_max_resids)
skewness(second_max_resids)
kurtosis(second_max_resids)




###################################################################################################
#Trial and error section. Created many models to try and find signifcant variables for final model.
###################################################################################################

#Use stepAIC to determine best model with variables from bestsubset. direction = 'both'
first_stepped_model = stepAIC(second_max_model)
summary(first_stepped_model)
first_model_resid = residuals.lm(first_stepped_model)
plot(second_data$Concession, first_model_resid)
qqnorm(first_model_resid)
vif(first_stepped_model)
skewness(first_model_resid)
kurtosis(first_model_resid)
#adj_r2 of .79.
#No variables are significant.
#High VIF:
#Enplaned, Deplaned, Enplan_2, Enplan_3

#Try same process without Enplaned to remove collinearity.
wanted_cols = c('Month and Year', 'Month', 'Concession', 'Deplaned', 'Transfer', 'Originating', 'Rental Car', 'Ground', 'UMCSENT', 'UMCSENTLag1', 'UMCSENTLag2', 'UMCSENTLag3', 'Deplan_2', 'Enplan_2', 'Enplan_3', 'Dest_3', 'Orig_2')
third_data = den_data[wanted_cols]
third_data = na.omit(third_data)
str(third_data)
third_max_model = lm(Concession ~ ., data = third_data)
summary(third_max_model)

#Use stepAIC to determine best model with variables from bestsubset. direction = 'both'
second_stepped_model = stepAIC(third_max_model)
summary(second_stepped_model)
second_model_resid = residuals.lm(second_stepped_model)
plot(third_data$Concession, second_model_resid)
qqnorm(second_model_resid)
vif(second_stepped_model)
#This model has an adj_r2 of .79
#Final vars:
#Month and Year, Month, Rental Car, Ground, UMCSENT, Enplan_3, Dest_3, Orig_2.
#Significan vars:
#Month and Year, Rental Car, Enplan_3, Dest_3, Orig_2
#High VIF:
#Orig_2, Dest_3, Enplan_3

#Try same process without Orig_2 to remove collinearity.
wanted_cols = c('Month and Year', 'Month', 'Concession', 'Deplaned', 'Transfer', 'Originating', 'Rental Car', 'Ground', 'UMCSENT', 'UMCSENTLag1', 'UMCSENTLag2', 'UMCSENTLag3', 'Deplan_2', 'Enplan_2', 'Enplan_3', 'Dest_3')
fourth_data = den_data[wanted_cols]
fourth_data = na.omit(fourth_data)
str(fourth_data)
fourth_max_model = lm(Concession ~ ., data = fourth_data)
summary(fourth_max_model)

#Use stepAIC to determine best model with variables from bestsubset. direction = 'both'
third_stepped_model = stepAIC(fourth_max_model)
summary(third_stepped_model)
third_model_resid = residuals.lm(third_stepped_model)
plot(fourth_data$Concession, third_model_resid)
qqnorm(third_model_resid)
vif(third_stepped_model)
#adj_r2 = .77
#Sig vars:
#Month and year, Originating, Rental Car, Enplan_3, Dest_3.
#High VIF:
#Originating, Enplan_3, Dest_3.



######################################################################################
#Model Selection
######################################################################################

str(den_data)
#Choose only significant vars for next model.
wanted_cols = c('Concession', 'Month and Year', 'Month', 'Cannabis?', 'Rental Car', 'Ground', 'Orig_2', 'Orig_dest_3', 'Enplan_3', 'UMCSENT_inv2', 'UMCSENT')
new_best_data = den_data[wanted_cols]
new_best_data = na.omit(new_best_data)

#Create model with best subset of variables.
new_max_model = lm(Concession ~ ., data = new_best_data)
summary(new_max_model)
vif(new_max_model)
new_max_resids = residuals.lm(new_max_model)
plot(new_best_data$Concession, new_max_resids)
qqnorm(new_max_resids)
skewness(new_max_resids)
kurtosis(new_max_resids)
#adj_r^2 = .80
#Sig vars:
#Month and year, month, Rental Car, Enplan_3, Orig_2, Orig_dest_3
#High VIF:
#UMCSENT, Enplan_3, Orig_2, Orig_dest_3, UMCSENT_Inv2

######################################
#choose only significant vars.
wanted_cols = c('Concession', 'Month and Year', 'Month', 'Rental Car', 'Ground', 'Enplan_3', 'Orig_dest_3', 'Orig_2')
new_best_data = den_data[wanted_cols]
new_best_data = na.omit(new_best_data)

#Create model with best subset of variables.
new_max_model = lm(Concession ~ ., data = new_best_data)
summary(new_max_model)
vif(new_max_model)
new_max_resids = residuals.lm(new_max_model)
plot(new_best_data$Concession, new_max_resids)
qqnorm(new_max_resids)
skewness(new_max_resids)
kurtosis(new_max_resids)


######################################
#remove vars for colliniarity.
wanted_cols = c('Concession', 'Month and Year', 'Month', 'Enplan_3')
den_data = na.omit(den_data)
new_best_data = den_data[wanted_cols]
str(new_best_data)
#########################################################
#BEST MODEL
#########################################################
#Create model with best subset of variables.
new_max_model = lm(Concession ~ ., data = new_best_data)
summary(new_max_model)
vif(new_max_model)
new_max_resids = residuals.lm(new_max_model)
plot(new_best_data$Concession, new_max_resids, main = 'Residuals', ylab = 'Residuals', xlab = 'Concession ($)')
qqnorm(new_max_resids)
skewness(new_max_resids)
kurtosis(new_max_resids)

##########################################################################################
#Try time series forcast
library(forecast)
wanted_cols = c('Enplaned', 'Deplaned', 'Transfer', 'Originating', 'Destination', 'Concession', 'Parking', 'Rental Car', 'Ground', 'Origin + Destin', 'UMCSENTLag1', 'UMCSENT', 'UMCSENTLag2', 'UMCSENTLag3')

ts_data = den_data[wanted_cols]
train_data = ts_data[1:59,]
str(ts_data)
#Create time series
for(col in wanted_cols){
    train_data[col] = ts(train_data[col], frequency = 12)
}

str(train_data)
ts_data[60:63,]
den_data$`Month and Year`[60:63]
test_data = den_data$Concession[60:63]
test_data
str(train_data)


#HW smoothing
model = hw(train_data$Concession, h=7)
plot.ts(ts_data$Concession)
plot(model)
hwsummary = summary(model)
hwsummary$`Point Forecast`

###############################################################################
#Compare models and choose best.

actual_values = test_data
actual_values
dates = den_data$`Month and Year`[60:63]
dates

predictions = predict.lm(new_max_model)
predictions = predictions[60:63]
predictions

plot(dates, actual_values, cex = 1.1, pch = 19, col = 'blue', ylim = c(4800000, 5900000), main = 'Predictions vs. Actual', ylab = 'Values', xlab = 'Date')
points(dates, predictions, cex = 1.1, pch = 19, col = 'red')
points(dates, hwsummary$`Point Forecast`[4:7], cex = 1.1, pch = 19, col = 'green')
legend('topleft', legend=c("Actual", "Lin Reg", 'HW'), col=c('blue', "red", "green"), pch = 19, cex=0.8)


lin_resids = predictions - actual_values
hw_resids = hwsummary$`Point Forecast`[4:7] - actual_values

lin_resids
hw_resids

sse_lin = sum((lin_resids**2))
sse_hw = sum((hw_resids**2))
sse_lin
sse_hw

mse_lin = mean(lin_resids**2)
mse_hw = mean(hw_resids**2)

mse_hw
mse_lin
mse_lin < mse_hw

########################################################
#Linear regression residuals are less than time series forecast.
print('Concession predictions')
print(predictions)



conc_predictions = predictions
rental_predictions = c(5038066, 4277723, 4390172, 4982349)
parking_predictions = c(15193598, 15048828, 14641666, 14770359)
ground_predictions = c(988790, 999092, 1009062, 1019365)

Predictions = rbind(conc_predictions, rental_predictions, parking_predictions, ground_predictions)
preds_df <- data.frame(name=c("Concessions", "Rentals", 'Parking', 'Ground'), Predictions = Predictions)
names(preds_df) = c('Revenue Type', 'Mar-17', 'Apr-17', 'May-17', 'Jun-17')
preds_df


den_data['Total_rev'] = den_data$Concession + den_data$Parking + den_data$Ground + den_data$`Rental Car`

real_totals = den_data$Total_rev[60:63]
real_totals

pred_totals = conc_predictions + rental_predictions + parking_predictions + ground_predictions

totals_mat = rbind(pred_totals, real_totals)
totals_mat
totals_df <- data.frame(name=c("Predicted Revenue", 'Actual Revenue'), totals_mat)
names(totals_df) = c('Revenue Type', 'Mar-17', 'Apr-17', 'May-17', 'Jun-17')
totals_df


plot(den_data$`Month and Year`, den_data$Total_rev, main = 'Total Revenue by Year', xlab = 'Date', ylab = 'Revenue', cex = 1.2, pch = 19, col = 'blue')
plot(den_data$Month, den_data$Total_rev, main = 'Total Revenue by Month', xlab = 'Month', ylab = 'Revenue', cex = 1.2, pch = 19, col = 'blue')
