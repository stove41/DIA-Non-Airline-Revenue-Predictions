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



#Remove first 3 rows to get rid of NAs in UMCSENTLag columns.
first_data = na.omit(den_data)
str(first_data)


plot(den_data$`Month and Year`, den_data$Parking)
######################################################################################################
#First try model with original vars.
first_try = lm(Parking ~ ., data = first_data)
summary(first_try)
first_resids = residuals.lm(first_try)
plot(first_data$Parking, first_resids)
qqnorm(first_resids)
vif(first_try)
skewness(first_resids)
kurtosis(first_resids)
#adj_r2 = 0.74
#Significant vars:
#Month, `Rental Car`, UMCSENT, UMCSENTLag1


#Use best subset method to find the best independent variables.
first_sub_full = regsubsets(Parking ~ ., data = first_data, nvmax = 27)
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
coef(first_sub_full, 9)
first_summary$adjr2[9]

###########################################################################
#Best CP model with no transformations added:
initial_cols = c('Parking', 'Month and Year', 'Month', 'Transfer', 'Destination', 'peak', 'UMCSENTLag3')
initial_best_data = first_data[initial_cols]
initial_max_model = lm(Parking ~ ., data = initial_best_data)
summary(initial_max_model)
vif(initial_max_model)
initial_max_resids = residuals.lm(initial_max_model)
plot(initial_best_data$Parking, initial_max_resids)
qqnorm(initial_max_resids)
skewness(initial_max_resids)
kurtosis(initial_max_resids)
#Adj_r2 = 0.69
#Sig vars:
# Month, Transfer



#Best adj_r2 model with no transformations added.
initial_cols = c('Parking', 'Month and Year', 'Month', 'Transfer', 'Destination', 'peak', 'Ground','UMCSENT', 'UMCSENTLag2', 'UMCSENTLag3')
initial_best_data = first_data[initial_cols]
initial_max_model = lm(Parking ~ ., data = initial_best_data)
summary(initial_max_model)
vif(initial_max_model)
initial_max_resids = residuals.lm(initial_max_model)
plot(initial_best_data$Parking, initial_max_resids)
qqnorm(initial_max_resids)
skewness(initial_max_resids)
kurtosis(initial_max_resids)
#Adj_r2 = 0.69
#Sig vars:
#Month and year, Month, Transfer, Ground



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
plot(den_data$Ground, den_data$Parking)
cor(den_data$Ground, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$Ground, i)
}
#Ground_inv2 and ground_inv had higher r_values. Adding to dataset.
ground_inv2 = bcPower(den_data$Ground, -2)
den_data['Ground_inv2'] = ground_inv2
ground_inv = bcPower(den_data$Ground, -1)
den_data['Ground_inv'] = ground_inv

#Concession
plot(den_data$Concession, den_data$Parking)
cor(den_data$Concession, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$Concession, i)
}
#Concession_inv2 and Concession_inv had higher r_values. Adding to dataset.
conc_inv2 = bcPower(den_data$Concession, -2)
den_data['Conc_inv2'] = conc_inv2
conc_inv = bcPower(den_data$Concession, -1)
den_data['Conc_inv'] = conc_inv

#Rental Car
plot(den_data$`Rental Car`, den_data$Parking)
cor(den_data$`Rental Car`, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$`Rental Car`, i)
}
#Rental_inv2 and rental_inv had higher r_values. Adding to dataset.
rental_inv2 = bcPower(den_data$`Rental Car`, -2)
den_data['Rental_inv2'] = rental_inv2
rental_inv = bcPower(den_data$`Rental Car`, -1)
den_data['Rental_inv'] = rental_inv


#Deplaned
plot(den_data$Deplaned, den_data$Parking)
cor(den_data$Deplaned, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$Deplaned, i)
}
#Deplan_inv2had higher r_values. Adding to dataset.
deplan_inv2 = bcPower(den_data$Deplaned, -2)
den_data['Deplan_inv2'] = deplan_inv2


#Enplaned
plot(den_data$Enplaned, den_data$Parking)
cor(den_data$Enplaned, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$Enplaned, i)
}
#Ground_inv2 and ground_inv had higher r_values. Adding to dataset.
enplan_inv2 = bcPower(den_data$Enplaned, -2)
den_data['Enplan_inv2'] = enplan_inv2
enplan_inv = bcPower(den_data$Enplaned, -1)
den_data['Enplan_inv'] = enplan_inv


#Destination
plot(den_data$Destination, den_data$Parking)
cor(den_data$Destination, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$Destination, i)
}
#Dest_inv2 and dest_inv had higher r_values. Adding to dataset.
dest_inv2 = bcPower(den_data$Destination, -2)
den_data['Dest_inv2'] = dest_inv2
dest_inv = bcPower(den_data$Destination, -1)
den_data['Dest_inv'] = dest_inv


#Originating
plot(den_data$Parking, den_data$Originating)
cor(den_data$Parking, den_data$Originating)
for(i in xforms){
    transform(den_data$Parking, den_data$Originating, i)
}
#Originating^2 transformation had the best r value. Adding to dataset.
orig_inv2 = bcPower(den_data$Originating, -2)
den_data['Orig_inv2'] = orig_inv2
orig_inv = bcPower(den_data$Originating, -1)
den_data['Orig_inv'] = orig_inv
orig_sqrt = bcPower(den_data$Originating, .5)
den_data['Orig_sqrt'] = orig_sqrt


#Transfer
plot(den_data$Parking, den_data$Transfer)
cor(den_data$Parking, den_data$Transfer)
for(i in xforms){
    transform(den_data$Parking, den_data$Transfer, i)
}
#None of the transformations had a high r2


#Origin + Destin
plot(den_data$Parking, den_data$`Origin + Destin`)
cor(den_data$Parking, den_data$`Origin + Destin`)
for(i in xforms){
    transform(den_data$Parking, den_data$`Origin + Destin`, i)
}
#Originating_inv2, inv, and sqrt transformation had the best r values. Adding to dataset.
orig_dest_inv2 = bcPower(den_data$`Origin + Destin`, -2)
den_data['Orig_dest_inv2'] = orig_dest_inv2
orig_dest_inv = bcPower(den_data$`Origin + Destin`, -1)
den_data['Orig_dest_inv'] = orig_dest_inv
orig_dest_sqrt = bcPower(den_data$`Origin + Destin`, .5)
den_data['Orig_dest_sqrt'] = orig_dest_sqrt


#UMCSENT
plot(den_data$Parking, den_data$UMCSENT)
cor(den_data$Parking, den_data$UMCSENT)
for(i in xforms){
    transform(den_data$Parking, den_data$UMCSENT, i)
}
#UMCSENT^-2, inv, and log transformations had the best r value. Adding to dataset.
umcsent_inv2 = bcPower(den_data$UMCSENT, -2)
den_data['UMCSENT_inv2'] = umcsent_inv2
umcsent_inv = bcPower(den_data$UMCSENT, -1)
den_data['UMCSENT_inv'] = umcsent_inv
umcsent_log = bcPower(den_data$UMCSENT, 0)
den_data['UMCSENT_log'] = umcsent_log

#Create new data columns
#Total passengers
den_data['total'] = den_data$Transfer + den_data$`Origin + Destin`
plot(den_data$total, den_data$Parking)
cor(den_data$total, den_data$Parking)
for(i in xforms){
    transform(den_data$Parking, den_data$total, i)
}
#Total_inv2 and inv had the best r values. Adding to dataset.
total_inv2 = bcPower(den_data$total, -2)
den_data['total_inv2'] = total_inv2
total_inv = bcPower(den_data$total, -1)
den_data['total_inv'] = total_inv
str(den_data)


#######################################################################
#Try new model with additional variables.
second_data = na.omit(den_data)
str(second_data)
second_try = lm(Parking ~ ., data = second_data)
summary(second_try)
vif(second_try)
#Adj_r2 = 0.74
#Sig vars: Month, UMCSENT, UMCSENTLag1, UMCSENT_inv2, UMCSENT_inv, UMCSENT_log

############################################################
#Use best subsets to determine best variables.
second_sub_full = regsubsets(Parking ~ ., data = second_data, nvmax = 45, really.big = TRUE)
second_summary = summary(second_sub_full)
#Print adj r^2 value of each model.
second_summary$adjr2
second_summary$cp
#Pick subset with highest adj_r2
which(second_summary$adjr2 == max(second_summary$adjr2))
which(second_summary$cp == min(second_summary$cp))
second_summary

coef(second_sub_full, 25)
second_summary$adjr2[25]
coef(second_sub_full, 10)
second_summary$adjr2[10]
#Best adj_r2 model used:
#Month and year, Month, Enplaned, Deplaned, Transfer, Originating, Rental Car, Ground, UMCSENT_all, Ground_inv, Rental_inv, UMCSENT_inv2.
#Adj_r2 = .74
#Best CP model used:
#Month and year, Month
#Adj_r2 = .62

#Adj_r2 wanted cols
wanted_cols = c('Month and Year', 'Month', 'Parking', 'Enplaned', 'Deplaned', 'Transfer', 'Originating', 'Rental Car', 'Ground', 'UMCSENT', 'UMCSENTLag2', 'UMCSENTLag3', 'UMCSENTLag1', 'Ground_inv', 'Rental_inv', 'UMCSENT_inv2')
second_data = den_data[wanted_cols]
second_data = na.omit(second_data)
str(second_data)
second_max_model = lm(Parking ~ ., data = second_data)
summary(second_max_model)
vif(second_max_model)
second_max_resids = residuals.lm(second_max_model)
plot(second_data$Parking, second_max_resids)
qqnorm(second_max_resids)
skewness(second_max_resids)
kurtosis(second_max_resids)
#adj_r2 = .74
#Sig Vars:
#Month and year, Month, 'Rental Car', UMCSENTLag1'


####################################################
#Best Model
den_data = na.omit(den_data)
str(den_data)
wanted_cols = c('Month and Year', 'Month', 'Parking', 'Ground', 'Enplaned', 'Destination')
best_data = den_data[wanted_cols]
best_model = lm(Parking~., data = best_data)
summary(best_model)
vif(best_model)
best_resids = residuals.lm(best_model)
plot(best_data$Parking, best_resids)
qqnorm(best_resids)
skewness(best_resids)
kurtosis(best_resids)

test_data = den_data$Parking[60:63]
actual_values = test_data
actual_values
dates = den_data$`Month and Year`[60:63]
dates

predictions = predict.lm(best_model)
predictions = predictions[60:63]
predictions

plot(dates, actual_values, cex = 1.1, pch = 19, col = 'blue', main = 'Predictions vs. Actual', ylab = 'Values', xlab = 'Date')
points(dates, predictions, cex = 1.1, pch = 19, col = 'red')
legend('topleft', legend=c("Actual", "Lin Reg", 'HW'), col=c('blue', "red", "green"), pch = 19, cex=0.8)

print('Parking predictions')
print(predictions)

