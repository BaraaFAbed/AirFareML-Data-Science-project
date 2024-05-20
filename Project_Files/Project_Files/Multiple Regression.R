# Multiple Regression #######################################################

#install.packages("caret")
library(caret)

setwd(r"(D:\Documents\AUS Spring 2024\STA 301\Final project\Airfare dataset)")
# airfares <- read.csv("Cleaned_dataset.csv")
# str(airfares)
# 
# # Encoding Date_of_journey into periods of two weeks
# 
# # Step 1: Convert to date format
# airfares$Date_of_journey <- as.Date(airfares$Date_of_journey)
# 
# # Step 2: Create the encoding function
# encode_date <- function(date) {
#   d1=as.Date("2023-01-16")
#   d2=as.Date("2023-01-31")
#   d3=as.Date("2023-02-01")
#   d4=as.Date("2023-02-15")
#   d5=as.Date("2023-02-16")
#   d6=as.Date("2023-02-28")
#   d7=as.Date("2023-03-01")
#   d8=as.Date("2023-03-15")
#   if (date >= d1 & date <= d2) {
#     return("Jan_16-31")
#   } else if (date >= d3 & date <= d4) {
#     return("Feb_1-15")
#   } else if (date >= d5 & date <= d6) {
#     return("Feb_16-28")
#   } else if (date >= d7 & date <= d8) {
#     return("Mar_1-15")
#   }
#   return(as.character(date)) # Return original date if not matching conditions
# }
# 
# # Step 3: Apply the encoding function
# airfares$period <- sapply(airfares$Date_of_journey, encode_date)
# unique(airfares$period)
# str(airfares)
# 
# # Encoding Journey_day as weekend/workday
# 
# airfares$day_type = ifelse(
#   airfares$Journey_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
#   "Workday", "Weekend")
# str(airfares)
# 
# # Removing Date_of_journey, Journey_day, Flight_code
# airfares = airfares[,-c(1, 2, 4)]
# str(airfares)

#write.csv(airfares, "modified_dataset.csv", row.names = FALSE)

airfares <- read.csv("modified_dataset.csv")
str(airfares)
dim(airfares)

# Categorical variable encoding into dummy variables
airfares$Airline = as.factor(airfares$Airline)
airfares$Class = as.factor(airfares$Class)
airfares$Source = as.factor(airfares$Source)
airfares$Departure = as.factor(airfares$Departure)
airfares$Total_stops = as.factor(airfares$Total_stops)
airfares$Arrival = as.factor(airfares$Arrival)
airfares$Destination = as.factor(airfares$Destination)
airfares$period = as.factor(airfares$period)
airfares$day_type = as.factor(airfares$day_type)
str(airfares)

contrasts(airfares$period)
contrasts(airfares$day_type)

summary(airfares$Fare) # median less than mean, indicating right skewed
hist(airfares$Fare) # yes, we can see right skewed

dim(airfares) # 1 target, 12 variables total, 452088 observations
sum(is.na(airfares)) # there are no missing values

# Sampling for train/test split

set.seed(86528)

s <- createDataPartition(y = airfares$Fare, p = 0.5,list = FALSE)
#s <- createDataPartition(y = airfares$Fare, p = 0.8,list = FALSE)
train <- airfares[s,]
test <- airfares[-s,]
dim(train)
dim(test)

#write.csv(train, "train.csv", row.names = FALSE)
#write.csv(test, "test.csv", row.names = FALSE)


# Perform best subsets regression (maximum 20 dummy variables limit)
# With interaction and polynomial terms added

#install.packages("leaps")
library(leaps)

regfit.full <- regsubsets(Fare ~ .-Days_left-Duration_in_hours+poly(Days_left,2)+poly(Duration_in_hours,2)+Airline*Class+day_type*Class, train, really.big=T) # default is 8 variables
# function above shows best models for 1, 2, 3, ..., 8 variables
# using the best subset algorithm
summary(regfit.full) # shows the best models
# regfit.full <- regsubsets(Fare ~ .-Days_left-Duration_in_hours+poly(Days_left,2)+poly(Duration_in_hours,2)+Airline*Class+day_type*Class, train,really.big=T,
#                           nvmax = 15)
reg.summary <- summary(regfit.full)
reg.summary

coef(regfit.full, best_num_of_vars_bic) # coefficients of best model
names(reg.summary) # shows variables of the summary e.g. R^2, adjusted R^2, BIC, Cp
reg.summary$rsq # viewing R^2 values of the summary
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
best_num_of_vars_bic = which.min(reg.summary$bic)
best_num_of_vars_bic

which.max(reg.summary$rsq)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

# plotting the model performance metrics vs number of variables

plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")

plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
best_num_of_vars_rsq = which.max(reg.summary$adjr2) # shows which model highest adjusted R^2
best_num_of_vars_rsq
points(best_num_of_vars_rsq, reg.summary$adjr2[best_num_of_vars_rsq], col = "red", cex = 2, 
       pch = 20) # create a red dot

plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
best_num_of_vars_bic = which.min(reg.summary$bic) # shows which model lowest BIC (best)
best_num_of_vars_bic
points(best_num_of_vars_bic, reg.summary$bic[best_num_of_vars_bic], col = "red", cex = 2,
       pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
best_num_of_vars_cp = which.min(reg.summary$cp) # shows which model lowest Cp (best)
best_num_of_vars_cp
points(best_num_of_vars_cp, reg.summary$cp[best_num_of_vars_cp], col = "red", cex = 2,
       pch = 20)

# below is used to fix plot margins too large error
par(mfrow=c(1,1))

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "bic")


# Perform stepwise, forward, and backward selection
# With interaction and polynomial terms added

#install.packages("leaps")
library(leaps)

# Stepwise
regfit.step <- regsubsets(Fare ~ .-Days_left-Duration_in_hours+poly(Days_left,2)+poly(Duration_in_hours,2)+Airline*Class+day_type*Class, train,
                         method = "seqrep")
reg.summary = summary(regfit.step)
reg.summary
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
step_best_num_of_vars_bic = which.min(reg.summary$bic)
step_best_num_of_vars_bic

# Forward
regfit.fwd <- regsubsets(Fare ~ .-Days_left-Duration_in_hours+poly(Days_left,2)+poly(Duration_in_hours,2)+Airline*Class+day_type*Class, train,
                         method = "forward")
reg.summary = summary(regfit.fwd)
reg.summary
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
fwd_best_num_of_vars_bic = which.min(reg.summary$bic)
fwd_best_num_of_vars_bic

# Backward
regfit.bwd <- regsubsets(Fare ~ .-Days_left-Duration_in_hours+poly(Days_left,2)+poly(Duration_in_hours,2)+Airline*Class+day_type*Class, train,
                         method = "backward")
reg.summary = summary(regfit.bwd)
reg.summary
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
bwd_best_num_of_vars_bic = which.min(reg.summary$bic)
bwd_best_num_of_vars_bic

# Coefficients of best model according to best subset, stepwise, forward, backward
coef(regfit.full, best_num_of_vars_bic)
coef(regfit.step, step_best_num_of_vars_bic)
coef(regfit.fwd, fwd_best_num_of_vars_bic)
coef(regfit.bwd, bwd_best_num_of_vars_bic)

# all methods agree on the significant variables

# so from this, if we consider any sig dummy as the whole variable sig,
# we have sig variables as airline, class, source, total stops, 
# destination, poly days left, airline*class


# Build the best model and evaluate regression assumptions ####################

# The best model includes the variables as stated above: 
# airline, class, source, total stops, 
# destination, poly days left, airline*class 


lm.fit = lm(Fare~Airline+Class+Source+Total_stops+
              Destination+poly(Days_left,2)+
              Airline*Class, data=train)
summary(lm.fit)
str(summary(lm.fit))
summary(lm.fit)$adj.r.squared
AIC(lm.fit)
BIC(lm.fit)
#install.packages("regclass")
# library(regclass)
# VIF(lm.fit)
names(coef(lm.fit)) # 52 dummy variables in total

# calculate Rsq and adjusted Rsq based on test data
preds=predict(lm.fit,newdata=test)
yhat=preds
SST=sum((test$Fare - mean(test$Fare))^2)
SSE=sum((test$Fare - yhat)^2)
R2=1-(SSE/SST)
R2
n = nrow(test)
p=length(lm.fit$coefficients) - 1
R2adj = 1 - ((1-R2) * (n-1)/(n-p-1))
R2adj

# residual plots
plot(lm.fit)

# y vs yhat
plot(test$Fare, yhat)
abline(0,1,col='red')

# evaluation of assumptions:
# normality: no (heavy tails)
# const variance: no (fanning out)
# independence: no (due to pattern)
# zero mean: no (due to outliers)
# linearity: no (due to outliers)

# To remedy the normality, we perform box cox transformation

# Find optimal lambda for box cox transformation
library(MASS)
bc <- boxcox(Fare~Airline+Class+Source+Total_stops+
               Destination+poly(Days_left,2)+
               Airline*Class, data=train)
lambda <- bc$x[which.max(bc$y)]
lambda

# we can see lambda = 0.14, which is very close to 0.
# Therefore we perform log transformation on the response variable

# fit new linear regression model using the log transformation
lm.fit.new = lm(log(Fare)~Airline+Class+Source+Total_stops+
                  Destination+poly(Days_left,2)+
                  Airline*Class, data=train)
summary(lm.fit.new)
str(summary(lm.fit.new))
summary(lm.fit.new)$adj.r.squared
AIC(lm.fit.new)
BIC(lm.fit.new)
names(coef(lm.fit.new)) # 52 dummy variables in total
coef(lm.fit.new)

# calculate Rsq and adjusted Rsq based on test data
preds=predict(lm.fit.new,newdata=test)
yhat=exp(preds)
SST=sum((test$Fare - mean(test$Fare))^2)
SSE=sum((test$Fare - yhat)^2)
R2=1-(SSE/SST)
R2
n = nrow(test)
p=length(lm.fit.new$coefficients) - 1
R2adj = 1 - ((1-R2) * (n-1)/(n-p-1))
R2adj

# residual plots
plot(lm.fit.new)

# y vs yhat
plot(test$Fare, yhat)
abline(0,1,col='red')

# From the residual plots, we can now see that
# all of the assumptions have been satisfied!

# We can also see from y vs yhat plot that the model
# has much better predictive ability.

###############################################################################


# NOTE: below approach produced a bit lower adj Rsq, so we wont use the below model


# Now that we fixed the assumptions, we can re-do variable selection
# on the transformed response (log(Fare))

# regfit.full <- regsubsets(log(Fare) ~ .-Days_left-Duration_in_hours+poly(Days_left,2)+poly(Duration_in_hours,2)+Airline*Class+day_type*Class, train, really.big=T) # default is 8 variables
# reg.summary <- summary(regfit.full)
# reg.summary
# best_num_of_vars_bic = which.min(reg.summary$bic)
# coef(regfit.full, best_num_of_vars_bic)
# 
# # variables selected: airline, class, source, total stops, destination, 
# # days left (NOT POLY 2), airline*class
# 
# # fit transformed response with selected variables
# lm.fit.new2 = lm(log(Fare)~Airline+Class+Source+Total_stops+
#                   Destination+Days_left+
#                   Airline*Class, data=train)
# summary(lm.fit.new2)
# str(summary(lm.fit.new2))
# summary(lm.fit.new2)$adj.r.squared
# AIC(lm.fit.new2)
# BIC(lm.fit.new2)
# names(coef(lm.fit.new2)) # 52 dummy variables in total
# coef(lm.fit.new2)
# 
# residual plots
# plot(lm.fit.new2)
# yhat=predict(lm.fit.new2,newdata=test)
# plot(test$Fare, exp(yhat)) # y vs yhat
# abline(0,1,col='red')
