#############Simple Regression #################
library(tidyverse)
library(caret)
library(MASS)
library(car)



train <- read.csv("D:/AUS/Program_Saves/R/STA301/Project/dataset/train.csv")
test <- read.csv("D:/AUS/Program_Saves/R/STA301/Project/dataset/test.csv")
str(train)
str(test)

train$Airline = as.factor(train$Airline)
train$Class = as.factor(train$Class)
train$Source = as.factor(train$Source)
train$Departure = as.factor(train$Departure)
train$Total_stops = as.factor(train$Total_stops)
train$Arrival = as.factor(train$Arrival)
train$Destination = as.factor(train$Destination)
train$period = as.factor(train$period)
train$day_type = as.factor(train$day_type)

str(train)

test$Airline = as.factor(test$Airline)
test$Class = as.factor(test$Class)
test$Source = as.factor(test$Source)
test$Departure = as.factor(test$Departure)
test$Total_stops = as.factor(test$Total_stops)
test$Arrival = as.factor(test$Arrival)
test$Destination = as.factor(test$Destination)
test$period = as.factor(test$period)
test$day_type = as.factor(test$day_type)

str(test)

set.seed(86528)

lm.fit = lm(Fare~Days_left, data=train)
summary(lm.fit)
str(summary(lm.fit))
summary(lm.fit)$adj.r.squared
summary(lm.fit)$r.squared
AIC(lm.fit)
BIC(lm.fit)

#plot(lm.fit)
yhat=predict(lm.fit,newdata=test)
plot(test$Fare, yhat) # y vs yhat
abline(0,1,col='red')

#install.packages("car")

bc <- boxcox(Fare~Days_left, data=train)
lambda <- bc$x[which.max(bc$y)]
lambda # lambda =  -0.1818182, hence we do log transform as it is close to 0

lm.fit.transform = lm(log(Fare)~Days_left, data=train)
summary(lm.fit.transform)
str(summary(lm.fit.transform))
summary(lm.fit.transform)$adj.r.squared
summary(lm.fit.transform)$r.squared
AIC(lm.fit.transform)
BIC(lm.fit.transform)
mean(lm.fit.transform$residuals^2)
#plot(lm.fit.transform)
yhat=predict(lm.fit.transform,newdata=test)
plot(test$Fare, exp(yhat)) # y vs yhat
abline(0,1,col='red')

lm.fit.quad = lm(log(Fare)~poly(Days_left,2), data=train)

summary(lm.fit.quad)
str(summary(lm.fit.quad))
summary(lm.fit.quad)$adj.r.squared
summary(lm.fit.quad)$r.squared
AIC(lm.fit.quad)
BIC(lm.fit.quad)
#plot(lm.fit.quad)
yhat=predict(lm.fit.quad,newdata=test)
plot(test$Fare, exp(yhat)) # y vs yhat
abline(0,1,col='red')

prediction_intervals <- predict(
  lm.fit.quad, 
  newdata = test, 
  interval = "prediction", 
  level = 0.95
)

exp(prediction_intervals)

confidence_intervals <- predict(
  lm.fit.quad, 
  newdata = test, 
  interval = "confidence", 
  level = 0.95
)
exp(confidence_intervals)

lm.fit.cube = lm(log(Fare)~poly(Days_left,3), data=train)
summary(lm.fit.cube)$adj.r.squared
summary(lm.fit.cube)$r.squared

anova(lm.fit.quad, lm.fit.cube) # according to anova model with cubic predictor is not significant

lm.fit.transform.2 = lm(log(Fare)~poly(Duration_in_hours,2), data=train)

summary(lm.fit.transform.2)
str(summary(lm.fit.transform.2))
summary(lm.fit.transform.2)$adj.r.squared
summary(lm.fit.transform.2)$r.squared
AIC(lm.fit.transform.2)
BIC(lm.fit.transform.2)
#plot(lm.fit.transform.2)
yhat=predict(lm.fit.transform.2,newdata=test)
plot(test$Fare, exp(yhat)) # y vs yhat
abline(0,1,col='red')

lm.fit.transform.3 = lm(log(Fare)~poly(Duration_in_hours,3), data=train)

summary(lm.fit.transform.3)
str(summary(lm.fit.transform.3))
summary(lm.fit.transform.3)$adj.r.squared
summary(lm.fit.transform.3)$r.squared
AIC(lm.fit.transform.3)
BIC(lm.fit.transform.3)
#plot(lm.fit.transform.3)
yhat=predict(lm.fit.transform.3,newdata=test)
plot(test$Fare, exp(yhat)) # y vs yhat
abline(0,1,col='red')

anova(lm.fit.transform.2, lm.fit.transform.3) # doing higher order is still significant but it complicates the model


################Binary Logistic Regression #############################
train <- read.csv("D:/AUS/Program_Saves/R/STA301/Project/dataset/train.csv")
test <- read.csv("D:/AUS/Program_Saves/R/STA301/Project/dataset/test.csv")

#write.csv(train, "D:/AUS/Program_Saves/R/STA301/Project/dataset/binary_logistic_test.csv", row.names=FALSE)

str(train)
str(test)

train$Airline = as.factor(train$Airline)
train$Class = as.factor(train$Class)
train$Source = as.factor(train$Source)
train$Departure = as.factor(train$Departure)
train$Total_stops = as.factor(train$Total_stops)
train$Arrival = as.factor(train$Arrival)
train$Destination = as.factor(train$Destination)
train$period = as.factor(train$period)
train$day_type = as.factor(train$day_type)

str(train)

test$Airline = as.factor(test$Airline)
test$Class = as.factor(test$Class)
test$Source = as.factor(test$Source)
test$Departure = as.factor(test$Departure)
test$Total_stops = as.factor(test$Total_stops)
test$Arrival = as.factor(test$Arrival)
test$Destination = as.factor(test$Destination)
test$period = as.factor(test$period)
test$day_type = as.factor(test$day_type)

str(test)

ticket_class <- character(length(train$Class))
for (i in seq_along(train$Class)) {
  if (train$Class[i] == "Business" || train$Class[i] == "First" || train$Class[i] == "Premium Economy" ) {
    ticket_class[i] <- "Premium"
  } else {
    ticket_class[i] <- "Economy"
  }
}
train <- cbind(ticket_class, train)
str(train)
summary(train)
train$ticket_class = as.factor(train$ticket_class)
train = train[,-c(3)] #removing the class that has all the classes
str(train)

ticket_class <- character(length(test$Class))
for (i in seq_along(test$Class)) {
  if (test$Class[i] == "Business" || test$Class[i] == "First" || test$Class[i] == "Premium Economy" ) {
    ticket_class[i] <- "Premium"
  } else {
    ticket_class[i] <- "Economy"
  }
}

test <- cbind(ticket_class, test)
str(test)
summary(test)
test$ticket_class = as.factor(test$ticket_class)
test = test[,-c(3)] #removing the class that has all the classes
str(test)



# this was a function I used in earlier semesters to print the confusion matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Economy', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Premium', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Economy', cex=1.2, srt=90)
  text(140, 335, 'Premium', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

glm.fit=glm(ticket_class~.,data=train,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,test, type="response")
glm.probs[1:10]
n=length(glm.probs)
glm.pred=rep("Economy",n)
glm.pred
glm.pred[glm.probs>.5]="Premium"
glm.pred
t = table(predict=glm.pred,actual=test$ticket_class)
t
cm <- confusionMatrix(t)
draw_confusion_matrix(cm)
#install.packages(pROC)
library("pROC")
myroc <- roc(test$ticket_class,predict(glm.fit, test, type = "response"))
plot(myroc)

coef(glm.fit)
fare_coef <- coef(glm.fit)["Fare"]
AirlineAirAsia_coef <- coef(glm.fit)["AirlineAirAsia"]
exp(fare_coef)
exp(AirlineAirAsia_coef)

#install.packages("tidyverse")

step.model <- glm.fit %>% stepAIC(trace = FALSE)
coef(step.model)

###########Ordinal Logistic Regression########################

train <- read.csv("D:/AUS/Program_Saves/R/STA301/Project/dataset/train.csv")
test <- read.csv("D:/AUS/Program_Saves/R/STA301/Project/dataset/test.csv")

str(train)
str(test)

train$Airline = as.factor(train$Airline)
train$Class = as.factor(train$Class)
train$Source = as.factor(train$Source)
train$Departure = as.factor(train$Departure)
train$Total_stops = as.factor(train$Total_stops)
train$Arrival = as.factor(train$Arrival)
train$Destination = as.factor(train$Destination)
train$period = as.factor(train$period)
train$day_type = as.factor(train$day_type)

str(train)

test$Airline = as.factor(test$Airline)
test$Class = as.factor(test$Class)
test$Source = as.factor(test$Source)
test$Departure = as.factor(test$Departure)
test$Total_stops = as.factor(test$Total_stops)
test$Arrival = as.factor(test$Arrival)
test$Destination = as.factor(test$Destination)
test$period = as.factor(test$period)
test$day_type = as.factor(test$day_type)

str(test)

ticket_class <- character(length(train$Class))
for (i in seq_along(train$Class)) {
  if (train$Class[i] == "Business" || train$Class[i] == "First") {
    ticket_class[i] <- "Premium"
  } else if(train$Class[i] == "Premium Economy"){
    ticket_class[i] <- "Premium Economy"
  }
  else{
    ticket_class[i] <- "Economy"
  }
}
train <- cbind(ticket_class, train)
str(train)
summary(train)
train$ticket_class = as.factor(train$ticket_class)
train = train[,-c(3)] #removing the class that has all the classes
str(train)

ticket_class <- character(length(test$Class))
for (i in seq_along(test$Class)) {
  if (test$Class[i] == "Business" || test$Class[i] == "First") {
    ticket_class[i] <- "Premium"
  } else if(test$Class[i] == "Premium Economy"){
    ticket_class[i] <- "Premium Economy"
  }
  else{
    ticket_class[i] <- "Economy"
  }
}

test <- cbind(ticket_class, test)
str(test)
summary(test)
test$ticket_class = as.factor(test$ticket_class)
test = test[,-c(3)] #removing the class that has all the classes
str(test)

ord_fit <- polr(ticket_class ~., data = train, Hess = TRUE)


summary(ord_fit)

ord_probs <- predict(ord_fit, newdata = test, type = "probs")

head(ord_probs)

ord_pred <- apply(ord_probs, 1, function(x) names(x)[which.max(x)])

head(ord_pred)

#this function waas used from https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
#we modified some parameters
conf_matrix <- function(df.true, df.pred, title = "", true.lab ="True Class", pred.lab ="Predicted Class",
                        high.col = 'blue', low.col = 'white') {
  #convert input vector to factors, and ensure they have the same levels
  df.true <- as.factor(df.true)
  df.pred <- factor(df.pred, levels = levels(df.true))
  
  #generate confusion matrix, and confusion matrix as a pecentage of each true class (to be used for color) 
  df.cm <- table(True = df.true, Pred = df.pred)
  df.cm.col <- df.cm / rowSums(df.cm)
  
  #convert confusion matrices to tables, and binding them together
  df.table <- reshape2::melt(df.cm)
  df.table.col <- reshape2::melt(df.cm.col)
  df.table <- left_join(df.table, df.table.col, by =c("True", "Pred"))
  
  #calculate accuracy and class accuracy
  acc.vector <- c(diag(df.cm)) / c(rowSums(df.cm))
  class.acc <- data.frame(Pred = "Class Acc.", True = names(acc.vector), value = acc.vector)
  acc <- sum(diag(df.cm)) / sum(df.cm)
  
  #plot
  ggplot() +
    geom_tile(aes(x=Pred, y=True, fill=value.y),
              data=df.table, size=0.2, color=grey(0.5)) +
    geom_tile(aes(x=Pred, y=True),
              data=df.table[df.table$True==df.table$Pred, ], size=1, color="black", fill = 'transparent') +
    scale_x_discrete(position = "top",  limits = c(levels(df.table$Pred), "Class Acc.")) +
    scale_y_discrete(limits = rev(unique(levels(df.table$Pred)))) +
    labs(x=pred.lab, y=true.lab, fill=NULL,
         title= paste0(title, "\nAccuracy ", round(100*acc, 1), "%")) +
    geom_text(aes(x=Pred, y=True, label=value.x),
              data=df.table, size=4, colour="black") +
    geom_text(data = class.acc, aes(Pred, True, label = paste0(round(100*value), "%"))) +
    scale_fill_gradient(low=low.col, high=high.col, labels = scales::percent,
                        limits = c(0,1), breaks = c(0,0.5,1)) +
    guides(size=F) +
    theme_bw() +
    theme(panel.border = element_blank(), legend.position = "bottom",
          axis.text = element_text(color='black'), axis.ticks = element_blank(),
          panel.grid = element_blank(), axis.text.x.top = element_text(angle = 30, vjust = 0, hjust = 0)) +
    coord_fixed()
  
} 

conf_matrix(test$ticket_class, ord_pred, title = "Conf. Matrix Example")

###############################################################################





