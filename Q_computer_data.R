# Problem Statement:-Predict price of the computer

Price <- read.csv(file.choose())

View(Price)
attach(Price)
summary(Price)
Price$X <- NULL
pairs(Price)   #we can say that the pairs function is not good enough to study the big data sets.

install.packages(mlr3)

library(mlr3)
price1 <- createDummyFeatures(Price,target = character(0L),method = "1-of-n",cols = NULL)
View(price1)
cor(price1)
summary(price1)

library(corpcor)
cor2pcor(cor(price1))   # Partial Correlation matrix - Pure Correlation  b/n the variables.

# The Linear Model of interest with all the columns

model.price <- lm(price~., data = price1)
summary(model.price)

## This model has the R-Squared value of 0.7756 and all the coefficients of the model are significant
##  But still we will try to check if there is any other way with which we can increase the R-Squares value.

 # Lets check the vif of the model
library(car)
vif(model.price)
 # Shows alias error hence

alias(lm(price~.,data = price1))
price1$cd.no <- NULL
price1$multi.no <- NULL
price1$premium.no <- NULL

# Back to VIF check and for that rebuild the model again 

model.price1 <- lm(price~., data = price1)

summary(model.price1)

# Still the same r-squared value withall the significant coefficients

vif(model.price1)

## If vif>10 then there exists collinearity among all the variables 
# since the vif<10, which signify that there is no collinearity between all the variables.

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.price1,id.n=2,id.cex=0.7)

#from the avplot we can say that the cd.yes is making the least contribution to our model.

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.price1)

# From this command we can't find the influence factor because of the large size of the data set
##  If you have any solution then you can help me with this

library(carData)
library(car)

windows()
influenceIndexPlot(model.price1,id.n=3)
influencePlot(model.price1)

# from above tests and plots I find out that the observation no.1441 and 1701 are most influencing
# Lets remove them and see if there are any changes to the model after that

# Model without obsevation no. 1441
model.price2 <- lm(price~.,data = price1[-1441,])
summary(model.price2)

# A very little improvement can be seen in the R-Squared Value 0.7767 from the above operation

# Let also remove the observation no. 1701

model.price3 <- lm(price~.,data = price1[-c(1441,1701),])
summary(model.price3)                                         

#R-Squared value improves again by little i.e 0.7777

#Lets try to create the model without all the influencing factors

model.price4 <- lm(price~.,data = price1[-c(1441,1701,3784,4478),])
summary(model.price4)

# We see the decrease in the R-Squared value than the increase hence i can conclude that
# the model no. 3 which is"model.price3" is the best one we ever got and we will go with the same.


## Final Prediction model is 

model.final <- lm(price~., data = price1[-c(1441,1701),])

# Lets Cross Validate our model 
# we are splitting the data into train and test
# First create the new data frame
price2 <- price1[-c(1441,1701),]

install.packages("caTools")
library(caTools)
split <- sample.split(price2$speed,SplitRatio = 0.70)
split
table(split)
price2_train <- subset(price2,split==TRUE)
price2_test <- subset(price2,split==FALSE)

# Building training model

model.train <- lm(price~.,data = price2)
summary(model.train)
sum(model.train$residuals)
mean(model.train$residuals)
training_RMSE <- sqrt(mean(model.train$residuals**2))
training_RMSE
plot(training_RMSE)

#predicting on test data

predtest <- predict(model.train,price2_test)
predtest
t
testing_errors <- price2_test$price-predtest
testing_errors

#calculating test RMSE

test_RMSE <- sqrt(mean(testing_errors**2))
test_RMSE     ## 269.2432
training_RMSE  ## 272.8675

# Final model
model.final <- lm(price~., data = price1[-c(1441,1701),])

# We can consider that our model is just fitting model
# if you think there are any corrections and improvements can be made then let me know and how can I do that.