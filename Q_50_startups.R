# Problem Statement:- Prepare a prediction model for profit of 50_startups data.

Profit <- read.csv(file.choose()) # choose the Cars.csv data set
View(Profit)
attach(Profit)
summary(Profit)

pairs(Profit)    #Finding correlation between input and outputs of the given data to check the degree of correlation between them.

Profit$State <- NULL   # Deleting the states column because its not needed in our data analysis.

View(Profit)

cor(Profit)    # Correlation Coefficient matrix - Strength & Direction of Correlation

library(corpcor) 
cor2pcor(cor(Profit))    # Partial Correlation matrix - Pure Correlation  b/n the varibles

# The Linear Model of interest with all the columns
model.profit <- lm(Profit~.,data=Profit)

summary(model.profit)

summary(model.profit)$r.squared    # Getting R-squared value seperately

### Lets create the data frame containing the model name and R-squared values of those models.

Q1 <- data.frame("SN" = 1, "Model Name" = c("Linear model with all columns"), "Model's R_squared value" = summary(model.profit)$r.squared, stringsAsFactors = FALSE)

View(Q1)

# Multicollinearity check
# Model based on only Administration 
# The significance level of the Administration and Marketing spend is not up to the required level so we need to find out the responsible factors.
# Hence we are going to check and verify them individually.
model.profitA <- lm(Profit~Administration,data=Profit)   
# Model based on only Administration

summary(model.profitA) # Administration became insignificant

summary(model.profitA)$r.squared

#Adding the value of this R_squared to the table
Q1 <- rbind(Q1,list(2,"Model based on only Administration",summary(model.profitA)$r.squared))

# Model based on only Marketing spend
model.profitMS <- lm(Profit~Marketing.Spend,data = Profit)
summary(model.profitMS) # Marketing.Spend became significant

#Adding the value of this R_squared to the table
Q1 <- rbind(Q1,list(3,"Model based on only Marketing Spend",summary(model.profitMS)$r.squared))

# Model based on administration  and Marketing.Spend
model.profitAMS<-lm(Profit~Administration+Marketing.Spend,data=Profit)
summary(model.profitAMS) # Both became Insignificant

#Adding the value of this R_squared to the table
Q1 <- rbind(Q1,list(4,"Model based on Administration and Marketing Spend",summary(model.profitAMS)$r.squared))

# Applying VIF function on model built on all inputs
## Variance Inflation factor to check collinearity b/n variables 
library(car)
vif(model.profit) # Original model
## vif>10 then there exists collinearity among all the variables 
# since the vif<10, which signify that there is no collinearity between all the variables.

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.profit,id.n=2,id.cex=0.7)
#from the avplot we can say that the administration is making the least contribution to our model.

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.profit)
#it shows that 5 observations are the influencing observations in our data.
#but we can't delete them because its a loss of our data and we can further try to reduce the no. by looking for most influencing one from them.

library(carData)
library(car)
## plotting Influential measures 
windows()
influenceIndexPlot(model.profit,id.n=3) # index plots for infuence measures
influencePlot(model.profit,id.n=3) # A user friendly representation of the above
#From the above influence plots we can reduce the possible least contributing observations to 3.

# Regression after deleting the 50th observation, which is influential observation
model_1<-lm(Profit~.,data=Profit[-50,])
summary(model_1)

Q1<-rbind(Q1,list(5,"Model without 50th observation",summary(model_1)$r.squared))

# Regression after deleting the 50th & 49th Observations
model_2<-lm(Profit~.,data=Profit[-c(50,49),])
summary(model_2)

Q1<-rbind(Q1,list(6,"Model without 50 and 49th observation",summary(model_2)$r.squared))


#Regression after deleting the 50th,49th and 47th observation
model_3<-lm(Profit~.,data=Profit[-c(50,49,47),])
summary(model_3)

Q1 <- rbind(Q1,list(7,"model without 50,49 and 47th observation",summary(model_3)$r.squared))

#Regression after deleting the 50,49,47,46th observations
model_4<-lm(Profit~.,data=Profit[-c(50,49,47,46),])
summary(model_4)

Q1 <- rbind(Q1,list(8,"Model without 50,49,47 and 46th observations",summary(model_4)$r.squared))

#still Administration and Marketing Spend doesn't becomes significant in any model
#We can try deleting the Administration column from the data

model_Ad <- lm(Profit~.-Administration,data=Profit)
summary(model_Ad)

Q1 <- rbind(Q1,list(9,"without Administration column",summary(model_Ad)$r.squared))


plot(lm(Profit~.-Administration,data=Profit[-c(50),])) # 50
summary(lm(Profit~.-Administration,data=Profit[-c(50),]))

Q1 <- rbind(Q1,list(10,"without administration and 50th observation",summary(lm(Profit~.-Administration,data=Profit[-c(50),]))$r.squared))

plot(lm(Profit~.-Administration,data=Profit[-c(49,50),])) # 49,50
summary(lm(Profit~.-Administration,data=Profit[-c(49,50),]))

Q1 <- rbind(Q1,list(11,"without administration and 49,50th observation",summary(lm(Profit~.-Administration,data=Profit[-c(49,50),]))$r.squared))

plot(lm(Profit~.-Administration,data=Profit[-c(47,49,50),])) # 47,49,50
summary(lm(Profit~.-Administration,data=Profit[-c(47,49,50),]))

Q1 <- rbind(Q1,list(12,"without administration and 47,49,50th observation",summary(lm(Profit~.-Administration,data=Profit[-c(47,49,50),]))$r.squared))


plot(lm(Profit~.-Administration,data=Profit[-c(46,47,49,50),])) # 46,47,49,50
summary(lm(Profit~.-Administration,data=Profit[-c(46,47,49,50),]))

Q1 <- rbind(Q1,list(13,"without administration and 46,47,49,50th observation",summary(lm(Profit~.-Administration,data=Profit[-c(46,47,49,50),]))$r.squared))


# Its not a feasible solution if we remove all the influential values
# We need to consider other assumptions likes
# Heteroscadasticity | Normal Distribution of Residuals

finalmodel<-lm(Profit~.-Administration,data=Profit[-c(50),])
summary(finalmodel)

Q1 <- rbind(Q1,list(14,"Selected Final model",summary(finalmodel)$r.squared))

# Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution

#Now lets convert the Q1 dataframe into excel file

install.packages(writexl)
library(writexl)
write_xlsx(Q1,"D:\\Data Science study\\assignment\\Sent\\Q1 R Squared Value.xlsx")
