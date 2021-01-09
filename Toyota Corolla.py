# -*- coding: utf-8 -*-
"""
Created on Wed Nov  4 19:17:33 2020

@author: Shubham
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.formula.api as smf

price = pd.read_csv("D:\\Data Science study\\assignment\\Sent\\5\\ToyotaCorolla.csv",encoding="ISO-8859-1")
type(price)
# As per our instruction we only need to use the specific columns from the whole dataframes
#These columns are :- "Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight"
#Now lets keep only these 9 columns
price1 = price[['Price','Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight']]
type(price1)
price1.head(10)
# Lets check correlatiion matrix
price1.corr()

#Let's check scatterplot and Histogram for the Dataframe
import seaborn as sns
sns.pairplot(price1)
price1.columns

#Let's create the model now
import statsmodels.formula.api as smf

price_model = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price1).fit()
price_model.summary()

# From the summary we can say that the Doors values are not significant and affects the model we created
#Lets check the influencing values of the dataframe
import statsmodels.api as sm
sm.graphics.influence_plot(price_model)
# It shows that the 80th record is influencing our model so lets try to remove this and see the result
price_model1 = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price1.drop(price1.index[80],axis=0)).fit()
price_model1.summary()

# Significance level of the model has incresed but still its not up to our level so we will see if there is any other influencing factors still present 

sm.graphics.influence_plot(price_model1)
# We found that the 221 record is now the influencing record of our current data
# Lets remove it and try again

price_model2 = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price1.drop(price1.index[[80,221]],axis=0)).fit()
price_model2.summary()

# Now all the values are significant and we also have high R-Squared value too 
# Hence we will select this model for the final work

#Let's create the new dataframe without the influencing records for further actions
price2 = price1.drop(price1.index[[80,221]],axis=0)

price_FinalModel = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price2).fit()
price_FinalModel.summary()

#Lets predict the values from our model
price2.columns
price_pred = price_FinalModel.predict(price2[['Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight']])
price_pred
price_pred = price_FinalModel.predict(price2)
price_pred

# Confidence value
print(price_FinalModel.conf_int(0.01))

#Let's calculate Vif valuess of all the input variables
rsq_Age_08_04 = smf.ols('Age_08_04~KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price2).fit().rsquared
vif_Age_08_04 = 1/(1-rsq_Age_08_04)

rsq_KM = smf.ols('KM~Age_08_04+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price2).fit().rsquared
vif_KM = 1/(1-rsq_KM)

rsq_HP = smf.ols('HP~Age_08_04+KM+cc+Doors+Gears+Quarterly_Tax+Weight',data = price2).fit().rsquared
vif_HP = 1/(1-rsq_HP)

rsq_cc = smf.ols('cc~Age_08_04+KM+HP+Doors+Gears+Quarterly_Tax+Weight',data = price2).fit().rsquared
vif_cc = 1/(1-rsq_cc)

rsq_Doors = smf.ols('Doors~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight',data = price2).fit().rsquared
vif_Door = 1/(1-rsq_Doors)

rsq_Gears = smf.ols('Gears~Age_08_04+KM+HP+cc+Doors+Quarterly_Tax+Weight',data = price2).fit().rsquared
vif_Gears = 1/(1-rsq_Gears)

rsq_Quarterly_Tax = smf.ols('Quarterly_Tax~Age_08_04+KM+HP+cc+Doors+Gears+Weight',data = price2).fit().rsquared
vif_Quarterly_Tax = 1/(1-rsq_Quarterly_Tax)

rsq_Weight = smf.ols('Weight~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax',data = price2).fit().rsquared
vif_Weight = 1/(1-rsq_Weight)

#Storing VIF values
price2_vif = {'Variables':['Age_08_04','KM','HP','cc','Doors','Gears','Quarterly_Tax','Weight'],'VIF':['NA',vif_Age_08_04,vif_KM,vif_HP,vif_cc,vif_Door,vif_Gears,vif_Quarterly_Tax,vif_Weight]}

#Added variable plot
sm.graphics.plot_partregress_grid(price_FinalModel)

#Linearity
plt.scatter(price2.Price,price_pred,c="r");plt.xlabel("Obsered_values");plt.ylabel("Fitted_values")

#Normality plot for residuals
#Histogram
plt.hist(price_FinalModel.resid_pearson)

import pylab
import scipy.stats as st

#Checking residuals are normally distributed
st.probplot(price_FinalModel.resid_pearson,dist = "norm",plot = pylab)

#Homoscedasticity
#Residuals Vs Fitted values
plt.scatter(price_pred,price_FinalModel.resid_pearson,c="r"),plt.axhline(y=0,color='blue');plt.xlabel("fitted_vlues");plt.ylabel("residuals")

#Lets check the data by splitting it into train and test data

from sklearn.model_selection import train_test_split
price2_train,price2_test = train_test_split(price2,test_size = 0.2)

#preparing model on train data
model_train = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price2_train).fit()
model_train.summary()
#train data prediction
train_pred = model_train.predict(price2_train)            

#train residual values
train_resid = train_pred-price2_train.Price

#RMSE value for train data 
train_rmse = np.sqrt(np.mean(train_resid*train_resid))
train_rmse

#predict on test database
test_pred = model_train.predict(price2_test)

#Test residual values
test_resid = test_pred-price2_test.Price

#RMSE value for test data
test_rmse = np.sqrt(np.mean(test_resid*test_resid))
test_rmse
train_rmse

# The test RMSE and Train RMSE are very close to each other so we can say that our model performance is better and the final model will be

price_FinalModel = smf.ols('Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight',data = price2).fit()
