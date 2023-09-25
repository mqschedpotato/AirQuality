# Import dataset

data = read.csv(file.choose("") , header = TRUE)
View(data)

# Drop unused variable
data = subset(data, select = -c(X, X.1, Date, Time))
View(data)

# Check datatype
str(data)

# Treat missing value
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(data, 2, p)
data1 = na.omit(data)
apply(data1, 2, p)

dim(data1)

#Analyze using OLS model
library(lmtest)
OLS_model1 = lm(RH~CO.GT.+PT08.S1.CO.+NMHC.GT.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+PT08.S3.NOx.+NO2.GT.+PT08.S4.NO2.+PT08.S5.O3.+T+AH, data = data1)
round(OLS_model1$coefficients, 2)

summary(OLS_model1)

# Variabel yang tidak signifikan pada model ini adalah PT08.S5.O3. saja

OLS_model2 = lm(RH~CO.GT.+PT08.S1.CO.+NMHC.GT.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+PT08.S3.NOx.+NO2.GT.+PT08.S4.NO2.+T+AH, data = data1)
round(OLS_model2$coefficients, 2)

summary(OLS_model2)
# Variabel signifikan semua

OLS_MSE = mean(OLS_model2$residuals^2)
OLS_MSE

# Analyze using Bayesian method
library(rstanarm)
model1_Bayes = stan_glm(RH~CO.GT.+PT08.S1.CO.+NMHC.GT.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+PT08.S3.NOx.+NO2.GT.+PT08.S4.NO2.+PT08.S5.O3.+T+AH, 
                        data = data1)
round(model1_Bayes$coefficients, 2)
round(posterior_interval(model1_Bayes),2)

model2_Bayes = stan_glm(RH~PT08.S1.CO.+NMHC.GT.+C6H6.GT.+PT08.S2.NMHC.+NOx.GT.+PT08.S3.NOx.+NO2.GT.+PT08.S4.NO2.+T+AH, data = data1)
round(model2_Bayes$coefficients, 2)
round(posterior_interval(model2_Bayes),2)

Bayes_MSE1 = mean(model1_Bayes$residuals^2)
Bayes_MSE1
