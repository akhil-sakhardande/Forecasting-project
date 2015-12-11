library(TSA) 
require("TSA") | install.packages("TSA")

# Setting your working directory
setwd("C:/Users/aksakhardande/Documents/Study/Indian School of Business/CBA 2015/Term 2/Forecasting Analytics 2/Group project")

file<-read.csv("time series_transformed.csv") 
file 

#datafull<-read.table("E:/MAT 353 Regression & Time Series/Time Series Project/TSproject 
#                     Full.txt",head=F, sep="") 
#datafull 


# create monthly time series data 
n <- length(file[,3]) 
enrol <- file[,3] 
#airfull<-datafull[,3] 


# plot time series
file.ts <- ts(file,frequency = 12, start = c(1998,1)) 
plot(file.ts,type="o",ylab="Enrollments(in thousands)",xlab="Year") 
title(main="Credit Card Enrollments", col.main="blue") 

#Shapiro-Wilk test to check for normality
install.packages("nortest")
library("nortest")
shapiro.test(file$Enrollments)

#Anderson-Darling normality test
library("nortest")
ad.test(file$Enrollments)

# Dividing the data in test and validation
length(file$X)
#179 records in Test and 19 in Validation
file_test = subset(file, file$Year %in% c("1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012"))
file_val = subset(file, file$Year %in% c("2013","2014"))
length(file_test$X)
length(file_val$X)

enrol <- file_test[,3]

##Model building data 
enrol.ts <- ts(enrol,frequency = 12, start = c(1998,1)) 
enrol.ts

##Original time series## 
##ARIMA(0,0,0)x(0,0,0)## 

model = arima(enrol.ts,order=c(0,0,0),seasonal=list(order=c(0,0,0),period=12)) 
model


qqnorm(window(rstandard(model),start=c(1998,1)),main="Q-Q Plot",col.main="green") 
qqline(window(rstandard(model),start=c(1998,1))) 
shapiro.test(residuals(model)) 

#plotting boxcox
BoxCox.ar(enrol.ts) 

#log timeseries 
enrol.log <- ts(log(enrol),frequency = 12, start = c(1998,1)) 


model1 = arima(log(enrol.ts),order=c(0,0,0),seasonal=list(order=c(0,0,0),period=12)) 
model1


qqnorm(window(rstandard(model1),start=c(1998,1)),main="Log transformed Q-Q Plot",col.main="green") 
qqline(window(rstandard(model1),start=c(1998,1))) 
shapiro.test(residuals(model1)) 


#Dickey-Fuller test## 
install.packages("fUnitRoots")
library("fUnitRoots")
adfTest(log(enrol.ts),lags=2,type = c("nc"))
adfTest(diff(diff(log(enrol.ts))),lags=2,type = c("nc"))


#Plot first seasonal and non-seasonal difference 
plot(diff(diff(log(enrol.ts)),lag=12),type="o",ylab="Log(Enrollments)",xlab="Year") 
title(main="Second order difference", col.main="green") 
plot(diff(log(enrol.ts),lag=12),type="o",ylab="Log(Enrollments)",xlab="Year") 
title(main="First order difference", col.main="green") 
plot(log(enrol.ts),type="o",ylab="Log(Enrollments)",xlab="Year") 
title(main="No difference", col.main="green") 


# plot sample ACF and FACP to determine the type of model to use 
par(mfrow=c(2,1)) 
acf(as.vector(log(enrol.ts),lag=12), lag.max=36) 
pacf(as.vector(log(enrol.ts),lag=12), lag.max=36) 


# Final Model 
# ARIMA(0,1,1)
# First seasonal & non-seasonal difference MA 
model2=arima(log(enrol.ts),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12)) 
model2

par(mfrow=c(2,1)) 
plot(window(rstandard(model2),start=c(1998,1)),xlab="Year",ylab='Standardized 
     Residuals',type='o',main="Residuals ARIMA(0,1,1)", col.main="green") 
abline(h=0) 

qqnorm(window(rstandard(model2),start=c(1998,1)),main='Normal Q-Q Plot ARIMA(0,1,1)',col.main="green") 
qqline(window(rstandard(model2),start=c(1998,1)))

par(mfrow=c(2,1)) 
acf(as.vector(window(rstandard(model2),start=c(1998,1))),lag.max=36,main="ACF ARIMA(0,1,1)", col.main="green") 
pacf(as.vector(window(rstandard(model2),start=c(1998,1))),lag.max=36,main="PACF ARIMA(0,1,1)", col.main="green") 

shapiro.test(residuals(model2)) 

# Residuals, residual ACF, Ljung-Box 
tsdiag(model2,gof=15,omit.initial=F) 
title(main="Residuals, Residuals ACF, Ljung-Box Test for Model 1", col.main="green") 


##Normality 
par(mfrow=c(2,2)) 
qqnorm(window(rstandard(m5w2),start=c(1996,1)),main='Normal Q-Q Plot for Model 
       1',col.main="blue") 
qqline(window(rstandard(m5w2),start=c(1996,1))) 
qqnorm(window(rstandard(m6w2),start=c(1996,1)),main='Normal Q-Q Plot for Model 
       2',col.main="blue") 

# forecasting 
plot(model2,n1=c(2012,1),n.ahead=12,xlab='Year',type='b', col="red",ylab='Credit card enrollments') 


###find forecasted MSE and PMAD 
#predict 12 steps ahead 
enrol.pred <-predict(model2,n.ahead=13)
enrol.pred
  
plot(model2,n.ahead=12,type='b', col="red",xlab='Year') 

abline(h=0) 


# Transform back to original model 
exp(enrol.pred$pred + (1/2)*(enrol.pred$se)^2) 


# Create lower and upper prediction interval bounds 
lower<-enrol.pred$pred-qnorm(0.975,0,1)*enrol.pred$se 
upper<-enrol.pred$pred+qnorm(0.975,0,1)*enrol.pred$se 
data.frame(Months=c(1:13),lower,upper) 


#Orignial intervals 
data.frame(Months=c(1:13),exp(lower),exp(upper)) 


# MODEL 3# 
# ARIMA(1,0,0) regular AR and sesonal difference 
model3=arima(log(enrol.ts),order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12)) 
model3 

par(mfrow=c(2,1)) 
plot(window(rstandard(model3),start=c(1998,1)),xlab="Year",ylab='Standardized Residuals',type='o',main="Residuals ARIMA(1,0,0)", col.main="green") 
abline(h=0) 

qqnorm(window(rstandard(model3),start=c(1998,1)),main='Normal Q-Q Plot ARIMA(1,0,0)',col.main="green") 
qqline(window(rstandard(model3),start=c(1998,1))) 

par(mfrow=c(2,1)) 
acf(as.vector(window(rstandard(model3),start=c(1998,1))),lag.max=36,main="ACF ARIMA(1,0,0)", col.main="green") 
pacf(as.vector(window(rstandard(model3),start=c(1998,1))),lag.max=36,main="PACF ARIMA(1,0,0)", col.main="green") 

shapiro.test(residuals(model3)) 


##forecasting 
par(mfrow=c(2,1)) 
plot(model3,n1=c(2013,1),n.ahead=12,xlab='Year',type='b', col="green", ylab='Enrollments') 
plot(m6w,n1=c(2008,1),n.ahead=24,xlab='Year',type='b',col="red", ylab='Air Traffic') 


par(mfrow=c(2,1)) 
plot(m6w,n1=c(2008,1),n.ahead=36,xlab='Year',type='b', col="red", ylab='Air Traffic') 
plot(m6w,n1=c(2008,1),n.ahead=48,xlab='Year',type='b', col="red", ylab='Air Traffic') 


###plot forecasting versus original 
par(mfrow=c(2,1)) 
plot(air.time,,type="o",ylab="Air Travel(in thousands)",xlab="Year") 
plot(m6w,n1=c(1996,1),n.ahead=12,xlab='Year',type='b', col="red", ylab='Air Traffic') 


###compare forecasted and predicted value 
fit6<-arima(log(enrol.ts),order=c(1,0,0),seasonal=list(order=c(0,1,0),period=12)) 
predict(fit6, n.ahead=12) 


###find forecasted MSE and PMAD 
#predict 12 steps ahead 
enrol.pred1 <- predict(model3,n.ahead=13)
enrol.pred1
plot(model3,n.ahead=12,type='b', col="red",xlab='Time') 
abline(h=0) 


# Transform back to original model 
b1 = exp(enrol.pred1$pred + (1/2)*(enrol.pred1$se)^2) 
b2 = data.frame(b1[2:13])
names(b2) <- "Pred_enrol"
b2

#100(1-alpha)% prediction intervals 


# Create lower and upper prediction interval bounds 
lower<-enrol.pred1$pred-qnorm(0.975,0,1)*enrol.pred1$se 
upper<-enrol.pred1$pred+qnorm(0.975,0,1)*enrol.pred1$se 
data.frame(Months=c(1:13),lower,upper) 


#Orignial intervals 
b3 = data.frame(Months=c(1:13),exp(lower),exp(upper)) 
b3
b3 <- b3[2:13,2:3]
names(b3) <- c("lower_bound","upper_bound")

Pred_model3 <- cbind(b2,b4,b3)
names(Pred_model3) <- c("Pred_enrol","Orig_enrol","lower_bound","upper_bound")

rownames(Pred_model3) <- 1:nrow(Pred_model3)
Pred_model3

#Models comparison## 


##Residuals, residual ACF, Ljung-Box 
tsdiag(m5w2,gof=15,omit.initial=F) 
title(main="Residuals, Residuals ACF, Ljung-Box Test for Model 1", col.main="blue") 
tsdiag(m6w2,gof=15,omit.initial=F) 
title(main="Residuals, Residuals ACF, Ljung-Box Test for Model 2", col.main="blue") 


##Normality 
par(mfrow=c(2,2)) 
qqnorm(window(rstandard(m5w2),start=c(1996,1)),main='Normal Q-Q Plot for Model 
       1',col.main="blue") 
qqline(window(rstandard(m5w2),start=c(1996,1))) 
qqnorm(window(rstandard(m6w2),start=c(1996,1)),main='Normal Q-Q Plot for Model 
       2',col.main="blue") 



qqline(window(rstandard(m6w2),start=c(1996,1))) 


shapiro.test(residuals(m5w2)) 
shapiro.test(residuals(m6w2)) 


##plot forecasting valued of both models versus original 
par(mfrow=c(3,1)) 
plot(log(air.time),,type="o",ylab="Air Travel(in thousands)",xlab="Year") 
title(main="Monthly Air Traffic January 1996 -December 2012", col.main="blue") 


plot(m5w,n1=c(1996,1),n.ahead=12,xlab='Year',type='b', col="red", ylab='Air Traffic(in 
     thousands)') 
title(main="Monthly Air Traffic January 1996 -December 2012 (Predicted with Model 1)", 
      col.main="blue") 


plot(m6w,n1=c(1996,1),n.ahead=12,xlab='Year',type='b', col="red", ylab='Air Traffic(in 
     thousands)') 
title(main="Monthly Air Traffic January 1996 -December 2012 (Predicted with Model 2)", 
      col.main="blue") 



####### Simulation

require("graphics")

#simulate 1000 records
model3.sim <- arima.sim(n = 1000, list(ar = c(0), ma = c(-0.5154,-0.9627)), sd = sqrt(0.01099))

#convert to antilog
model3.sim <- exp(model3.sim)
model3.sim

#plot
plot(model3.sim,main="Simulated values from the ARIMA model",col="blue")

