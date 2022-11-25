#Set working directory
setwd("C:/Users/aborthakur/Desktop/Hackathon")

#Change the data format
d$betterdates<-as.Date(d$Date,format="%d-%b-%y")

#Load data
library(readr)
HackathonRound1 <- read.csv("C:/Users/aborthakur/Desktop/Hackathon/HackathonRound1.csv")
d<-HackathonRound1

#Fit linear models using dplyr package 
library(dplyr)
fitted_models<-d %>% group_by(Share.Names) %>% do(model = lm(Close.Price~betterdates, data = .))

#fitted_models$model

#Get the coefficients
library(broom)
fitted_models %>% tidy(model)
#rowwise(fitted_models) %>% tidy(model)

##Predict values
pred_values<-augment(fitted_models,model)

#Using data.table function
library(data.table)
d<-data.table(d)
d[,coef(lm(Close.Price~Average.Price)),by=Share.Names]

###Forecasting method (CLOSE PRICE) ####
##########################

HackathonRound1 <- read.csv("C:/Users/aborthakur/Desktop/Hackathon/HackathonRound1.csv")
traindata<-HackathonRound1[,c(1,2,8)]
check<-traindata[which(traindata$Share.Names=='Share1'),]
library(forecast)
y <- ts(check$Close.Price, frequency=7)
fit <- ets(y)
fc <- forecast(fit,h=21) ##Forecast for the next 25 days
plot(fc)

pred<-data.frame(fc)

write.csv(pred,'share1.csv')

accuracy(fc) ##Check accuracy

##AUTO ARIMA Model
arima<-auto.arima(y)
forecast<-forecast(arima,h=21)
plot(forecast)

##Check the AIC for best model
barplot(c(ETS=fit$aic, ARIMA=arima$aic),col="light blue",ylab="AIC") ## ARIMA is better


###Forecasting method (OPEN PRICE) ####
##########################

HackathonRound1 <- read.csv("C:/Users/aborthakur/Desktop/Hackathon/HackathonRound1.csv")
traindata<-HackathonRound1[,c(1,2,4)]
check<-traindata[which(traindata$Share.Names=='Share20'),]
library(forecast)
y <- ts(check$Open.Price, frequency=7)
fit <- ets(y)
fc <- forecast(fit,h=21) ##Forecast for the next 25 days
plot(fc)

pred<-data.frame(fc)

write.csv(pred,'share20.csv')






