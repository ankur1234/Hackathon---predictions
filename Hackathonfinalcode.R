
#Set working directory
setwd()

###Forecasting method (CLOSE PRICE) ####

HackathonRound1 <- read.csv("./Data/HackathonRound1.csv")
traindata<-HackathonRound1[,c(1,2,8)]
check<-traindata[which(traindata$Share.Names=='Share1'),]

##Convert the data in time series format###
library(forecast)
y <- ts(check$Close.Price, frequency=7)

##AUTO ARIMA Model
arima<-auto.arima(y)
forecast<-forecast(arima,h=21)
plot(forecast)

pred<-data.frame(forecast)

write.csv(pred,'share1.csv')

###Forecasting method (OPEN PRICE) ####
traindata<-HackathonRound1[,c(1,2,4)]
check<-traindata[which(traindata$Share.Names=='Share1'),]
library(forecast)
y <- ts(check$Open.Price, frequency=7)

##AUTO ARIMA Model
arima<-auto.arima(y)
forecast<-forecast(arima,h=21)
plot(forecast)

pred<-data.frame(forecast)

write.csv(pred,'share1.csv')



model_ts <- group_by(traindata, Share.Names) %>% do(fit=ts(.$Close.Price, freq=7))

model_fits <- group_by(traindata, Share.Names) %>% do(fit=auto.arima(.$Close.Price))

model_fits[, sapply(model_fits$fit, function(x) plot(forecast(x, 21)))]


preds <-  traindata %>%    
  group_by(Share.Names) %>%   
  do( {
    xt <- ts( .[,3], frequency=12)
    fit <-     auto.arima(xt)
    pred <- forecast(fit, h=12)
    data.frame(.,pred)
  }) 

preds <-  traindata %>% group_by(Share.Names) %>%   
  do({
    y <- ts( .[,3], frequency=7)
    fit <-     auto.arima(y)
    pred <- forecast(fit, h=2)
    as.data.frame(.,pred)
  })


data_s<-split(traindata,traindata$Share.Names)
mod <- lapply(data_s, function(x) auto.arima(x$Close.Price))
res <- mapply(function(mod, data_s) forecast(mod,21), mod, data_s)
forecasts <- lapply(apply(res, 2, list), function(x) x[[1]]$upper)
pred<-as.data.frame(forecasts)




  
