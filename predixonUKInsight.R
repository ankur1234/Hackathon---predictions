

#Set working directory
setwd()

##Load libraries
library(forecast)
library(dplyr)
library(reshape2)
library(forecastHybrid)

###Forecasting method (CLOSE PRICE) ####
########################################

HackathonRound1 <- read.csv("./Data/HackathonRound1.csv")
dataupdate <- read.csv("./Data/DataUpdate_Hackathon.csv")
newdata<-rbind(HackathonRound1,dataupdate)

traindata<-newdata[,c(1,2,8)]

##Split the dataset by Share names
data_s<-split(traindata,traindata$Share.Names)

## Apply HYBRID model (Auto Arima and ETS)
mod <- lapply(data_s, function(x) hybridModel(ts(x$Close.Price),models="ae",weights="insample.errors"))

##Forecast for the next 4 trading days (Consider last two values for the final submission)
res <- mapply(function(mod, data_s) forecast(mod,4), mod, data_s)

## Extract the upper limit values
forecasts <- lapply(apply(res, 2, list), function(x) x[[1]]$upper)

##Convert into a data frame
pred<-as.data.frame(forecasts)

##Select the upper 95% confidence interval data points
pred<-select(pred,matches(".80."))

##Select the last last two days (3rd and 4th day share price)
pred<-pred[3:4,]

##Remove the '.95' from each column name
names(pred) = gsub(pattern = ".80.*", replacement = "", x = names(pred)) 

##Remove the 'Share' text from the column names (THIS WILL ENABLE TO FILTER the data)
names(pred) <- gsub( "Share",  "", names(pred), fixed = TRUE)

##Transpose the data frame
pred<-data.frame(t(pred))

##Save the file, name the Share column, filter by share number and reload it
#write.csv(clean,'clean.csv')
#clean <- read.csv("./Data/clean.csv")


## Save the file
write.csv(pred,'closeprice.csv')


###Forecasting method (OPEN PRICE) ####
#######################################
traindata<-newdata[,c(1,2,4)]

data_s<-split(traindata,traindata$Share.Names)

## Apply HYBRID model (Auto Arima and ETS)
mod <- lapply(data_s, function(x) hybridModel(ts(x$Open.Price),models="ae",weights="insample.errors"))

res <- mapply(function(mod, data_s) forecast(mod,4), mod, data_s)
forecasts <- lapply(apply(res, 2, list), function(x) x[[1]]$upper)
pred<-as.data.frame(forecasts)

##Select the upper 95% confidence interval data points
pred<-select(pred,matches(".80."))

##Select the last last two days (3rd and 4th day share price)
pred<-pred[3:4,]

##Remove the '.95' from each column name
names(pred) = gsub(pattern = ".80.*", replacement = "", x = names(pred)) 

##Remove the 'Share' text from the column names (THIS WILL ENABLE TO FILTER the data)
names(pred) <- gsub( "Share",  "", names(pred), fixed = TRUE)

##Transpose the data frame
pred<-data.frame(t(pred))


write.csv(pred,'openprice.csv')

###Consider the last two days share prices corresponding to each share and copy paste in the sumsbission file.
###We are considering the upper 95% confidence interval data points. 

##Plotting
#model_fits <- group_by(traindata, Share.Names) %>% do(fit=auto.arima(.$Close.Price))
#model_fits[, sapply(model_fits$fit, function(x) plot(forecast(x, 4)))]



