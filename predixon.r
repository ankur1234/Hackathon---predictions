
setwd("C:/Users/aborthakur/Desktop/Analytics competition/Modelling")
train=read.csv("Train.csv")
train<-train[-c(1:4)]
#summary(train)


#Replace NA with zeroes (Use only if the NA's are not replaced with mean)
#train[is.na(train)]=0

#Replace the missing values in each column with the mean of that particular column 
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}

#AMELIA-Imputation of missing values
library(Amelia)
Amelia::AmeliaView()

##MICE (Get the train and test sample seperately)
library(mice)
#imputed_Data <- mice(train, m=5, maxit = 50, method = 'pmm', seed = 500)
#completeData<-complete(imputed_Data,1) # Extract 1st file out of 5 files

# Not working****
#modelfit <- with(data=imputed_Data,exp=lm(YPLL.Rate~Perc.Fair.Poor.Health+Physically.Unhealthy.Days+Mentally.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Obese+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Chlamydia.Rate+Teen.Birth.Rate+Perc.Uninsured+Pr.Care.Physician.Ratio+Dentist.Ratio+Prev.Hosp.Stay.Rate+Perc.Diabetic.Screening+Perc.Mammography+Perc.High.School.Grad+Perc.Some.College+Perc.Unemployed+Perc.Children.in.Poverty+Perc.No.Soc.Emo.Support+Perc.Single.Parent.HH+Violent.Crime.Rate+Avg.Daily.Particulates+Perc.pop.in.viol+Rec.Facility.Rate+Perc.Limited.Access+Perc.Fast.Foods))
#combine <- pool(modelfit)
#summary(combine)

#pool.r.squared(modelfit)
#pred<-predict(modelfit,test.dat)


#Split train into 80:20 (For checking the rsme)
#indexes<-sample(1:nrow(train), size=0.2*nrow(train))
#testdata<-train[indexes,]
#traindata<-train[-indexes,]
#rmse<-sqrt(mean((traindata$YPLL.Rate-pred)^2))

#Read test data
test=read.csv("Test.csv")
test.dat = test[-c(1:4)]

#Replace NA in test set with mean (If testset is used by manipulating with MICE or Amelia packages, don't use this)
#for(i in 1:ncol(test.dat)){
  test.dat[is.na(test.dat[,i]), i] <- mean(test.dat[,i], na.rm = TRUE)
}

#SVM
library(e1071)
model_svm <- svm(YPLL.Rate~. ,data=train)
pred_svm <- predict(model_svm, test.dat)

#Error test (Run this by changing the prediction above to traindata)
rmse<-sqrt(mean((train$YPLL.Rate-pred_svm)^2))

#Random forest
library(randomForest)
rf_fit<-randomForest(YPLL.Rate~.,data=train,ntree=500)
pred_rf <- predict(rf_fit, test.dat)

#Error test
rmse<-sqrt(mean((train$YPLL.Rate-pred_rf)^2))

#Linear model
model_lm<-lm(YPLL.Rate~.,data=train)
model_lm <- lm(YPLL.Rate~Physically.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Chlamydia.Rate+Perc.Uninsured+Pr.Care.Physician.Ratio+Prev.Hosp.Stay.Rate+Perc.Diabetic.Screening+Perc.High.School.Grad+Perc.Some.College+Perc.Children.in.Poverty+Perc.No.Soc.Emo.Support+Perc.pop.in.viol,data=train)
pred_lm <- predict(model_lm, test.dat)

#Error test
rmse<-sqrt(mean((train$YPLL.Rate-pred_lm)^2))

#Ensemble
#pred<-(pred_svm*2+pred_rf)/3
#pred<-(pred_svm+pred_rf)/2
pred<-(pred_lm+pred_rf*9)/10
#pred<-(pred_lm*4+pred_rf*6)/10

pred<-(pred_lm+pred_rf*9)/10


sample_submission=data.frame("ID"=test$ID,"Predicted"=pred)
write.csv(sample_submission,"Sample_submission.csv",row.names=F)









#####
#####............EXTRA MODELLING-DO NOT RUN.................####

library(Metrics)
rmse<-rmse(traindata$YPLL.Rate,predictedY)

#Neural network
library(nnet)
x <- traindata[,2:30]
y <- traindata[,1]

fit <- nnet(YPLL.Rate~.,traindata, size=12, maxit=500, linout=T, decay=0.01)
predictions <- predict(fit, x, type="raw")
rmse <- mean((y - predictions)^2)




#Linear Model
model <- lm(YPLL.Rate~., data=train)
summary(model)

model2<-lm(YPLL.Rate~Physically.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Chlamydia.Rate+Perc.Uninsured+Pr.Care.Physician.Ratio+Prev.Hosp.Stay.Rate+Perc.Diabetic.Screening+Perc.High.School.Grad+Perc.Some.College+Perc.Children.in.Poverty+Perc.No.Soc.Emo.Support+Perc.pop.in.viol,data=train)

model3<-lm(YPLL.Rate~Physically.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Teen.Birth.Rate+Perc.Uninsured+Pr.Care.Physician.Ratio+Prev.Hosp.Stay.Rate+Perc.Diabetic.Screening+Perc.High.School.Grad+Perc.Some.College+Perc.Children.in.Poverty+Perc.No.Soc.Emo.Support+Perc.Single.Parent.HH+Perc.pop.in.viol+Perc.Limited.Access,data=train)

#Log transformation
log <- function(x) ifelse(x <= 0, 0, base::log(x))
modelnew<-lm(YPLL.Rate~log(Perc.Fair.Poor.Health)+Physically.Unhealthy.Days+Mentally.Unhealthy.Days+log(Perc.Low.Birth.Weight)+log(Perc.Smokers)+log(Perc.Obese)+log(Perc.Physically.Inactive)+log(Perc.Excessive.Drinking)+log(MV.Mortality.Rate)+log(Chlamydia.Rate)+log(Teen.Birth.Rate)+log(Perc.Uninsured)+log(Pr.Care.Physician.Ratio)+log(Dentist.Ratio)+log(Prev.Hosp.Stay.Rate)+log(Perc.Diabetic.Screening)+log(Perc.Mammography)+log(Perc.High.School.Grad)+log(Perc.Some.College)+log(Perc.Unemployed)+log(Perc.Children.in.Poverty)+log(Perc.No.Soc.Emo.Support)+log(Perc.Single.Parent.HH)+log(Violent.Crime.Rate)+Avg.Daily.Particulates+log(Perc.pop.in.viol)+log(Rec.Facility.Rate)+log(Perc.Limited.Access)+log(Perc.Fast.Foods),data=traindata)

modelnew1<-lm(YPLL.Rate~Physically.Unhealthy.Days+log(Perc.Low.Birth.Weight)+log(Perc.Smokers)+log(Perc.Physically.Inactive)+log(Perc.Excessive.Drinking)+log(MV.Mortality.Rate)+log(Chlamydia.Rate)+log(Teen.Birth.Rate)+log(Perc.Uninsured)+log(Pr.Care.Physician.Ratio)+log(Perc.Diabetic.Screening)+log(Perc.Mammography)+log(Perc.High.School.Grad)+log(Perc.Children.in.Poverty)+log(Perc.No.Soc.Emo.Support)+log(Perc.Single.Parent.HH)+log(Perc.pop.in.viol)+log(Perc.Fast.Foods),data=traindata)

#Read test data
test <- read.csv("C:/Users/aborthakur/Desktop/Analytics competition/Modelling/Test.csv")
test.dat = test[-c(1:4)]

#Replace NA in test set with mean
for(i in 1:ncol(test.dat)){
  test.dat[is.na(test.dat[,i]), i] <- mean(test.dat[,i], na.rm = TRUE)
}


#Predict
## Score test data
pred <- predict(model2, test.dat)

sample_submission=data.frame("ID"=test$ID,"Predicted"=pred)
write.csv(sample_submission,"Sample_submission.csv",row.names=F)

## Correlation matrix to test for Multicollinearity
sample<-train
correlation<-cor(sample)
correlation<-as.data.frame(correlation)
write.csv(correlation,"correlation.csv")

model <- lm(YPLL.Rate~., data=sample)
pred <- predict(model, test.dat)

sample_submission=data.frame("ID"=test$ID,"Predicted"=pred)
write.csv(sample_submission,"Sample_submission.csv",row.names=F)


model4<-lm(YPLL.Rate~Physically.Unhealthy.Days+Mentally.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Chlamydia.Rate+Perc.Uninsured+Pr.Care.Physician.Ratio+Prev.Hosp.Stay.Rate+Perc.Diabetic.Screening+Perc.High.School.Grad+Perc.Some.College+Perc.Children.in.Poverty+Perc.pop.in.viol+Perc.Limited.Access, data=sample)

model5<-lm(YPLL.Rate~Physically.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Chlamydia.Rate+Perc.Uninsured+Perc.Diabetic.Screening+Perc.High.School.Grad+Perc.Children.in.Poverty+Perc.Limited.Access, data=sample)

model6<-lm(YPLL.Rate~Physically.Unhealthy.Days+Perc.Low.Birth.Weight+Perc.Smokers+Perc.Physically.Inactive+Perc.Excessive.Drinking+MV.Mortality.Rate+Chlamydia.Rate+Teen.Birth.Rate+Perc.Uninsured+Pr.Care.Physician.Ratio+Perc.Diabetic.Screening+Perc.High.School.Grad+Perc.Children.in.Poverty+Perc.Single.Parent.HH+Perc.pop.in.viol,data=sample)


#Remove the outliers
Mean<-mean(train$Perc.Fair.Poor.Health)
SD<-sd(train$Perc.Fair.Poor.Health)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Fair.Poor.Health"]>=MaxLimit){
    train[i,"Perc.Fair.Poor.Health"]<-MaxLimit
  }
  if(train[i,"Perc.Fair.Poor.Health"]<=MinLimit){
    train[i,"Perc.Fair.Poor.Health"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Low.Birth.Weight)
SD<-sd(train$Perc.Low.Birth.Weight)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Low.Birth.Weight"]>=MaxLimit){
    train[i,"Perc.Low.Birth.Weight"]<-MaxLimit
  }
  if(train[i,"Perc.Low.Birth.Weight"]<=MinLimit){
    train[i,"Perc.Low.Birth.Weight"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Smokers)
SD<-sd(train$Perc.Smokers)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Smokers"]>=MaxLimit){
    train[i,"Perc.Smokers"]<-MaxLimit
  }
  if(train[i,"Perc.Smokers"]<=MinLimit){
    train[i,"Perc.Smokers"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Obese)
SD<-sd(train$Perc.Obese)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Obese"]>=MaxLimit){
    train[i,"Perc.Obese"]<-MaxLimit
  }
  if(train[i,"Perc.Obese"]<=MinLimit){
    train[i,"Perc.Obese"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Physically.Inactive)
SD<-sd(train$Perc.Physically.Inactive)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Physically.Inactive"]>=MaxLimit){
    train[i,"Perc.Physically.Inactive"]<-MaxLimit
  }
  if(train[i,"Perc.Physically.Inactive"]<=MinLimit){
    train[i,"Perc.Physically.Inactive"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Excessive.Drinking)
SD<-sd(train$Perc.Excessive.Drinking)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Excessive.Drinking"]>=MaxLimit){
    train[i,"Perc.Excessive.Drinking"]<-MaxLimit
  }
  if(train[i,"Perc.Excessive.Drinking"]<=MinLimit){
    train[i,"Perc.Excessive.Drinking"]<-MinLimit
  }
}

Mean<-mean(train$MV.Mortality.Rate)
SD<-sd(train$MV.Mortality.Rate)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"MV.Mortality.Rate"]>=MaxLimit){
    train[i,"MV.Mortality.Rate"]<-MaxLimit
  }
  if(train[i,"MV.Mortality.Rate"]<=MinLimit){
    train[i,"MV.Mortality.Rate"]<-MinLimit
  }
}

Mean<-mean(train$Chlamydia.Rate)
SD<-sd(train$Chlamydia.Rate)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Chlamydia.Rate"]>=MaxLimit){
    train[i,"Chlamydia.Rate"]<-MaxLimit
  }
  if(train[i,"Chlamydia.Rate"]<=MinLimit){
    train[i,"Chlamydia.Rate"]<-MinLimit
  }
}

Mean<-mean(train$Teen.Birth.Rate)
SD<-sd(train$Teen.Birth.Rate)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Teen.Birth.Rate"]>=MaxLimit){
    train[i,"Teen.Birth.Rate"]<-MaxLimit
  }
  if(train[i,"Teen.Birth.Rate"]<=MinLimit){
    train[i,"Teen.Birth.Rate"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Uninsured)
SD<-sd(train$Perc.Uninsured)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Uninsured"]>=MaxLimit){
    train[i,"Perc.Uninsured"]<-MaxLimit
  }
  if(train[i,"Perc.Uninsured"]<=MinLimit){
    train[i,"Perc.Uninsured"]<-MinLimit
  }
}

Mean<-mean(train$Pr.Care.Physician.Ratio)
SD<-sd(train$Pr.Care.Physician.Ratio)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Pr.Care.Physician.Ratio"]>=MaxLimit){
    train[i,"Pr.Care.Physician.Ratio"]<-MaxLimit
  }
  if(train[i,"Pr.Care.Physician.Ratio"]<=MinLimit){
    train[i,"Pr.Care.Physician.Ratio"]<-MinLimit
  }
}

Mean<-mean(train$Dentist.Ratio)
SD<-sd(train$Dentist.Ratio)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Dentist.Ratio"]>=MaxLimit){
    train[i,"Dentist.Ratio"]<-MaxLimit
  }
  if(train[i,"Dentist.Ratio"]<=MinLimit){
    train[i,"Dentist.Ratio"]<-MinLimit
  }
}

Mean<-mean(train$ Prev.Hosp.Stay.Rate)
SD<-sd(train$ Prev.Hosp.Stay.Rate)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Prev.Hosp.Stay.Rate"]>=MaxLimit){
    train[i,"Prev.Hosp.Stay.Rate"]<-MaxLimit
  }
  if(train[i,"Prev.Hosp.Stay.Rate"]<=MinLimit){
    train[i,"Prev.Hosp.Stay.Rate"]<-MinLimit
  }
}

Mean<-mean(train$ Perc.Diabetic.Screening)
SD<-sd(train$ Perc.Diabetic.Screening)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Diabetic.Screening"]>=MaxLimit){
    train[i,"Perc.Diabetic.Screening"]<-MaxLimit
  }
  if(train[i,"Perc.Diabetic.Screening"]<=MinLimit){
    train[i,"Perc.Diabetic.Screening"]<-MinLimit
  }
}

Mean<-mean(train$Perc.High.School.Grad)
SD<-sd(train$Perc.High.School.Grad )
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.High.School.Grad"]>=MaxLimit){
    train[i,"Perc.High.School.Grad"]<-MaxLimit
  }
  if(train[i,"Perc.High.School.Grad"]<=MinLimit){
    train[i,"Perc.High.School.Grad"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Children.in.Poverty)
SD<-sd(train$Perc.Children.in.Poverty)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Children.in.Poverty"]>=MaxLimit){
    train[i,"Perc.Children.in.Poverty"]<-MaxLimit
  }
  if(train[i,"Perc.Children.in.Poverty"]<=MinLimit){
    train[i,"Perc.Children.in.Poverty"]<-MinLimit
  }
}


Mean<-mean(train$Perc.Mammography)
SD<-sd(train$Perc.Mammography)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Mammography"]>=MaxLimit){
    train[i,"Perc.Mammography"]<-MaxLimit
  }
  if(train[i,"Perc.Mammography"]<=MinLimit){
    train[i,"Perc.Mammography"]<-MinLimit
  }
}

Mean<-mean(train$Perc.No.Soc.Emo.Support)
SD<-sd(train$Perc.No.Soc.Emo.Support)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.No.Soc.Emo.Support"]>=MaxLimit){
    train[i,"Perc.No.Soc.Emo.Support"]<-MaxLimit
  }
  if(train[i,"Perc.No.Soc.Emo.Support"]<=MinLimit){
    train[i,"Perc.No.Soc.Emo.Support"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Single.Parent.HH)
SD<-sd(train$Perc.Single.Parent.HH)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Single.Parent.HH"]>=MaxLimit){
    train[i,"Perc.Single.Parent.HH"]<-MaxLimit
  }
  if(train[i,"Perc.Single.Parent.HH"]<=MinLimit){
    train[i,"Perc.Single.Parent.HH"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Single.Parent.HH)
SD<-sd(train$Perc.Single.Parent.HH)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Single.Parent.HH"]>=MaxLimit){
    train[i,"Perc.Single.Parent.HH"]<-MaxLimit
  }
  if(train[i,"Perc.Single.Parent.HH"]<=MinLimit){
    train[i,"Perc.Single.Parent.HH"]<-MinLimit
  }
}

Mean<-mean(train$Violent.Crime.Rate)
SD<-sd(train$Violent.Crime.Rate)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Violent.Crime.Rate"]>=MaxLimit){
    train[i,"Violent.Crime.Rate"]<-MaxLimit
  }
  if(train[i,"Violent.Crime.Rate"]<=MinLimit){
    train[i,"Violent.Crime.Rate"]<-MinLimit
  }
}

Mean<-mean(train$Perc.pop.in.viol)
SD<-sd(train$Perc.pop.in.viol)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.pop.in.viol"]>=MaxLimit){
    train[i,"Perc.pop.in.viol"]<-MaxLimit
  }
  if(train[i,"Perc.pop.in.viol"]<=MinLimit){
    train[i,"Perc.pop.in.viol"]<-MinLimit
  }
}

Mean<-mean(train$Rec.Facility.Rate)
SD<-sd(train$Rec.Facility.Rate)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Rec.Facility.Rate"]>=MaxLimit){
    train[i,"Rec.Facility.Rate"]<-MaxLimit
  }
  if(train[i,"Rec.Facility.Rate"]<=MinLimit){
    train[i,"Rec.Facility.Rate"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Limited.Access)
SD<-sd(train$Perc.Limited.Access)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Limited.Access"]>=MaxLimit){
    train[i,"Perc.Limited.Access"]<-MaxLimit
  }
  if(train[i,"Perc.Limited.Access"]<=MinLimit){
    train[i,"Perc.Limited.Access"]<-MinLimit
  }
}

Mean<-mean(train$Perc.Fast.Foods)
SD<-sd(train$Perc.Fast.Foods)
MinLimit<-Mean-2*SD
MaxLimit<-Mean+2*SD
for(i in 1:nrow(train)){
  if(train[i,"Perc.Fast.Foods"]>=MaxLimit){
    train[i,"Perc.Fast.Foods"]<-MaxLimit
  }
  if(train[i,"Perc.Fast.Foods"]<=MinLimit){
    train[i,"Perc.Fast.Foods"]<-MinLimit
  }
}
