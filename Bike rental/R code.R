setwd("C:/Users/Rishabh/Desktop/Edwisor-new/Project-2")
day_data = read.csv("day.csv", header = T)

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','MASS')
lapply(x,require, character = T)
rm(x)

#################################Exploratory Data Analysis###############################
for (i in c(3:9)){
  day_data[,i] = as.factor(day_data[,i])
}
#Ignore the "casual" and "registered" fields as their sum is equal to the "cnt" field
day_data = day_data[,-c(14,15)]

library(sqldf)
library(ggplot2)

#Get the average count of bikes rent by season, weekday
season_summary_by_weekday = sqldf('select season, weekday, avg(cnt) as count from day_data group by season, weekday')

ggplot(day_data, aes(x=weekday, y=count, color=season))+
  geom_point(data = season_summary_by_weekday, aes(group = season))+
  geom_line(data = season_summary_by_weekday, aes(group = season))+
  ggtitle("Bikes Rent By Season")+ scale_colour_hue('Season',breaks = levels(day_data$season), labels=c('spring', 'summer', 'fall', 'winter'))
                                                    
# Get the average count of bikes rent by weathersit, weekday
weather_summary_by_weekday = sqldf('select weathersit, weekday, avg(cnt) as count from day_data group by weathersit, weekday')

ggplot(day_data, aes(x=weekday, y=count, color=weathersit))+
  geom_point(data = weather_summary_by_weekday, aes(group = weathersit))+
  geom_line(data = weather_summary_by_weekday, aes(group = weathersit))+
  ggtitle("Bikes Rent By Weather")+ scale_colour_hue('Weathersit',breaks = levels(day_data$weathersit), labels=c('Clear or mild cloudy', 'mist or mist and cloudy', 'snow or rain'))

#################################MISSING VALUE ANALYSIS##################################
sum(is.na(day_data)) #No need of missing value analysis as no values are missing

#################################OUTLIER ANALYSIS####################################
#DENORMALIZING
#day_data$temp = day_data$temp*(47)-8
#day_data$atemp = day_data$atemp*(66) - 16
#day_data$hum = day_data$hum*100
#day_data$windspeed = day_data$windspeed*67

numeric_index = c(10,11,12,13,14)
numeric_data = day_data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames)){
  assign(paste0("outlier_values",i), boxplot.stats(numeric_data[,i])$out)
}

boxplot(day_data$temp, main="temp", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values1, collapse=", ")), cex=0.6)

boxplot(day_data$atemp, main="atemp", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values2, collapse=", ")), cex=0.6)

boxplot(day_data$hum, main="hum", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values3, collapse=", ")), cex=0.6)

boxplot(day_data$windspeed, main="windspeed", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values4, collapse=", ")), cex=0.6)

boxplot(day_data$cnt, main="count", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values5, collapse=", ")), cex=0.6)

#loop to remove from all variables
for(i in cnames){
  print(i)
  val = day_data[,i][day_data[,i] %in% boxplot.stats(day_data[,i])$out]
  #print(length(val))
  day_data = day_data[which(!day_data[,i] %in% val),]
}

#################################FEATURE SELECTION####################################
## Correlation Plot 
corrgram(day_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

##Anova Test of Independence
factor_index = sapply(day_data, is.factor )
factor_data = day_data[,factor_index]
factor_data = factor_data[,-1]
factor_data = cbind(factor_data, day_data$cnt)
colnames(factor_data[8]) = "cnt"
fnames = colnames(factor_data)

attach(day_data)
aov_test = aov(day_data$cnt ~ (season*yr*mnth*holiday*weekday*workingday*weathersit))
aov_test1 = aov(cnt~.)

day_data = day_data[,-c(1,2,4,10)]

############################FEATURE SCALING############################################
#View(day_data)
#specific_index = c(10,11,12)
#specific_data = day_data[,specific_index]
#snames = colnames(specific_data)
#for (i in snames){
#  day_data[,i] = (day_data[,i] - min(day_data[,i]))/(max(day_data[,i])-min(day_data[,i]))
#}

############################MODEL DEVELOPMENT##########################################
#Clean the environment
rmExcept("day_data")

#Divide the data into Train and Test
sample_index = sample(1:nrow(day_data), 0.8*nrow(day_data))
train = day_data[sample_index,]
test = day_data[-sample_index,]

################Decision Tree Model#####################################################
##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-10])
predictions_DT = round(predictions_DT)

summary(fit)
summary(predictions_DT)
#Calculating error using rmse  and rmsle measures
library(Metrics)
rmse(test[,10], predictions_DT)
rmsle(test[,10], predictions_DT) 

#rmse = 1323.362
#rmsle = 0.35
##################run regression model#################################################

lm_model = lm(cnt ~. ,data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,-10])
predictions_LR = round(predictions_LR)

regr.eval(test[,10], predictions_LR, stats = "rmse")

summary(predictions_LR)
# From above summary we saw negative values of predicted count.
# We don't want negative values as forecast for bike count. Replace all negative numbers with 22 (the minimum cnt value in the given dataset)
Output1Mod = predictions_LR
Output1Mod[predictions_LR<=0] = 22
summary(Output1Mod)

rmsle(test[,10], Output1Mod)
#rmse = 1281.519
#rmsle = 0.54

############### Now performs stepwise model selection by AIC with both directions(Forward, Backward)#######
library(MASS)
lm_modelAIC = stepAIC(lm_model, direction="both")
summary(lm_modelAIC)

#Predict
predictions_StR = predict(lm_modelAIC, test)
predictions_StR = round(predictions_StR)

#Lets compute the root-mean-square error value between actual and predicted
library(Metrics)
test_rmse = rmse(test$cnt, predictions_StR)
print("root-mean-square error between actual and predicted")
print(test_rmse)

#Alternate Method
regr.eval(test[,10], predictions_StR, stats = "rmse")
#rmse = 1279.609

# Let's check the summary of predicted count values
cat("\n")
print("summary of predicted count values")
summary(predictions_StR)

# summary of actual count values
print("summary of actual count values")
summary(test$cnt)

# From above summary we saw negative values of predicted count.
# We don't want negative values as forecast for bike count. Replace all negative numbers with 22 (the minimum cnt value in the given dataset)
Output2Mod = predictions_StR
Output2Mod[predictions_StR<=0] = 22

# Check again the summary of predicted count values
print("summary of predicted count values after replaced the negative values")
summary(Output2Mod)

# As we replaced the negative values, the rmse value got reduced
print("root-mean-square error value after replaced the negative values")
print(rmse(test$cnt,Output2Mod))
#rmse = 1268.481

cat("\n")
#If we want to penalize under-prediction of demand, rmsle might be a better metric
test_rmsle = rmsle(test$cnt, Output2Mod)
print("root-mean-square-log error value after replaced the negative values")
print(test_rmsle)
#rmsle = 0.55

# Since we got negative predicted values, let's do log transformation and run regression model again
#####################################Log Transformation############################
lm_model_log = lm(log(cnt)~., data = train)

# Now performs stepwise model selection on log model
lm_modelAIC_log = stepAIC(lm_model_log, direction="both")

predictions_StR_log = predict(lm_modelAIC_log,newdata=test)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
predictions_StR_nonlog = exp(predictions_StR_log)
predictions_StR_nonlog = round(predictions_StR_nonlog)

# Let's check the summary of predicted count values
print("summary of predicted count values after log transformation")
summary(predictions_StR_nonlog)

# Check rmsle value again, it got reduced from 0.55 to 0.34
test_nonlog_rmsle = rmsle(test$cnt,predictions_StR_nonlog)
print("root-mean-square-log error value after log transformation")
print(test_nonlog_rmsle)
#rmsle = 0.3439
output = (cbind(predictions_StR_nonlog,test$cnt))
write.csv(output,"predictions-test.csv", row.names = T)

# Let's check the Residual vs Fitted plot
plot(lm_model_log$fitted.values, lm_model_log$residuals)
