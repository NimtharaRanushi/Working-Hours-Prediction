library(dplyr)
#install.packages("gapminder")
library(gapminder)
library(leaps)
library(glmnet)
library(ggplot2)

library(MASS)
#install.packages("usdm")
library(usdm)
library(car)
library(corrplot)
#install.packages("texreg")
library(texreg)
#install.packages("MPV")
library(MPV)
library(Matrix)

######################################################################


getwd()
setwd("D:\\colombo uni\\3rd year 2nd sem\\ST 3082\\Group Project\\Project 1")

Data = read.table("train.csv",sep = ",",header = TRUE) # Importing the data set
anyNA(Data) 

df = as.data.frame(Data)
anyNA(df) #Check whether there is any missing values

dim(df) # Dimensions of the data set

str(df) #Structure of the data set
nrow(unique(df))==nrow(df)

N = nrow(df) #Number of observations
N


# Data cleaning part
# Omit the data with native.country is equal to the "South"
unique(df$native.country)
for(i in 1:N){ 
  if(df$native.country[i]==" South"){
    df$native.country[i] = NA 
  }else{ 
    next 
  } 
} 

anyNA(df)
df1 = na.omit(df)
anyNA(df1)
N1 = nrow(df1)
N1
# Omit the data with work class is equal to the "Never-worked"
unique(df1$workclass)
for (j in 1:N1){
  if(df1$workclass[j]==' Never-worked'){
    df1$workclass[j]= NA
  }else{
    next
  }
}
anyNA(df1)
df2 = na.omit(df1)
anyNA(df2)
N2 = nrow(df2)
N2

# Split the data set into training and test sets
set.seed(1000)
dt=sort(sample(nrow(df2),nrow(df2)*.8))
train=df2[dt,] 
test=df2[-dt,]

anyNA(train)

n = nrow(train) # Number of observations in the training set
n


# Re-code the work-class variable
unique(train$workclass)
for (i in 1:n){
  if((train$workclass[i]==' Self-emp-not-inc')|(train$workclass[i]==' Self-emp-inc')){
    train$workclass[i]='Self-employed'
  }else if(train$workclass[i]==' State-gov'){
    train$workclass[i]='Government'
  }else if(train$workclass[i]==' Local-gov'){
    train$workclass[i]='Government'
  }else if(train$workclass[i]==' Federal-gov'){
    train$workclass[i]='Government'
  }
}

unique(train$workclass) # To confirm the re-coding process

# Re-code the marital-status variable
unique(train$marital.status)
for (i in 1:n){
  if(train$marital.status[i]==' Never-married'){
    train$marital.status[i]="Unmarried"
  }else if((train$marital.status[i]==' Married-civ-spouse')|(train$marital.status[i]==" Married-spouse-absent")|(train$marital.status[i]==" Married-AF-spouse")){
    train$marital.status[i]="Married"
  }else if((train$marital.status[i]==" Divorced")|(train$marital.status[i]==" Separated")|(train$marital.status[i]==" Widowed")){
    train$marital.status[i]="Seperated/Widowed"
  }
}
unique(train$marital.status) #To confirm the re-code

# Re-code the race variable
unique(train$race)
for (i in 1:n){
  if(train$race[i]==" White"){
    train$race[i]="White"
  }else{
    train$race[i]="Black/Other"
  }
}


unique(train$race) #To confirm the re-code

# Re-code the education variable
unique(train$education)
for (i in 1:n){
  if (train$education[i]==" HS-grad"){
    train$education[i]="HighSchool-Graduate"
  }else if((train$education[i]==" Some-college")|(train$education[i]==" 7th-8th")|(train$education[i]==" 9th")|(train$education[i]==" 11th")|(train$education[i]==" 12th")|(train$education[i]==" 5th-6th")|(train$education[i]==" 10th")|(train$education[i]==" 1st-4th")|(train$education[i]==" Preschool")){
    train$education[i]="School Level"
  }else if((train$education[i]==" Bachelors")|(train$education[i]==" Prof-school")|(train$education[i]==" Masters")|(train$education[i]==" Doctorate")){
    train$education[i]="Degree Holder"
  }else if((train$education[i]==" Assoc-voc")|(train$education[i]==" Assoc-acdm")){
    train$education[i]="Associatiate Degree Holder"
  }
}

unique(train$education)


unique(train$occupation)
for (i in 1:n){
  if((train$occupation[i]==" Armed-Forces")){
    train$occupation[i]=" Protective-serv"
  }
}
unique(train$occupation)
unique(train$occupation)
for (i in 1:n){
  if((train$occupation[i]==" Adm-clerical")|(train$occupation[i]==" Exec-managerial")|(train$occupation[i]==" Prof-specialty")){
    train$occupation[i]='Administrative'
  }else if((train$occupation[i]==" Farming-fishing")|(train$occupation[i]==" Craft-repair")|(train$occupation[i]==" Craft-repair")){
    train$occupation[i]='Self-emp'
  }else if((train$occupation[i]==" Machine-op-inspct")|(train$occupation[i]==" Tech-support")){
    train$occupation[i]="Tech"
  }else if((train$occupation[i]==" Protective-serv")|(train$occupation[i]==" Other-service")|(train$occupation[i]==" Sales")|(train$occupation[i]==" Transport-moving")){
    train$occupation[i]='Others'
  }else if((train$occupation[i]==" Handlers-cleaners")|(train$occupation[i]==" Priv-house-serv")){
    train$occupation[i]="Cleaning"
  }
}
unique(train$occupation)




# Re-code the native.country variable
unique(train$native.country)
for (i in 1:n){
  if(train$native.country[i]==" United-States" ){
    train$native.country[i]="USA"
  }else{
    train$native.country[i]="Other Country"
  }
}
unique(train$native.country)

rosnerTest(train$hours.per.week, k = 20, alpha = 0.05, warn = TRUE)

# # # Export the training set to a csv file
csv_file_path = "D:\\colombo uni\\3rd year 2nd sem\\ST 3082\\Group Project\\Project 1\\Images\\Book2.csv"
write.csv(train, file = csv_file_path, row.names = FALSE)


########################################################
## recode test data
# Re-code the work-class variable
n = nrow(test)
unique(test$workclass)
for (i in 1:n){
  if((test$workclass[i]==' Self-emp-not-inc')|(test$workclass[i]==' Self-emp-inc')){
    test$workclass[i]='Self-employed'
  }else if(test$workclass[i]==' State-gov'){
    test$workclass[i]='Government'
  }else if(test$workclass[i]==' Local-gov'){
    test$workclass[i]='Government'
  }else if(test$workclass[i]==' Federal-gov'){
    test$workclass[i]='Government'
  }
}

unique(test$workclass) # To confirm the re-coding process

# Re-code the marital-status variable
unique(test$marital.status)
for (i in 1:n){
  if(test$marital.status[i]==' Never-married'){
    test$marital.status[i]="Unmarried"
  }else if((test$marital.status[i]==' Married-civ-spouse')|(test$marital.status[i]==" Married-spouse-absent")|(test$marital.status[i]==" Married-AF-spouse")){
    test$marital.status[i]="Married"
  }else if((test$marital.status[i]==" Divorced")|(test$marital.status[i]==" Separated")|(test$marital.status[i]==" Widowed")){
    test$marital.status[i]="Seperated/Widowed"
  }
}
unique(test$marital.status) #To confirm the re-code

# Re-code the race variable
unique(test$race)
for (i in 1:n){
  if(test$race[i]==" White"){
    test$race[i]="White"
  }else{
    test$race[i]="Black/Other"
  }
}


unique(test$race) #To confirm the re-code

# Re-code the education variable
unique(test$education)
for (i in 1:n){
  if (test$education[i]==" HS-grad"){
    test$education[i]="HighSchool-Graduate"
  }else if((test$education[i]==" Some-college")|(test$education[i]==" 7th-8th")|(test$education[i]==" 9th")|(test$education[i]==" 11th")|(test$education[i]==" 12th")|(test$education[i]==" 5th-6th")|(test$education[i]==" 10th")|(test$education[i]==" 1st-4th")|(test$education[i]==" Preschool")){
    test$education[i]="School Level"
  }else if((test$education[i]==" Bachelors")|(test$education[i]==" Prof-school")|(test$education[i]==" Masters")|(test$education[i]==" Doctorate")){
    test$education[i]="Degree Holder"
  }else if((test$education[i]==" Assoc-voc")|(test$education[i]==" Assoc-acdm")){
    test$education[i]="Associatiate Degree Holder"
  }
}

unique(test$education)

unique(test$occupation)
for (i in 1:n){
  if((test$occupation[i]==" Adm-clerical")|(test$occupation[i]==" Exec-managerial")|(test$occupation[i]==" Prof-specialty")){
    test$occupation[i]='Administrative'
  }else if((test$occupation[i]==" Farming-fishing")|(test$occupation[i]==" Craft-repair")|(test$occupation[i]==" Craft-repair")){
    test$occupation[i]='Self-emp'
  }else if((test$occupation[i]==" Machine-op-inspct")|(test$occupation[i]==" Tech-support")){
    test$occupation[i]="Tech"
  }else if((test$occupation[i]==" Protective-serv")|(test$occupation[i]==" Other-service")|(test$occupation[i]==" Sales")|(test$occupation[i]==" Transport-moving")){
    test$occupation[i]='Others'
  }else if((test$occupation[i]==" Handlers-cleaners")|(test$occupation[i]==" Priv-house-serv")){
    test$occupation[i]="Cleaning"
  }
}
unique(test$occupation)

# Re-code the native.country variable
unique(test$native.country)
for (i in 1:n){
  if(test$native.country[i]==" United-States" ){
    test$native.country[i]="USA"
  }else{
    test$native.country[i]="Other Country"
  }
}
unique(test$native.country)

######################## outlier detection

train
MD = mahalanobis(train[,c(1,3,5,11,12,13)],colMeans(train[,c(1,3,5,11,12,13)]),cov(train[,c(1,3,5,11,12,13)]))
train1 = train
train1
train1$MD = round(MD,3)
hist(train1$MD)
med = median(train1$MD)
train1
train1$outlier_maha = FALSE
train1$outlier_maha[train1$MD>20] = TRUE
train1
length(train1$outlier_maha[train1$outlier_maha==TRUE])
length(train$hours.per.week)
length(train1$outlier_maha[train1$outlier_maha==FALSE])
train1$outlier_maha[train1$outlier_maha==TRUE]=NA
train1
train1 = train1[!is.na(train1$outlier_maha), ]
train1 = train1[-16]
train1 = train1[-16]
train1
train2 = train1
length(train2$hours.per.week)

########################################################
## multiple regression
#install.packages("caret")
all.reg <- lm(hours.per.week ~.,data = train)
summary(all.reg)

############## checking for assumptions
install.packages("performance")
install.packages("see")
library(performance)
library(see)
install.packages("patchwork")
library(patchwork)
check_model(all.reg)

plot(all.reg, 1)
# Get the model residuals
model_residuals = all.reg$residuals
model_residuals
sd_reg = (all.reg$residuals-mean(all.reg$residuals))/sd(all.reg$residuals)
sd_reg
j = NULL
for(i in 1:length(sd_reg)){
  if((sd_reg[i]>2) | (sd_reg[i]<(-2))){
    j[i] = 1
  }else{
      j[i] = 0
    }
}
j

ind =  which(j == 1)
ind
length(ind)
train1 = train[-ind,]
train1
length(train[,15])
length(train1[,15])
train1$hours.per.week = log(train1$hours.per.week)
train1
all.reg2 <- lm(hours.per.week ~.,data = train1)
summary(all.reg2)
plot(all.reg2, 1)
# Get the model residuals
model_residuals2 = all.reg2$residuals
model_residuals2

ks.test(model_residuals, "pnorm", mean = 0, sd = sd(model_residuals))

install.packages("nortest")
library("nortest")
ad.test(model_residuals2)
# Plot the result
hist(model_residuals)

# Plot the residuals
qqnorm(model_residuals)
# Plot the Q-Q line
qqline(model_residuals)
install.packages("carData")
library(carData)
install.packages("car")
library(car)
plot(all.reg, 1)
durbinWatsonTest(all.reg)
##nvcTest(all.reg)


## MAPE
pred_test = predict(all.reg,newdata = test)
pred_train = predict(all.reg,newdata = train)
pred_train
install.packages("MLmetrics")
library("MLmetrics")
MAPE(pred_test, test$hours.per.week)
MAPE(pred_train, train$hours.per.week)

## R^2 
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

R_test = RSQUARE(test$hours.per.week,pred_test)
R_train = RSQUARE(train$hours.per.week,pred_train)
R_test
R_train

## RMESE
RMSE(pred_test, test$hours.per.week)
RMSE(pred_train, train$hours.per.week)


############################################ best subset selection
library(leaps)
regfit.full<-regsubsets(train$hours.per.week~.,data=train,nvmax = 35)
reg.sum = summary(regfit.full)
reg.sum
data.frame(
  adj.R2 = which.max(reg.sum$adjr2),
  CP = which.min(reg.sum$cp),
  BIC = which.min(reg.sum$bic)
)

coef(regfit.full,14)


################################ foward selection
## forward
regit.fwd = regsubsets(train$hours.per.week~.,data=train,nvmax = 35,method = "forward")
reg.sum2 = summary(regit.fwd)
reg.sum2
data.frame(
  adj.R2_fwd = which.max(reg.sum2$adjr2),
  CP_fwd = which.min(reg.sum2$cp),
  BIC_fwd = which.min(reg.sum2$bic)
)

coef(regit.fwd,17)

## backward
regit.bwd = regsubsets(train$hours.per.week~.,data=train,nvmax = 35,method = "backward")
reg.sum3 = summary(regit.fwd)
reg.sum3
data.frame(
  adj.R2_bwd = which.max(reg.sum3$adjr2),
  CP_bwd = which.min(reg.sum3$cp),
  BIC_bwd = which.min(reg.sum3$bic)
)

coef(regit.bwd,17)

## random forest

install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)


# Define the parameter grid for tuning
param_grid <- expand.grid(mtry = c(2,3,4),
                          ntree = c(100,200,300))

# Train the model using grid search
ctrl <- caret::trainControl(method = "cv",  # 10-fold cross-validation
                     number = 10,
                     search = "grid")    # Number of folds


rf_grid <- train(hours.per.week ~ ., 
                 data = train, 
                 method = "rf",          # Random Forest method
                 trControl = ctrl,       # Cross-validation settings
                 tuneGrid = param_grid)  # Parameter grid for tuning

# Summary of the best model
print(rf_grid)

# Predict using the best model
predictions <- predict(rf_grid, test)

# Evaluate the model
confusion_matrix <- table(predictions, test_data$Species)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
