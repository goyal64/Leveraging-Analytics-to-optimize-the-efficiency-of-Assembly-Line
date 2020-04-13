#Export Data
library('readxl')
ds <- read_excel("BAIM Project Data.xlsx", sheet = "Days_ALL Lines_Shift1")

#rename Columns
colnames(ds) <- c('Linename','LineId','Date','People','HoursRan','TotEEhours','UnitsBlt','FeetBlt',
                  'SpanCount','ChordCount','WebCount','PlateCount','TCPitchCount','BCPitchCount',
                  'WGCount','SDCount','GBCount','VSCount','UnitsEEhour','FeetEEhour') 


#subset data according to lineId
ds1 <- ds[ds$LineId==1,]
ds2 <- ds[ds$LineId==2,]
ds3 <- ds[ds$LineId==3,]
ds4 <- ds[ds$LineId==4,]
ds6 <- ds[ds$LineId==6,]
ds12 <- ds[ds$LineId==12,]


#removing unwanted columns
ds1 <-ds1[,c(6:18)]
ds2 <-ds2[,c(6:18)]
ds3 <-ds3[,c(6:18)]
ds4 <-ds4[,c(6:18)]
ds6 <-ds6[,c(6:18)]
ds12<-ds12[,c(6:18)]

#Generating fake data
library("OpenMx")


ds1Fake<-mxGenerateData(ds1, nrows=25000)
ds2Fake<-mxGenerateData(ds2, nrows=25000)
ds3Fake<-mxGenerateData(ds3, nrows=25000)
ds4Fake<-mxGenerateData(ds4, nrows=25000)
ds6Fake<-mxGenerateData(ds6, nrows=25000)
ds12Fake<-mxGenerateData(ds12, nrows=25000)


#Exporting fake data
write.csv(ds1Fake, "line1.csv")
write.csv(ds2Fake, "line2.csv")
write.csv(ds3Fake, "line3.csv")
write.csv(ds4Fake, "line4.csv")
write.csv(ds6Fake, "line6.csv")
write.csv(ds12Fake, "line12.csv")

#Since the distributions of the generated data is similar to the original data we use only the original data for the modelling

#caret CV Grid search using Random Forest

library(caret)
d<- ds1
names(d)[1] <- "TotEEhours" 
descrCor <-  cor(d[,2:ncol(d)])                           # correlation matrix
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .80) # num Xs with cor > t
summary(descrCor[upper.tri(descrCor)])                    

highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.80)
filteredDescr <- d[,2:ncol(d)][,-highlyCorDescr] # remove those specific columns
descrCor2 <- cor(filteredDescr)                  # calculate a new cor matrix
# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update dataset by removing those filtered vars that were highly correlated
d <- cbind(d$TotEEhours, filteredDescr)
names(d)[1] <- "TotEEhours"
rm(filteredDescr, descrCor, descrCor2, highCorr, highlyCorDescr)  # clean up
d<- d[,1:9]
inTrain <- createDataPartition(y = d$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
# training and test data sets
tr <- d[inTrain,]
te <- d[-inTrain,]

#Random Forest usinf Caret
library("randomForest")
control <- trainControl(method="cv", number=nrow(tr),summaryFunction = defaultSummary)
metric <- "RMSE"
rf_default <- train(TotEEhours~., data=tr, method="rf", metric=metric, trControl=control,)
print(rf_default)

ypred_RF<-predict(rf_default, newdata = te)
library("Metrics")
print(rmse(predict(rf_default, newdata = tr),tr$TotEEhours))
print(rmse(ypred_RF,te$TotEEhours))

# Since the test rmse is far higher than train rmse we move on from random Forest

# linear regression and regularization models forline 1

# training and test data sets
set.seed(0)
library("caret")
library(MASS)
#Detecting and removing outliers
boxplot(ds1$TotEEhours)
boxplot.stats(ds1$TotEEhours)$out 
ds1<-ds1[!(ds1$TotEEhours==4 | ds1$TotEEhours==6 | ds1$TotEEhours==27 ),]

boxplot(ds1$FeetBlt)
boxplot.stats(ds1$FeetBlt)$out 
for (i in boxplot.stats(ds1$FeetBlt)$out ){
  ds1<-ds1[!(ds1$FeetBlt==i) ,]  
}


#normalize input features
preprocessParams <- preProcess(ds1[,2:13], method=c("range"))
ds1[,2:13] <- predict(preprocessParams, ds1[,2:13])

#splitting data
inTrain <- createDataPartition(y = ds1$TotEEhours,          # outcome variable
                               p = .85,        # % of train data you wa nt
                               list = FALSE)


tr <- ds1[inTrain,]
te <- ds1[-inTrain,]
fit1 <- train(TotEEhours ~ ., method = "lm", data = tr, trControl = trainControl(method = "LOOCV"))


summary(fit1)
pred<- predict(fit1, newdata = te)
print('LR')
print(rmse(predict(fit1, newdata = tr),tr$TotEEhours))
print(rmse(pred,te$TotEEhours))

#Linear with stepwise feature selection
fit1 <- lm(TotEEhours~.,data=tr,y=TRUE, x=TRUE)
step <- stepAIC(fit1, direction="both")
step$anova 
ds1Step <- ds1[,c(1,2,3,5,6,8)]


set.seed(0)
inTrain <- createDataPartition(y = ds1Step$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
trStep <- ds1Step[inTrain,]
teStep <- ds1Step[-inTrain,]



fitStep1<- train(TotEEhours ~ ., method = "lm", data = trStep, trControl = trainControl(method = "LOOCV"))
pred<- predict(fitStep1, newdata = teStep)
print('LR with step wise')
print(rmse(predict(fitStep1, newdata = trStep),trStep$TotEEhours))
print(rmse(predict(fitStep1, newdata = teStep),teStep$TotEEhours))
summary(fitStep1)

#Ridge and Lasso input preparation

x<- model.matrix(TotEEhours~., tr)[,-1]
y<- tr$TotEEhours

#for step wise features
xStep<-model.matrix(TotEEhours~., trStep)[,-1]
yStep<-trStep$TotEEhours

#Ridge
set.seed(0) 
library("glmnet")
cv <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
x.test <- model.matrix(TotEEhours ~., te)[,-1]
library("tidyverse")
predictions <- model %>% predict(x.test) %>% as.vector()

# Model performance metrics
print('Ridge')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Ridge for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 0, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Ridge with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

#Lasso
set.seed(0) 
cvLasso <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cvLasso$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cvLasso$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(TotEEhours ~., te)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Lasso')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Lasso for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 1, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Lasso with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

# linear regression and regularization models for line 2

# training and test data sets
# training and test data sets
set.seed(0)
library("caret")
library(MASS)
#Detecting and removing outliers
boxplot(ds2$TotEEhours)
boxplot.stats(ds2$TotEEhours)$out 
ds2<-ds2[!(ds2$TotEEhours<=10 ),]

boxplot(ds2$FeetBlt)
boxplot.stats(ds2$FeetBlt)$out 
for (i in boxplot.stats(ds2$FeetBlt)$out ){
  ds2<-ds2[!(ds2$FeetBlt==i) ,]  
}

#normalize input features
preprocessParams <- preProcess(ds2[,2:13], method=c("range"))
ds2[,2:13] <- predict(preprocessParams, ds2[,2:13])

#splitting data
inTrain <- createDataPartition(y = ds2$TotEEhours,          # outcome variable
                               p = .85,        # % of train data you wa nt
                               list = FALSE)


tr <- ds2[inTrain,]
te <- ds2[-inTrain,]
fit2 <- train(TotEEhours ~ ., method = "lm", data = tr, trControl = trainControl(method = "LOOCV"))


summary(fit2)
pred<- predict(fit2, newdata = te)
print('LR')
print(rmse(predict(fit2, newdata = tr),tr$TotEEhours))
print(rmse(pred,te$TotEEhours))

#Linear with stepwise feature selection
fit2 <- lm(TotEEhours~.,data=tr,y=TRUE, x=TRUE)
step <- stepAIC(fit2, direction="both")
step$anova 
ds2Step <- ds2[,c(1,2,3,4,5,6,8,12)]


set.seed(0)
inTrain <- createDataPartition(y = ds2Step$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
trStep <- ds2Step[inTrain,]
teStep <- ds2Step[-inTrain,]



fitStep2<- train(TotEEhours ~ ., method = "lm", data = trStep, trControl = trainControl(method = "LOOCV"))
pred<- predict(fitStep2, newdata = teStep)
print('LR with step wise')
print(rmse(predict(fitStep2, newdata = trStep),trStep$TotEEhours))
print(rmse(predict(fitStep2, newdata = teStep),teStep$TotEEhours))
summary(fitStep2)

#Ridge and Lasso input preparation

x<- model.matrix(TotEEhours~., tr)[,-1]
y<- tr$TotEEhours

#for step wise features
xStep<-model.matrix(TotEEhours~., trStep)[,-1]
yStep<-trStep$TotEEhours

#Ridge
set.seed(0) 
library("glmnet")
cv <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
x.test <- model.matrix(TotEEhours ~., te)[,-1]
library("tidyverse")
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Ridge')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Ridge for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 0, nfolds = nrow(xStep), grouped = FALSE)

# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()

# Model performance metrics
print('Ridge with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

#Lasso
set.seed(0) 
cvLasso <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cvLasso$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cvLasso$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(TotEEhours ~., te)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Lasso')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Lasso for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 1, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Lasso with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))



# linear regression and regularization models for line 3

# training and test data sets
set.seed(0)
library("caret")
library(MASS)
#Detecting and removing outliers
boxplot(ds3$TotEEhours)
boxplot.stats(ds3$TotEEhours)$out 
ds3<-ds3[!(ds3$TotEEhours>=20),]

boxplot(ds3$UnitsBlt)
boxplot.stats(ds3$UnitsBlt)$out 
ds3<-ds3[!(ds3$UnitsBlt>=90) ,]  


#normalize input features
preprocessParams <- preProcess(ds3[,2:13], method=c("range"))
ds3[,2:13] <- predict(preprocessParams, ds3[,2:13])

#splitting data
inTrain <- createDataPartition(y = ds3$TotEEhours,          # outcome variable
                               p = .85,        # % of train data you wa nt
                               list = FALSE)


tr <- ds3[inTrain,]
te <- ds3[-inTrain,]
fit3 <- train(TotEEhours ~ ., method = "lm", data = tr, trControl = trainControl(method = "LOOCV"))


summary(fit3)
pred<- predict(fit3, newdata = te)
print('LR')
print(rmse(predict(fit3, newdata = tr),tr$TotEEhours))
print(rmse(pred,te$TotEEhours))

#Linear with stepwise feature selection
fit3 <- lm(TotEEhours~.,data=tr,y=TRUE, x=TRUE)
step <- stepAIC(fit3, direction="both")
step$anova 
ds3Step <- ds3[,c(1,2,3,4,5,9,10)]


set.seed(0)
inTrain <- createDataPartition(y = ds3Step$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
trStep <- ds3Step[inTrain,]
teStep <- ds3Step[-inTrain,]



fitStep2<- train(TotEEhours ~ ., method = "lm", data = trStep, trControl = trainControl(method = "LOOCV"))
pred<- predict(fitStep2, newdata = teStep)
print('LR with step wise')
print(rmse(predict(fitStep2, newdata = trStep),trStep$TotEEhours))
print(rmse(predict(fitStep2, newdata = teStep),teStep$TotEEhours))
summary(fitStep2)

#Ridge and Lasso input preparation

x<- model.matrix(TotEEhours~., tr)[,-1]
y<- tr$TotEEhours

#for step wise features
xStep<-model.matrix(TotEEhours~., trStep)[,-1]
yStep<-trStep$TotEEhours

#Ridge
set.seed(0) 
library("glmnet")
cv <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
x.test <- model.matrix(TotEEhours ~., te)[,-1]
library("tidyverse")
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Ridge')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Ridge for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 0, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Ridge with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

#Lasso
set.seed(0) 
cvLasso <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cvLasso$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cvLasso$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(TotEEhours ~., te)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Lasso')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Lasso for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 1, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Lasso with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))


# linear regression and regularization models for line 4


set.seed(0)
library("caret")
library(MASS)
#Detecting and removing outliers
boxplot(ds4$TotEEhours)
boxplot.stats(ds4$TotEEhours)$out 
ds4<-ds4[!(ds4$TotEEhours==1),]

boxplot(ds4$FeetBlt)
boxplot.stats(ds4$FeetBlt)$out 
for (i in boxplot.stats(ds4$FeetBlt)$out ){
  ds4<-ds4[!(ds4$FeetBlt==i) ,]  
}

#normalize input features
preprocessParams <- preProcess(ds4[,2:13], method=c("range"))
ds4[,2:13] <- predict(preprocessParams, ds4[,2:13])

#splitting data
inTrain <- createDataPartition(y = ds4$TotEEhours,          # outcome variable
                               p = .85,        # % of train data you wa nt
                               list = FALSE)


tr <- ds4[inTrain,]
te <- ds4[-inTrain,]
fit4 <- train(TotEEhours ~ ., method = "lm", data = tr, trControl = trainControl(method = "LOOCV"))


summary(fit4)
pred<- predict(fit4, newdata = te)
print('LR')
print(rmse(predict(fit4, newdata = tr),tr$TotEEhours))
print(rmse(pred,te$TotEEhours))

#Linear with stepwise feature selection
fit4 <- lm(TotEEhours~.,data=tr,y=TRUE, x=TRUE)
step <- stepAIC(fit4, direction="both")
step$anova 
ds4Step <- ds4[,c(1,3,7,9,13)]


set.seed(0)
inTrain <- createDataPartition(y = ds4Step$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
trStep <- ds4Step[inTrain,]
teStep <- ds4Step[-inTrain,]



fitStep2<- train(TotEEhours ~ ., method = "lm", data = trStep, trControl = trainControl(method = "LOOCV"))
pred<- predict(fitStep2, newdata = teStep)
print('LR with step wise')
print(rmse(predict(fitStep2, newdata = trStep),trStep$TotEEhours))
print(rmse(predict(fitStep2, newdata = teStep),teStep$TotEEhours))
summary(fitStep2)

#Ridge and Lasso input preparation

x<- model.matrix(TotEEhours~., tr)[,-1]
y<- tr$TotEEhours

#for step wise features
xStep<-model.matrix(TotEEhours~., trStep)[,-1]
yStep<-trStep$TotEEhours

#Ridge
set.seed(0) 
library("glmnet")
cv <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
x.test <- model.matrix(TotEEhours ~., te)[,-1]
library("tidyverse")
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Ridge')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Ridge for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 0, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Ridge with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

#Lasso
set.seed(0) 
cvLasso <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cvLasso$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cvLasso$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(TotEEhours ~., te)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Lasso')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Lasso for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 1, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Lasso with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

# linear regression and regularization models for line 6

# training and test data sets
set.seed(0)
library("caret")
library(MASS)
#Detecting and removing outliers
boxplot(ds6$TotEEhours)
boxplot.stats(ds6$TotEEhours)$out 
ds6<-ds6[!(ds6$TotEEhours==4.5 ),]

boxplot(ds6$FeetBlt)
boxplot.stats(ds6$FeetBlt)$out 
for (i in boxplot.stats(ds6$FeetBlt)$out ){
  ds6<-ds6[!(ds6$FeetBlt==i) ,]  
}

#normalize input features
preprocessParams <- preProcess(ds6[,2:13], method=c("range"))
ds6[,2:13] <- predict(preprocessParams, ds6[,2:13])

#splitting data
inTrain <- createDataPartition(y = ds6$TotEEhours,          # outcome variable
                               p = .85,        # % of train data you wa nt
                               list = FALSE)


tr <- ds6[inTrain,]
te <- ds6[-inTrain,]
fit6 <- train(TotEEhours ~ ., method = "lm", data = tr, trControl = trainControl(method = "LOOCV"))


summary(fit6)
pred<- predict(fit6, newdata = te)
print('LR')
print(rmse(predict(fit6, newdata = tr),tr$TotEEhours))
print(rmse(pred,te$TotEEhours))

#Linear with stepwise feature selection
fit6 <- lm(TotEEhours~.,data=tr,y=TRUE, x=TRUE)
step <- stepAIC(fit6, direction="both")
step$anova 
ds6Step <- ds6[,c(1,3,5,7,8,11)]


set.seed(0)
inTrain <- createDataPartition(y = ds6Step$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
trStep <- ds6Step[inTrain,]
teStep <- ds6Step[-inTrain,]



fitStep2<- train(TotEEhours ~ ., method = "lm", data = trStep, trControl = trainControl(method = "LOOCV"))
pred<- predict(fitStep2, newdata = teStep)
print('LR with step wise')
print(rmse(predict(fitStep2, newdata = trStep),trStep$TotEEhours))
print(rmse(predict(fitStep2, newdata = teStep),teStep$TotEEhours))
summary(fitStep2)

#Ridge and Lasso input preparation

x<- model.matrix(TotEEhours~., tr)[,-1]
y<- tr$TotEEhours

#for step wise features
xStep<-model.matrix(TotEEhours~., trStep)[,-1]
yStep<-trStep$TotEEhours

#Ridge
set.seed(0) 
library("glmnet")
cv <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
x.test <- model.matrix(TotEEhours ~., te)[,-1]
library("tidyverse")
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Ridge')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Ridge for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 0, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Ridge with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

#Lasso
set.seed(0) 
cvLasso <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cvLasso$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cvLasso$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(TotEEhours ~., te)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Lasso')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Lasso for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 1, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
com<-data.frame(predictions,teStep$TotEEhours)
# Model performance metrics
print('Lasso with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))


# linear regression and regularization models for line 12
# training and test data sets
set.seed(0)
library("caret")
library(MASS)
#Detecting and removing outliers
boxplot(ds12$TotEEhours)
boxplot.stats(ds12$TotEEhours)$out 
ds12<-ds12[!(ds12$TotEEhours<=6 | ds12$TotEEhours>=35 ),]

boxplot(ds12$FeetBlt)
boxplot.stats(ds12$FeetBlt)$out 
for (i in boxplot.stats(ds12$FeetBlt)$out ){
  ds12<-ds12[!(ds12$FeetBlt==i) ,]  
}
table(ds$FeetBlt)
#normalize input features
preprocessParams <- preProcess(ds12[,2:13], method=c("range"))
ds12[,2:13] <- predict(preprocessParams, ds12[,2:13])

#splitting data
inTrain <- createDataPartition(y = ds12$TotEEhours,          # outcome variable
                               p = .85,        # % of train data you wa nt
                               list = FALSE)


tr <- ds12[inTrain,]
te <- ds12[-inTrain,]
fit12 <- train(TotEEhours ~ ., method = "lm", data = tr, trControl = trainControl(method = "LOOCV"))


summary(fit12)
pred<- predict(fit12, newdata = te)
print('LR')
print(rmse(predict(fit12, newdata = tr),tr$TotEEhours))
print(rmse(pred,te$TotEEhours))

#Linear with stepwise feature selection
fit12 <- lm(TotEEhours~.,data=tr,y=TRUE, x=TRUE)
step <- stepAIC(fit12, direction="both")
step$anova 
ds12Step <- ds12[,c(1,2,3,4,7,8,11)]


set.seed(0)
inTrain <- createDataPartition(y = ds12Step$TotEEhours,          # outcome variable
                               p = .869,        # % of train data you want
                               list = FALSE)
trStep <- ds12Step[inTrain,]
teStep <- ds12Step[-inTrain,]



fitStep2<- train(TotEEhours ~ ., method = "lm", data = trStep, trControl = trainControl(method = "LOOCV"))
pred<- predict(fitStep2, newdata = teStep)
com<- data.frame(pred,teStep$TotEEhours)
print('LR with step wise')
print(rmse(predict(fitStep2, newdata = trStep),trStep$TotEEhours))
print(rmse(predict(fitStep2, newdata = teStep),teStep$TotEEhours))
summary(fitStep2)

#Ridge and Lasso input preparation

x<- model.matrix(TotEEhours~., tr)[,-1]
y<- tr$TotEEhours

#for step wise features
xStep<-model.matrix(TotEEhours~., trStep)[,-1]
yStep<-trStep$TotEEhours

#Ridge
set.seed(0) 
library("glmnet")
cv <- cv.glmnet(x, y, alpha = 0, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)
x.test <- model.matrix(TotEEhours ~., te)[,-1]
library("tidyverse")
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Ridge')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Ridge for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 0, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 0, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Ridge with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))

#Lasso
set.seed(0) 
cvLasso <- cv.glmnet(x, y, alpha = 1, nfolds = nrow(x), grouped = FALSE)
# Display the best lambda value
cvLasso$lambda.min

model <- glmnet(x, y, alpha = 1, lambda = cvLasso$lambda.min)
# Dsiplay regression coefficients
coef(model)

x.test <- model.matrix(TotEEhours ~., te)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
print('Lasso')
print(rmse(predict(model,x),tr$TotEEhours))
print(rmse(predictions,te$TotEEhours))

#Lasso for step wise
set.seed(0) 
cv <- cv.glmnet(xStep, yStep, alpha = 1, nfolds = nrow(xStep), grouped = FALSE)
# Display the best lambda value
cv$lambda.min

model <- glmnet(xStep, yStep, alpha = 1, lambda = cv$lambda.min)
# Display regression coefficients
coef(model)

xStep.test <- model.matrix(TotEEhours ~., teStep)[,-1]
predictions <- model %>% predict(xStep.test) %>% as.vector()
# Model performance metrics
print('Lasso with stepwise')
print(rmse(predict(model,xStep),trStep$TotEEhours))
print(rmse(predictions,teStep$TotEEhours))
