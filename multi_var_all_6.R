closeAllConnections()
rm(list=ls())

#Error function
LogLossBinary = function(actual, predicted, eps = 1e-15) {  
  predicted = pmin(pmax(predicted, eps), 1-eps)  
  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}

setwd('C:/Users/Amin/Documents/R/allstate')
train = read.csv('train.csv')
train <- subset(train, train$Model != "")
train.no.res <- train[,1:31]
test = read.csv('test.csv')

#combine two data
library(dplyr)
full <- bind_rows(train.no.res,test)
full$Model <- NULL

#How old is a car
full.old <- full
full.old$old <- c(full$CalendarYear - full$ModelYear + 1)
table(full.old$old[1:99801],train$Response)
table(full.old$old[1:99801])
table(full.old$old[99802:139801])

full.old$old[full.old$old >= 10] <- 10 #combine car older than 14

#combine rare Make
full.old.make <- full.old
full.old.make$Make <- as.character(full.old.make$Make)
rare_make <- c('AG', 'AX', 'BG', 'BR', 'E', 'Z')
full.old.make$Make[full.old.make$Make %in% rare_make] <- 'RR'
table(full.old.make$Make[1:99801])
table(full.old.make$Make[99802:139801])

#conver to factor
full.old.make$Make <- factor(full.old.make$Make)
full.old.make$old <- factor(full.old.make$old)
summary(full.old.make)

#Seperate train and test
#add Response and remove CalanderYear and ModelYear
full.old.make$ModelYear <- NULL
full.old.make$CalendarYear <- NULL

full.old.make$Cat3 <- factor(full.old.make$Cat3)
full.old.make$Cat6 <- factor(full.old.make$Cat6)
str(full.old.make)
trainClean <- full.old.make[1:99801,]
testClean <- full.old.make[99802:139801,]

trainClean$Response <- train$Response
trainClean$Cat6 <- factor(trainClean$Cat6)

head(trainClean)
str(trainClean)
str(train)
str(test)

table(train$Response, train$Cat3)
summary(test$Cat3)

#GBM model
library(gbm)
gbmModel = gbm(formula = Response ~ . - Cat6,
               distribution = "bernoulli",
               data = trainClean,
               n.trees = 2000,
               shrinkage = .01,
               n.minobsinnode = 20,
#               interaction.depth = 4,
               cv.folds = 5,
               n.cores = 4)
summary(gbmModel)
gbmTrees = gbm.perf(gbmModel)

#apply to train data
gbmPredictions = predict(object = gbmModel,
                         newdata = trainClean,
                         n.trees = gbmTrees,
                         type = "response")
LogLossBinary(trainClean$Response, gbmPredictions)

#apply model to testClean and save
gbmTestPredictions = predict(object = gbmModel,
                             newdata = testClean,
                             n.trees = gbmTrees,
                             type = "response")


#Check the distribution
library(ggplot2)
ppp <- data.frame(gbmTestPredictions)
summary(ppp)
ggplot() +
  geom_density(data = ppp,  aes(x=gbmTestPredictions), color = 'red') +     #without Cat3 & Cat6
  geom_density(data = pp1, aes(x=gbmPredictions), color = 'blue') +    #without Cat6
  geom_density(data = pp2, aes(x=gbmPredictions), color = 'green') +   #depth 4
  geom_density(data = pp3, aes(x=gbmPredictions), color = 'orange') +  #depth 4 cvfold 5
  geom_density(data = pp4, aes(x=gbmPredictions), color = 'yellow') +  #cvfold 5
  geom_density(data = pp5, aes(x=gbmTestPredictions), color = 'black')

outputDataSet = data.frame("RowID" = test$RowID,
                           "ProbabilityOfResponse" = gbmTestPredictions)

write.csv(outputDataSet, "gbmBenchmarkSubmission6.csv", row.names = FALSE)

testscale <- rescale(gbmTestPredictions,c(0.22,0.32))
pp6 <- data.frame(testscale)
summary(pp6)

ggplot() +
  geom_density(data = pp6, aes(x=testscale), color = 'red')

outputDataSet = data.frame("RowID" = test$RowID,
                           "ProbabilityOfResponse" = testscale)
summary(outputDataSet$ProbabilityOfResponse)

write.csv(outputDataSet, "gbmBenchmarkSubmission66.csv", row.names = FALSE)
