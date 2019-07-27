#Loading libs
#suppressWarnings(suppressMessages(library(caret)))
library(ggplot2, quietly = T)
library(caret, quietly = T)
library(readr, quietly = T)
library(C50, quietly = T)
library(mlbench, quietly = T)
library(bda, quietly = T)
library(zoo, quietly = T)

#Loading data
mydata <- read.csv("data/CompleteResponses.csv")
summary(mydata)
str(mydata)

#Preprocessing the data
#mydata$brand <- as.factor(mydata$brand)
#mydata$zipcode <- as.factor(mydata$zipcode)
#mydata$elevel <- as.factor(mydata$elevel)
#mydata$car <- as.factor(mydata$car)
# can this code be optimized (avoid hardcoding)?
mydata[,c(3:5,7)] <- lapply(mydata[,c(3:5,7)] , factor)

#Binning age groups
#mydata$age <- cut(mydata$age,breaks=c(0,35,50,65,Inf))

#Normalisation
#Z-Score Standardization (good if there are outliers)
#mydata[1,6] <- as.data.frame( scale(mydata[1,6] ))
#dfNorm <- as.data.frame(lapply(mydata["salary"], normalize))

#create correlation matrix (not needed, did it for fun)
mydata.cor = cor(mydata)
mydata.cor
#Set seed to know the random order
set.seed(998)

#Data sample 
#mydata <- mydata[sample(1:nrow(mydata), 2000,replace=FALSE),]

#define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(mydata$brand, p = .75, list = FALSE)
training <- mydata[inTraining,]
testing <- mydata[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1,
                           preProc = c("center", "scale", "range"))


#train Linear Regression model
# The tuneGrid parameter lets us decide which values the main parameter will take
# While tuneLength only limit the number of default parameters to use.
C5Fit <- train(brand~., data = training, method = "C5.0", trControl=fitControl, tuneLength = 1)

#check the results
C5Fit

#How the model prioritized each feature in the training
varImp(C5Fit)

### Random Forest Pipeline ###
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(3,6,10,12,18))

#Train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
rfFit <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid)

#training results
rfFit

#How the model prioritized each feature in the training
varImp(rfFit)


#############

fitControlGBM <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(998)
gbmFit1 <- train(brand~., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

#############

gbmGrid <-  expand.grid(interaction.depth = c(5), 
                        n.trees = (3)*50, 
                        shrinkage = c(0.1,0.11,0.135, 0.134),
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(998)
gbmFit2 <- train(brand~., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2


#loading incomplete data to predict
incompleteData <- read.csv("data/SurveyIncomplete.csv")

#preprocess new file
incompleteData$brand <- as.factor(incompleteData$brand)
incompleteData$zipcode <- as.factor(incompleteData$zipcode)
incompleteData$elevel <- as.factor(incompleteData$elevel)
incompleteData$car <- as.factor(incompleteData$car)

#predict the brand
pred <- predict(gbmFit2, newdata = testing)
summary(pred)

postResample(pred, testing$brand)

incompleteData$brand <- predict(gbmFit2, newdata = incompleteData)
summary(incompleteData$brand)

#confusion matrix
confusionMatrix(pred, testing$brand)

testing$pred  <- pred
#One variable scatter plot
#qplot(seq_along(testing$brand), testing$brand)


ggplot(data=testing)+
  geom_point(color='darkblue', mapping =aes(x=salary, y=age), subset(testing , brand %in% c("0"))) 

ggplot(data=testing) +
  geom_point(mapping =aes(x=salary, y=age, color=(pred==1))) +
  facet_grid(brand ~ . ) +
  guides(color=FALSE)

ggplot(mydata, aes(x=testing$brand, )) +
  geom_point(aes(color=(region==4), size=in.store), shape=1) +
  facet_grid(in.store ~ region)+
  geom_smooth()


#combined data
complete <-do.call(rbind, list(incompleteData, mydata))
summary(complete)

#change 0 to online and 1 to store (more meaningful)
incompleteData$brand <- ifelse(incompleteData$brand == 1, "Sony", "Acer")

#incomplete
ggplot(data = incompleteData, mapping = aes(x = brand, fill = brand)) +
  geom_bar(position = "dodge") +
  labs(x="Brand", y="Count", title="Brand Predictions for the incomplete data") +
  geom_text(mapping = aes(label = sprintf("%0.2f",..count../sum(..count..)*100)), stat = "count", vjust = -0.5)+
  expand_limits(y=3400)
label=sprintf("%0.2f", round(a, digits = 2))


#complete
ggplot(data = complete, mapping = aes(x = brand, fill = brand)) +
  geom_bar(position = "dodge") +
  labs(x="Brand", y="Count", title="Brand Predictions for the complete data") +
  geom_text(mapping = aes(label = sprintf("%0.2f",..count../sum(..count..)*100)), stat = "count", vjust = -0.5)+
  expand_limits(y=10000)
label=sprintf("%0.2f", round(a, digits = 2))