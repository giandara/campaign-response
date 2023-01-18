##set directory
setwd("file location path")

##clear workspace
rm(list =ls())

##read data
data <- read.csv(file = "MarketingCampaign.csv",
                na.strings = c("", " "),
                stringsAsFactors = FALSE)

##load packages
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071) # SVM
library(MLeval) # ROC Curve Plot

##data overview
str(data)
summary(data)


##1 . Variable treatment
#target variable - factorize categorical variable
data$Response <- factor(data$Response)


#distribution plot
plot(data$Response,
     main = "Campaign Response")

#other variables
str(data)
facs <- c('State','EmploymentStatus','Gender','Marital.Status','Location.Code','Policy.Type','Vehicle.Class','Sales.Channel','Renew.Offer.Type','Policy') ##nominal
ords <- c('Education', 'Coverage','Vehicle.Size') ##ordinal variable
nums <- names(data)[!names(data) %in% c(facs, ords, 'Response')]

#factorization of nominal
data[ ,facs] <- lapply(X = data[ , facs], 
                     FUN = factor)
#factorization of ordinal
data$Coverage <- factor(x = data$Coverage, 
                       levels = c("Basic", "Extended", "Premium"),
                       ordered = TRUE)
data$Education <- factor(x = data$Education, 
                        levels = c("High School or Below", "College","Bachelor", "Master","Doctor"),
                        ordered = TRUE)

data$Vehicle.Size <- factor(x = data$Vehicle.Size, 
                        levels = c("Small", "Medsize", "Large"),
                        ordered = TRUE)


#date variable treatment
data$Effective.To.Date <- as.Date(data$Effective.To.Date, "%m/%d/%Y")

#extract Days
data$Effective.to.Day <- format(data$Effective.To.Date, format="%d")

#transform to numeric variable
data$Effective.to.Day <- as.numeric(data$Effective.to.Day)

#visualization of the Day variable -- might need to exclude the variable
dev.off()   
ggplot(data = data, mapping = aes(x = Effective.to.Day,
                                fill = Response)) +
  geom_bar()
#visualization of employment status
dev.off()   

ggplot(data = data, mapping = aes(x = EmploymentStatus,
                                  fill = Response)) +
  geom_bar()

#visualization of income
dev.off()   

ggplot(data = data, mapping = aes(x = Marital.Status,
                                  fill = Response)) +
  geom_bar()

##2. Missing Value Imputation
any(is.na(data))




#append transformed variable to nums vector
nums <- c(nums, 'Effective.to.Day')

#drop Customer and Effective to Date from nums
nums <- nums[-c(1,3)]

#all predictor variables
vars <- c(facs, ords, nums)

#Drop unnecessary variables
data1 <- data[-c(1,6)]

##4. Training and Testing 
set.seed(831) 
sub <- createDataPartition(y = data1$Response, # target variable
                           p = 0.75, # % in training
                           list = FALSE)

# Subset the transformed data
# to create the training (train)
# and testing (test) datasets
train1 <- data1[sub, ] # create train dataframe
test1 <- data1[-sub, ] # create test dataframe

#------------------------------------------
##Analysis

##Decision Trees
#1. Basic Model
##Class imbalance
#Class weighting

#Class-weight
target_var <- train1$Response # identify target variable
weights <- c(sum(table(target_var))/(nlevels(target_var)*table(target_var)))
weights

#Case-weight
wghts <- weights[match(x = target_var, 
                       table = names(weights))]
wghts

#control object
set.seed(918)
ctrl_DT <- trainControl(method = "repeatedcv", #repeated k-fold cross validation
                        number = 5,
                        repeats = 3)
set.seed(831) # initialize random seed

#base model
DTFit_weighted <- train(x = train1[, vars],
                  y = train1$Response,
                  method = "rpart", # use the rpart package
                  trControl = ctrl_DT, # control object
                  tuneLength = 5, # try 5 cp values
                  weights = wghts) # identify vector of case weights


DTFit_weighted

#train model 
DTFit_train <- predict(object = DTFit_weighted,
                    newdata = train1)
DT_train_conf <- confusionMatrix(data = DTFit_train, # predictions
                           reference = train1$Response, # actual
                           positive = "Yes",
                           mode = "everything")

DT_train_conf

#test model
DTFit_test <- predict(object = DTFit_weighted,
                       newdata = test1)

DT_test_conf <- confusionMatrix(data = DTFit_test, # predictions
                           reference = test1$Response, # actual
                           positive = "Yes",
                           mode = "everything")
DT_test_conf

##Goodness of Fit
#1. Overall level
cbind(Training = DT_train_conf$overall,
      Testing = DT_test_conf$overall)

#2. Class level
cbind(Training = DT_train_conf$byClass,
      Testing = DT_test_conf$byClass)

##Hyperparameter tuning
confusionMatrix(DTFit_weighted)
DTFit_weighted$finalModel$variable.importance

save.image(file = "Final_group 8.RData")
