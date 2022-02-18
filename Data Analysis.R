rm(list=ls())
library(readxl)
library(e1071)
library(ROCR)
library(ggplot2)
library(MASS)

#Heartrate Dataset
hr <- read_excel("DataSets/heartrate.xlsx", 
                 col_types = c("numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))
#variables description 
hr1 <- matrix(hr$serum_creatinine,299,1) #Explanatory variable - Serum creatinine
hr2 <- matrix(hr$ejection_fraction,299,1) #Explanatory variable - ejection fraction

x<-cbind(hr1,hr2) #Matrix including explanatory variables

y <-  hr$death #Response Variable 


#########################################################################################
#########################################################################################
#########################################################################################


#Assing the explanatory variables as a factor of the respone variable and then stored in a datafram
x[y == 1,] = x[y == 1,] + 1
dat <- data.frame(x, y = as.factor(y))

#scatter plot of explanatory variables with a description of the response variable 
ggplot(data.frame(x), aes(X1, X2, colour = factor(y))) + geom_point() + ggtitle("Fatality Prediction: Cadiovascular Diseases")


########################################################################################
########################################################################################

#First linear SVM model
svmfit <- svm(y ~ ., data = dat, kernel = "linear",cost = 10, scale = FALSE)
svmfit
svmfit$index #Support Vectors

#Cross Validation to select the model with the appropriate COST parameter 
tune_svm <- tune(svm, y~., data = dat, kernel = "linear",scale=FALSE, ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune_svm) #Performance results with error and dispersion for varying cost

#Final Classifier fit
bestmod <-tune_svm$best.model #Model with cost of 1 selected due lowest error
summary(bestmod)
bestmod$cost #Model with cost of 1 selected due lowest error

#Classification Plots for Support Vector Classifier models (Similar to the Decision Boundary)
plot(svmfit, dat, col = c("red","blue"))
plot(bestmod, dat, col = c("red","blue"))

#Confusion Matrix for best linear Support Vector Classifier
event_pred <- predict(bestmod, dat)
table(predicted <- event_pred, true = dat$y)


########################################################################################
########################################################################################

#Probability Distribution
#Support Vector Classifier
ev3<- predict(bestmod, dat,decision.values = TRUE)
prob3<-attr(ev3, "decision.values")
range(prob3)

#Transform [-1,1] to [0,1]
logit_prob3 <- 1/(1+exp(-prob3))

truehist(logit_prob3, nbins = 8);title("Probability of Fatality (Linear SVM)")