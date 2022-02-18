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


########################################################################################
########################################################################################

#Vizualization of Support Vector Classifier
#Function to create a grid
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid <- make.grid(x) #Grid containing explanatory variables
ygrid <- predict(bestmod, xgrid) #Grid contaning predictions made from the Support Vector Classifier

#Grid illustration of optimal hyperplace and support vectors 
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2);title(main ="Fatality Prediction: Cadiovascular Diseases")
points(x, col = y + 3, pch = 19)
points(x[bestmod$index,], pch = 10, cex = 2)

#Regression coefficients for optimal hyperplane
beta <- drop(t(bestmod$coefs)%*%x[bestmod$index,])
beta0 <- bestmod$rho

#Grid llustation with optimal hyperplane
plot(xgrid, col=c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2);title(main ="Fatality Prediction: Cadiovascular Diseases")
points(x, col = y + 3, pch = 19)
abline(beta0 / beta[2], -beta[1] / beta[2])#Hyperplane

#Grid llustation with maximum margin and optimal hyperplane
plot(xgrid, col=c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2);title(main ="Fatality Prediction: Cadiovascular Diseases")
points(x, col = y + 3, pch = 19)
abline(beta0 / beta[2], -beta[1] / beta[2])#Hyperplane
abline((beta0 - 1) / beta[2], -beta[1] / beta[2]) #Negative Margin
abline((beta0 + 1) / beta[2], -beta[1] / beta[2]) #Positive Margin

#Grid llustation with maximum margin and support vectors
plot(xgrid, col=c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2);title(main ="Fatality Prediction: Cadiovascular Diseases")
points(x[bestmod$index,], col = y + 3, pch = 19)
points(x[bestmod$index,], pch = 5, cex = 2)
abline((beta0 - 1) / beta[2], -beta[1] / beta[2]) #Negative Margin
abline((beta0 + 1) / beta[2], -beta[1] / beta[2]) #Positive Margin

#Grid llustation with maximum margin and non support vectors
plot(xgrid, col=c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2);title(main ="Fatality Prediction: Cadiovascular Diseases")
points(x[-bestmod$index,], col = y + 3, pch = 19)
points(x[-bestmod$index,], pch = 5, cex = 2)
abline((beta0 - 1) / beta[2], -beta[1] / beta[2]) #Negative Margin
abline((beta0 + 1) / beta[2], -beta[1] / beta[2]) #Positive Margin


#############################=############################################################
#########################################################################################
#########################################################################################
#########################################################################################
#Support Vector Machine (Non linear)

#First non linear Suport Vector Machine 
svmmachine = svm(y~., data = dat, kernel = "radial",  gamma = 1, cost = 1, scale=FALSE)

#Cross Validation to select the model with the appropriate COST and GAMMA parameter without scaling
tune_machine = tune(svm, y~., data = dat, kernel = "radial",scale=FALSE,ranges = list(cost = c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))
summary(tune_machine)
best_machine = tune_machine$best.model
summary(best_machine)
best_machine$cost #Model with cost of 10 selected due lowest error
best_machine$gamma #Model with gamma of 0.5 selected due lowest error

#Cross Validation to select the model with the appropriate COST and GAMMA parameter with scale
tune_machine2 = tune(svm, y~., data = dat, kernel = "radial",scale=TRUE,ranges = list(cost = c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))
summary(tune_machine2)
best_machine2 = tune_machine2$best.model
summary(best_machine2)
best_machine2$cost #Model with cost of 10 selected due lowest error
best_machine2$gamma #Model with gamma of 0.5 selected due lowest error

#Classification plots for non linear support vector machine
plot(svmmachine, dat, col = c("red","blue"))
plot(best_machine, dat, col = c("red","blue"))
plot(best_machine2, dat, col = c("red","blue"))

