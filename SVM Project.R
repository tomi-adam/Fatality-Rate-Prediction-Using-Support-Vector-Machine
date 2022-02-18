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


########################################################################################
########################################################################################

#Confusion Matrix of Training Data
#SVM without scale
event_pred2 <- predict(best_machine, dat)
table(predicted <- event_pred2, true = dat$y) 

#SVM with scale
event_pred21 <- predict(best_machine2, dat)
table(predicted <- event_pred21, true = dat$y)

########################################################################################
########################################################################################

#Probability Distribution
#SVM without scale
ev<- predict(best_machine, dat,decision.values = TRUE) 
prob<-attr(ev, "decision.values")
range(prob)

#SVM with scale
ev2<- predict(best_machine2, dat,decision.values = TRUE)
prob2<-attr(ev2, "decision.values")
range(prob2)

#Transform [-1,1] to [0,1]
logit_prob <- 1/(1+exp(-prob))
logit_prob2 <- 1/(1+exp(-prob2))


truehist(logit_prob, nbins = 7.5);title("Probability of Fatality (Radial SVM without scale)")
truehist(logit_prob2, nbins = 15);title("Probability of Fatality (Radial SVM with scale)")

########################################################################################
########################################################################################

#Vizualization of SVM
make.grid = function(x, n = 150) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid <- make.grid(x)
ygrid2 <- predict(best_machine, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid2)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[best_machine$index,], pch = 10, cex = 2)

ygrid21 <- predict(best_machine2, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid21)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[best_machine2$index,], pch = 10, cex = 2)

######################################################################################
######################################################################################

#ROC
rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}


fitted_opt_linear = attributes(predict(bestmod, dat, decision.values = TRUE))$decision.values
rocplot(fitted_opt_linear, dat$y, main = "FATALITY PREDICTION ROC")

fitted_opt_radial1 = attributes(predict(best_machine , dat, decision.values = TRUE))$decision.values
rocplot(fitted_opt_radial1, dat$y,  add = TRUE, col = "red")

fitted_opt_radial2 = attributes(predict(best_machine2 , dat, decision.values = TRUE))$decision.values
rocplot(fitted_opt_radial2, dat$y,  add = TRUE, col = "blue")


######################################################################################
######################################################################################

#Decision Boundary
#Decision boundary function 
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
 
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

decisionplot(bestmod, dat, class = "y", main = "Decision Boundary: Support Vector Classifier (linear)")
decisionplot(best_machine, dat, class = "y", main = "Decision Boundary: Support Vector Machine (Radial without scale)")
decisionplot(best_machine2, dat, class = "y", main = "Decision Boundary: Support Vector Machine (Radial with scale)")

#########################################################################################
#########################################################################################
#########################################################################################



