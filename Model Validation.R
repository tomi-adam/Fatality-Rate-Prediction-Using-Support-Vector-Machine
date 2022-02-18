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
