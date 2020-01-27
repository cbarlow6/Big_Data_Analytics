rm(list = ls())
setwd("C:/Users/lxue5/Dropbox/2019Sp/CIS8695/3 NB SVM KNN")

library(e1071)  # For NB and SVM
library(caret)  #select tuning parameters
library(DMwR)    # For KNN
library(MASS)   # contains the data
# install.packages("kknn")
library(kknn)   #weighted k-nearest neighbors

# Import training and validation (test) data
data(Pima.tr)
data(Pima.te)
pima <- rbind(Pima.tr, Pima.te)

# Standardize data
pima.scale <- data.frame(scale(pima[, -8]))
pima.scale$type <- pima$type

