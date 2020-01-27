rm(list = ls())
setwd("C:/Users/lxue5/Dropbox/2019Sp/CIS8695/1 Regression")
# options(repos = c(CRAN = "http://cran.rstudio.com"))

Tayko.df <- read.csv("CSV_Tayko.csv")

# select variables for regression
selected.var <- c("Spending","Address_US","Freq","last_update","Web","Gender",
                  "Address_RES")

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(Tayko.df)[1]), dim(Tayko.df)[1]*0.7)  
train.df <- Tayko.df[train.index, selected.var]
valid.df <- Tayko.df[-train.index, selected.var]


Tayko.lm <- lm(Spending ~ ., data = train.df)
options(scipen = 999)
summary(Tayko.lm)   # Get model summary

car::vif(Tayko.lm)


library(forecast)
Tayko.lm.pred <- predict(Tayko.lm, valid.df)

accuracy(Tayko.lm.pred, valid.df$Spending)

Tayko.lm.step <- step(Tayko.lm, direction = "forward")
summary(Tayko.lm.step)

null=lm(Spending~1, data=train.df)    
step(null, scope=list(lower=null, upper=Tayko.lm), direction="forward")

# Logistic Regression
# select variables for logistic regression
selected.var <- c("Purchase","Address_US","Freq","last_update","Web","Gender",
                  "Address_RES")

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(Tayko.df)[1]), dim(Tayko.df)[1]*0.7)  
train.df <- Tayko.df[train.index, selected.var]
valid.df <- Tayko.df[-train.index, selected.var]

logit.reg <- glm(Purchase ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
logit.reg.pred.purchase<-ifelse(logit.reg.pred > 0.5, 1, 0)

library(caret)
confusionMatrix(as.factor(logit.reg.pred.purchase), 
                as.factor(valid.df$Purchase))

# Cross-validation
train.df$Purchase <- factor(train.df$Purchase, levels = c(0,1))
valid.df$Purchase <- factor(valid.df$Purchase, levels = c(0,1))

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
cv.purchase <- train(Purchase ~ ., 
                 data=train.df, method="glm", family="binomial",
                 trControl = ctrl)
summary(cv.purchase)

cv.pred <- predict(cv.purchase, valid.df, type = "prob")
cv.pred.purchase <- predict(cv.purchase, valid.df, type = "raw")

confusionMatrix(cv.pred.purchase, as.factor(valid.df$Purchase))



x <- as.matrix(train.df[,c("Address_US","Freq","last_update","Web","Gender",
                           "Address_RES")])
y <- factor(train.df$Purchase,levels=c(0,1))

CvLassoMod <- cv.glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001, 
                        family="binomial")
best.lambda <- CvLassoMod$lambda.min

x_pred <- as.matrix(valid.df[,c("Address_US","Freq","last_update","Web","Gender",
                                "Address_RES")])
Lasso.pred <- predict(CvLassoMod, s = best.lambda, newx = x_pred, type="response")
confusionMatrix(as.factor(ifelse(Lasso.pred>0.5,1,0)), as.factor(valid.df$Purchase))





