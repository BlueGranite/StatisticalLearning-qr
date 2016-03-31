#####################
## Model Selection ##
#####################

library(ISLR)
summary(Hitters)

# Remove missing values
Hitters=na.omit(Hitters)
# Check for missing values
with(Hitters,sum(is.na(Salary)))

###############
# Best Subset #
###############
install.packages("leaps")
library(leaps)

# Use best subset
regfit.full=regsubsets(Salary~.,data=Hitters)

# Display star next to best subset of size 1
summary(regfit.full)

# Now run best subset using all 19 variables
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
reg.summary
names(reg.summary)

# Plot a cp summary
# cp is estimate of prediction error
plot(reg.summary$cp,xlab="Number of variables",ylab="Cp")
# Find the min value on the plot

# Display best subset model based on min value
points(10,reg.summary$cp[10],pch=20,col="red")

# There is a plot for the regsubsets object
# Black squares indicate variable is in, white is out
plot(regfit.full,scale="Cp")

# Coefficients of best subset model
coef(regfit.full,10)

##################################
### Forward Stepwise Selection ###
##################################

# Use regsubsets with "forward" method
regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")

############################################
### Model Selection Using Validation Set ###
############################################

# Dimensions of hitters dataset
dim(Hitters)

# Random number seed set to 1
set.seed(1)

# Set training set, sample of size 180 from sequence 1:263
train=sample(seq(263),180,replace=FALSE)
train

# Use training subset
regfit.fwd=regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method="forward")

# Work out validation errors
# Set up a vector with 19 slots
val.errors=rep(NA,19)

# Build test model matrix
# Index based on -train (exclude obs indexed by train)
# aka negative index set
x.test=model.matrix(Salary~.,data=Hitters[-train,])

# Now make predictions for each model
# for each i in 1:19

# Extract coefficients in mode of size "id"
# No predict method for raked subsets
# Index columns by names on coefficient vector
# then do matrix multiplied by coefficient vector
# Compute MSE

for(i in 1:19){
  coefi=coef(regfit.fwd,id=i)
  pred=x.test[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2)
}

# Perform forward stepwise
plot(sqrt(val.errors),ylab="Root MSE",ylim=c(300,400),pch=19,type="b")
points(sqrt(regfit.fwd$rss[-1]/180),col="blue",pch=19,type="b")
legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19)

# Write a predict method for 'regsubsets'
# object = regsubsets,
# newdata = data to use
# id = model id
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

# Model selection by Cross-Validation
set.seed(11)

# Create vector from 1:10, length number of hitters
# Sample then shuffles that vector
folds=sample(rep(1:10,length=nrow(Hitters)))
folds
table(folds)

# Create a matrix for errors (10 rows, 19 col)
cv.errors=matrix(NA,10,19)
# Go through double loop
for(k in 1:10){
  # Fit a regsubsets model
  # Training data = all observations whose fold is not equal to k
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
  # Iterate through loop i = 1:19
  for(i in 1:19){
    # Use predict model
    # Best.fit is regsubsets object (Predict.regsubsets)
    # Predict at observations with fold ID = k
    # for each id = i
    pred=predict(best.fit,Hitters[folds==k,],id=i)
    # Compute MSE
    cv.errors[k,i]=mean( (Hitters$Salary[folds=k]-pred)^2)
  }
}

# Process the output matrix
# Take column means (10 rows, each w MSE for that fold)
rmse.cv=sqrt(apply(cv.errors,2,mean))
plot(rmse.cv,pch=19,type="b")

##################################
### Ridge Regression and Lasso ###
##################################
install.packages("glmnet")
library(glmnet)
x=model.matrix(Salary~.-1,data=Hitters)
y=Hitters$Salary

# First we fit a ridge regression model
# Use glmnet with alpha=0 (alpha=1 is lasso, 0 is ridge)
# Also cv.glmnet will do x-validation
fit.ridge=glmnet(x,y,alpha=0)

# Plot coefficients
plot(fit.ridge,xvar="lambda",label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

# Lasso model
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

# Use earlier train division to select
# 'lambda' for lasso and predict
lasso.tr=glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)

# y-train is vector of length 83
# y[-train]-vector recycles vector 89 times, column-wise
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")

# Extracts component lambda, 
# orders in descending order
# Grabs best lambda
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)
