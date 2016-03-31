##############
# Resampling #
##############
require(ISLR)
require(boot)
?cv.glm

# Plot MPG against horsepower
plot(mpg~horsepower, data=Auto)

# Leave-one out cross validation
glm.fit=glm(mpg~horsepower, data=Auto)

# Refits model for LOOCV - Runs slow
# First number in result is raw (lieu) result
# Second number is bias corrected version
cv.glm(Auto,glm.fit)$delta

# Lets write simple function to use
# Leave one out classification error
# Aka formula 5.2 in ISLR
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

# Try LOOCV
loocv(glm.fit)

# Setup vector for error collection
cv.error=rep(0,5)
degree=1:5

# Iterate through d in degree
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

# 10-fold Cross-Validation
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")

#############
# Bootstrap #
#############

# Minimum risk investment - Section 5.2

# This is the minimum risk investment function
# alpha = var y - covariance x,y /
# var x + var y - 2 * cov x,y
alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

# Run alpha on portoflio columns
alpha(Portfolio$X,Portfolio$Y)

# What is the standard error of alpha?

# Wrapper to allow bootstrap to function
# takes data frame and index
# and computes statistic
alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
}

# Run index on portfolio 1-100
alpha.fn(Portfolio,1:100)

# Choose seed for random sampling
# Use random sample instead of sample 1:n

set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

# Use bootstrap function
boot.out=boot(Portfolio,alpha.fn,R=1000)

# Summary of bootstrap
boot.out
# Plots boot
plot(boot.out)
