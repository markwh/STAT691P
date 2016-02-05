








## simulate logistic regression data


rm(list=ls())

# need to install package "mvtnorm"

library("mvtnorm")

library("Amelia")

library("ellipse")

library("plyr")

library("corrgram")

library("Hmisc")

library(caret)

library(pscl)

library(glmnet)




# number of data values to simulate

N <- 6000

set.seed(284695)

# p is probability

p <- rep(NA,N)

# x's are continuous covariates

x1 <- rep(NA,N)
x2 <- rep(NA,N)
x3 <- rep(NA,N)
x4 <- rep(NA,N)
x5 <- rep(NA,N)
x6 <- rep(NA,N)


# simulate correlated data

num.corr.predictors <- 3

x <- matrix(NA,N,num.corr.predictors)

# next, specify the variance-covariance matrix

var.diagonal <- 1  # this is variance on diagonal

cov.off.diag <- 0.9*var.diagonal  # covariance of predictors

# the following sets the means to zero

mean.vec <- rep(0,num.corr.predictors)

# the following assigns the covariance matrix

sigma.mat <- matrix(cov.off.diag,num.corr.predictors,num.corr.predictors)

# the following assigns the diagonal of the covariance matrix

for (i in 1:num.corr.predictors){
sigma.mat[i,i] <- var.diagonal
}


# create matrix of 3 correlated predictors

x <- round(rmvnorm(N,mean=mean.vec, sigma=sigma.mat ),6)
         
x1 <- x[,1]
x2 <- x[,2]
x3 <- x[,3]

# create independent normally distributed predictors

x4 <- round(rnorm(N,mean=2,sd=1),6)

x5 <- round(rnorm(N,mean=-2,sd=1),6)

# create a binary predictor; can change prob. of success

x6 <- rbinom(N, 1, prob=0.3)


#### create new variables based on the first 6 predictors

x7 <- x1*x4
x8 <- x2*x4*x5
x9 <- x1^2
x10 <- x3*x2
x11 <- (x2)^3
x12 <- x2*x6


# assign values for beta coefficients

b1 <- 0.82
b2 <- -2.1
b3 <- 1.05
b4 <- -0.65
b5 <- 0.73
b6 <- 0.5

b7 <- 0.6
b8 <- -0.7
b9 <- 0.9
b10 <- 1.1
b11 <- -1.4
b12 <- 1.1

# simulate probabilities based on logistic regression model

for (i in 1:N){
p[i] <- exp( b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i] + b6*x6[i] +
b7*x7[i] + b8*x8[i] + b9*x9[i] + b10*x10[i] + b11*x11[i] + b12*x12[i] )/(1+
exp( b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i]+ b6*x6[i] +
b7*x7[i] + b8*x8[i] + b9*x9[i] + b10*x10[i] + b11*x11[i] + b12*x12[i] ))
}


# simulate binary "y" values based on probabilities

y <- rep(NA,N)

for (i in 1:N){
y[i] <- rbinom(1,1,p[i])
}


# check the values of the logistic regression model, that
# they are all significant at alpha = 0.1 or less

logistic.out <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 -1, family=binomial(logit))


# compare the coefficients here with the values that were simulated

summary(logistic.out)


# write out the simulation data, only writing out
# the 6 data variables that were created, not additional
# terms such as interaction terms or polynomial terms

sim.data.out <- cbind(y,x1,x2,x3,x4,x5,x6)

##  create training data and test data


sim.data.train <- sim.data.out[1:round((2/3)*N,0),]

# write out test data, HERE, KEEP THE y outcome value in the
# data for checking the accuracy; later, the y outcome value
# will be removed when writing out the data to a file

sim.data.test <- data.frame(sim.data.out[(round((2/3)*N,0)+1):N,])


#####  NEW STEPWISE HERE  #####



train.data.fin <- sim.data.train

## split training data into a training batch and a validation batch
## use 80% for training, 20% for validation later

train.percent <- 0.8

## the "createDataPartition" function is from the "caret" package

training.rows <- createDataPartition(train.data.fin[,1], 
                                     p = train.percent, list = FALSE)
train.batch <- data.frame(train.data.fin[training.rows, ])
validate.batch <- data.frame(train.data.fin[-training.rows, ])



###########################
#### START STEPWISE HERE
###########################

# use a model with all three-way interactions and
# polynomial terms up to third-order for all variables.

# "poly(x1,degree=3,raw=TRUE)[,:3]" uses only the second and third
# column, which are: x1^2, x1^3 

fit.logistic.step1.pre <- glm(y ~ (x1 + x2 + x3 + x4 + x5 + x6)^3 + poly(x1,degree=3,raw=TRUE)[,2:3] + poly(x2,degree=3,raw=TRUE)[,2:3] + poly(x3,degree=3,raw=TRUE)[,2:3] + poly(x4,degree=3,raw=TRUE)[,2:3] + poly(x5,degree=3,raw=TRUE)[,2:3] + poly(x6,degree=3,raw=TRUE)[,2:3],data=train.batch,family=binomial("logit"))

fit.logistic.step1 <- step(fit.logistic.step1.pre,direction="backward",test="F")

summary(fit.logistic.step1)


# there is no R^2 in logistic regression, but can use
# McFadden R^2 index, from library(pscl): this is pseudo-R^2

mcfadden.step1 <- pR2(fit.logistic.step1)[4]



############################
###### MODEL EVALUATION
############################


# use logistic regression model from "fit.logistic.step1"
# and apply it to the "validation.batch" data

glm.predict1.pre <- predict(fit.logistic.step1,validate.batch)

# change probabilities to binary "0,1" values

glm.predict1 <- as.numeric(glm.predict1.pre > 0.5)

# check the accuracy with "confusionMatrix"

outcome1 <- confusionMatrix(glm.predict1, validate.batch[,1])


##########


### model prediction for remaining 1/3 of data



# use logistic regression model from "fit.logistic.step1"
# and apply it to the "test.batch" data (the remaining
# 1/3 of data)

glm.predict2.pre <- predict(fit.logistic.step1,sim.data.test)

# change probabilities to binary "0,1" values

glm.predict2 <- as.numeric(glm.predict2.pre > 0.5)


# check accuracy with "confusionMatrix"

outcome2 <- confusionMatrix(glm.predict2, sim.data.test[,1])


#### THIS IS THE LAST STEP, once all simulation work is
#### complete


# first, remove the "y" outcome from the test data set

sim.data.test.write <- sim.data.test[,-1]

# write out all data in plain text, tab-delimited forma; 
# directory needs to be edited

# "options(scipen=10)" forces R not to use exponential notation,
# such "e+10", etc.

options(scipen=10)
write.table(sim.data.out,
"c:/aprojcourse/titanic/data/model1.logistic.all.txt",sep="\t",
row.names=F,col.names=F)
options(scipen=0)

# write out train data; directory needs to be edited

options(scipen=10)
write.table(sim.data.train,
"c:/aprojcourse/titanic/data/model1.logistic.train.txt",sep="\t",
row.names=F,col.names=F)
options(scipen=0)

# write out test data with outcome removed; directory needs to be edited

options(scipen=10)
write.table(sim.data.test.write,
"c:/aprojcourse/titanic/data/model1.logistic.test.txt",sep="\t",
row.names=F,col.names=F)
options(scipen=0)









