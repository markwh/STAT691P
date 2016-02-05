










# remove all saved values from R

rm(list=ls())

library("Amelia")

library("ellipse")

library("plyr")

library("corrgram")

library("Hmisc")

library(caret)

library(pscl)

library(glmnet)

# create a PDF file.  The file location needs to be edited.

pdf("c:/aprojcourse/titanic/r/figures/explore.pdf")


# There is a "titanictrain.csv" and "titanictest.csv" file.  
# The "titanictrain.csv" file has outcomes of whether or not 
# a passenger survived.  The "titanic test.csv" does not have 
# outcome data.

# Read in data

# First, create data types

train.data.types <- c('integer',   # PassengerId
                        'numeric',    # survived 
                        'factor',    # pclass, was character
                        'character', # name
                        'factor',    # sex, was character
                        'numeric',   # age
                        'integer',   # sibsp
                        'integer',   # parch
                        'character', # ticket
                        'numeric',   # fare
                        'character', # cabin
                        'factor'     # embarked, was character
)

missing.values <- c("NA","")

# (to check data type: class(name))

# The following reads in the data.  Need to change the
# subdirectory for data location.

train.data <- read.csv("c:/aprojcourse/titanic/data/train.csv",
colClasses=train.data.types,na.strings=missing.values)

train.data.new <- train.data

# graph missing data by data column
# red color is missing, black is not missing

require(Amelia)
missmap(train.data.new, main="Titanic Data - Map of Missing Data", 
        col=c("red", "black"), legend=TRUE)


# plot the data


par(mfrow=c(2,2))

barplot(table(train.data.new$survived),
        names.arg = c("died", "survived"),
        main="Variable: survived \n(Passenger Outcome)", col="black")

#x11()

barplot(table(train.data.new$pclass), 
        names.arg = c("first", "second", "third"),
        main="Variable: pclass \n(passenger traveling class)", col="red")

#x11()

barplot(table(train.data.new$sex), main="Variable: sex \n(gender)", col="blue")

#x11()


hist(train.data.new$age, main="Variable: age", xlab = NULL, col="darkgreen")

#x11()

par(mfrow=c(2,2))

barplot(table(train.data.new$sibsp), 
main="Variable: sibsp \n(number of siblings + spouse aboard)", col="darkblue")

#x11()

barplot(table(train.data.new$parch), 
main="Variable: parch \n(number of parents + children aboard)", 
        col="violet")

#x11()

hist(train.data.new$fare, main="Variable: fare \n(fee paid for ticket[s])", xlab = NULL, col="darkred")

#x11()

barplot(table(train.data.new$embarked), 
        names.arg = c("Cherbg", "Queens", "Southamp"),
        main="Variable: embarked \n(the port they left from)", col="darkviolet")


par(mfrow=c(2,2))

mosaicplot(train.data.new$pclass ~ train.data.new$survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=c("blue","red"), xlab="Class of Travel", ylab="Survived")

# show gender by survival

mosaicplot(train.data.new$sex ~ train.data.new$survived, 
           main="Passenger Fate by Gender", shade=FALSE, 
color=c("blue","red"), xlab="Sex", ylab="Survived")

# show embarkation by survival 

mosaicplot(train.data.new$embarked ~ train.data.new$survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=c("blue","red"), 
xlab="Embark Location\nC=Cherbg, Q=Queens, S=Southamp", 
ylab="Survived")


# show age by survival

boxplot(train.data.new$age ~ train.data.new$survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

corrgram.data <- train.data.new
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$survived <- as.numeric(corrgram.data$survived)
corrgram.data$pclass <- as.numeric(corrgram.data$pclass)
corrgram.data$age <- as.numeric(corrgram.data$age)
## generate correlogram
corrgram.vars <- c("survived", "pclass",  "age",
                   "sibsp", "parch", "fare")


corrgram.data.final <- corrgram.data[,corrgram.vars]

corr.matrix.train <- cor(corrgram.data.final,use="pairwise.complete.obs")

colorvals <- colorRamp(c("#CC0000","white","#3366CC"),space="Lab")

plotcorr(corr.matrix.train, col=rgb(colorvals((corr.matrix.train+1)/2), maxColorValue=255))

# this shows just correlation as numbers
         
corrgram(corr.matrix.train, type="cor",upper.panel=panel.conf,
lower.panel=panel.conf)


# close the graphic file.

graphics.off()

