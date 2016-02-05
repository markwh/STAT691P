# There is a "train.csv" and "test.csv" file.  The "train.csv"
# file has outcomes of whether or not a passenger survived.
# The "test.csv" does not have outcome data.
library(ProjectTemplate)
load.project()


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

# The following reads in the data.

train.data <- read.csv("data/titanictrain.csv", na.strings = c("NA", "")) %>% 
  mutate(sex = as.factor(sex), 
         embarked = as.factor(embarked), 
         cabin = as.character(cabin),
         cabin = ifelse(cabin == "", NA, cabin))

train.data.new <- train.data

# graph missing data by data column
# red color is missing, black is not missing

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




###############################
## MISSING DATA
###############################


###############################
## 1) MISSING DATA FOR "age"
###############################

## first, check the values for age:

summary(train.data.new$age)

## the average age is 29.70, with 177 missing values

## We will fill in average ages by title

## First, get title from each data value
## by using a newly-created function named "getTitle":

getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
  data$title <- substr(data$name, title.dot.start+2, title.comma.end-1)
  return (data$title)
}  

#  create titles for the training data

train.data.new$title <- getTitle(train.data.new)

# check which titles are unique

unique(train.data.new$title)

# check which of the titles have missing data
# using "bystats" function from "Hmisc" package

#options(digits=2)
require(Hmisc)
bystats(train.data.new$age, train.data.new$title, 
fun=function(x)c(Mean=mean(x),Median=median(x)))

# The only missing were the following

titles.missing.age.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")

# the next step imputes the median age for each title
# the default for "impute" function is median

for (v in titles.missing.age.train){
train.data.new$age[which(train.data.new$title==v)] <- impute(train.data.new$age[which(train.data.new$title==v)])
}

# check the new "train.data.new$age" values and see 
# that none are now missing

summary(train.data.new$age)

bystats(train.data.new$age, train.data.new$title, 
fun=function(x)c(Mean=mean(x),Median=median(x)))


###################################
## 2) MISSING DATA FOR "embarked"
###################################

## first, change "embarked" to a factor

train.data.new$embarked <- as.factor(train.data.new$embarked)

## the following shows 2 "NA"s for "embarked"

summary(train.data.new$embarked)

## next, we change the "NA"s for embarked to the
## most common value, "S":

train.data.new$embarked[which(is.na(train.data.new$embarked))] <- 'S'

###############################
## 3) MISSING DATA FOR "fare"
###############################

# first, show a summary of the variable "fare"

summary(train.data.new$fare)

## some of the "fare" values are zero. One value of
## "512.30" shows that some tickets may have been
## purchased in groups.  The zero fare could have
## been for a baby.  We will treat the zero fares
## as missing data, and then impute the fares by
## class.

## look at a subset of small fares by age, title, pclass, fare

subset(train.data.new, fare < 7)[order(subset(train.data.new, fare < 7)$fare, 
                          subset(train.data.new, fare < 7)$pclass), 
                          c("age", "title", "pclass", "fare")]

## next, we will impute the fare by class
## first, replace the "0"s with "NA", since the
## "impute" function uses "NA"s.

train.data.new$fare[ which( train.data.new$fare == 0 )] <- NA


## next, create a variable that contains the unique classes

pclass.levels <- sort(unique(train.data.new$pclass))

## this next statement imputes fare by class; again, the
## "impute" function imputes the median value of the class

for (v in pclass.levels) {
train.data.new$fare[ which( train.data.new$pclass == v)] <- impute(train.data.new$fare[which( train.data.new$pclass == v)])
}



