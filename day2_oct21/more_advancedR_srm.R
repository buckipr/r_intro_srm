#-----------------------------------------------------------------------------#
# Intro to R Workshop                                                         #
# Sociological Research Methods                                               #
# October 21st                                                                #
#                                                                             #
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# 0: SET UP 
#-----------------------------------------------------------------------------#
# 0.1: Create Time Stamp and Set Working Directory
date()      ## print the current date and time at the Console
getwd()     ## print the current working directory at the Console
dir()       ## print the names of files/folders in the current directory
#setwd("C:/Users/Student/Chewbacca")  ## set the working directory
dir()

# 0.2: Install Packages: ggplot2 (this could take a while) & haven
# 0.2.1: ggplot2
library()       ## list packages that are installed on the computer
                ## if we don't see ggplot2, then we can install with:
                ## install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# 0.2.2: haven
library(haven)

help(package = "haven")
?read_dta

#-----------------------------------------------------------------------------#
#1: LOAD & CLEAN DATA
#-----------------------------------------------------------------------------#
# 1.1: Get with the Loadin'
data2 <- read_dta("introR_data2.dta")
names(data2)
summary(data2)

# 1.2: Breaking out the Cleaning Supplies
# 1.2.1: data2$gnp -- create new variable data2$gnpClean that has no negative values
summary(data2$gnp)
data2$gnp

data2$gnpClean <- data2$gnp  ## create new variable
## remember the 2 steps:
## (1) create an index (TRUE/FALSE) to identify rows/observations that need to be changed
## (2) assign new values
data2$gnp < 0
cbind(data2$gnp, data2$gnp < 0)
cbind(data2$gnp, as.character(data2$gnp < 0))

data2$gnpClean[data2$gnp < 0] <- NA

## sanity check
cbind(data2$gnp, data2$gnpClean)

# 1.2.2: data2$unemployed --> data2$unempClean
summary(data2$unemployed)
data2$unemployed

data2$unempClean <- data2$unemployed  ## create new variable
data2$unempClean[data2$unemployed == 23] <- NA
data2$unempClean[data2$unemployed == 6]  <- NA
data2$unempClean[data2$unemployed > 200000]  <- NA
cbind(data2$unemployed, data2$unempClean)

# 1.3: Exercises

## [1] Create a new version of data2$employed, called data2$empClean, by
##     replacing all values greater than 100 with NA

## [2] Create a new variable data2$empLow that takes a value of 0 if the
##     employment rate is greater than or equal to the median, and 1 for 
##     years when the employment rate is less than the median.

## [3] Suppose a reviewer suggested you use the 32nd quanitle (as opposed to the) median
##     for your cut point for low employment.  How would you find this value?

## [4] Look up the help documentation for the recode() function.  What library is it in?

## [5] The following command loads an example dataset that comes with R/RStudio
##data(sleep)
##     Use recode() to create a new variable sleep$gender, with values "female" for group==1
##     and "male" for group==2

## [6] Remove the sleep data set.


#-----------------------------------------------------------------------------#
# 2: TAKE GGPLOT2 FOR A QUICK SPIN
#-----------------------------------------------------------------------------#
help(package = ggplot2)
?qplot

# 2.1: basic plot
qplot(population, empClean, data = data2)
qplot(log(population), empClean, data = data2)

# 2.2: add a third dimension
qplot(population, empClean, data = data2, color = year)
qplot(population, empClean, data = data2, geom = c("point", "smooth"))
qplot(year, empClean, data = data2, geom = c("point", "path"))

# 2.3: Exercise create a new categorical variable, named decade, that has 
#      values 40 (for the years 1940-1949); 50 (for the years 1950-1959);
#      and 60 (1960-1969)
qplot(population, empClean, data = data2, shape = decade)

#-----------------------------------------------------------------------------#
# 3: TABLE OF DESCRIPTIVE STATISTICS
#-----------------------------------------------------------------------------#

## suppose we are only going to analyze data2, we can attach() this
## data frame so we have direct acess to the variables
attach(data2)
#### undo with detach(data2)

?mean  ## explain order of arguments and names
mean(gnpdeflator)
mean(x = gnpdeflator)
mean(na.rm = TRUE, gnpdeflator)


## means & standard deviations
data2Means <- c(mean(gnpdeflator, na.rm=TRUE),
                mean(gnp, na.rm=TRUE),
                mean(unemployed, na.rm=TRUE),
                mean(armedforces, na.rm=TRUE),
                mean(population, na.rm=TRUE),
                mean(year, na.rm=TRUE),
                mean(employed, na.rm=TRUE))
data2Means

data2SD <- c(sd(gnpdeflator, na.rm=TRUE),
             sd(gnp, na.rm=TRUE),
             sd(unemployed, na.rm=TRUE),
             sd(armedforces, na.rm=TRUE),
             sd(population, na.rm=TRUE),
             sd(year, na.rm=TRUE),
             sd(employed, na.rm=TRUE))
data2SD

## create a matrix with means & standard deviations
data2Tab1 <- cbind(data2Means, data2SD)
rownames(data2Tab1) <- names(data2)
colnames(data2Tab1) <- c("Means", "Standard Deviations")
data2Tab1

## write out results to CSV file
write.csv(data2Tab1, file="data2Tab1.csv")

new_tool <- function(x) {
  x_plus_1 <- x + 1
  return(x_plus_1)
}

new_tool(x)


## Exercise: can you add % of cases with missing data (by variable)?

#-----------------------------------------------------------------------------#
# 4: REGRESSION MODELS
#-----------------------------------------------------------------------------#
detach(data2) # detaching just to show a feature of data frames
## formulas: dependent variable ~ indVar_1 + indVar_2
## (note: subset option)

# 4.1: Model 1
mod1 <- lm(gnp ~ armedforces + population, data=data2)
mod1
names(mod1)

#### regression coefficients
mod1$coefficients  ## can also use coef(mod1)

#### fitted values
mod1$fitted ## can also use fitted(mod1)

#### summary
summary(mod1)
logLik(mod1)
plot(mod1)
dev.off()

par(mfrow=c(2,2))
plot(mod1)
dev.off()

# 4.2: Model 2
mod2 <- lm(gnp ~ armedforces + population + employed, data=data2)
summary(mod2)
names(summary(mod2))
summary(mod2)$r.squared

anova(mod1, mod2)
BIC(mod1, mod2)

# 4.3: Model 3
mod3 <- lm(gnp ~ armedforces + population*employed, data=data2)
summary(mod3)

## write results to csv
length(coef(mod1))
length(coef(mod2))
length(coef(mod3))

### create matrix to hold results
data2Tab2 <- matrix(NA, nrow=5, ncol=3)

data2Tab2[1:3,1] <- coef(mod1)
data2Tab2[1:4,2] <- coef(mod2)
data2Tab2[1:5,3] <- coef(mod3)
data2Tab2
rownames(data2Tab2) <- names(coef(mod3))
colnames(data2Tab2) <- c("Model 1", "Model 2", "Model 3")
data2Tab2

write.csv(data2Tab2, file="data2Tab2.csv")

# 4.4: Exercises
## [1] Add standard errors in parentheses (hint: check out the paste command)
adjective <- "sooper kool"
paste("This ", "is", "   the paste", adjective, "ComManD", sep = "")

## [2] How do you run a logistic regression?  Fit a logit model using the 
##     macroeconomic dataset, data2, on population and gnpClean.  Are the
##     effects statistically significant?


#---------------------------------------------------------------------#
# 99: ALL DONE
#---------------------------------------------------------------------#
save.image("introR_data2_models.RData")
