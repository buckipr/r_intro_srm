#-----------------------------------------------------------------------------#
# Intro to R Workshop                                                         #
# Sociological Research Methods                                               #
# October 19th                                                                #
#                                                                             #
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# 0: BASIC COMMANDS
#-----------------------------------------------------------------------------#

# 0.1: Set up Working Directory
date()              ## print the current date and time at the Console
getwd()             ## print the current working directory at the Console
dir()               ## print the names of files/folders in the current directory
dir("../")          ## print the contents in the folder "above" the current one
setwd("/Users/thomas.3912/School/IPR/Workshops/SocResearchMethods")       ## set the working directory to the folder "above" the current one
## alternatively, from the window menus: "Session" -> "Set Working Directory" -> "Choose Directory..."
dir()

# 0.2: Creating Objects
ls()                   ## what objects are in R's memory?

x <- 3                 ## create a new object called "x" that holds the value 3
x                      ## print the content(s) / value(s) by evaluating the object's name
y <- c(5, -2, 0.2)     ## create another object by concatenating 3 numbers
y
z <- c("red", "green") ## we can use characters (or strings) as well
z
ls()

## What does this do?
new_z <- c("red", "green", 3)

## finally, if you wanted to remove an object, say z, 
rm(z)
ls()


# 0.3: Check Libraries and Access Help Files
library()               ## list the libraries (or packages) that are installed on your computer
library(Yoda_Jedi_Master)
library(help = ggplot)  ## oops!
library(help = ggplot2) ##
# install.packages("ggplot2")  note to Jason: install furniture
?aes
library(ggplot2)
?aes

help.search("weighted mean")  ## can also use: ?? "weighted mean"
help("weighted.mean")         ## can also use: ?weighted.mean
?spatstat.geom::weighted.median

# 0.4: Exercises

## [1] Create a new object/vector, called X, that holds the numbers for 1 to 10.

## [2] Create a new vector, called X_log, that is equal to the log of X.

## [3] Create 4 new vectors that hold letters a, b, c, and d,  Call the new vectors: 
##     (1) _new_vector, (2) 4timesX, (3) new_vector, and (4) new.vector

## [4] Describe what happens with the following commands (without running them)
n <- 3
n
n <- n + n
n
3 + N

## [Bonus] write an if statement that only creates the folder Chewbacca
##         if it does not already exist.


#-----------------------------------------------------------------------------#
# Brief break to learn about data types and data structures                   # 
#-----------------------------------------------------------------------------#
## useful functions: rep(), length(), dim(), nrow(), ncol(), summary(), 
#                    sort(), order()
x <- 1:10
x
length(x)
summary(x)
sort(x)  
order(x)     ## hmm...not very interesting
x <- 10:1
x
sort(x)
order(x)     ## how do we get the order from largest to smallest?

## basic indexing, missing data, and ugh!
x[2]
x[2] <- NA  ## we will change the second number to NA (R's way of indicating missing data/values)
sum(x)      ## uh-oh, can you figure out how to fix this so we get an actual number?



#-----------------------------------------------------------------------------#
# 1: PREPARE EXAMPLE DATA SET
#-----------------------------------------------------------------------------#

# 1.0: Load
data1 <- read.csv("introR_data1.csv") ## data on US States
class(data1)                          ## data frames can contain different types of variables
names(data1)                          ## print variable names
#edit(data1)                          ## open up spreadsheet-like editor (careful!)
                                      #### Look, but don't touch PLEASE!!!!
                                      ## in R Studio check out this df in Environment Pane
dim(data1)                            ## dimensions (i.e., # of cases & variables)

# 1.1: Explore & Clean Variables

# 1.1.1: Variable: data1$X (state names)
## we access variables in data frames with $
## "X" as a variable name is not helpful!
data1$X

data1$stateName <- data1$X  ## let's preserve the original data and create a new variable
names(data1)
data1$stateName

length(data1$stataName)     ## huh?!?

## index a vector (matrix/data frame) using [element number] ([row, column])
data1$stateName[1]
data1[1, 1]

## quick data checks
data1$stateName == "Ohio"         ## did Ohio make it in?
table(data1$stateName == "Ohio")  ## a little easier to digest as a table
#### R is an object oriented language!!!

which(data1$stateName == "Ohio")  ## which element (row) is it?
data1$stateName[35]

#### ummm....is this really a good idea?
data1$stateName[which(data1$stateName == "Ohio")]
data1$stateName[data1$stateName == "Ohio"]


unique(data1$stateName)          ## last I checked, there should be 50
length(unique(data1$stateName))  ## how many unique values do we have
                                 ## uh-oh! (code is getting a little scary again)

## Exercises

#### [1] What "type" of variable is stateName? What does this mean?

#### [2] Write an R command that prints out the 7th, 12th, and 33rd state names

#### [3] Write an R command that sorts the state names in reverse alphabetical order

#### [Bonus] Create a new variable that contains the state names, but in lower case letters

#### [Bonus * 5,000] Write an R command that prints out the state names that begin with the letter I


# 1.1.2: Variable: data1$Population
names(data1)
## start with a summary
summary(data1$Population)    ## "NA" is how R denotes a missing value
table(data1$Population < 0)  ## what happened to NAs?


## negative numbers?!? hmm.... again, let's create a new variable to
## preserve original data (or we could create a new data frame)
data1$cleanPop <- data1$Population
data1$cleanPop == data1$Population  ## are these two vectors the same?

## to clean this variable, we need:
## (1) a way to index all the negative values
## (2) reassign new values to a state if it has a negative value
cbind(data1$stateName, data1$Population < 0)  ## cbind() concatenates vectors into columns
                                              ## rbind() does the same by rows

## ok, so ```data1$Population < 0``` will serve as an index for negative
## population sizes.  now we want to reassign new values, say -99,
## to our new variable ```data1$cleanPop```
data1$stateName[data1$Population < 0]

## Huh...I thought there was only 1!  What the heck is going on her?!?









is.na(data1$Population)
table(is.na(data1$Population))
data1$stateName[data1$Population < 0 | is.na(data1$Population)] 

data1$cleanPop[data1$Population < 0 | is.na(data1$Population)] <- -99

cbind(data1$stateName, data1$cleanPop, data1$Population < 0)

## maybe -99 as missing is not such a good idea (R will think it is a
## numeric value); let's go with NA (can you make the change?)

#### all done?
summary(data1$cleanPop)
sort(data1$cleanPop)

## Exercises

#### [1] Create a new, cleaned version of the variable data1$Illiteracy

#### [2] Create a new, cleaned version of the variable data1$Life.Exp

#### [3] Create a new, cleaned version of data1$HS.Grad

#### [Bonus * 3] Clean the variable data1$Income and create a new variable log(Income)
####         hint: needs to be converted to NUMERIC values

# 1.2: Recode & Transform Variables
## bigPop as a yes/no variable
data1$bigPop <- NA
cbind(data1$stateName, data1$cleanPop, data1$bigPop)

## remember the steps: create an index, then reassign values
data1$bigPop[data1$cleanPop >  1000] <- TRUE
data1$bigPop[data1$cleanPop <= 1000] <- FALSE

## sanity check
cbind(data1$stateName, data1$cleanPop, data1$bigPop)

table(data1$bigPop)
table(data1$bigPop, useNA="ifany") ## useNA="always" is another option

## logPop
data1$logPop <- log(data1$cleanPop)  ## hmmm....what's up with this warning message?
summary(data1$logPop)


#---------------------------------------------------------------------#
# 99: ALL DONE
#---------------------------------------------------------------------#
save.image("introR_data1_clean.RData")  ## this command saves everything currently loaded in R's memory
                                        ## you can reload your saved workspace with the following commands
#load("intro_data1_clean.RData")