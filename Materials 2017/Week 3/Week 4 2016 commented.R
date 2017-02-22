######################################################################################
# February 4, 2016  ##################################################################
# Author: Kyle L Wilson ##############################################################
# Objective: Build week 4 code to teach data management and data plotting in R #######
######################################################################################
rm(list=ls(all=TRUE))
ints <- 1:50
#To find "types" of numbers, %/% and %% are handy

#%% returns the remainder
ints%%2

#%/% returns the quotient
ints%/%3

#Even numbers
ints%%2 == 0
ints[ints%%2 == 0]

#numbers divisible by 5 or 6
ints[ints%%5 == 0 | ints%%6 == 0]


#<<<<<<<Numbers that are not divisible by 4>>>>>>>>
ints[ints%%4 != 0]

#Can also use brackets for more complicated selections
ints[(ints%%5 == 0 | ints%%6 == 0) & ints<=30]
ints[ints%%5 == 0 | (ints%%6 == 0 & ints<=30)]


#################################################################
### Problem 1 ###################################################
#<<<<<<Use logical operators to simultaneously find the odd numbers between -50 and 0 (inclusive) and the even numbers between 0 and 50 (inclusive)
ints <- -50:50
ints[(ints <=0 & ints%%2 == 1) | (ints >=0 & ints%%2 == 0)]


rm(list=ls(all=TRUE))

#########################
# There are multiple ways to read data into R
# most of them use some sort of read.X where the X can be delimited, csv, table, etc.
# the primitive version of reading in data into R uses the function called 'scan'.
# If you have complex data, you may attempt to read in this data using scan()
#########################

setwd("/home/oscar/Dropbox/R wizardry/2017/Datasets")
getwd()
list.files()

option.1 <- read.csv("compounds_triplicates.csv") # doesn't read the header, and reads in all character data as factors

#or without setting the working directory first

option.2 <- read.csv("/home/oscar/Dropbox/R wizardry/2017/Datasets/compounds_triplicates.csv")

#Renaming the dataset as "data":
data <- option.1


str(data)
data$compound
unique(data$compound)

##############
## Can use subset() to split the data based on some logical argument contained within the dataset
## For example, let's only look at data from the first half of the experiment (before day 200)
##############
early_data <- subset(data,data$day < 200)
unique(early_data$day)

#### Adding a column of NEW DATA to an existing dataset

data$Staff <- rep(c("Oscar","Kyle"),length(data$day)/2)

# Alternatively could do that more robustly
data$Staff <- rep(c("Oscar","Kyle"),1000)[1:length(data$day)]
head(data)

#=======================================================================
#-----------------------(4.1) missing data and NA's---------------------
#=======================================================================
###Introduce random NA's, then deal with the introduced NA's

#In addition to TRUE and FALSE, NA is another type of logical variable
class(NA)

#NA represents an empty slot and are also produced when invalid math is attempted
as.numeric("a")

#Unfortunately, we often have to deal with missing data so lets throw some in
mean(data$methane)
data[c(2,20,200),6] <- NA
data[50,5] <- NA
head(data)
mean(data$methane)
#Having NA messes up a lot of functions
apply(data[,c("methane","sd")],2,FUN=median)
mean(data[,5])

#Often there are built in ways to ignore them
#most common functions have a parameter such as na.rm, na.omit, na.exclude, etc...

mean(data[,5], na.rm = T) 
mean(data[,"methane"], na.rm=T)

#But we can use logic to ignore them ourselves using is.na()
cbind(data[1:10,5], !is.na(data[1:10,5]))

# Problem 2:
# <<<<Select the data from column 5 which is not na, calculate its mean>>>>>>>

data.frame(data[1:10,5], !is.na(data[1:10,5]))
mean(data[!is.na(data[,8]),5])


#=======================================================================
#---------------(4.2) Calculations based on another column--------------
#=======================================================================
colnames(data)
#tapply applys a function to one column based on the groups found in another column
tapply(data$methane,data$salinity,FUN=mean,na.rm=TRUE)
tapply(data$sd,data$group,FUN=sd,na.rm=TRUE)

#aggregate is the general form, allowing you to group by more than one column
aggregate(data$methane,list(data$compound,data$group),mean)
aggregate(data$sd,list(data$day),mean)
aggregate(data$sd,list(data$compound,data$group,data$day), na.rm = T ,mean)
?aggregate
aggregate(data$biomass,list(data$nutrient,data$year),mean,na.rm = T)

# ALTERNATIVELY, this form keeps the names
aggregate(methane~compound+group,data=data,FUN=mean,na.rm=T)
aggregate(cbind(methane,sd)~compound+group,data=data,FUN=mean,na.rm=T)

## Let's say you wanted to quickly calculate your sample size for each categorical factor of
# nutrient and year. How would we do that in such a way that R is not counting the NAs?
# There are three ways:
aggregate(data,list(data$compound,data$group),function(x)(sum(!is.na(x))))
aggregate(data,list(data$compound,data$group),function(x)(length(x[!is.na(x)])))

temp <- subset(data,data$methane!="NA")
dim(temp)
aggregate(temp,list(temp$compound,temp$group),length)

#<<<<<<<<<< Problem 3 >>>>>>>>>>>>>
# Calculate the median in methane data by day, compound, and group

ag <- aggregate(methane~day+compound+group,data=data,FUN=median,na.rm=T)

###
# Here is a variety of ways to plot box-and-whisker plots in R
boxplot(methane~group,data=ag,col="steelblue",ylab="Median Methane Production",range=0,ylim=c(0,110))
boxplot(methane~group,data=ag,col="steelblue",ylab="Median Methane Production",range=1,ylim=c(0,110))
boxplot(methane~group,data=ag,col="steelblue",ylab="Median Methane Production",outline=F,ylim=c(0,110),boxwex=0.25)


#=======================================================================
#-----------------(4.3) Create and export a summary table---------------
#=======================================================================

ag <- aggregate(methane~day+compound+group,data=data,FUN=median,na.rm=T)
ag

getwd()
dir()
if(sum(dir()== "methane medians.csv")>0){print("WATCH OUT!!!! FILE ALREADY EXISTS")}
write.csv(ag, "methane medians.csv",row.names=FALSE)



#=======================================================================
#--------------(4.4) selecting data based on another column-------------
#=======================================================================

#We have been selecting data based on itself
#We can also select data based on other data

data <- read.csv("compound.csv",header=T,stringsAsFactors = FALSE,row.names = 1)

#We can find the odd letters
letters[(1:26)%%2 == 1]

#We can select the data collected on day 1
data[data$day == 1,]

#We can select the data collected on day 1 and day 10
data[data$day == 1 | data$day == 10,]

#We can select based on 2 or more data types
data[data$day == 1 & data$compound == "unamended",]

#We can store this subsetted data into a new variable
subdata = data[data$day == 1 & data$compound == "unamended",]
subdata$methane
subdata$sd


# <<<<<<<<<< Problem 4 >>>>>>>>>>>>
#  Create a table using tapply, calculating the mean methane for the Anthracene compound
#   for day 1 across all 3 salinity treatments. Write this table as a csv

unique(data$day)
temp <- tapply(data$methane[data$day == 1 & data$compound == "Anthracene"], 
               data$salinity[data$day == 1 & data$compound == "Anthracene"], mean)
?tapply
subdata <- data[data$day == 1 & data$compound == "Anthracene",]
temp <- tapply(subdata$methane, subdata$salinity, mean)
temp

write.csv(temp, "Anthracene.csv")


#=======================================================================
#-----------------(4.5) Finding data based on its value-----------------
#=======================================================================

data$day == 1

which(data$day == 1) 
which(is.na(data$day))
which(data$methane == max(data$methane, na.rm = T))
data[which(data$methane == max(data$methane, na.rm = T)),]
