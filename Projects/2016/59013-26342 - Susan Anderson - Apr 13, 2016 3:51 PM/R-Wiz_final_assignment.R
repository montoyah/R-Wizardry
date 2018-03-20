##### Susan Anderson
##### R-wizardy Final Project
#To be used with PDF file, this contains the raw code
#Comments and explanations are in the PDF 

rm(list=ls(all=TRUE))
#install packages
install.packages("tidyr")
install.packages("dplyr")
library('tidyr')
library('dplyr')


#_________________ Gather Function _________________________

#Generate data frame
set.seed(10)
messy.lakes <- data.frame(
  id = 1:4,
  week = sample(rep(c('1','2'), each =2)),
  nitrite.1 = runif(4),
  nitrite.2 =runif(4),
  phosphorus.1 = runif(4),
  phosphorus.2 = runif(4)
)
print(messy.lakes)

#Use the GATHER function: 
tidier.lakes <- gather(messy.lakes, key, nutrient, -id, -week)
print(tidier.lakes)

#___________________ Spread Function ____________________________


#This uses the data frame that was used previously in GATHER
spread(tidier.lakes, key, nutrient, fill= NA, convert=F, drop= T)
print(tidier.lakes)

#_____________ Read in dirty_iris.csv ___________
#Read in data
setwd("/Users/Susan/Documents/R_Code")
getwd()

dir()
mydata <- read.csv("dirty_iris.csv")

#Look at data
head(mydata)

#Rename the column so you can call it 
colnames(mydata) <- c("xx")

#_______________________ Separate Function ________________

mydata<-separate(mydata, xx, into=c("sep.l","sep.w", "pet.l", "pet.w", "species"), sep=",")
head(mydata)


#_______________________ Unite Function ____________________

un.data<-unite_(mydata,"sep.w.l", c("sep.l", "sep.w"), sep="_")
head(un.data)
united.data <- unite(mydata,sep.wl, c(1:2), sep="_")
head(united.data)
uni.data<- unite(mydata,newc, c(1:3), sep="_")
head(uni.data)


#________________ Extract_numeric Function _______________

example1 <- c("$4,000.00", "50%", "$7", "9b", "#84m")

extract_numeric(example1)


#________________ Fill Function _______________

example2 <- data.frame(Month = 1:12, Year = c(2000, rep(NA, 11)))
example2[6,2] <-2001
print(example2)

filled<- fill(example2, Year)


#________________ Full_seq Function _______________

part<- c(1,3,6, 7, 10)
full_seq(part,1)
full_seq(part, 0.5)


#________________ Replace_na Function _______________

example3 <- data_frame(x = c(12, 23, NA, 35, NA), y = c("nitrogen", NA, "phosphorus", NA, "carbon"))
print(example3)
replace_na(example3, list(x = 0, y = "unknown"))




