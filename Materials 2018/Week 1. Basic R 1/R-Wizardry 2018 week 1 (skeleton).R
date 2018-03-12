
# R-Wizardry 2018

* R-Studio and environment: getting around and setting it up
* Data management and spreadsheet organization before starting. 
* Plan well before starting to work.



## Day 1

1.1 R as a calculator
1.2 Commenting your code 
1.3 Assigning variables
1.4 Data structure


### R as a calculator
R can do anything your basic, scientific, or graphic calcularor can.

#### Basic math


#### Mathematical functions


#### Plot equations


#### Common functions

?help
?c
?seq
?setwd()
?sort()
?dir()
?head()
?names()
?summary()
?dim()
?range()
?max()
?min()
?sum()
?pairs
?plot




### Commenting your code
#Inside a chunk of code in RMarkdown or anywhere in a R script, all text afer a hastag (#) will be ignored by R -and by many other programming languages. It's very useful to keep track of changes in your code and add explanations to the scripts. It is, in general, part of best coding practices to keep things tidy and organized.

#Comments may/should appear in three places:

#* The top of your script
#* Above every function you create
#* In line



#At the beginning of the script, describing the purpose of your script and what you are trying to solve

   #In line: Describing a part of your code that is not inmediatly obvious what it is for. 




### Assigning variables
#Now, let's create a new chunk of code by pressing simultaneously CTRL + ALT + i and create some variables.

#To run one line of code at the time into the console, press CTRL + ENTER (PC/LINUX) or COMMAND-ENTER (Mac). To run several lines at the time, highligth the lines of interest and proceed as describes above.




#Two ways to assign variables:
 #Left hand assigner (original way to assign results) 
 #Rigth hand assigner


# sometimes the <- is necessary for functions originally created in S. often seen on R help forums if you Google future issues



### Data basics
#### R processes at each new line unless you use a semicolon (;)






#### R generally ignores spaces and tabs, use this to make code readable



#A more complex example
for (i in unique(raw_area_long$date)){sub.dat<-subset(raw_area_long, raw_area_long$date==i)
umoles<-(((sub.dat$area[sub.dat$date==i]-calib_coeff$intercept[calib_coeff$date==i])/calib_coeff$slope[calib_coeff$date==i])/1000)*sub.dat$headspace_mL[sub.dat$date==i]}

#Using spaces to organize the above code:

for (i in unique(raw_area_long$date)) {
  
  sub.dat <- subset (raw_area_long, raw_area_long$date == i) 
  umoles <- ( ( ( sub.dat$area[sub.dat$date == i] - calib_coeff$intercept [calib_coeff$date == i] ) / calib_coeff$slope [ calib_coeff$date == i] ) / 1000 ) * sub.dat$headspace_mL 

}


#Under some special circumstances, spaces are required, e.g. when using function paste() and its argument "sep = ' ' ".






#### R calculates the right side of the assignment first the result is then applied to the left


  

 # this just overwrote your old 'result' object. Remember not to SCREW yourself


i = 1
i = i + 1
i = i + 1
i = i + 1
i = i + 1
i = i + 1
i



#### Naming conventions for objects stored in R
#* Cannot start with a number
#* Cannot have spaces in between the name
#* Avoid naming your variables using names already used by R.

#Basically, you have the following options:
#All lower case: e.g. myfirstobject
#Period separated: e.g. my.first.object
#use underscores: e.g. my_first_object
#Lower case: e.g. myFirstObject
#Upper case: e.g. MyFirstObject

#The most important aspect of naming is being concise and consistent!







#Case-sensitive






b; B; a; A; a + a;



#Named variables are stored differently than a function. **Avoid using functions' names to name your variables...**





  #This is an object
  # as soon as you add the () and pass it a value, it operates as the function



### Data structures
#### Vectors

#Let's start with vectors, the most common of R's data structures. Vectors are the bulding blocks of more complex objects.

#### Atomic vectors
 
#Atomic vectors are 1-dimensional data structures that must be all the same type


# Logical



# Integer



# Double



# Character





#A couple of points on atomic vectors:

#* They are constructed using the `c()` function
#* To specify an integer use the L suffix
#* A vector must be the all the same type
#* Use `typeof()` to see what type you have or an "is" function to check type


typeof(int_vector)

is.character(char_vector)

is.numeric(logical_vector)



#### Lists

#A list is like an atomic vector but each item in the list can be any type, including other lists, atomic vectors, data frames, or matrices. Use `list()` to make a list.


my_list = 



#Lists are very powerful and although confusing at first it is worth spending time learning how to use them.  In particular when we come to the "apply" family of functions we will see how lists can make our lives much easier.

#### Factors

#Ah, the dreaded factors!  They are used to store categorical variables and although it is tempting to think of them as character vectors this is a dangerous mistake (you will get scratched, badly!).  

#Factors make perfect sense if you are a statistician designing a programming language (!) but to everyone else they exist solely to torment us with confusing errors.  

#A factor is really just an integer vector with an additional attribute, `levels()`, which defines the possible values.


crazy_factor = 

print(crazy_factor)

levels(crazy_factor)

as.integer(crazy_factor) #Notice the alphabetic rearrangment in the results! Important to keep in mind when looping (week 5)



#But why not just use character vectors, you ask?  

#Believe it or not factor do have some useful properties.  For example factors allow you to specify all possible values a variable may take even if those values are not in your dataset.


cool_animals =

cool_animals_factor = 

table(cool_animals_factor)



#But for the most part factors are important for various statistics involving categorical variables, as you'll see for things like linear models.  Love 'em or hate 'em, factors are integral to using R so better learn 'em.

#### Matrices


#A matrix is a 2-dimensional vector and like atomic vectors must be all of a single type.

mat = 


  



#Matrices can have row and column names


colnames(mat) = 

rownames(mat) = 

print(mat)


#### Arrays


#Arrays are matrices with more than two dimensions. For example, an array of (5, 4, 3) has three slices (we can think of them as layers), each having five rows and four columns. As it happens for matrices, arrays can store only a single type of data.


mat = 

mat.2 = 
  
mat.3 = 

my.array = 

str(my.array)



#Let's assign names to rows, columns and matrices in the array.


array.rows = 
  
array.columns = 

array.matrices = 

my.array.named = 



#To access data from an array, select the row, column and slice of interest.


#Let's get the datum in row two and column four from matrix 3


#Access all the observations (data points) crossing column 2 in matrix 3 by leaving the row space blank.






#### Data frames


#Data frames are very powerful data structures in R and will play a central role in most of the work you'll do.  These are probably the most familiar data structures to most people as they resemble spreadsheets quite closely, at least on the surface.

#You can think of data frames as a set of identically sized lists lined up together in columns with a few key features.  Each column must be of the same type but column types can be any of the basic data types (character, integer, etc).  

#This makes them very useful for structured heterogeneous data, like what many of you generate in the lab everyday.  However, it is very important to remember that they are not spreadsheets and to use them effectively you need to forget everything you've learned about Excel (which is probably a good idea anyway).


#Here let's use a data frame that comes built in to R








#Notice the `$` notation, similar to what we saw for lists.  We can use this to extract singe columns.






#Alternatively,





#And now for some basic indexing.


# get the first 3 rows of the last 2 columns


# get the 10th row of the 'Petal.Width' column


# get the entire 4th row



#A brief on exploratory data analysis





#### S4 objects
#Is one of R's object oriented systems -the other two are S3 and R5. S4 objects are becoming more popular due to their capacity to efficiently handle big amounts of metadata (we can think of matrices where we can store different types of data), where each "matrix" is a slot. 

#S4 objects are more strict and hard to work with than S3. For example, S3 objecs are easy to intercovert (a data frame into a matrix) by simply setting the class attribute; that's not the case for S4 objects.


setClass("Hockey Team", representation(team = "character", city = "character"))

hockey <- new("Hockey Team", team = "Calgary Flames", city = "Calgary")

hockey


#### Special data: NA and NaN values
#Missing values in R are handled as NA or (Not Available). Impossible values (like the results of dividing by zero) are represented by NaN (Not a Number). These two types of values, specially NAs, have special ways to be dealt with otherwise it may lead to errors in the functions.



brand <- 
wheat.type <- 
rating <-

NA.example <- 
is.na(NA.example)

mean()
mean() #Avoid using just "T" as an abbreviation for "TRUE"




## Summary of week 1

#1.1 R can be used as a graphing calculator but R is a programming language and software.

#1.2 Commenting your code as you build your scripts up is a fundamental part of best coding practices. Do it always so your future you will not hate the paste you! It's for your own mental health.

#1.3 The capability of assigning results into variables is one of the most powerfull virtues of R which allows to simplify the data management for complex operations.

#1.4 There are several data structers that allow us to store data into variables.

