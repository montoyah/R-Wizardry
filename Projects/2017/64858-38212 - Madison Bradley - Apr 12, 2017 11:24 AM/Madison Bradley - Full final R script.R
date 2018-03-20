#start session by clearing the clearing the environment 
rm(list=ls(all=TRUE))
#check the current working directory
getwd()
#set the working directory to where the data sets are saved
setwd("C:/Users/Owner/Desktop/RWizardry/Final Project/coordinatedata")

#load the packages we'll be using so that they're ready
library(ggplot2)
library(dplyr)
library(linkR)
library(svgViewR)

##data sets are produced as an excel file from ProAnalyst
##data sets were cropped from initiation of jumping behavior set to time=0 to 5 frames post take-off
##data sets then saved as a .csv (comma deliminated) text file because it's a file type that R can handle  

#input the datasets using the "read.csv" function. header is true because the first row is column names, check.names is false because names I don't want R to change them
##copy address of files from file explorer 
##for the purposes of this exercise, let's input one jump each from four individuals, two from the control strain and two from the longshanks strain 

#Control Strain 

###Data for individual AC1
####jump 1, height = 15cm
ac1_1<-read.csv("file:///C:/Users/Owner/Desktop/RWizardry/Final Project/coordinatedata/AC_1_standardizeddata_seq1_15cm.csv", header=TRUE, check.names = FALSE)

###Data for individual CC1
####jump 1, height = 15cm
cc1_1<-read.csv("file:///C:/Users/Owner/Desktop/RWizardry/Final Project/coordinatedata/CC_1_standardizeddata_seq1_15cm.csv",header=TRUE, check.names = FALSE)

#Longshanks line 2 strain

###Data for individual P2
####jump 1, height = 15cm
p2_1<-read.csv("file:///C:/Users/Owner/Desktop/RWizardry/Final Project/coordinatedata/P2_1_standardizeddata_seq1_15cm.csv",header=TRUE, check.names = FALSE)

###Data for individual N2
####jump 1, height = 15cm

n2_1<-read.csv("file:///C:/Users/Owner/Desktop/RWizardry/Final Project/coordinatedata/P2_1_standardizeddata_seq1_15cm.csv",header=TRUE, check.names = FALSE)

#inquire about data sets structure 
#inqure about column names
#all sets are formatted the same, so can just do one data set for an example

str(ac1_1)
colnames(ac1_1)

#Start analysis of motion

##take-off time in seconds 
##basically, simple calculation to see how long it takes the mouse to leave the ground after initiating the behaviour
#film data taken at 250 frames per second so we can calculate the time interval and store it as an object.

time.int<-1/250
time.int

##can just calculate for each data set
(nrow(ac1_1)-5)*time.int
##but, the more typing the more mistakes so let's make a simple function to calculate this for us 
##use same equation as above, but make it generalized (i.e remove the specific names of the data sets and replace with a function parameter)
##could make this funciton more flexible and allow for a time interval parameter, but it's not necessary because all are taken at 250 fps
takeoff.time<- function(data){
  (nrow(data)-5)*time.int
}

#now let's calculate take off time for all of the individuals
##store times an object, store individual names, and store strain so that they can easily be bound into a data frame
times<-c(takeoff.time(ac1_1),takeoff.time(cc1_1),takeoff.time(n2_1),takeoff.time(p2_1))
indiv<-c("ac1","cc1","n2","p2")
strain<-as.factor(c("control","control","longshanks","longshanks"))

#use data.frame instead of cbind to make sure that the original types of data (i.e. numeric for the time and character otherwise) are maintained
takeoff.times<-data.frame(times,indiv,strain)
str(takeoff.times)

#compare using t test t.test(y~x) use tilda because x is a binary factor and not a number
#boxplot is hideous because there's only four data points, but the purpose of the technique is displayed
boxplot(takeoff.times$times~takeoff.times$strain)
t.test(takeoff.times$times~takeoff.times$strain)

#takeoff velocity is the instaneous velocity as the mouse leaves the ground. This is calculated as the difference in position vector between the final time interval and the immediately preceeding time interval
#in cm/s
#there are always 5 frames after takeoff 
#using the hip landmark because it is a part of the main body (could also use eye)
#the formula that this is using is: (((x2-x1)^2+(y2-y1)^2+(z2-z1)^2)^0.5)/delta time

((((ac1_1[(nrow(ac1_1)-6),"HIP_X"]-ac1_1[(nrow(ac1_1)-5),"HIP_X"])^2)+((ac1_1[(nrow(ac1_1)-6),"HIP_Y"]-ac1_1[(nrow(ac1_1)-5),"HIP_Y"])^2)+((ac1_1[(nrow(ac1_1)-6),"HIP_Z"]-ac1_1[(nrow(ac1_1)-5),"HIP_Z"])^2))^0.5)/time.int

#this is a ton of typing so let's make a function to do the job for the rest of the data sets
#they all have the same column names by default of the proanalyst output so we can make a function that only needs to take in the name of the data set. this function could be made more flexible to include parameters for the x2,x1, y2,y1,z2,z1

takeoff.velocity<- function(data){
  ((((data[(nrow(data)-6),"HIP_X"]-data[(nrow(data)-5),"HIP_X"])^2)+((data[(nrow(data)-6),"HIP_Y"]-data[(nrow(data)-5),"HIP_Y"])^2)+((data[(nrow(data)-6),"HIP_Z"]-data[(nrow(data)-5),"HIP_Z"])^2))^0.5)/time.int
}

takeoff.velocity(ac1_1)

##flexible version of the function that allows full flexibility of parameters and allows for 2D analysis as well

takeoff.velocity.flex<- function(data,x,y,z=NULL,dtime){
  ((((data[(nrow(data)-6),x]-data[(nrow(data)-5),x])^2)+((data[(nrow(data)-6),y]-data[(nrow(data)-5),y])^2)+((data[(nrow(data)-6),z]-data[(nrow(data)-5),z])^2))^0.5)/dtime
}

takeoff.velocity.flex(ac1_1,"HIP_X","HIP_Y","HIP_Z",time.int)

#let's make a data frame for takeoff velocity like we did for takeoff time
v.takeoff<-c(takeoff.velocity(ac1_1),takeoff.velocity(cc1_1),takeoff.velocity(n2_1),takeoff.velocity(p2_1))
indiv<-c("ac1","cc1","n2","p2")
strain<-as.factor(c("control","control","longshanks","longshanks"))

#use data.frame instead of cbind to make sure that the original types of data (i.e. numeric for the time and character otherwise) are maintained
v.takeoffs<-data.frame(v.takeoff,indiv,strain)
str(v.takeoffs)

#compare using t test t.test(y~x) use tilda because x is a binary factor and not a number
#boxplot is hideous because there's only four data points, but the purpose of the technique is displayed
boxplot(v.takeoffs$v.takeoff~v.takeoffs$strain)
t.test(v.takeoffs$v.takeoff~v.takeoffs$strain)

########angular velocity for loop and function 
##time interval of 0.004 is set as a default but can be changed if need be 

angular_velocity<- function(data,joint1=NULL,joint2=NULL, joint3=NULL,time.int=0.004){
  joints<-c(joint1,joint2,joint3)
  ang_vel<-matrix(NA,nrow=nrow(data)-1,ncol=3,dimnames=list( NULL,c("Hip_Ang_Vel","Knee_Ang_Vel","Ankle_Ang_Vel")))
  idnum<-0
  for(j in joints){
    if(!is.null(j)){
      joint<-data[,j]
      idnum<-idnum+1
      for(i in 1:(nrow(data)-1)){
        ang_vel[i,idnum]<-(joint[i+1]-joint[i])/time.int
      }
    }
  }
  print(ang_vel)
}

ang_vel_ac1_1<-angular_velocity(data=ac1_1,joint1="HIP_ANG",joint2 = "KNEE_ANG",joint3 = "ANKLE_ANG")
ang_vel_cc1_1<-angular_velocity(data=cc1_1,joint1="HIP_ANG",joint2 = "KNEE_ANG",joint3 = "ANKLE_ANG")
ang_vel_n2_1<-angular_velocity(data=n2_1,joint1="HIP_ANG",joint2 = "KNEE_ANG",joint3 = "ANKLE_ANG")
ang_vel_p2_1<-angular_velocity(data=p2_1,joint1="HIP_ANG",joint2 = "KNEE_ANG",joint3 = "ANKLE_ANG")

####make results of all individuals into a four data frames

#AC1
ang_vel_ac1_1<-as.data.frame(ang_vel_ac1_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_vel_ac1_1) 
time<- seq(from=0.004,to=0.004*nrow(ang_vel_ac1_1),by=0.004)
individual<- rep("ac1",length=nrow(ang_vel_ac1_1))
Strain<- rep("control",length=nrow(ang_vel_ac1_1)) #use capital "S" because the object strain already exists in environment from earlier
ang_vel_ac1_1<-cbind(time,ang_vel_ac1_1,individual,Strain)
str(ang_vel_ac1_1)

#CC1
ang_vel_cc1_1<-as.data.frame(ang_vel_cc1_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_vel_cc1_1)
time<- seq(from=0.004,to=0.004*nrow(ang_vel_cc1_1),by=0.004)
individual<- rep("cc1",length=nrow(ang_vel_cc1_1))
Strain<- rep("control",length=nrow(ang_vel_cc1_1))
ang_vel_cc1_1<-cbind(time,ang_vel_cc1_1,individual,Strain)
str(ang_vel_cc1_1)

#N2
ang_vel_n2_1<-as.data.frame(ang_vel_n2_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_vel_n2_1)
time<- seq(from=0.004,to=0.004*nrow(ang_vel_n2_1),by=0.004)
individual<- rep("n2",length=nrow(ang_vel_n2_1))
Strain<- rep("longshanks",length=nrow(ang_vel_n2_1))
ang_vel_n2_1<-cbind(time,ang_vel_n2_1,individual,Strain)
str(ang_vel_n2_1)

#P2
ang_vel_p2_1<-as.data.frame(ang_vel_p2_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_vel_p2_1)
time<- seq(from=0.004,to=0.004*nrow(ang_vel_p2_1),by=0.004)
individual<- rep("p2",length=nrow(ang_vel_p2_1))
Strain<- rep("longshanks",length=nrow(ang_vel_p2_1))
ang_vel_p2_1<-cbind(time,ang_vel_p2_1,individual,Strain)
str(ang_vel_p2_1)

##join into one giant data set using dplyr
angular.velocity<- full_join(ang_vel_ac1_1,full_join(ang_vel_cc1_1,full_join(ang_vel_n2_1,ang_vel_p2_1))) #warning message comes up so check structure
str(angular.velocity) #everything looks ok 

######angular acceleration for loop and function 
##time interval of 0.004 is set as a default but can be changed if need be 

angular_acceleration<- function(data,joint1=NULL,joint2=NULL, joint3=NULL,time.int=0.004){
  joints<-c(joint1,joint2,joint3)
  ang_acc<-matrix(NA,nrow=nrow(data)-1,ncol=3,dimnames=list(NULL,c("Hip_Ang_Acc","Knee_Ang_Acc","Ankle_Ang_Acc")))
  idnum<-0
  for(j in joints){
    if(!is.null(j)){
      joint<-data[,j]
      idnum<-idnum+1
      for(i in 1:(nrow(data)-1)){
        ang_acc[i,idnum]<-(joint[i+1]-joint[i])/time.int
      }
    }
  }
  print(ang_acc)
}

ang_acc_ac1_1<-angular_acceleration(data=ang_vel_ac1_1,joint1="Hip_Ang_Vel",joint2 = "Knee_Ang_Vel",joint3 = "Ankle_Ang_Vel")
ang_acc_cc1_1<-angular_acceleration(data=ang_vel_cc1_1,joint1="Hip_Ang_Vel",joint2 = "Knee_Ang_Vel",joint3 = "Ankle_Ang_Vel")
ang_acc_n2_1<-angular_acceleration(data=ang_vel_n2_1,joint1="Hip_Ang_Vel",joint2 = "Knee_Ang_Vel",joint3 = "Ankle_Ang_Vel")
ang_acc_p2_1<-angular_acceleration(data=ang_vel_p2_1,joint1="Hip_Ang_Vel",joint2 = "Knee_Ang_Vel",joint3 = "Ankle_Ang_Vel")

####make results of all individuals into a four data frames

#AC1
ang_acc_ac1_1<-as.data.frame(ang_acc_ac1_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_acc_ac1_1) 
Time<- seq(from=0.004,to=0.004*nrow(ang_acc_ac1_1),by=0.004)
individual<- rep("ac1",length=nrow(ang_acc_ac1_1))
Strain<- rep("control",length=nrow(ang_acc_ac1_1)) #use capital "S" because the object strain already exists in environment from earlier
ang_acc_ac1_1<-cbind(Time,ang_acc_ac1_1,individual,Strain)
str(ang_acc_ac1_1)

#CC1
ang_acc_cc1_1<-as.data.frame(ang_acc_cc1_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_acc_cc1_1)
Time<- seq(from=0.004,to=0.004*nrow(ang_acc_cc1_1),by=0.004)
individual<- rep("cc1",length=nrow(ang_acc_cc1_1))
Strain<- rep("control",length=nrow(ang_acc_cc1_1))
ang_acc_cc1_1<-cbind(Time,ang_acc_cc1_1,individual,Strain)
str(ang_acc_cc1_1)

#N2
ang_acc_n2_1<-as.data.frame(ang_acc_n2_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_acc_n2_1)
Time<- seq(from=0.004,to=0.004*nrow(ang_acc_n2_1),by=0.004)
individual<- rep("n2",length=nrow(ang_acc_n2_1))
Strain<- rep("longshanks",length=nrow(ang_acc_n2_1))
ang_acc_n2_1<-cbind(Time,ang_acc_n2_1,individual,Strain)
str(ang_acc_n2_1)

#P2
ang_acc_p2_1<-as.data.frame(ang_acc_p2_1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(ang_acc_p2_1)
Time<- seq(from=0.004,to=0.004*nrow(ang_acc_p2_1),by=0.004)
individual<- rep("p2",length=nrow(ang_acc_p2_1))
Strain<- rep("longshanks",length=nrow(ang_acc_p2_1))
ang_acc_p2_1<-cbind(Time,ang_acc_p2_1,individual,Strain)
str(ang_acc_p2_1)

##join into one giant data set using dplyr
angular.acceleration<- ang_acc_ac1_1%>%full_join(ang_acc_cc1_1)%>%full_join(ang_acc_n2_1)%>%full_join(ang_acc_p2_1) #warning message comes up so check structure
str(angular.acceleration) #everything looks ok 



##angular acceleration
#hip
angular.acceleration%>%
  ggplot(data=.,aes(time,y=Hip_Ang_Acc))+
  facet_grid(Strain~.)+
  labs(x="Time (s)", y="Angular acceleration of hip (deg/s^2)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth()

#knee
angular.acceleration%>%
  ggplot(data=.,aes(time,y=Knee_Ang_Acc))+
  facet_grid(Strain~.)+
  labs(x="Time (s)", y="Angular acceleration of knee (deg/s^2)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth()

#ankle
angular.acceleration%>%
  ggplot(data=.,aes(time,y=Ankle_Ang_Acc))+
  facet_grid(Strain~.)+
  labs(x="Time (s)", y="Angular acceleration of ankle (deg/s^2)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth()

####using approx() function to interpolate from 0 to 100 %


approx(seq_along(ac1_1[,"HIP_ANG"]),ac1_1[,"HIP_ANG"],n=100)

##approx() function gives an $x and $y output, where $x is the position along the sequence and $y is the output we want 

interpolate<- function(data,joint1=NULL,joint2=NULL, joint3=NULL){
  joints<-c(joint1,joint2,joint3)
  inter<-matrix(NA,nrow=100,ncol=3,dimnames=list(NULL,c("Hip_Ang","Knee_Ang","Ankle_Ang")))
  idnum<-0
  for(j in joints){
    if(!is.null(j)){
      joint<-data[,j]
      idnum<-idnum+1
      inter[,idnum]<-approx(seq_along(joint),joint,n=100)$y
    }
  }
  print(inter)
}

int.ac1_1<-interpolate(ac1_1,"HIP_ANG","KNEE_ANG","ANKLE_ANG")

ang_vel_ac1_1<-angular_velocity(data=ac1_1,joint1="HIP_ANG",joint2 = "KNEE_ANG",joint3 = "ANKLE_ANG")

# time interval needs to change because of the interpolation

int.ang_vel_ac1_1<-angular_velocity(data=int.ac1_1,joint1="Hip_Ang",joint2 = "Knee_Ang",joint3 = "Ankle_Ang",time.int = ((nrow(ac1_1)-1)*0.004/100))

##to save ourselves some trouble, let's combine the angular_velocity() and interpolate() functions into a standardized.angular.velocity() function 

standardized.angular.velocity<- function(data,joint.a=NULL,joint.b=NULL, joint.c=NULL,time.int=((nrow(data)-1)*0.004/100)){
  angular_velocity(data=(interpolate(data,joint.a,joint.b,joint.c)),joint1="Hip_Ang",joint2 = "Knee_Ang",joint3 = "Ankle_Ang",time.int = ((nrow(data)-1)*0.004/100))
}

stnd.ang.vel.ac1<-standardized.angular.velocity(ac1_1,"HIP_ANG","KNEE_ANG","ANKLE_ANG")
stnd.ang.vel.cc1<-standardized.angular.velocity(cc1_1,"HIP_ANG","KNEE_ANG","ANKLE_ANG")
stnd.ang.vel.n2<-standardized.angular.velocity(n2_1,"HIP_ANG","KNEE_ANG","ANKLE_ANG")
stnd.ang.vel.p2<-standardized.angular.velocity(p2_1,"HIP_ANG","KNEE_ANG","ANKLE_ANG")

####make results of all individuals into a four data frames

#AC1
stnd.ang.vel.ac1<-as.data.frame(stnd.ang.vel.ac1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(stnd.ang.vel.ac1) 
percent<- seq(from=1,to=99)
individual<- rep("ac1",length=nrow(stnd.ang.vel.ac1))
Strain<- rep("control",length=nrow(stnd.ang.vel.ac1)) #use capital "S" because the object strain already exists in environment from earlier
stnd.ang.vel.ac1<-cbind(percent,stnd.ang.vel.ac1,individual,Strain)
str(stnd.ang.vel.ac1)

#CC1
stnd.ang.vel.cc1<-as.data.frame(stnd.ang.vel.cc1, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(stnd.ang.vel.cc1) 
percent<- seq(from=1,to=99)
individual<- rep("cc1",length=nrow(stnd.ang.vel.cc1))
Strain<- rep("control",length=nrow(stnd.ang.vel.cc1)) #use capital "S" because the object strain already exists in environment from earlier
stnd.ang.vel.cc1<-cbind(percent,stnd.ang.vel.cc1,individual,Strain)
str(stnd.ang.vel.cc1)

#N2
stnd.ang.vel.n2<-as.data.frame(stnd.ang.vel.n2, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(stnd.ang.vel.n2) 
percent<- seq(from=1,to=99)
individual<- rep("n2",length=nrow(stnd.ang.vel.n2))
Strain<- rep("longshanks",length=nrow(stnd.ang.vel.n2)) #use capital "S" because the object strain already exists in environment from earlier
stnd.ang.vel.n2<-cbind(percent,stnd.ang.vel.n2,individual,Strain)
str(stnd.ang.vel.n2)

#P2
stnd.ang.vel.p2<-as.data.frame(stnd.ang.vel.p2, stringsAsFactors=FALSE) #as.data.frame so that different types of data can be in it
str(stnd.ang.vel.p2) 
percent<- seq(from=1,to=99)
individual<- rep("p2",length=nrow(stnd.ang.vel.p2))
Strain<- rep("longshanks",length=nrow(stnd.ang.vel.p2)) #use capital "S" because the object strain already exists in environment from earlier
stnd.ang.vel.p2<-cbind(percent,stnd.ang.vel.p2,individual,Strain)
str(stnd.ang.vel.p2)

#join all together

stnd.angular.velocity<- full_join(stnd.ang.vel.ac1,full_join(stnd.ang.vel.cc1,full_join(stnd.ang.vel.n2,stnd.ang.vel.p2)))#warning message comes up so check structure
str(stnd.angular.velocity) 




###statistically comparing maximums of the peaks of the angular velocity and angular acceleration curves 
max.ang.vel.hip<-c(max(ang_vel_ac1_1$Hip_Ang_Vel),max(ang_vel_cc1_1$Hip_Ang_Vel),max(ang_vel_n2_1$Hip_Ang_Vel),max(ang_vel_p2_1$Hip_Ang_Vel))
max.ang.vel.knee<-c(max(ang_vel_ac1_1$Knee_Ang_Vel),max(ang_vel_cc1_1$Knee_Ang_Vel),max(ang_vel_n2_1$Knee_Ang_Vel),max(ang_vel_p2_1$Knee_Ang_Vel))
max.ang.vel.ankle<-c(max(ang_vel_ac1_1$Ankle_Ang_Vel),max(ang_vel_cc1_1$Ankle_Ang_Vel),max(ang_vel_n2_1$Ankle_Ang_Vel),max(ang_vel_p2_1$Ankle_Ang_Vel))
Strains<- c("control","control","longshanks","longshanks")

max.vel.data<- data.frame(max.ang.vel.hip,max.ang.vel.knee,max.ang.vel.ankle,Strains)

#comparing hip
boxplot(max.vel.data$max.ang.vel.hip~max.vel.data$Strains)
t.test(max.vel.data$max.ang.vel.hip~max.vel.data$Strains)

#comparing knee
boxplot(max.vel.data$max.ang.vel.knee~max.vel.data$Strains)
t.test(max.vel.data$max.ang.vel.knee~max.vel.data$Strains)

#comparing ankle 
boxplot(max.vel.data$max.ang.vel.ankle~max.vel.data$Strains)
t.test(max.vel.data$max.ang.vel.ankle~max.vel.data$Strains)

####LINKR CODE

#Creates 3D animated, interactive visualizations in Scalable Vector Graphics (SVG) format that can be viewed in a web browser.

#step 1: Joint coords
##need initial positions of joints
print(ac1_1[1,])

#middle position of joints
print(ac1_1[nrow(ac1_1)/2,])

#3/4 position of joints
print(ac1_1[0.75*nrow(ac1_1),])

#final position of joints 
print(ac1_1[nrow(ac1_1),])

#make an object with initial positions
#joint.coords<- (metatarsal(x,y,z), ankle(x,y,z), knee(x,y,z), hip(x,y,z)), eye(x,y,z))
#joint.coor<-rbind(c(7.29,3.15,-0.23),c(6.87,4.43,-0.13),c(7.46,4.22,0.67),c(6.39,4.36,0.86),c(7.95,2.68,6.70)) #round to second decimalfor demo purposes
#translate so that y and z are switched to make it more intuitive to visualize (this way the x axis is a transverse section of the body, y axis is a sagittal section of the body, and z axis is a coronal section)
joint.coor<-rbind(c(7.29,-0.23,3.15),c(6.87,-0.13,4.43),c(7.46,0.67,4.22),c(6.39,0.86,4.36),c(7.95,6.70,2.68),c(7.01,-0.26,2.67),c(6.71,0.11,4.09),c(8.16,0.66,4.19),c(7.12,1.25,3.70),c(8.43,8.78,2.16),c(7.34,0.08,3.62),c(7.47,1.04,3.84),c(8.97,2.64,3.96),c(8.33,3.38,3.11),c(9.24,11.5,1.87),c(9.35,4.48,0.92),c(9.30,5.13,1.99),c(10.26,6.47,1.05),c(9.82,6.95,2.35),c(10.58,14.81,0.91))

## or make it using subsets 
joint.coord<- rbind(c(ac1_1[1,"MT_X"],ac1_1[1,"MT_Z"],ac1_1[1,"MT_Y"]),
                    c(ac1_1[1,"ANKLE_X"],ac1_1[1,"ANKLE_Z"],ac1_1[1,"ANKLE_Y"]),
                    c(ac1_1[1,"KNEE_X"],ac1_1[1,"KNEE_Z"],ac1_1[1,"KNEE_Y"]),
                    c(ac1_1[1,"HIP_X"],ac1_1[1,"HIP_Z"],ac1_1[1,"HIP_Y"]),
                    c(ac1_1[1,"EYE_X"],ac1_1[1,"EYE_Z"],ac1_1[1,"EYE_Y"]),
                    c(ac1_1[0.5*nrow(ac1_1),"MT_X"],ac1_1[0.5*nrow(ac1_1),"MT_Z"],ac1_1[0.5*nrow(ac1_1),"MT_Y"]),
                    c(ac1_1[0.5*nrow(ac1_1),"ANKLE_X"],ac1_1[0.5*nrow(ac1_1),"ANKLE_Z"],ac1_1[0.5*nrow(ac1_1),"ANKLE_Y"]),
                    c(ac1_1[0.5*nrow(ac1_1),"KNEE_X"],ac1_1[0.5*nrow(ac1_1),"KNEE_Z"],ac1_1[0.5*nrow(ac1_1),"KNEE_Y"]),
                    c(ac1_1[0.5*nrow(ac1_1),"HIP_X"],ac1_1[0.5*nrow(ac1_1),"HIP_Z"],ac1_1[0.5*nrow(ac1_1),"HIP_Y"]),
                    c(ac1_1[0.5*nrow(ac1_1),"EYE_X"],ac1_1[0.5*nrow(ac1_1),"EYE_Z"],ac1_1[0.5*nrow(ac1_1),"EYE_Y"]),
                    c(ac1_1[0.75*nrow(ac1_1),"MT_X"],ac1_1[0.75*nrow(ac1_1),"MT_Z"],ac1_1[0.75*nrow(ac1_1),"MT_Y"]),
                    c(ac1_1[0.75*nrow(ac1_1),"ANKLE_X"],ac1_1[0.75*nrow(ac1_1),"ANKLE_Z"],ac1_1[0.75*nrow(ac1_1),"ANKLE_Y"]),
                    c(ac1_1[0.75*nrow(ac1_1),"KNEE_X"],ac1_1[0.75*nrow(ac1_1),"KNEE_Z"],ac1_1[0.75*nrow(ac1_1),"KNEE_Y"]),
                    c(ac1_1[0.75*nrow(ac1_1),"HIP_X"],ac1_1[0.75*nrow(ac1_1),"HIP_Z"],ac1_1[0.75*nrow(ac1_1),"HIP_Y"]),
                    c(ac1_1[0.75*nrow(ac1_1),"EYE_X"],ac1_1[0.75*nrow(ac1_1),"EYE_Z"],ac1_1[0.75*nrow(ac1_1),"EYE_Y"]),
                    c(ac1_1[nrow(ac1_1),"MT_X"],ac1_1[nrow(ac1_1),"MT_Z"],ac1_1[nrow(ac1_1),"MT_Y"]),
                    c(ac1_1[nrow(ac1_1),"ANKLE_X"],ac1_1[nrow(ac1_1),"ANKLE_Z"],ac1_1[nrow(ac1_1),"ANKLE_Y"]),
                    c(ac1_1[nrow(ac1_1),"KNEE_X"],ac1_1[nrow(ac1_1),"KNEE_Z"],ac1_1[nrow(ac1_1),"KNEE_Y"]),
                    c(ac1_1[nrow(ac1_1),"HIP_X"],ac1_1[nrow(ac1_1),"HIP_Z"],ac1_1[nrow(ac1_1),"HIP_Y"]),
                    c(ac1_1[nrow(ac1_1),"EYE_X"],ac1_1[nrow(ac1_1),"EYE_Z"],ac1_1[nrow(ac1_1),"EYE_Y"]))
#step 2:joint types
joint.types <- c("S", "S", "S", "R", "S","S", "S", "S", "R", "S","S", "S", "S", "R", "S","S", "S", "S", "R", "S") #the only important contraint is about the hip joint 

#step 3: joint contraints 
# Define joint constraints
joint.cons <- list(NA, NA, NA, c(1,0,-1), NA,NA, NA, NA, c(1,0,-1), NA,NA, NA, NA, c(1,0,-1), NA,NA, NA, NA, c(1,0,-1), NA) # constraint is a vector perpendiciular to the plane of movement 

#step 4: Joint connections

##link 0= "ground" 
##link 1,5,9,13= foot
##link 2,6,10,14= tibia and fibula
##link 3,7,11,15= femur
##link 4,8,12,16= "body"


joint.conn<- rbind(c(0,1), c(1,2), c(2,3), c(3,4),c(4,0),c(0,5), c(5,6), c(6,7), c(7,8),c(8,0),c(0,9), c(9,10), c(10,11), c(11,12),c(12,0),c(0,13), c(13,14), c(14,15), c(15,16),c(16,0))


#step 5: define linkages using the "defineLinkage" function

#make an object with linkage definitions 
#basically plug in the names of objects that you made in previous 4 steps
?defineLinkage

linkage<- defineLinkage(joint.coor = joint.coor, joint.types = joint.types, joint.cons = joint.cons, joint.conn = joint.conn)

#step 6: visualize linkage using the "drawLinkage" function

# Draw linkage
?drawLinkage
drawLinkage(linkage, file='jumping sequence') #default method is an html file saved to the working directory 

#if you change the method to plot the function will print a 2D plot 
drawLinkage(linkage, method=plot)

#file='YOUR FILE NAME HERE' will save an .html file in your set working directory

#######GRAPHS

#need angular.velocity to be a data frame because ggplot2 doesn't know how to handle data as class matrix
##facet_grid(Strain~.) wraps into rows based on Strain, where facet_grid(~Strain) wraps into columns 
###can visually assess the timing of the peaks of the curves (keeping in mind the overall takeoff time is longer in the control mice, and for future studies will be interpolated from 0-100% instead of by time in seconds)

##angular velocity
#hip
angular.velocity%>%
  ggplot(data=.,aes(time,y=Hip_Ang_Vel))+
  facet_grid(Strain~.)+
  labs(x="Time (s)", y="Angular velocity of hip (deg/s)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth(color="black")+
  theme_minimal()

#knee
angular.velocity%>%
  ggplot(data=.,aes(time,y=Knee_Ang_Vel))+
  facet_grid(Strain~.)+
  labs(x="Time (s)", y="Angular velocity of knee (deg/s)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth(color="black")+
  theme_minimal()

#ankle
angular.velocity%>%
  ggplot(data=.,aes(time,y=Ankle_Ang_Vel))+
  facet_grid(Strain~.)+
  labs(x="Time (s)", y="Angular velocity of ankle (deg/s)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth(color="black")+
  theme_minimal()

## standardized angular velocity graphs 
#hip

stnd.angular.velocity%>%
  ggplot(data=.,aes(percent,y=Hip_Ang_Vel))+
  facet_grid(Strain~.)+
  labs(x="Percent of jump sequence", y="Angular velocity of hip (deg/s)")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth(color="black")+
  theme_minimal()

#knee
stnd.angular.velocity%>%
  ggplot(data=.,aes(percent,y=Knee_Ang_Vel,color=Strain))+
  labs(x="Percent of jump sequence", y="Angular velocity of Knee (deg/s)")+
  facet_grid(Strain~.)+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth(colour="black")+
  theme_minimal()

#Ankle
stnd.angular.velocity%>%
  ggplot(data=.,aes(percent,y=Ankle_Ang_Vel))+
  labs(x="Percent of jump sequence", y="Angular velocity of Ankle (deg/s)")+
  facet_grid(Strain~.)+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  geom_smooth(colour="black")+
  theme_minimal()