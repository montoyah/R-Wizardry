####Loading the Packages####
install.package('lme4')
install.package('sjPlot')
library(lme4) #The main modelling package
library(arm)  #Provides some additional functions
library(sjPlot) #Visualizing models and their attributes

#####Reading in the Data####
rm(list=ls())
getwd()#Make sure the data is in this directory
data<-read.csv('mpb.csv') 

####Making Preliminary Plots####
?hist
?plot#Taking a general look at the data before we do any analyses

par(mfrow=(c(2,2))) #creates a 2 by 2 area to plot graphs


hist(data$drymass.mg.,xlab='Dry Mass (mg)',main=NA)
hist(data$width.mm., xlab= 'Width (mm)',main=NA)
hist(data$Volume, xlab= 'Volume (mm^3)',main=NA)
plot(data$sex, xlab= 'Sex',ylab='Frequency',main=NA)#Use plot since sex is categorical

####Yet more plotting####
plot(data$sex,data$drymass.mg., xlab='sex',
     ylab='Drymass (mg)',pch=16,col='darkblue')

plot(data$Volume,data$drymass.mg.,xlab='volume (mg^3)',
     ylab='Drymass (mg)',pch=16,col='darkgreen')

plot(data$width.mm.,data$drymass.mg.,xlab='width (mm)',
     ylab='Drymass (mg)',pch=16, col='red')

####Basic Linear Regressions####
?lm
?summary
?display


lm1<-lm(formula = drymass.mg.~sex,data=data)
summary(lm1)
display(lm1)
plot(lm1)#Provides 4 diagnostic plots

lm2<-lm(formula = drymass.mg.~Volume,data=data)
display(lm2)
plot(lm2)

lm3<-lm(formula = drymass.mg.~width.mm.,data=data)
display(lm3)
plot(lm3)


####Generalized Linear Models####
?glm
?AIC


GLM1<-glm(formula = drymass.mg.~Volume+sex+width.mm.,data=data)
summary(GLM1)
display(GLM1)
AIC(GLM1)
plot(GLM1) 

#Looking at non-normal data with GLM
hist(data$procto)
GLM2<-glm(formula = procto~drymass.mg.+Volume+sex+width.mm.,
          family='poisson',data=data) 
summary(GLM2)
display(GLM2)
AIC(GLM2)
plot(GLM2) 

####Linear Mixed Models####
?lmer
?anova

#rm(list=ls()) clean up the global environment if you like

#Single varying intercept effect
M1<-lmer(formula = drymass.mg.~Volume+sex+width.mm.+(1|year),data=data)
display(M1)
ranef(M1)
coef(M1)

#Varying slope effect
M2<-lmer(formula = drymass.mg.~Volume+sex+width.mm.+
         (0+width.mm.|year),data=data)
display(M2)
ranef(M2)
coef(M2)

#Varying slope and intercept model
M3<-lmer(formula = drymass.mg.~Volume+sex+width.mm.+
           (1+width.mm.|year),data=data)
display(M3)
ranef(M3)
coef(M3)

#Nested Random Effects in a Varying Intercept Model
M4<-lmer(formula=drymass.mg.~Volume+sex+width.mm.+
           (1|year/park/site),data=data)
display(M4)
ranef(M4)
coef(M4)
anova(M1,M4,test='F')

####Generalized Linear Mixed Models####
?glmer


#Diagnostic plots
par(mfrow=(c(2,2)))
hist(data$length.mm.)
hist(data$totalmites,xlab='total mites') #Looking at how our data is distributed
hist(data$tarsonemus, xlab='tarsonemus') #These 3 histograms are invidual genera of mites
hist(data$trichou, xlab='trichou')    
hist(data$procto, xlab='procto')

?glmer
M5<-glmer(formula = totalmites~length.mm.:width.mm.+drymass.mg.+
            (1|year/park/site),data=data,family='poisson') #colon=interaction

display(M5)
ranef(M5)
coef(M5)

#Treating a categorical response variable as numeric
M_s<-lmer(as.numeric(sex)~drymass.mg.+width.mm.+
            (1|year/park/site),data=data)
display(M_s)
ranef(M_s)
coef(M_s)


M6<-glmer(procto~drymass.mg.+
            (1|year/park/site),data=data,family='poisson')
display(M6)
ranef(M6)
coef(M6)

M7<-glmer(tarsonemus~drymass.mg.+
            (1|year/park/site),data=data,family='poisson')
display(M7)
ranef(M6)
coef(M6)

M8<-glmer(trichou~drymass.mg.+
            (1|year/park/site),data=data,family='poisson')
display(M8)
ranef(M8)
coef(M8)

M9<-lmer(condition~drymass.mg.+width.mm.+procto+
            (0+drymass.mg.|year/park/site),data=data)

display(M9)
ranef(M9)
coef(M9)

####Plotting our data using sjPlot####
?sjp.lm
?sjp.lmer
?sjp.glm
?sjp.glmer

sjp.lm(lm2)#Plots the linear model with CIs
sjp.lmer(M4,type='fe.resid') #Plots fixed effects and residuals
sjp.lmer(M4,type='fe.pred')  #Plots regressions for each fixed effect
sjp.glm(GLM2) #Plots incident rate ratios
sjp.glmer(M5,'re') #Plots random effects 
sjp.glmer(M5,'fe') #Plots fixed effects

sessionInfo() #Keep track of what packages you used