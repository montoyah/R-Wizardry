rm(list=ls(all=TRUE))
# Updating and calling packages 
update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
library(dplyr)
library(ggplot2)
library(lme4)

setwd("/Users/Haydeeakin/Desktop/Flight mill raw data for R")
flight <- read.csv("flightmill complete.csv", header=TRUE)
str(flight)

# =====Understanding the data set
head(flight)
summary(flight)
names(flight)


# ====First glance at the data
# Density plot for distance flown
dpt<-ggplot(flight, aes(distance_km))
dpt+geom_density()
dp<-ggplot(flight, aes(distance_km))
dp+geom_density() + xlim(0, 1) + ylim(0,10)

# Blox plot for average speed
bp <- ggplot(flight, aes(beetle_id, speed_km_hr))
bp + geom_boxplot() + facet_grid(sex~.)


# =====Selecting data to start analysis
temp<-select(flight,beetle_id,log_num,julian_emergence_date,sex,pre_flight_weight_mg,length_mm,preflight_total_mites,distance_km,speed_km_hr,volume_mm3) %>%
  mutate(log_num=factor(log_num)) %>%
  group_by(beetle_id) %>%
  summarize(avgspeed=mean(speed_km_hr),lognum=unique(log_num),totaldist=sum(distance_km),avgdist=mean(distance_km),len=unique(length_mm),sex=unique(sex),weight=unique(pre_flight_weight_mg),mites=unique(preflight_total_mites),volume=unique(volume_mm3))
temp

# =====Who flies faster?
#Boys and girls fly the same speed
ggplot(temp,aes(sex,avgspeed))+geom_jitter()+labs(x='Sex',y='Average flight speed')


# =====Who flies more?
#Distances boys and girls fly
#Boys and girls fly about the same
ggplot(temp,aes(totaldist))+geom_histogram()+facet_wrap(~sex,ncol=1)


# =====Do mites matter?
ggplot(temp,aes(mites,totaldist,colour=sex))+geom_point()+geom_smooth(method='lm',se=F)
a<-lm(log(totaldist)~log(mites+1),data=temp)
summary(a) #Without a random effect, looks like more mites = long distance
residuals(a)
summary(residuals(a))
# mites matter controling for log (non-independence due to log), (1|lognum) random intercept effect
a2<-lmer(log(totaldist)~log(mites+1)+(1|lognum),data=temp,REML=F)
summary(a2)
drop1(a2,test="Chisq")

# mites don't matter on how fast beetles fly
ggplot(temp,aes(mites,avgspeed,colour=sex))+geom_point()+geom_smooth(method='lm',se=F)
a3<-lmer(avgspeed~log(mites+1)+(1|lognum),data=temp,REML=F)
drop1(a3,test="Chisq")


# =====Females are heavier and fly longer distances than males
names(temp)
ggplot(temp,aes(weight,totaldist,colour=sex))+geom_point()+geom_smooth(method='lm',se=F)
a4<-lm(totaldist~weight*sex,data=temp)
summary(a4)


# ====beetle condition index
ggplot(temp,aes(sex,weight))+geom_jitter()+labs(x='Sex',y='Weight')
ggplot(temp,aes(sex,volume))+geom_jitter()+labs(x='Sex',y='Volume')
ggplot(temp,aes(volume,weight,color=sex))+geom_point()+geom_smooth(method='lm',se=F)
a5<-lm(volume~weight*sex,data=temp)
summary(a5)


# ====Beetle condition doesnt explain total distance flown
plot(residuals(a5),temp$totaldist,xlab="Beetle body condition",ylab="Total distance flown")


#=====No sexual differences in beetle body condition
plotbysex<-data.frame(cbind(condi=residuals(a5),sex=temp$sex))
plotbysex<-mutate(plotbysex,sex=factor(sex))
ggplot(plotbysex,aes(sex,condi,fill=sex))+geom_boxplot()+guides(fill=FALSE)+coord_flip() +stat_summary(fun.y=mean, geom="point", shape=5, size=4)+stat_boxplot(geom ='errorbar')


# ====Beetle condition doesnt explain average speed
plot(residuals(a5),temp$avgspeed,xlab="Beetle body condition",ylab="Average speed")


# ====Bar grapgh for total distance flown per beetle
dista.freq<- data.frame(Beetle=temp$beetle_id,Distance=temp$totaldist)
ggplot(dista.freq, aes(Beetle,Distance))+geom_bar(stat = "identity")

