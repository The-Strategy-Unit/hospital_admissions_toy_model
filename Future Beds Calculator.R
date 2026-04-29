## Future Beds Calculator ##

### READ IN DATA ##
library(readxl)
data<-read_excel("Collating the data.xlsx")
#data<-read_excel("Collating the data general and accute only.xlsx")
from_93<-data[7:38,]
###


### USER INPUTS ###

# What do you assume will be the % change in admissions year on year from 2026 onwards
admission_yearly_percentage_change = 1

# What do you assume will be the % change in LoS year on year from 2026 onwards
los_yearly_percentage_change = 0

# What is an acceptable occupancy level 
occpuancy_fixed_level = 0.87

####

### WORKINGS ###
years<-c(from_93$Year,2026,2027,2028,2029,2030,2031,2032,2033,2034,2035)
admissions<-from_93$`All admissions`
beddays<-from_93$`All beddays`
beds<-from_93$Beds
los<-from_93$avgLoS
occupancy<-from_93$`occupancy`# derived from beddays`

## for each future year, 2026 onwards, calculate the growth or reduction in admissions
## LoS and occupancy and then use these to calculate the number of beds.

## Method for beds calculation:
# We're looking at an MMinf queue (so an unconstrained queueing system)
# The number in system (NIS) for this queueing model can be described by a 
# Poisson distriution with mean equal to admissions * mean LoS (Whitt :: get orig ref)
#(remember to match units so years*years or days*days)
# from this we can look at the distribution of the number in the system
# if there were no bed constraints, i.e. the occupancy distribution and we set 
# the number of beds equal to the user set quantile of this distribution 
# i.e. x s.t. P(NIS <= x) = occupancy target



for(i in 1:(length(years)-length(from_93$Year))){
  ## adds one entry to each array during each loop
  admissions <- c(admissions,admissions[length(admissions)] +  admissions[length(admissions)] *admission_yearly_percentage_change/100)
  los <- c(los , los[length(los)] + los[length(los)]*los_yearly_percentage_change/100 )
  beddays <- c(beddays , admissions[length(admissions)]*los[length(los)])
  ## Using MMinf as model to work out # beds to reach set performance
  beds <- c(beds, qpois(occpuancy_fixed_level, admissions[length(admissions)]*los[length(los)]/365))
  occupancy<- c(occupancy, occpuancy_fixed_level)
}
###

### PLOTS ###
col_vec <- ifelse(as.numeric(years) > 2025, "red", "black")
par(mfrow=c(2,2))
plot(years,admissions/1000000,col = col_vec,ylab="Admissions (millions)",xlab="",pch=19,ylim=c(min(admissions)/10000000,25),main="Admissions")
plot(years,los,col = col_vec,ylab="Average LoS (days)",xlab="",pch=19,ylim=c(min(los),los[1]),main="LoS")
#plot(years,beddays/1000000,col = col_vec,ylab="Beddays (millions)",xlab="",pch=19,ylim=c(min(beddays)/1000000,max(beddays)/1000000),main="Beddays")
plot(years,beds/1000,col = col_vec,ylab="Beds (thousands)",xlab="",pch=19,ylim=c(min(beds)/1000-10,max(beds)/1000),main="Beds")
#points(years,c(qpois(1-probability_of_block, from_93$`E(n(t))`),beds[(length(beds)-4):length(beds)])/1000,col="blue",pch=19)
plot(years,occupancy,col=col_vec,ylab="Occupancy",xlab = "",pch=19,main="Occupancy")

####


## END ##