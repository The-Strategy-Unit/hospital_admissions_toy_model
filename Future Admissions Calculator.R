## Future Admissions Calculator
#### Solving for Admissions LoS or Occupancy

## note that the optim function is a root finder so what we're doing is giving it a root to find by
## inputting lots of options for one parameter and stopping when it finds the one that gives us the 
## user selected number of beds (as beds - beds = 0 i.e. the minimum/root).


### Function to solve for ADMISSIONS ####

solve_for_admissions<-function(admissions,LoS,occupancy,beds){
  # admissions: admissions rate in a future year
  # beds: number of beds allowed in a future year
  # LoS: assumed average LoS in a future year
  # occupancy: assumed occupancy in a future year
  return(abs(beds - qpois(occupancy, admissions*LoS/365)))
}
#opt <- optim(par = c(18742360), fn = solve_for_admissions  , LoS = LoS_input, occupancy = occupancy_input, beds = beds_input)
# return opt$par i.e. number of admissions


## Future Admissions Calculator ##

### READ IN DATA ##
library(readxl)
data<-read_excel("Collating the data.xlsx")
#data<-read_excel("Collating the data general and accute only.xlsx")
from_93<-data[7:38,]
###


### USER INPUTS ###

# What do you assume will be the % change in admissions year on year from 2026 onwards
beds_yearly_percentage_change = 1

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

## for each future year, 2026 onwards, calculate the growth or reduction in beds
## LoS and occupancy and then use these to calculate the number of admissions.

## Method for admissions calculation:
# We're just doing a 1D line search to find admissions that returns the number
# of beds we want

for(i in 1:(length(years)-length(from_93$Year))){
  ## adds one entry to each array during each loop
  los <- c(los , los[length(los)] + los[length(los)]*los_yearly_percentage_change/100 )
  beddays <- c(beddays , admissions[length(admissions)]*los[length(los)])
  ## Using MMinf as model to work out # beds to reach set performance
  beds <- c(beds, beds[length(beds)] + beds[length(beds)]*beds_yearly_percentage_change/100)
  occupancy<- c(occupancy, occpuancy_fixed_level)
  # now solve for admissions
  admissions <- c(admissions,optim(par = c(18742360),fn = solve_for_admissions  , LoS = los[length(los)], occupancy = occupancy[length(occupancy)], beds = beds[length(beds)], method="Brent",,lower=15000000,upper=26000000)$par)
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


## END ######









### No longer the focus but below if needed ###
### Calculate ### LoS ####

#solve_for_LoS<-function(LoS,admissions,occupancy,beds){
  # admissions: admissions rate in a future year
  # beds: number of beds allowed in a future year
  # LoS: assumed average LoS in a future year
  # occupancy: assumed occupancy in a future year
#  return(abs(beds - qpois(occupancy, admissions*LoS/365)))
#}
#opt <- optim(par = c(1), fn = solve_for_LoS  , admissions = admissions_input, occupancy = occupancy_input, beds = beds_input)
#return opt$par

#check
#qpois(occupancy_input, admissions_input*opt$par/365)

### Calculate ### Occupancy ####

#solve_for_occupancy<-function(occupancy,admissions,LoS,beds){
  # admissions: admissions rate in a future year
  # beds: number of beds allowed in a future year
  # LoS: assumed average LoS in a future year
  # occupancy: assumed occupancy in a future year
#  return(abs(beds - qpois(occupancy, admissions*LoS/365)))
#}
#opt <- optim(par = c(0.5), fn = solve_for_occupancy  , admissions = admissions_input, LoS = LoS_input, beds = beds_input)
#return opt$par

#check
#qpois(opt$par, admissions_input*LoS_input/365)

###
# In the app we will need to perform this optimisation for each horizon year so we'll need to loop
# over them
