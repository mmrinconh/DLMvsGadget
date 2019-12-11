load("/home/marga/Documents/Data_limited course/DLM/ReferenceMSEmodel.Rdata")
#Cojo AnchovyHCR del último año
setwd("~/Documents/Data_limited course/DLM/Shiny DLM tool/fmsyreference")

ages<-c(0,10)
ages<-c(ages[],length(ages[1]:ages[2]))
months<-c(0,11)
months<-c(months[],length(months[1]:months[2]))
years<-c(0,48)
years<-c(years[],length(years[2]:years[1]))
sims<-c(1,1000)
sims<-c(sims[],length(sims[2]:sims[1]))

AnchovyHCRplus<-array(0, dim = c(ages[3],months[3],100,sims[3]))
#AnchovyHCRplusF<-array(NA, dim = c(ages[3],months[3],100,sims[3],length(seq(0.02,1.5,0.01))))

require(abind)
AnchovyHCR1<-abind(AnchovyHCR,AnchovyHCRplus, along=3)
#AnchovyHCR1$F<-seq(0.02,1.5,0.01)
AnchovyHCRbiomassplus<-array(0, dim = c(ages[3],months[3],100,sims[3]))
AnchovyHCR1biomass<-abind(AnchovyHCRbiomass,AnchovyHCRbiomassplus, along=3)



#copiado del original, parámetros que se van a usar luego
#par(mfrow=c(2,1))
#natural Mortality from GPDM
M<-0.075
#!Carrying capacity in eggs (based Xavier paper)
CC<-2.86*10^12
#The lower lambda the higher survival
lambda<-0.15
#The greater ro the higher survival
ro<-0.4
D_sd<-0.4
#!Target monthly F value 
FishMortTarget<-0.04
WindUp<-2
slope<-0.013
logD_mean<-4.6

#par(mfrow=c(1,1))
#Simulate environmental conditions
Wind<-runif(1000,2.25,15)


expP<-0.37*1+0.04*4+0.37*2+0.22*3
#!Target F is the same for both HCR May to Oct, then it depends on Wind and Temperature

# plot(5*Wind*expP,FishMortTarget*(WindUp-slope*5*(Wind*expP-2.25)), ylim=c(0,FishMortTarget*WindUp), main = "EHCR with W and T", xlab ="Sum of Windy days*SpawnTimes", ylab="F")
# abline(h=FishMortTarget/2, col = "red", lty = 2) 
# abline(h=FishMortTarget, col = "black", lty = 2) 
# 
# plot(5*Wind,FishMortTarget*(WindUp-expP*slope*5*(Wind-2.25)), ylim=c(0,FishMortTarget*WindUp), main = "EHCR with W and T", xlab ="Sum of Windy days", ylab="F")
# abline(h=FishMortTarget/2, col = "red", lty = 2) 
# abline(h=FishMortTarget, col = "black", lty = 2) 


#range(5*Wind*expP)
#hist(5*Wind*expP)


Rel_F<-matrix(1,1000,49)

#Dimensions
ages<-c(0,10)
ages<-c(ages[],length(ages[1]:ages[2]))
months<-c(0,11)
months<-c(months[],length(months[1]:months[2]))
years<-c(0,48)
years<-c(years[],length(years[2]:years[1]))
sims<-c(1,1000)
sims<-c(sims[],length(sims[2]:sims[1]))

iniQuant  <- array(0, dim = c(ages[3],months[3],years[3],sims[3]))
Months<-c("May","June","July","Aug","Sept","Oct", "Nov","Dec","Jan","Feb","March","Apr")
ageX<-c(0:10)
dimnames(iniQuant)<-list(ageClass=ageX, month=Months,year=years[1
                                                                ]:years[2],iter=sims[1]:sims[2])

Catchplus<-array(0, dim = c(months[3]*100,sims[3]))
CatchHCR1<-rbind(CatchHCR,Catchplus)
#CatchHCR1[]<-0
#CatchAnnual<-array(NA, dim = c(years[3],sims[3]))
CatchAnnualplus<-array(0, dim = c(100,sims[3]))
CatchAnnualHCR1<-rbind(CatchAnnualHCR,CatchAnnualplus)

RecruitsAnnual<-array(0, dim = c(years[3],sims[3]))
RecruitsAnnualplus<-array(0, dim = c(100,sims[3]))
RecruitsAnnualHCR1<-CatchAnnualHCR1
Referencebiomass<-AnchovyAnnualHCR
for (s in 1:sims[3]){
  y<-49
Referencebiomass[y,s]<-sum(AnchovyHCRbiomass[5:10,"Nov",y,s],AnchovyHCRbiomass[4:10,"Dec",y,s],AnchovyHCRbiomass[3:10,"Jan",y,s],AnchovyHCRbiomass[2:10,"Feb",y,s],AnchovyHCRbiomass[1:10,"March",y,s] ,AnchovyHCRbiomass[1:10,"Apr",y,s],AnchovyHCRbiomass[2:11,"May",y,s],AnchovyHCRbiomass[3:11,"June",y,s],AnchovyHCRbiomass[4:11,"July",y,s],AnchovyHCRbiomass[5:11,"Aug",y,s],AnchovyHCRbiomass[6:11,"Sept",y,s],AnchovyHCRbiomass[6:10,"Oct",y,s])
}
#CatchHCR<-array(NA, dim = c(months[3]*years[3],sims[3]))

#CatchHCRbiomass<-CatchHCR
CatchHCRbiomass<-CatchHCR1
#CatchAnnualHCR<-array(NA, dim = c(years[3],sims[3]))
#RecruitsAnnualHCR<-array(NA, dim = c(years[3],sims[3]))
#CatchAnnualHCR[]<-0
#AnchovyAnnualHCRbiomassplus<-array(0, dim = c(100,sims[3]))
#rbind(CatchAnnualHCR,CatchAnnualplus)
AnchovyAnnualHCRbiomass<-CatchAnnualHCR1 #las biomasas me da igual que cojan datos que no son de biomasa, me interesa sobretodo las dimensiones
AnchovyAnnualHCRplus<-array(0, dim = c(100,sims[3]))
AnchovyAnnualHCR1<-rbind(AnchovyAnnualHCR,AnchovyAnnualHCRplus)
#CatchAnnualHCRbiomass<-CatchAnnualHCR
CatchAnnualHCRbiomassplus<-array(0, dim = c(100,sims[3]))
CatchAnnualHCR1biomass<-rbind(CatchAnnualHCRbiomass, CatchAnnualHCRbiomassplus)
#RecruitsAnnualHCR[]<-0

#CatchHCRAges<-AnchovyHCR
CatchHCRAgesplus<-array(NA, dim = c(ages[3],months[3],100,sims[3]))

CatchHCR1Ages<-abind(CatchHCRAges,CatchHCRAgesplus, along=3)
Trick<-iniQuant 
Trick[]<-0
for(a in 2:5)
{
  Trick[2:11,a,1,1]<-Trick[1:10,a-1,1,1]+1
}
for(a in 6:12)
{
  Trick[1:10,a,1,1]<-Trick[1:10,a-1,1,1]+1
}
Trick[2:11,"May",2,1]<-Trick[1:10,"Apr",1,1]+1
for(a in 2:5)
{
  Trick[2:11,a,2,1]<-Trick[1:10,a-1,2,1]+1
}
for(a in 6:12)
{
  Trick[1:10,a,2,1]<-Trick[1:10,a-1,2,1]+1
}
Trick[7:11,"May",2,1]<-Trick[6:10,"Apr",2,1]+1
Trick[8:11,"June",2,1]<-Trick[7:10,"May",2,1]+1
Trick[9:11,"July",2,1]<-Trick[8:10,"June",2,1]+1
Trick[10:11,"Aug",2,1]<-Trick[9:10,"July",2,1]+1
Trick[11:11,"Sept",2,1]<-Trick[10:10,"Aug",2,1]+1
AgeStructure<-Trick[,,2,1]
#AgeStructure

SpawnersAge<-AgeStructure
SpawnersAge[SpawnersAge[]<10]<-0
SpawnersAge[,6:12]<-0
SpawnersAge
SpawnersWeightAtAge<-SpawnersAge*1.50907-9.2221
SpawnersWeightAtAge[SpawnersWeightAtAge[]<0]<-0
SpawnersWeightAtAge
#Number of eggs per one spawner at age
#Growth rate assumed linear with 8.33 g/mo
#Fecundity assumed to be 450 eggs/gram per female
#Fecundity is assumed to be 225 eggs/gram because we assume females are 50% of population
FecAge<-SpawnersWeightAtAge*225








library(XLConnect)               # load XLConnect package 

age_length<-read.csv("/home/marga/GADGET/Anchovy/modelageweightanchovy.csv", header=TRUE)


colnames(age_length)<-c("Average_age_month","Average_length", "Average_weight")
age_length<-age_length[-1,]
Biomassstructure<-AgeStructure
Biomassstructure[] <- 0

Biomassstructure <- age_length$Average_weight[match(AgeStructure, age_length$Average_age_month)]#grams
Biomassstructure<-matrix(as.numeric(levels(Biomassstructure)[Biomassstructure]),nrow=11, ncol=12)
#end population model
colnames(Biomassstructure)<-colnames(AgeStructure)
WindHCR<-array(NA, dim = c(149,sims[3]))


for (FishMortTarget  in seq(0.02,0.1,0.01))
{
for(s in 1:100)
{
 
 
for (y in 50:149) {
  #!Target monthly F value for May until Oct 
  
  SumWind<-0
  
  #Adults May
  AnchovyHCR1[2:11,"May",y,s]<-AnchovyHCR1[1:10,"Apr",y-1,s]*exp(-M-FishMortTarget)
  CatchHCR1[1+12*(y-1),s]<-sum(AnchovyHCR1[1:10,"Apr",y-1,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[2:11,"May",y,s]<-AnchovyHCR1[1:10,"Apr",y-1,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[1+12*(y-1),s]<-sum(AnchovyHCR1[1:10,"Apr",y-1,s]*Biomassstructure[1:10,"Apr"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[2:11,"May",y,s]<-AnchovyEHCR[1:10,"Apr",y-1,s]*exp(-M-FishMort)
  #CatchEHCR[1+12*(y-1),s]<-sum(AnchovyEHCR[1:10,"Apr",y-1,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Eggs May
  #Probability of spawning even based on historical data
  x<-runif(1,0,100)
  if(x>=96) p<-4 else 
    if(x<=37) p<-1 else 
      if(x>37 && x<71) p<-2 else p<-3
  AnchovyHCR1[1,"May",y,s]<-sum(AnchovyHCR1[,"May",y,s]*FecAge[,"May"])*p 
  AnchovyHCR1[1,"May",y,s]<-min(AnchovyHCR1[1,"May",y,s],CC)
  #AnchovyEHCR[1,"May",y,s]<-sum(AnchovyEHCR[,"May",y,s]*FecAge[,"May"])*p 
  #AnchovyEHCR[1,"May",y,s]<-min(AnchovyEHCR[1,"May",y,s],CC)
  
  #Adults June
  AnchovyHCR1[3:11,"June",y,s]<-AnchovyHCR1[2:10,"May",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[2+12*(y-1),s]<-sum(AnchovyHCR1[2:10,"May",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[2+12*(y-1),s]<-sum(AnchovyHCR1[2:10,"May",y,s]*Biomassstructure[2:10,"May"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[3:11,"June",y,s]<-AnchovyHCR1[2:10,"May",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[3:11,"June",y,s]<-AnchovyEHCR[2:10,"May",y,s]*exp(-M-FishMort)
  #CatchEHCR[2+12*(y-1),s]<-sum(AnchovyEHCR[2:10,"May",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Eggs June
  #Probability of spawning even based on historical data
  x<-runif(1,0,100)
  if(x>=96) p<-4 else 
    if(x<=37) p<-1 else 
      if(x>37 && x<71) p<-2 else p<-3
  AnchovyHCR1[1,"June",y,s]<-sum(AnchovyHCR1[,"June",y,s]*FecAge[,"June"])*p
  AnchovyHCR1[1,"June",y,s]<-min(AnchovyHCR1[1,"June",y,s],CC)
 # AnchovyEHCR[1,"June",y,s]<-sum(AnchovyEHCR[,"June",y,s]*FecAge[,"June"])*p
  #AnchovyEHCR[1,"June",y,s]<-min(AnchovyEHCR[1,"June",y,s],CC)
  #Juvenials June
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  SumWind<-SumWind+Wind
  AnchovyHCR1[2,"June",y,s]<-AnchovyHCR1[1,"May",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[2,"June",y,s]<-AnchovyEHCR[1,"May",y,s]*exp(-lambda*Wind)
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  
  #Adults July
  AnchovyHCR1[4:11,"July",y,s]<-AnchovyHCR1[3:10,"June",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[3+12*(y-1),s]<-sum(AnchovyHCR1[3:10,"June",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[3+12*(y-1),s]<-sum(AnchovyHCR1[3:10,"June",y,s]*Biomassstructure[3:10,"June"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[4:11,"July",y,s]<-AnchovyHCR1[3:10,"June",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[4:11,"July",y,s]<-AnchovyEHCR[3:10,"June",y,s]*exp(-M-FishMort)
  #CatchEHCR[3+12*(y-1),s]<-sum(AnchovyEHCR[3:10,"June",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Eggs July
  #Probability of spawning even based on historical data
  x<-runif(1,0,100)
  if(x>=96) p<-4 else 
    if(x<=37) p<-1 else 
      if(x>37 && x<71) p<-2 else p<-3
  AnchovyHCR1[1,"July",y,s]<-sum(AnchovyHCR1[,"July",y,s]*FecAge[,"July"])*p
  AnchovyHCR1[1,"July",y,s]<-min(AnchovyHCR1[1,"July",y,s],CC)
  #AnchovyEHCR[1,"July",y,s]<-sum(AnchovyEHCR[,"July",y,s]*FecAge[,"July"])*p
  #AnchovyEHCR[1,"July",y,s]<-min(AnchovyEHCR[1,"July",y,s],CC)
  #Juvenials July
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  SumWind<-SumWind+Wind
  AnchovyHCR1[2:3,"July",y,s]<-AnchovyHCR1[1:2,"June",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[2:3,"July",y,s]<-AnchovyEHCR[1:2,"June",y,s]*exp(-lambda*Wind)
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  
  #Adults Aug
  AnchovyHCR1[5:11,"Aug",y,s]<-AnchovyHCR1[4:10,"July",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[4+12*(y-1),s]<-sum(AnchovyHCR1[4:10,"July",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[4+12*(y-1),s]<-sum(AnchovyHCR1[4:10,"July",y,s]*Biomassstructure[4:10,"July"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[5:11,"Aug",y,s]<-AnchovyHCR1[4:10,"July",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[5:11,"Aug",y,s]<-AnchovyEHCR[4:10,"July",y,s]*exp(-M-FishMort)
  #CatchEHCR[4+12*(y-1),s]<-sum(AnchovyEHCR[4:10,"July",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Eggs Aug
  #Probability of spawning even based on historical data
  x<-runif(1,0,100)
  if(x>=96) p<-4 else 
    if(x<=37) p<-1 else 
      if(x>37 && x<71) p<-2 else p<-3
  AnchovyHCR1[1,"Aug",y,s]<-sum(AnchovyHCR1[,"Aug",y,s]*FecAge[,"Aug"])*p
  AnchovyHCR1[1,"Aug",y,s]<-min(AnchovyHCR1[1,"Aug",y,s],CC)
  #AnchovyEHCR[1,"Aug",y,s]<-sum(AnchovyEHCR[,"Aug",y,s]*FecAge[,"Aug"])*p
  #AnchovyEHCR[1,"Aug",y,s]<-min(AnchovyEHCR[1,"Aug",y,s],CC)
  #Juvenials Aug
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  SumWind<-SumWind+Wind
  AnchovyHCR1[2:4,"Aug",y,s]<-AnchovyHCR1[1:3,"July",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[2:4,"Aug",y,s]<-AnchovyEHCR[1:3,"July",y,s]*exp(-lambda*Wind)
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  
  #Adults Sept
  AnchovyHCR1[6:11,"Sept",y,s]<-AnchovyHCR1[5:10,"Aug",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[5+12*(y-1),s]<-sum(AnchovyHCR1[5:10,"Aug",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[5+12*(y-1),s]<-sum(AnchovyHCR1[5:10,"Aug",y,s]*Biomassstructure[5:10,"Aug"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[6:11,"Sept",y,s]<-AnchovyHCR1[5:10,"Aug",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[6:11,"Sept",y,s]<-AnchovyEHCR[5:10,"Aug",y,s]*exp(-M-FishMort)
  #CatchEHCR[5+12*(y-1),s]<-sum(AnchovyEHCR[5:10,"Aug",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Eggs Sept
  #Eggs Sept
  #Probability of spawning even based on historical data
  x<-runif(1,0,100)
  if(x>=96) p<-4 else 
    if(x<=37) p<-1 else 
      if(x>37 && x<71) p<-2 else p<-3
  AnchovyHCR1[1,"Sept",y,s]<-sum(AnchovyHCR1[,"Sept",y,s]*FecAge[,"Sept"])*p
  AnchovyHCR1[1,"Sept",y,s]<-min(AnchovyHCR1[1,"Sept",y,s],CC)
  #AnchovyEHCR[1,"Sept",y,s]<-sum(AnchovyEHCR[,"Sept",y,s]*FecAge[,"Sept"])*p
  #AnchovyEHCR[1,"Sept",y,s]<-min(AnchovyEHCR[1,"Sept",y,s],CC)
  #Juvenials Sept
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  SumWind<-SumWind+Wind
  AnchovyHCR1[2:4,"Sept",y,s]<-AnchovyHCR1[1:3,"Aug",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[2:4,"Sept",y,s]<-AnchovyEHCR[1:3,"Aug",y,s]*exp(-lambda*Wind)
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100))
  AnchovyHCR1[5,"Sept",y,s]<-AnchovyHCR1[4,"Aug",y,s]*ro*F_D
  #AnchovyEHCR[5,"Sept",y,s]<-AnchovyEHCR[4,"Aug",y,s]*ro*F_D
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  #Adults Oct
  AnchovyHCR1[6:10,"Oct",y,s]<-AnchovyHCR1[6:10,"Sept",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[6+12*(y-1),s]<-sum(AnchovyHCR1[6:10,"Sept",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[6+12*(y-1),s]<-sum(AnchovyHCR1[6:10,"Sept",y,s]*Biomassstructure[6:10,"Sept"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[6:10,"Oct",y,s]<-AnchovyHCR1[6:10,"Sept",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[6:10,"Oct",y,s]<-AnchovyEHCR[6:10,"Sept",y,s]*exp(-M-FishMort)
  #CatchEHCR[6+12*(y-1),s]<-sum(AnchovyEHCR[6:10,"Sept",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Juvenials Oct
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  SumWind<-SumWind+Wind
  AnchovyHCR1[1:3,"Oct",y,s]<-AnchovyHCR1[1:3,"Sept",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[1:3,"Oct",y,s]<-AnchovyEHCR[1:3,"Sept",y,s]*exp(-lambda*Wind)
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100))
  AnchovyHCR1[4:5,"Oct",y,s]<-AnchovyHCR1[4:5,"Sept",y,s]*ro*F_D
  #AnchovyEHCR[4:5,"Oct",y,s]<-AnchovyEHCR[4:5,"Sept",y,s]*ro*F_D
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  WindHCR[y,s]<-SumWind
  
  #the fishing for the rest of the year depends on previous months' wind conditions
  # if (FishMortTarget*{WindUp+5*(SumWind/5-2.25)/(5*(2.25-8.62))} <= FishMortTarget/2) 
  # {
  #   FishMort<-FishMortTarget/2
  # }
  # else
  # {
  #   FishMort<-FishMortTarget*{WindUp+5*(SumWind/5-2.25)/(5*(2.25-8.62))}
  # }
  
  #Rel_F[s,y]<- FishMort/FishMortTarget
  
  #Adults Nov quitando veda de Noviembre
  AnchovyHCR1[6:10,"Nov",y,s]<-AnchovyHCR1[6:10,"Oct",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[7+12*(y-1),s]<-sum(AnchovyHCR1[6:10,"Oct",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[7+12*(y-1),s]<-sum(AnchovyHCR1[6:10,"Oct",y,s]*Biomassstructure[6:10,"Oct"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[6:10,"Nov",y,s]<-AnchovyHCR1[6:10,"Oct",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  
  #AnchovyEHCR[6:10,"Nov",y,s]<-AnchovyEHCR[6:10,"Oct",y,s]*exp(-M)#-FishMort)
  # CatchEHCR[7+12*(y-1),s]<-sum(AnchovyEHCR[6:10,"Oct",y,s]*(1-exp(-M)-FishMort)))*FishMort/(FishMort+M)
  #Juvenials 
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  AnchovyHCR1[1:2,"Nov",y,s]<-AnchovyHCR1[1:2,"Oct",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[1:2,"Nov",y,s]<-AnchovyEHCR[1:2,"Oct",y,s]*exp(-lambda*Wind)
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100))
  AnchovyHCR1[3:5,"Nov",y,s]<-AnchovyHCR1[3:5,"Oct",y,s]*ro*F_D
  #AnchovyEHCR[3:5,"Nov",y,s]<-AnchovyEHCR[3:5,"Oct",y,s]*ro*F_D
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  #Adults Dec quitando veda Diciembre
  AnchovyHCR1[5:10,"Dec",y,s]<-AnchovyHCR1[5:10,"Nov",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[8+12*(y-1),s]<-sum(AnchovyHCR1[5:10,"Nov",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[8+12*(y-1),s]<-sum(AnchovyHCR1[5:10,"Nov",y,s]*Biomassstructure[5:10,"Nov"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[5:10,"Dec",y,s]<-AnchovyHCR1[5:10,"Nov",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[5:10,"Dec",y,s]<-AnchovyEHCR[5:10,"Nov",y,s]*exp(-M)#-FishMort)
  # CatchEHCR[8+12*(y-1),s]<-sum(AnchovyEHCR[5:10,"Nov",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Juvenials
  #Number of days in a month with strong easterlies wind
  Wind<-runif(1,2.25,15)
  AnchovyHCR1[1,"Dec",y,s]<-AnchovyHCR1[1,"Nov",y,s]*exp(-lambda*Wind)
  #AnchovyEHCR[1,"Dec",y,s]<-AnchovyEHCR[1,"Nov",y,s]*exp(-lambda*Wind)
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100))
  AnchovyHCR1[2:4,"Dec",y,s]<-AnchovyHCR1[2:4,"Nov",y,s]*ro*F_D
  #AnchovyEHCR[2:4,"Dec",y,s]<-AnchovyEHCR[2:4,"Nov",y,s]*ro*F_D
  #!Adjust Fishing mortality based on wind
  #FishMort<-max(FishMort*(WindUp-0.012*(Wind-2.25)),0.85*FishMort)
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  #Adults Jan
  AnchovyHCR1[4:10,"Jan",y,s]<-AnchovyHCR1[4:10,"Dec",y,s]*exp(-M)#-FishMortTarget)
  #CatchHCR[9+12*(y-1),s]<-sum(AnchovyHCR[4:10,"Dec",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[4:10,"Jan",y,s]<-AnchovyEHCR[4:10,"Dec",y,s]*exp(-M)#-FishMort)
  #CatchEHCR[9+12*(y-1),s]<-sum(AnchovyEHCR[4:10,"Dec",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Juvenials
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100)) 
  AnchovyHCR1[1:3,"Jan",y,s]<-AnchovyHCR1[1:3,"Dec",y,s]*ro*F_D
  #AnchovyEHCR[1:3,"Jan",y,s]<-AnchovyEHCR[1:3,"Dec",y,s]*ro*F_D
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  #Adults Feb y quitando veda
  AnchovyHCR1[3:10,"Feb",y,s]<-AnchovyHCR1[3:10,"Jan",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[10+12*(y-1),s]<-sum(AnchovyHCR1[3:10,"Jan",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[10+12*(y-1),s]<-sum(AnchovyHCR1[3:10,"Jan",y,s]*Biomassstructure[3:10,"Jan"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[3:10,"Feb",y,s]<-AnchovyHCR1[3:10,"Jan",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[3:10,"Feb",y,s]<-AnchovyEHCR[3:10,"Jan",y,s]*exp(-M)#-FishMort)
  # CatchEHCR[10+12*(y-1),s]<-sum(AnchovyEHCR[3:10,"Jan",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Juvenials
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100)) 
  AnchovyHCR1[1:2,"Feb",y,s]<-AnchovyHCR1[1:2,"Jan",y,s]*ro*F_D
  #AnchovyEHCR[1:2,"Feb",y,s]<-AnchovyEHCR[1:2,"Jan",y,s]*ro*F_D
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  #Adults March
  AnchovyHCR1[2:10,"March",y,s]<-AnchovyHCR1[2:10,"Feb",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[11+12*(y-1),s]<-sum(AnchovyHCR1[2:10,"Feb",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[11+12*(y-1),s]<-sum(AnchovyHCR1[2:10,"Feb",y,s]*Biomassstructure[2:10,"Feb"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[2:10,"March",y,s]<-AnchovyHCR1[2:10,"Feb",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[2:10,"March",y,s]<-AnchovyEHCR[2:10,"Feb",y,s]*exp(-M-FishMort)
  #CatchEHCR[11+12*(y-1),s]<-sum(AnchovyEHCR[2:10,"Feb",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  #Juvenials
  #Modelling monthly discharge
  D<-rlnorm(1,logD_mean,D_sd)
  F_D<-dnorm(log(D)-log(100))
  AnchovyHCR1[1,"March",y,s]<-AnchovyHCR1[1,"Feb",y,s]*ro*F_D
  #AnchovyEHCR[1,"March",y,s]<-AnchovyEHCR[1,"Feb",y,s]*ro*F_D
  #!Adjust Fishing mortality based on discharges
  #F_factor<-max(F_D/dnorm(0),0.85)
  #FishMort<-F_factor*FishMort
  
  #Adults Apr
  AnchovyHCR1[1:10,"Apr",y,s]<-AnchovyHCR1[1:10,"March",y,s]*exp(-M-FishMortTarget)
  CatchHCR1[12+12*(y-1),s]<-sum(AnchovyHCR1[1:10,"March",y,s]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCRbiomass[12+12*(y-1),s]<-sum(AnchovyHCR1[1:10,"March",y,s]*Biomassstructure[1:10,"March"]*(1-exp(-M-FishMortTarget)))*FishMortTarget/(FishMortTarget+M)
  CatchHCR1Ages[1:10,"Apr",y,s]<-AnchovyHCR1[1:10,"March",y,s]*(1-exp(-M-FishMortTarget))*FishMortTarget/(FishMortTarget+M)
  #AnchovyEHCR[1:10,"Apr",y,s]<-AnchovyEHCR[1:10,"March",y,s]*exp(-M-FishMort)
  #CatchEHCR[12+12*(y-1),s]<-sum(AnchovyEHCR[1:10,"March",y,s]*(1-exp(-M-FishMort)))*FishMort/(FishMort+M)
  m0<-1+12*(y-1)
  m1<-12+12*(y-1)
  CatchAnnualHCR1[y,s]<- sum(CatchHCR1[m0:m1,s])
  CatchAnnualHCR1biomass[y,s]<- sum(CatchHCRbiomass[m0:m1,s])
  AnchovyHCR1biomass[,,y,s]<-AnchovyHCR1[,,y,s]*Biomassstructure
  
  AnchovyAnnualHCR1[y,s]<-sum(AnchovyHCR1[5:10,"Nov",y,s],AnchovyHCR1[4:10,"Dec",y,s],AnchovyHCR1[3:10,"Jan",y,s],AnchovyHCR1[2:10,"Feb",y,s],AnchovyHCR1[1:10,"March",y,s]
                             ,AnchovyHCR1[1:10,"Apr",y,s],AnchovyHCR1[2:11,"May",y,s],AnchovyHCR1[3:11,"June",y,s],AnchovyHCR1[4:11,"July",y,s],AnchovyHCR1[5:11,"Aug",y,s],AnchovyHCR1[6:11,"Sept",y,s],AnchovyHCR1[6:10,"Oct",y,s])
  
  AnchovyAnnualHCRbiomass[y,s]<-sum(AnchovyHCR1biomass[5:10,"Nov",y,s],AnchovyHCR1biomass[4:10,"Dec",y,s],AnchovyHCR1biomass[3:10,"Jan",y,s],AnchovyHCR1biomass[2:10,"Feb",y,s],AnchovyHCR1biomass[1:10,"March",y,s] ,AnchovyHCR1biomass[1:10,"Apr",y,s],AnchovyHCR1biomass[2:11,"May",y,s],AnchovyHCR1biomass[3:11,"June",y,s],AnchovyHCR1biomass[4:11,"July",y,s],AnchovyHCR1biomass[5:11,"Aug",y,s],AnchovyHCR1biomass[6:11,"Sept",y,s],AnchovyHCR1biomass[6:10,"Oct",y,s])
  
  RecruitsAnnualHCR1[y,s]<-AnchovyHCR1[5,"Nov",y,s]+AnchovyHCR1[4,"Dec",y,s]+AnchovyHCR1[3,"Jan",y,s]+AnchovyHCR1[2,"Feb",y,s]+AnchovyHCR1[1,"March",y,s]
  
  #CatchAnnualEHCR[y,s]<- sum(CatchEHCR[m0:m1,s])
  #RecruitsAnnualEHCR[y,s]<-AnchovyEHCR[5,"Nov",y,s]+AnchovyEHCR[4,"Dec",y,s]+AnchovyEHCR[3,"Jan",y,s]+AnchovyEHCR[2,"Feb",y,s]+AnchovyEHCR[1,"March",y,s]
  
  #CatchHalfAnnualHCR[y,s]<- sum(CatchEHCR[(m0+6):m1,s])
  #CatchHalfAnnualHCR1[y,s]<-CatchHalfAnnualHCR[y,s]
  #    if (y==49){
  #    CatchHalfAnnualHCR1[y,s]<-1
  #    }else{
  #    CatchHalfAnnualHCR1[y,s]<-sum(CatchEHCR[(m0+10):(m1+6),s])}
}
}
 lastbiomass<- AnchovyAnnualHCRbiomass[149,1:100]
 lastcatch<-CatchAnnualHCR1[149,1:100]
  save(lastbiomass,lastcatch, FishMortTarget,file=paste0("results", FishMortTarget*100,".Rdata",sep=""))
}

FishMortTarget<-0.02
load(paste0("/home/marga/Documents/Data_limited course/DLM/Shiny DLM tool/fmsyreference/results",FishMortTarget*100,".Rdata"))
Fvsfuture<-data.frame(FishMortTarget,lastcatch,sim=1:100)
for (FishMortTarget in seq(0.03,0.1,0.01)){
load(paste0("/home/marga/Documents/Data_limited course/DLM/Shiny DLM tool/fmsyreference/results",FishMortTarget*100,".Rdata"))
Fvsfuture<-rbind(Fvsfuture,data.frame(FishMortTarget,lastcatch,sim=1:100))
}
Fvsfuture
ggplot(Fvsfuture %>%filter(sim <11),aes(FishMortTarget,lastcatch))+geom_line()+facet_wrap(~sim,scales="free")
#FishhMortTarget<-0.02
require(tidyverse)
Fvsfuture%>%filter(sim <5)

Fmsy<-Fvsfuture%>%group_by(sim)%>%filter(lastcatch==max(lastcatch)) %>%filter(sim<11) 

save(Fmsy, file="Fsyreference.Rdata")

