# Data Science for Political Science Final Project

# Thomas Malik Salem

setwd("C:/Users/malik/Downloads/Data Science for Political Science Data Sets")

# Load the statewide Florida Election Data

Floridastatewide <- read.csv("Floridastatewide.csv")

# Deduce the vote share between each party in each election cycle

Floridastatewide$Margin <- (Floridastatewide$Democrat.Margin....- Floridastatewide$Republican.Margin...)

#Calculate the voter registration advantage entering each election

Floridastatewide$RegistrationEdge <- (((Floridastatewide$Registered.Democrats - Floridastatewide$Registered.Republicans)/ Floridastatewide$Total.Registered.Voters) *100)

#Employ 2 loops to tabulate the margin and voter registration shift between each election year 

floridamarginshift <- rep(NA, length(Floridastatewide$Margin)-1)

floridapartisanvoteshift <- rep(NA, length(Floridastatewide$Margin)-1)

for(i in 1:length(floridamarginshift)){
  floridamarginshift[i] <- (Floridastatewide$Margin[i+1]-Floridastatewide$Margin[i])
  floridapartisanvoteshift[i] <- ifelse(floridamarginshift[i]>0,"Democrat", "Republican")
}

floridaregistrationshift <- rep(NA, length(Floridastatewide$RegistrationEdge)-1)

floridapartisanregistrationshift <- rep(NA, length(Floridastatewide$RegistrationEdge)-1)
  
for(i in 1:length(floridaregistrationshift)){
  floridaregistrationshift[i] <- (Floridastatewide$RegistrationEdge[i+1]-Floridastatewide$RegistrationEdge[i])
  floridapartisanregistrationshift[i] <- ifelse(floridaregistrationshift[i]>0,"Democrat", "Republican")
}

# Craft a confusion matrix to discern any patterns 

floridaregistrationandvoting <- table(Registration = floridapartisanregistrationshift, Results = floridapartisanvoteshift )

floridaregistrationandvoting  

registrationaccuracystatewide <- (4/4)*100

#The voter registration tracked with the election returns in each case

#Load the county level data for Florida

floridacounty <- read.csv("floridacounty.csv")

#Extrapolate the margins separating the two parties in each election year 

floridacounty$margin2004 <- (floridacounty$Democrat.Margin.2004- floridacounty$Republican.Margin.2004)
  
floridacounty$margin2008 <- (floridacounty$Democrat.Margin.2008- floridacounty$Republican.Margin.2008)

floridacounty$margin2012 <- (floridacounty$Democrat.Margin.2012- floridacounty$Republican.Margin..2012)

floridacounty$margin2016 <- (floridacounty$Democrat.Margin.2016- floridacounty$Republican.Margin.2016)

floridacounty$margin2020 <- (floridacounty$Democrat.Margin.2020- floridacounty$Republican.Margin.2020)

#Calculate the partisan voter registration in each county for every year 

floridacounty$registration2004 <- (((floridacounty$Democrat.Registration.2004-floridacounty$Republican.Registration.2004)/ floridacounty$Total.Registration2004) *100)

floridacounty$registration2008 <- (((floridacounty$Democrat.Registration.2008-floridacounty$Republican.Registration.2008)/ floridacounty$Total.Registration.2008) *100)

floridacounty$registration2012 <- (((floridacounty$Democrat.Registration..2012-floridacounty$Republican.Registration.2012)/ floridacounty$Total.Registration.2012) *100)

floridacounty$registration2016 <- (((floridacounty$Democrat.Registration.2016-floridacounty$Republican.Registration.2016)/ floridacounty$Total.Registration..2016) *100)

floridacounty$registration2020 <- (((floridacounty$Democrat.Registration.2020-floridacounty$Republican.Registration.2020)/ floridacounty$Total.Registration.2020) *100)

#Similar to earlier, construct 2 loops per year to report the partisan shift of each county in registration in election results
#and record the number of similar and contrary shifts

#2004 to 2008

floridamarginshiftcounty2004to2008 <- rep(NA, length(floridacounty$margin2004))

floridapartisanvoteshift2004to2008 <- rep(NA, length(floridacounty$margin2004))

for(i in 1:length(floridamarginshiftcounty2004to2008)){
  floridamarginshiftcounty2004to2008[i] <- (floridacounty$margin2008[i]-floridacounty$margin2004[i])
  floridapartisanvoteshift2004to2008[i] <- ifelse(floridamarginshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

floridaregistrationshiftcounty2004to2008<- rep(NA, length(floridacounty$registration2004))

floridapartisanregistrationshift2004to2008<- rep(NA, length(floridacounty$registration2004))

for(i in 1:length(floridaregistrationshiftcounty2004to2008)){
  floridaregistrationshiftcounty2004to2008[i] <- (floridacounty$registration2008[i]-floridacounty$registration2004[i])
  floridapartisanregistrationshift2004to2008[i] <- ifelse(floridaregistrationshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

registrationandvoting2004to2008<- table(Registration = floridapartisanregistrationshift2004to2008, Results =  floridapartisanvoteshift2004to2008 )

registrationandvoting2004to2008

florida2004to2008accuracy <- (36/67)*100

floridacorrect <- 19+17

floridawrong <- 9+22

#Repeat for the 2008 to 2012 cycle 

floridamarginshiftcounty2008to2012 <- rep(NA, length(floridacounty$margin2008))

floridapartisanvoteshift2008to2012 <- rep(NA, length(floridacounty$margin2008))

for(i in 1:length(floridamarginshiftcounty2008to2012)){
  floridamarginshiftcounty2008to2012[i] <- (floridacounty$margin2012[i]-floridacounty$margin2008[i])
  floridapartisanvoteshift2008to2012[i] <- ifelse(floridamarginshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

floridaregistrationshiftcounty2008to2012<- rep(NA, length(floridacounty$registration2008))

floridapartisanregistrationshift2008to2012<- rep(NA, length(floridacounty$registration2008))

for(i in 1:length(floridaregistrationshiftcounty2008to2012)){
  floridaregistrationshiftcounty2008to2012[i] <- (floridacounty$registration2012[i]-floridacounty$registration2008[i])
  floridapartisanregistrationshift2008to2012[i] <- ifelse(floridaregistrationshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

floridaregistrationandvoting2008to2012<- table(Registration = floridapartisanregistrationshift2008to2012, Results =  floridapartisanvoteshift2008to2012 )

floridaregistrationandvoting2008to2012

florida2008to2012accuracy <- (59/67)*100

floridacorrect + 2 +57

floridawrong + 5 +3 

floridacorrect <- 95

floridawrong <- 39

#Repeat for the 2012 to 2016 cycle 

floridamarginshiftcounty2012to2016 <- rep(NA, length(floridacounty$margin2012))

floridapartisanvoteshift2012to2016 <- rep(NA, length(floridacounty$margin2012))

for(i in 1:length(floridamarginshiftcounty2012to2016)){
  floridamarginshiftcounty2012to2016[i] <- (floridacounty$margin2016[i]-floridacounty$margin2012[i])
  floridapartisanvoteshift2012to2016[i] <- ifelse(floridamarginshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

floridaregistrationshiftcounty2012to2016<- rep(NA, length(floridacounty$registration2012))

floridapartisanregistrationshift2012to2016<- rep(NA, length(floridacounty$registration2012))

for(i in 1:length(floridaregistrationshiftcounty2012to2016)){
  floridaregistrationshiftcounty2012to2016[i] <- (floridacounty$registration2016[i]-floridacounty$registration2012[i])
  floridapartisanregistrationshift2012to2016[i] <- ifelse(floridaregistrationshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

floridaregistrationandvoting2012to2016<- table(Registration = floridapartisanregistrationshift2012to2016, Results =  floridapartisanvoteshift2012to2016 )

floridaregistrationandvoting2012to2016

florida2012to2016accuracy <- (57/67)*100
  
floridacorrect + 3 +54

floridawrong + 9 +1 

floridacorrect <- 152

floridawrong <- 49

#Lastly, impose the code on the 2016 to 2020 election cycle 

floridamarginshiftcounty2016to2020 <- rep(NA, length(floridacounty$margin2016))

floridapartisanvoteshift2016to2020 <- rep(NA, length(floridacounty$margin2016))

for(i in 1:length(floridamarginshiftcounty2016to2020)){
  floridamarginshiftcounty2016to2020[i] <- (floridacounty$margin2020[i]-floridacounty$margin2016[i])
  floridapartisanvoteshift2016to2020[i] <- ifelse(floridamarginshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

floridaregistrationshiftcounty2016to2020<- rep(NA, length(floridacounty$registration2016))

floridapartisanregistrationshift2016to2020<- rep(NA, length(floridacounty$registration2016))

for(i in 1:length(floridaregistrationshiftcounty2016to2020)){
  floridaregistrationshiftcounty2016to2020[i] <- (floridacounty$registration2020[i]-floridacounty$registration2016[i])
  floridapartisanregistrationshift2016to2020[i] <- ifelse(floridaregistrationshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

floridaregistrationandvoting2016to2020<- table(Registration = floridapartisanregistrationshift2016to2020, Results =  floridapartisanvoteshift2016to2020 )

floridaregistrationandvoting2016to2020

florida2016to2020accuracy <- (51/67)*100

floridacorrect + 11+40

floridawrong + 14+2 

floridacorrect <- 203

floridawrong <- 65

203+65

Floridacountylevelaccuracy <-(203/268) 

Floridacountylevelaccuracy

overallcorrect <- 203

overallwrong <- 65

#At the the county level, the voter registration trends correctly forecast the election trend in 
#75.75% of instances between 2004 and 2020. 

#Load the statewide voter registration and election data for Arizona

arizonastatewide <- read.csv("arizonastatewide.csv")

#Perform the necessary computations to describe the partisan shift in registration and election
#results for each cycle in the sample

arizonastatewide$Margin <- (arizonastatewide$Democrat.Margin....- arizonastatewide$Republican.Margin...)

arizonastatewide$RegistrationEdge <- (((arizonastatewide$Registered.Democrats - arizonastatewide$Registered.Republicans)/ arizonastatewide$Total.Registered.Voters) *100)

#Assess if the state trended Republican or Democrat with regard to registration and election results

arizonastatewidemarginshift <- rep(NA, length(arizonastatewide$Margin)-1)

arizonapartisanvoteshift <- rep(NA, length(arizonastatewide$Margin)-1)

for(i in 1:length(arizonastatewidemarginshift)){
  arizonastatewidemarginshift[i]<- (arizonastatewide$Margin[i+1]-arizonastatewide$Margin[i])
  arizonapartisanvoteshift[i] <- ifelse(arizonastatewidemarginshift[i]>0,"Democrat", "Republican")
}

arizonaregistrationshift <- rep(NA, length(arizonastatewide$RegistrationEdge)-1)

arizonapartisanregistrationshift <- rep(NA, length(arizonastatewide$RegistrationEdge)-1)

for(i in 1:length(arizonaregistrationshift)){
  arizonaregistrationshift[i] <- (arizonastatewide$RegistrationEdge[i+1]-arizonastatewide$RegistrationEdge[i])
  arizonapartisanregistrationshift[i] <- ifelse(arizonaregistrationshift[i]>0,"Democrat", "Republican")
}

#Implement a confusion matrix to assess the tracking of the two variables

arizonaregistrationandvoting <- table(Registration = arizonapartisanregistrationshift, Results = arizonapartisanvoteshift )

arizonaregistrationandvoting  

registrationaccuracystatewide <- (8/8)*100

#Akin to Florida, Arizona's statewide voter registration shifts and election results have demonstrated a parallel trend

#Upload the county level election results and voter registration data for Arizona

arizonacounty <- read.csv("arizonacounty.csv")

#Assess the partisan vote and registration shift for each county between each cycle

arizonacounty$margin2004 <- (arizonacounty$Democrat.Margin.2004- arizonacounty$Republican.Margin.2004)

arizonacounty$margin2008 <- (arizonacounty$Democrat.Margin.2008- arizonacounty$Republican.Margin.2008)

arizonacounty$margin2012 <- (arizonacounty$Democrat.Margin.2012- arizonacounty$Republican.Margin..2012)

arizonacounty$margin2016 <- (arizonacounty$Democrat.Margin.2016- arizonacounty$Republican.Margin.2016)

arizonacounty$margin2020 <- (arizonacounty$Democrat.Margin.2020- arizonacounty$Republican.Margin.2020)

arizonacounty$registration2004 <- (((arizonacounty$Democrat.Registration.2004-arizonacounty$Republican.Registration.2004)/ arizonacounty$Total.Registration2004) *100)

arizonacounty$registration2008 <- (((arizonacounty$Democrat.Registration.2008-arizonacounty$Republican.Registration.2008)/ arizonacounty$Total.Registration.2008) *100)

arizonacounty$registration2012 <- (((arizonacounty$Democrat.Registration..2012-arizonacounty$Republican.Registration.2012)/ arizonacounty$Total.Registration.2012) *100)

arizonacounty$registration2016 <- (((arizonacounty$Democrat.Registration.2016-arizonacounty$Republican.Registration.2016)/ arizonacounty$Total.Registration..2016) *100)

arizonacounty$registration2020 <- (((arizonacounty$Democrat.Registration.2020-arizonacounty$Republican.Registration.2020)/ arizonacounty$Total.Registration.2020) *100)

#Forge 2 loops to evaluate the voter registration and election trends in each county
#and apply to every cycle featured in the data 

#2004 to 2008

arizonamarginshiftcounty2004to2008 <- rep(NA, length(arizonacounty$margin2004))

arizonapartisanvoteshift2004to2008 <- rep(NA, length(arizonacounty$margin2004))

for(i in 1:length(arizonamarginshiftcounty2004to2008)){
  arizonamarginshiftcounty2004to2008[i] <- (arizonacounty$margin2008[i]-arizonacounty$margin2004[i])
  arizonapartisanvoteshift2004to2008[i] <- ifelse(arizonamarginshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

arizonaregistrationshiftcounty2004to2008<- rep(NA, length(arizonacounty$registration2004))

arizonapartisanregistrationshift2004to2008<- rep(NA, length(arizonacounty$registration2004))

for(i in 1:length(arizonaregistrationshiftcounty2004to2008)){
  arizonaregistrationshiftcounty2004to2008[i] <- (arizonacounty$registration2008[i]-arizonacounty$registration2004[i])
  arizonapartisanregistrationshift2004to2008[i] <- ifelse(arizonaregistrationshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

arizonaregistrationandvoting2004to2008<- table(Registration = arizonapartisanregistrationshift2004to2008, Results =  arizonapartisanvoteshift2004to2008 )

arizonaregistrationandvoting2004to2008

arizona2004to2008accuracy <- (8/15)*100

arizonacorrect <- 8

arizonawrong <-7

#2008 to 2012

arizonamarginshiftcounty2008to2012 <- rep(NA, length(arizonacounty$margin2008))

arizonapartisanvoteshift2008to2012 <- rep(NA, length(arizonacounty$margin2008))

for(i in 1:length(arizonamarginshiftcounty2008to2012)){
  arizonamarginshiftcounty2008to2012[i] <- (arizonacounty$margin2012[i]-arizonacounty$margin2008[i])
  arizonapartisanvoteshift2008to2012[i] <- ifelse(arizonamarginshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

arizonaregistrationshiftcounty2008to2012<- rep(NA, length(arizonacounty$registration2008))

arizonapartisanregistrationshift2008to2012<- rep(NA, length(arizonacounty$registration2008))

for(i in 1:length(arizonaregistrationshiftcounty2008to2012)){
  arizonaregistrationshiftcounty2008to2012[i] <- (arizonacounty$registration2012[i]-arizonacounty$registration2008[i])
  arizonapartisanregistrationshift2008to2012[i] <- ifelse(arizonaregistrationshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

arizonaregistrationandvoting2008to2012<- table(Registration = arizonapartisanregistrationshift2008to2012, Results =  arizonapartisanvoteshift2008to2012 )

arizonaregistrationandvoting2008to2012

arizona2008to2012accuracy <- (7/15)*100

arizonacorrect + 8

arizonawrong+7

arizonacorrect <- 16

arizonawrong <- 14

#2012 to 2016

arizonamarginshiftcounty2012to2016 <- rep(NA, length(arizonacounty$margin2012))

arizonapartisanvoteshift2012to2016 <- rep(NA, length(arizonacounty$margin2012))

for(i in 1:length(arizonamarginshiftcounty2012to2016)){
  arizonamarginshiftcounty2012to2016[i] <- (arizonacounty$margin2016[i]-arizonacounty$margin2012[i])
  arizonapartisanvoteshift2012to2016[i] <- ifelse(arizonamarginshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

arizonaregistrationshiftcounty2012to2016<- rep(NA, length(arizonacounty$registration2012))

arizonapartisanregistrationshift2012to2016<- rep(NA, length(arizonacounty$registration2012))

for(i in 1:length(arizonaregistrationshiftcounty2012to2016)){
  arizonaregistrationshiftcounty2012to2016[i] <- (arizonacounty$registration2016[i]-arizonacounty$registration2012[i])
  arizonapartisanregistrationshift2012to2016[i] <- ifelse(arizonaregistrationshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

arizonaregistrationandvoting2012to2016<- table(Registration = arizonapartisanregistrationshift2012to2016, Results =  arizonapartisanvoteshift2012to2016 )

arizonaregistrationandvoting2012to2016

arizona2012to2016accuracy <- (15/15)*100

arizonacorrect +15

arizonacorrect <- 31

#2016 to 2020

arizonamarginshiftcounty2016to2020 <- rep(NA, length(arizonacounty$margin2016))

arizonapartisanvoteshift2016to2020 <- rep(NA, length(arizonacounty$margin2016))

for(i in 1:length(arizonamarginshiftcounty2016to2020)){
  arizonamarginshiftcounty2016to2020[i] <- (arizonacounty$margin2020[i]-arizonacounty$margin2016[i])
  arizonapartisanvoteshift2016to2020[i] <- ifelse(arizonamarginshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

arizonaregistrationshiftcounty2016to2020<- rep(NA, length(arizonacounty$registration2016))

arizonapartisanregistrationshift2016to2020<- rep(NA, length(arizonacounty$registration2016))

for(i in 1:length(arizonaregistrationshiftcounty2016to2020)){
  arizonaregistrationshiftcounty2016to2020[i] <- (arizonacounty$registration2020[i]-arizonacounty$registration2016[i])
  arizonapartisanregistrationshift2016to2020[i] <- ifelse(arizonaregistrationshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

arizonaregistrationandvoting2016to2020<- table(Registration = arizonapartisanregistrationshift2016to2020, Results =  arizonapartisanvoteshift2016to2020 )

arizonaregistrationandvoting2016to2020

arizona2016to2020accuracy <- (9/15)*100

arizonacorrect +9

arizonawrong + 6

arizonacorrect <-40

arizonawrong <-20

overallcorrect +40

overallwrong +20

overallcorrect <- 243

overallwrong <- 85

arizonacountylevelaccuracy <-(40/60) 

arizonacountylevelaccuracy

#Although worse than Arizona, the voter registration data predicted the county level
#election  shift in 66.66% of cases between 2004 and 2020

#Load the statewide data for the state of Colorado

coloradostatewide <- read.csv("coloradostatewide.csv")

#Determine the margin and voter registration figures for each election year

coloradostatewide$Margin <- (coloradostatewide$Democrat.Margin....- coloradostatewide$Republican.Margin...)

coloradostatewide$RegistrationEdge <- ((coloradostatewide$Registered.Democrats - coloradostatewide$Registered.Republicans)/ coloradostatewide$Total.Registered.Voters) *100

#Assess if the state trended Republican or Democrat with regard to registration and election results

coloradostatewidemarginshift <- rep(NA, length(coloradostatewide$Margin)-1)

coloradopartisanvoteshift <- rep(NA, length(coloradostatewide$Margin)-1)

for(i in 1:length(coloradostatewidemarginshift)){
  coloradostatewidemarginshift[i]<- (coloradostatewide$Margin[i+1]-coloradostatewide$Margin[i])
  coloradopartisanvoteshift[i] <- ifelse(coloradostatewidemarginshift[i]>0,"Democrat", "Republican")
}

coloradoregistrationshift <- rep(NA, length(coloradostatewide$RegistrationEdge)-1)

coloradopartisanregistrationshift <- rep(NA, length(coloradostatewide$RegistrationEdge)-1)

for(i in 1:length(coloradoregistrationshift)){
  coloradoregistrationshift[i] <- (coloradostatewide$RegistrationEdge[i+1]-coloradostatewide$RegistrationEdge[i])
  coloradopartisanregistrationshift[i] <- ifelse(coloradoregistrationshift[i]>0,"Democrat", "Republican")
}

#Draw a confusion matrix to gauge any patterns between the variables

coloradoregistrationandvoting <- table(Registration = coloradopartisanregistrationshift, Results = coloradopartisanvoteshift )

coloradoregistrationandvoting 

registrationaccuracystatewide <- (11/12) *100

#Colorado's registration trend and election results aligned in 3 out of 4 election cycles. The 2016 
#presidential election poses a slight outlier.

#Collect the county level voter registration statistics

coloradocounty <- read.csv("coloradocounty.csv")

#Tabulate the election and voter registration margins in each county in each year 

coloradocounty$margin2004 <- (coloradocounty$Democrat.Margin.2004- coloradocounty$Republican.Margin.2004)

coloradocounty$margin2008 <- (coloradocounty$Democrat.Margin.2008- coloradocounty$Republican.Margin.2008)

coloradocounty$margin2012 <- (coloradocounty$Democrat.Margin.2012- coloradocounty$Republican.Margin..2012)

coloradocounty$margin2016 <- (coloradocounty$Democrat.Margin.2016- coloradocounty$Republican.Margin.2016)

coloradocounty$margin2020 <- (coloradocounty$Democrat.Margin.2020- coloradocounty$Republican.Margin.2020)

coloradocounty$registration2004 <- (((coloradocounty$Democrat.Registration.2004-coloradocounty$Republican.Registration.2004)/ coloradocounty$Total.Registration2004) *100)

coloradocounty$registration2008 <- (((coloradocounty$Democrat.Registration.2008-coloradocounty$Republican.Registration.2008)/ coloradocounty$Total.Registration.2008) *100)

coloradocounty$registration2012 <- (((coloradocounty$Democrat.Registration..2012-coloradocounty$Republican.Registration.2012)/ coloradocounty$Total.Registration.2012) *100)

coloradocounty$registration2016 <- (((coloradocounty$Democrat.Registration.2016-coloradocounty$Republican.Registration.2016)/ coloradocounty$Total.Registration..2016) *100)

coloradocounty$registration2020 <- (((coloradocounty$Democrat.Registration.2020-coloradocounty$Republican.Registration.2020)/ coloradocounty$Total.Registration.2020) *100)

#Through 2 loops, rate the partisan trend of each county with regard to election results and voter
#registration and compare with a confusion matrix. Conduct the process for each election cycle. 

#2004 to 2008

coloradomarginshiftcounty2004to2008 <- rep(NA, length(coloradocounty$margin2004))

coloradopartisanvoteshift2004to2008 <- rep(NA, length(coloradocounty$margin2004))

for(i in 1:length(coloradomarginshiftcounty2004to2008)){
  coloradomarginshiftcounty2004to2008[i] <- (coloradocounty$margin2008[i]-coloradocounty$margin2004[i])
  coloradopartisanvoteshift2004to2008[i] <- ifelse(coloradomarginshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

coloradoregistrationshiftcounty2004to2008<- rep(NA, length(coloradocounty$registration2004))

coloradopartisanregistrationshift2004to2008<- rep(NA, length(coloradocounty$registration2004))

for(i in 1:length(coloradoregistrationshiftcounty2004to2008)){
  coloradoregistrationshiftcounty2004to2008[i] <- (coloradocounty$registration2008[i]-coloradocounty$registration2004[i])
  coloradopartisanregistrationshift2004to2008[i] <- ifelse(coloradoregistrationshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

coloradoregistrationandvoting2004to2008<- table(Registration = coloradopartisanregistrationshift2004to2008, Results =  coloradopartisanvoteshift2004to2008 )

coloradoregistrationandvoting2004to2008

colorado2004to2008accuracy <- (38/64)*100

coloradocorrect <- 38

coloradowrong <- 26

#2008 to 2012

coloradomarginshiftcounty2008to2012 <- rep(NA, length(coloradocounty$margin2008))

coloradopartisanvoteshift2008to2012 <- rep(NA, length(coloradocounty$margin2008))

for(i in 1:length(coloradomarginshiftcounty2008to2012)){
  coloradomarginshiftcounty2008to2012[i] <- (coloradocounty$margin2012[i]-coloradocounty$margin2008[i])
  coloradopartisanvoteshift2008to2012[i] <- ifelse(coloradomarginshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

coloradoregistrationshiftcounty2008to2012<- rep(NA, length(coloradocounty$registration2008))

coloradopartisanregistrationshift2008to2012<- rep(NA, length(coloradocounty$registration2008))

for(i in 1:length(coloradoregistrationshiftcounty2008to2012)){
  coloradoregistrationshiftcounty2008to2012[i] <- (coloradocounty$registration2012[i]-coloradocounty$registration2008[i])
  coloradopartisanregistrationshift2008to2012[i] <- ifelse(coloradoregistrationshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

coloradoregistrationandvoting2008to2012<- table(Registration = coloradopartisanregistrationshift2008to2012, Results =  coloradopartisanvoteshift2008to2012 )

coloradoregistrationandvoting2008to2012

colorado2008to2012accuracy <- (43/64)*100

coloradocorrect + 43

coloradowrong + 21

coloradocorrect <- 81

coloradowrong <-47

#2012 to 2016 

coloradomarginshiftcounty2012to2016 <- rep(NA, length(coloradocounty$margin2012))

coloradopartisanvoteshift2012to2016 <- rep(NA, length(coloradocounty$margin2012))

for(i in 1:length(coloradomarginshiftcounty2012to2016)){
  coloradomarginshiftcounty2012to2016[i] <- (coloradocounty$margin2016[i]-coloradocounty$margin2012[i])
  coloradopartisanvoteshift2012to2016[i] <- ifelse(coloradomarginshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

coloradoregistrationshiftcounty2012to2016<- rep(NA, length(coloradocounty$registration2012))

coloradopartisanregistrationshift2012to2016<- rep(NA, length(coloradocounty$registration2012))

for(i in 1:length(coloradoregistrationshiftcounty2012to2016)){
  coloradoregistrationshiftcounty2012to2016[i] <- (coloradocounty$registration2016[i]-coloradocounty$registration2012[i])
  coloradopartisanregistrationshift2012to2016[i] <- ifelse(coloradoregistrationshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

coloradoregistrationandvoting2012to2016<- table(Registration = coloradopartisanregistrationshift2012to2016, Results =  coloradopartisanvoteshift2012to2016 )

coloradoregistrationandvoting2012to2016

colorado2012to2016accuracy <- (47/64)*100

coloradocorrect +47

coloradowrong +17

coloradocorrect <- 128

coloradowrong <-64

#2016 to 2020

coloradomarginshiftcounty2016to2020 <- rep(NA, length(coloradocounty$margin2016))

coloradopartisanvoteshift2016to2020 <- rep(NA, length(coloradocounty$margin2016))

for(i in 1:length(coloradomarginshiftcounty2016to2020)){
  coloradomarginshiftcounty2016to2020[i] <- (coloradocounty$margin2020[i]-coloradocounty$margin2016[i])
  coloradopartisanvoteshift2016to2020[i] <- ifelse(coloradomarginshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

coloradoregistrationshiftcounty2016to2020<- rep(NA, length(coloradocounty$registration2016))

coloradopartisanregistrationshift2016to2020<- rep(NA, length(coloradocounty$registration2016))

for(i in 1:length(coloradoregistrationshiftcounty2016to2020)){
  coloradoregistrationshiftcounty2016to2020[i] <- (coloradocounty$registration2020[i]-coloradocounty$registration2016[i])
  coloradopartisanregistrationshift2016to2020[i] <- ifelse(coloradoregistrationshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

coloradoregistrationandvoting2016to2020<- table(Registration = coloradopartisanregistrationshift2016to2020, Results =  coloradopartisanvoteshift2016to2020 )

coloradoregistrationandvoting2016to2020

colorado2016to2020accuracy <- (47/64)*100

coloradocorrect+47

coloradowrong +17

coloradocorrect <-175

coloradowrong <-81

overallcorrect +175

overallwrong +81

overallcorrect <- 418

overallwrong <-166

coloradocountylevelaccuracy <- (175/256)

coloradocountylevelaccuracy

#Colorado's 64 counties tracked with voter registrations in 68.36% of iterations, exceeding Arizona yet 
#still lagging behind Florida.

#Load the statewide data for North Carolina

northcarolinastatewide <- read.csv("northcarolinastatewide.csv")

#Compute the election margins and voter registration figures statewide between 2004 and 2020

northcarolinastatewide$Margin <- (northcarolinastatewide$Democrat.Margin....- northcarolinastatewide$Republican.Margin...)

northcarolinastatewide$RegistrationEdge <- ((northcarolinastatewide$Registered.Democrats - northcarolinastatewide$Registered.Republicans)/northcarolinastatewide$Total.Registered.Voters) *100

#Classify each year as a Republican or Democratic trend with a loop


northcarolinastatewidemarginshift <- rep(NA, length(northcarolinastatewide$Margin)-1)

northcarolinapartisanvoteshift <- rep(NA, length(northcarolinastatewide$Margin)-1)

for(i in 1:length(northcarolinastatewidemarginshift)){
  northcarolinastatewidemarginshift[i]<- (northcarolinastatewide$Margin[i+1]-northcarolinastatewide$Margin[i])
  northcarolinapartisanvoteshift[i] <- ifelse(northcarolinastatewidemarginshift[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationshift <- rep(NA, length(northcarolinastatewide$RegistrationEdge)-1)

northcarolinapartisanregistrationshift <- rep(NA, length(northcarolinastatewide$RegistrationEdge)-1)

for(i in 1:length(northcarolinaregistrationshift)){
  northcarolinaregistrationshift[i] <- (northcarolinastatewide$RegistrationEdge[i+1]-northcarolinastatewide$RegistrationEdge[i])
  northcarolinapartisanregistrationshift[i] <- ifelse(northcarolinaregistrationshift[i]>0,"Democrat", "Republican")
}

#Erect a confusion matrix to discern the pattern 

northcarolinaregistrationandvoting <- table(Registration = northcarolinapartisanregistrationshift, Results = northcarolinapartisanvoteshift )

northcarolinaregistrationandvoting 

registrationaccuracystatewide <- (14/16)*100

#As demonstrated by Colorado, North Carolina's election trend and registration behavior matched in 3 out of 4 instances
#In 2020, North Carolina moved off to the left electorally despite advancements in Republican registration. 

#Download the county level statistics

northcarolinacounty <- read.csv("northcarolinacounty.csv")

#Deduce the electoral margins and voter registrations in each county in every year 


northcarolinacounty$margin2004 <- (northcarolinacounty$Democrat.Margin.2004- northcarolinacounty$Republican.Margin.2004)

northcarolinacounty$margin2008 <- (northcarolinacounty$Democrat.Margin.2008- northcarolinacounty$Republican.Margin.2008)

northcarolinacounty$margin2012 <- (northcarolinacounty$Democrat.Margin.2012- northcarolinacounty$Republican.Margin..2012)

northcarolinacounty$margin2016 <- (northcarolinacounty$Democrat.Margin.2016- northcarolinacounty$Republican.Margin.2016)

northcarolinacounty$margin2020 <- (northcarolinacounty$Democrat.Margin.2020- northcarolinacounty$Republican.Margin.2020)

northcarolinacounty$registration2004 <- (((northcarolinacounty$Democrat.Registration.2004-northcarolinacounty$Republican.Registration.2004)/ northcarolinacounty$Total.Registration2004) *100)

northcarolinacounty$registration2008 <- (((northcarolinacounty$Democrat.Registration.2008-northcarolinacounty$Republican.Registration.2008)/ northcarolinacounty$Total.Registration.2008) *100)

northcarolinacounty$registration2012 <- (((northcarolinacounty$Democrat.Registration..2012-northcarolinacounty$Republican.Registration.2012)/ northcarolinacounty$Total.Registration.2012) *100)

northcarolinacounty$registration2016 <- (((northcarolinacounty$Democrat.Registration.2016-northcarolinacounty$Republican.Registration.2016)/ northcarolinacounty$Total.Registration..2016) *100)

northcarolinacounty$registration2020 <- (((northcarolinacounty$Democrat.Registration.2020-northcarolinacounty$Republican.Registration.2020)/ northcarolinacounty$Total.Registration.2020) *100)

#Design 2 loops to classify and compare the electoral and registration trends for each county 
# and apply to each cycle between 2004 and 2020. Afterwards, assess patterns with a confusion matrix

#2004 to 2008

northcarolinamarginshiftcounty2004to2008 <- rep(NA, length(northcarolinacounty$margin2004))

northcarolinapartisanvoteshift2004to2008 <- rep(NA, length(northcarolinacounty$margin2004))

for(i in 1:length(northcarolinamarginshiftcounty2004to2008)){
  northcarolinamarginshiftcounty2004to2008[i] <- (northcarolinacounty$margin2008[i]-northcarolinacounty$margin2004[i])
  northcarolinapartisanvoteshift2004to2008[i] <- ifelse(northcarolinamarginshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationshiftcounty2004to2008<- rep(NA, length(northcarolinacounty$registration2004))

northcarolinapartisanregistrationshift2004to2008<- rep(NA, length(northcarolinacounty$registration2004))

for(i in 1:length(northcarolinaregistrationshiftcounty2004to2008)){
  northcarolinaregistrationshiftcounty2004to2008[i] <- (northcarolinacounty$registration2008[i]-northcarolinacounty$registration2004[i])
  northcarolinapartisanregistrationshift2004to2008[i] <- ifelse(northcarolinaregistrationshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationandvoting2004to2008<- table(Registration = northcarolinapartisanregistrationshift2004to2008, Results =  northcarolinapartisanvoteshift2004to2008 )

northcarolinaregistrationandvoting2004to2008

northcarolina2004to2008accuracy <- (39/100)*100

northcarolinacorrect <-39

northcarolinawrong <-61

#2008 to 2012

northcarolinamarginshiftcounty2008to2012 <- rep(NA, length(northcarolinacounty$margin2008))

northcarolinapartisanvoteshift2008to2012 <- rep(NA, length(northcarolinacounty$margin2008))

for(i in 1:length(northcarolinamarginshiftcounty2008to2012)){
  northcarolinamarginshiftcounty2008to2012[i] <- (northcarolinacounty$margin2012[i]-northcarolinacounty$margin2008[i])
  northcarolinapartisanvoteshift2008to2012[i] <- ifelse(northcarolinamarginshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationshiftcounty2008to2012<- rep(NA, length(northcarolinacounty$registration2008))

northcarolinapartisanregistrationshift2008to2012<- rep(NA, length(northcarolinacounty$registration2008))

for(i in 1:length(northcarolinaregistrationshiftcounty2008to2012)){
  northcarolinaregistrationshiftcounty2008to2012[i] <- (northcarolinacounty$registration2012[i]-northcarolinacounty$registration2008[i])
  northcarolinapartisanregistrationshift2008to2012[i] <- ifelse(northcarolinaregistrationshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationandvoting2008to2012<- table(Registration = northcarolinapartisanregistrationshift2008to2012, Results =  northcarolinapartisanvoteshift2008to2012 )

northcarolinaregistrationandvoting2008to2012

northcarolina2008to2012accuracy <- (81/100)*100

northcarolinacorrect +81

northcarolinawrong +19

northcarolinacorrect <- 120

northcarolinawrong <- 80

#2012 to 2016

northcarolinamarginshiftcounty2012to2016 <- rep(NA, length(northcarolinacounty$margin2012))

northcarolinapartisanvoteshift2012to2016 <- rep(NA, length(northcarolinacounty$margin2012))

for(i in 1:length(northcarolinamarginshiftcounty2012to2016)){
  northcarolinamarginshiftcounty2012to2016[i] <- (northcarolinacounty$margin2016[i]-northcarolinacounty$margin2012[i])
  northcarolinapartisanvoteshift2012to2016[i] <- ifelse(northcarolinamarginshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationshiftcounty2012to2016<- rep(NA, length(northcarolinacounty$registration2012))

northcarolinapartisanregistrationshift2012to2016<- rep(NA, length(northcarolinacounty$registration2012))

for(i in 1:length(northcarolinaregistrationshiftcounty2012to2016)){
  northcarolinaregistrationshiftcounty2012to2016[i] <- (northcarolinacounty$registration2016[i]-northcarolinacounty$registration2012[i])
  northcarolinapartisanregistrationshift2012to2016[i] <- ifelse(northcarolinaregistrationshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationandvoting2012to2016<- table(Registration = northcarolinapartisanregistrationshift2012to2016, Results =  northcarolinapartisanvoteshift2012to2016 )

northcarolinaregistrationandvoting2012to2016

northcarolina2012to2016accuracy <- (88/100)*100

northcarolinacorrect +88

northcarolinawrong +12

northcarolinacorrect <-208

northcarolinawrong <-92

#2016 to 2020

northcarolinamarginshiftcounty2016to2020 <- rep(NA, length(northcarolinacounty$margin2016))

northcarolinapartisanvoteshift2016to2020 <- rep(NA, length(northcarolinacounty$margin2016))

for(i in 1:length(northcarolinamarginshiftcounty2016to2020)){
  northcarolinamarginshiftcounty2016to2020[i] <- (northcarolinacounty$margin2020[i]-northcarolinacounty$margin2016[i])
  northcarolinapartisanvoteshift2016to2020[i] <- ifelse(northcarolinamarginshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationshiftcounty2016to2020<- rep(NA, length(northcarolinacounty$registration2016))

northcarolinapartisanregistrationshift2016to2020<- rep(NA, length(northcarolinacounty$registration2016))

for(i in 1:length(northcarolinaregistrationshiftcounty2016to2020)){
  northcarolinaregistrationshiftcounty2016to2020[i] <- (northcarolinacounty$registration2020[i]-northcarolinacounty$registration2016[i])
  northcarolinapartisanregistrationshift2016to2020[i] <- ifelse(northcarolinaregistrationshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

northcarolinaregistrationandvoting2016to2020<- table(Registration = northcarolinapartisanregistrationshift2016to2020, Results =  northcarolinapartisanvoteshift2016to2020 )

northcarolinaregistrationandvoting2016to2020

northcarolina2016to2020accuracy <- (58/100)*100

northcarolinacorrect + 58

northcarolinawrong +42 

northcarolinacorrect <-266

northcarolinawrong <- 134

overallcorrect +266

overallwrong +134

overallcorrect <-684

overallwrong <- 300

northcarolinacountylevelaccuracy <- (266/400)

northcarolinacountylevelaccuracy

#At the county level, voter registration by party proves accurate about 66.5% of the time. These figures
#correlate with the trends demonstrated by Arizona's counties

#Compile and load the statewide data from Pennslyvania

pennslyvaniastatewide <- read.csv("pennslyvaniastatewide.csv")

#Assess the shift in margin and registration between each year 

pennslyvaniastatewide$Margin <- (pennslyvaniastatewide$Democrat.Margin....- pennslyvaniastatewide$Republican.Margin...)

pennslyvaniastatewide$RegistrationEdge <- ((pennslyvaniastatewide$Registered.Democrats - pennslyvaniastatewide$Registered.Republicans)/pennslyvaniastatewide$Total.Registered.Voters) *100

#Classify and cross reference the registration and electoral trends through effective execution of a loop 


pennslyvaniastatewidemarginshift <- rep(NA, length(pennslyvaniastatewide$Margin)-1)

pennslyvaniapartisanvoteshift <- rep(NA, length(pennslyvaniastatewide$Margin)-1)

for(i in 1:length(pennslyvaniastatewidemarginshift)){
  pennslyvaniastatewidemarginshift[i]<- (pennslyvaniastatewide$Margin[i+1]-pennslyvaniastatewide$Margin[i])
  pennslyvaniapartisanvoteshift[i] <- ifelse(pennslyvaniastatewidemarginshift[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationshift <- rep(NA, length(pennslyvaniastatewide$RegistrationEdge)-1)

pennslyvaniapartisanregistrationshift <- rep(NA, length(pennslyvaniastatewide$RegistrationEdge)-1)

for(i in 1:length(pennslyvaniaregistrationshift)){
  pennslyvaniaregistrationshift[i] <- (pennslyvaniastatewide$RegistrationEdge[i+1]-pennslyvaniastatewide$RegistrationEdge[i])
  pennslyvaniapartisanregistrationshift[i] <- ifelse(pennslyvaniaregistrationshift[i]>0,"Democrat", "Republican")
}

#To visualize any patterns, construct a confusion matrix 

pennslyvaniaregistrationandvoting <- table(Registration = pennslyvaniapartisanregistrationshift, Results = pennslyvaniapartisanvoteshift )

pennslyvaniaregistrationandvoting 

registrationaccuracystatewide <- (17/20)*100

#Pennslyvania's voter registrations and elections aligned in 3 out of the 4 election cycles. 2020
#broke the correlation between the two metrics

#Upload the county level data entries 

pennslyvaniacounty <- read.csv("pennslyvaniacounty.csv")

#Tabulate the election and registration shifts for each county for all election cycles

pennslyvaniacounty$margin2004 <- (pennslyvaniacounty$Democrat.Margin.2004- pennslyvaniacounty$Republican.Margin.2004)

pennslyvaniacounty$margin2008 <- (pennslyvaniacounty$Democrat.Margin.2008- pennslyvaniacounty$Republican.Margin.2008)

pennslyvaniacounty$margin2012 <- (pennslyvaniacounty$Democrat.Margin.2012- pennslyvaniacounty$Republican.Margin..2012)

pennslyvaniacounty$margin2016 <- (pennslyvaniacounty$Democrat.Margin.2016- pennslyvaniacounty$Republican.Margin.2016)

pennslyvaniacounty$margin2020 <- (pennslyvaniacounty$Democrat.Margin.2020- pennslyvaniacounty$Republican.Margin.2020)

pennslyvaniacounty$registration2004 <- (((pennslyvaniacounty$Democrat.Registration.2004-pennslyvaniacounty$Republican.Registration.2004)/ pennslyvaniacounty$Total.Registration2004) *100)

pennslyvaniacounty$registration2008 <- (((pennslyvaniacounty$Democrat.Registration.2008-pennslyvaniacounty$Republican.Registration.2008)/ pennslyvaniacounty$Total.Registration.2008) *100)

pennslyvaniacounty$registration2012 <- (((pennslyvaniacounty$Democrat.Registration..2012-pennslyvaniacounty$Republican.Registration.2012)/ pennslyvaniacounty$Total.Registration.2012) *100)

pennslyvaniacounty$registration2016 <- (((pennslyvaniacounty$Democrat.Registration.2016-pennslyvaniacounty$Republican.Registration.2016)/ pennslyvaniacounty$Total.Registration..2016) *100)

pennslyvaniacounty$registration2020 <- (((pennslyvaniacounty$Democrat.Registration.2020-pennslyvaniacounty$Republican.Registration.2020)/ pennslyvaniacounty$Total.Registration.2020) *100)

#Sort the counties according to electoral and registration trend in each election cycle through use of 2 loops. 
#Afterwords, formulate a confusion matrix to visualize and assess any patterns.

#2004 to 2008

pennslyvaniamarginshiftcounty2004to2008 <- rep(NA, length(pennslyvaniacounty$margin2004))

pennslyvaniapartisanvoteshift2004to2008 <- rep(NA, length(pennslyvaniacounty$margin2004))

for(i in 1:length(pennslyvaniamarginshiftcounty2004to2008)){
  pennslyvaniamarginshiftcounty2004to2008[i] <- (pennslyvaniacounty$margin2008[i]-pennslyvaniacounty$margin2004[i])
  pennslyvaniapartisanvoteshift2004to2008[i] <- ifelse(pennslyvaniamarginshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationshiftcounty2004to2008<- rep(NA, length(pennslyvaniacounty$registration2004))

pennslyvaniapartisanregistrationshift2004to2008<- rep(NA, length(pennslyvaniacounty$registration2004))

for(i in 1:length(pennslyvaniaregistrationshiftcounty2004to2008)){
  pennslyvaniaregistrationshiftcounty2004to2008[i] <- (pennslyvaniacounty$registration2008[i]-pennslyvaniacounty$registration2004[i])
  pennslyvaniapartisanregistrationshift2004to2008[i] <- ifelse(pennslyvaniaregistrationshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationandvoting2004to2008<- table(Registration = pennslyvaniapartisanregistrationshift2004to2008, Results =  pennslyvaniapartisanvoteshift2004to2008 )

pennslyvaniaregistrationandvoting2004to2008

pennslyvania2004to2008accuracy <- (62/67)*100

pennslyvaniacorrect <- 62

pennslyvaniawrong <- 5

#2008 to 2012

pennslyvaniamarginshiftcounty2008to2012 <- rep(NA, length(pennslyvaniacounty$margin2008))

pennslyvaniapartisanvoteshift2008to2012 <- rep(NA, length(pennslyvaniacounty$margin2008))

for(i in 1:length(pennslyvaniamarginshiftcounty2008to2012)){
  pennslyvaniamarginshiftcounty2008to2012[i] <- (pennslyvaniacounty$margin2012[i]-pennslyvaniacounty$margin2008[i])
  pennslyvaniapartisanvoteshift2008to2012[i] <- ifelse(pennslyvaniamarginshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationshiftcounty2008to2012<- rep(NA, length(pennslyvaniacounty$registration2008))

pennslyvaniapartisanregistrationshift2008to2012<- rep(NA, length(pennslyvaniacounty$registration2008))

for(i in 1:length(pennslyvaniaregistrationshiftcounty2008to2012)){
  pennslyvaniaregistrationshiftcounty2008to2012[i] <- (pennslyvaniacounty$registration2012[i]-pennslyvaniacounty$registration2008[i])
  pennslyvaniapartisanregistrationshift2008to2012[i] <- ifelse(pennslyvaniaregistrationshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationandvoting2008to2012<- table(Registration = pennslyvaniapartisanregistrationshift2008to2012, Results =  pennslyvaniapartisanvoteshift2008to2012 )

pennslyvaniaregistrationandvoting2008to2012

pennslyvania2008to2012accuracy <- (56/67)*100

pennslyvaniacorrect +56

pennslyvaniawrong +11 

pennslyvaniacorrect <- 118

pennslyvaniawrong <- 16

#2012 to 2016

pennslyvaniamarginshiftcounty2012to2016 <- rep(NA, length(pennslyvaniacounty$margin2012))

pennslyvaniapartisanvoteshift2012to2016 <- rep(NA, length(pennslyvaniacounty$margin2012))

for(i in 1:length(pennslyvaniamarginshiftcounty2012to2016)){
  pennslyvaniamarginshiftcounty2012to2016[i] <- (pennslyvaniacounty$margin2016[i]-pennslyvaniacounty$margin2012[i])
  pennslyvaniapartisanvoteshift2012to2016[i] <- ifelse(pennslyvaniamarginshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationshiftcounty2012to2016<- rep(NA, length(pennslyvaniacounty$registration2012))

pennslyvaniapartisanregistrationshift2012to2016<- rep(NA, length(pennslyvaniacounty$registration2012))

for(i in 1:length(pennslyvaniaregistrationshiftcounty2012to2016)){
  pennslyvaniaregistrationshiftcounty2012to2016[i] <- (pennslyvaniacounty$registration2016[i]-pennslyvaniacounty$registration2012[i])
  pennslyvaniapartisanregistrationshift2012to2016[i] <- ifelse(pennslyvaniaregistrationshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationandvoting2012to2016<- table(Registration = pennslyvaniapartisanregistrationshift2012to2016, Results =  pennslyvaniapartisanvoteshift2012to2016 )

pennslyvaniaregistrationandvoting2012to2016

pennslyvania2012to2016accuracy <- (61/67)*100

pennslyvaniacorrect +61

pennslyvaniawrong +6

pennslyvaniacorrect <- 179

pennslyvaniawrong <-22

#2016 to 2020

pennslyvaniamarginshiftcounty2016to2020 <- rep(NA, length(pennslyvaniacounty$margin2016))

pennslyvaniapartisanvoteshift2016to2020 <- rep(NA, length(pennslyvaniacounty$margin2016))

for(i in 1:length(pennslyvaniamarginshiftcounty2016to2020)){
  pennslyvaniamarginshiftcounty2016to2020[i] <- (pennslyvaniacounty$margin2020[i]-pennslyvaniacounty$margin2016[i])
  pennslyvaniapartisanvoteshift2016to2020[i] <- ifelse(pennslyvaniamarginshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationshiftcounty2016to2020<- rep(NA, length(pennslyvaniacounty$registration2016))

pennslyvaniapartisanregistrationshift2016to2020<- rep(NA, length(pennslyvaniacounty$registration2016))

for(i in 1:length(pennslyvaniaregistrationshiftcounty2016to2020)){
  pennslyvaniaregistrationshiftcounty2016to2020[i] <- (pennslyvaniacounty$registration2020[i]-pennslyvaniacounty$registration2016[i])
  pennslyvaniapartisanregistrationshift2016to2020[i] <- ifelse(pennslyvaniaregistrationshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

pennslyvaniaregistrationandvoting2016to2020<- table(Registration = pennslyvaniapartisanregistrationshift2016to2020, Results =  pennslyvaniapartisanvoteshift2016to2020 )

pennslyvaniaregistrationandvoting2016to2020

pennslyvania2016to2020accuracy <- (26/67)*100

pennslyvaniacorrect +26

pennslyvaniawrong +41

pennslyvaniacorrect <- 205

pennslyvaniawrong <-63

overallcorrect +205

overallwrong +63

overallcorrect <-889

overallwrong <-363

pennslyvaniacountylevelaccuracy <- (205/268) 

pennslyvaniacountylevelaccuracy

#Pennsylvania demonstrates a trend similar to Florida. The voter registration trends and election shifts matched in 76.49% of cases. 

#Download the statewide data for the state of Oregon

oregonstatewide <- read.csv("oregonstatewide.csv")

#Total the election margins and registration shifts in each year

oregonstatewide$Margin <- (oregonstatewide$Democrat.Margin....- oregonstatewide$Republican.Margin...)

oregonstatewide$RegistrationEdge <- ((oregonstatewide$Registered.Democrats - oregonstatewide$Registered.Republicans)/oregonstatewide$Total.Registered.Voters) *100

#Crossrefence the voter registration shifts and election results through application of a loop

oregonstatewidemarginshift <- rep(NA, length(oregonstatewide$Margin)-1)

oregonpartisanvoteshift <- rep(NA, length(oregonstatewide$Margin)-1)

for(i in 1:length(pennslyvaniastatewidemarginshift)){
  oregonstatewidemarginshift[i]<- (oregonstatewide$Margin[i+1]-oregonstatewide$Margin[i])
  oregonpartisanvoteshift[i] <- ifelse(oregonstatewidemarginshift[i]>0,"Democrat", "Republican")
}

oregonregistrationshift <- rep(NA, length(oregonstatewide$RegistrationEdge)-1)

oregonpartisanregistrationshift <- rep(NA, length(oregonstatewide$RegistrationEdge)-1)

for(i in 1:length(oregonregistrationshift)){
  oregonregistrationshift[i] <- (oregonstatewide$RegistrationEdge[i+1]-oregonstatewide$RegistrationEdge[i])
  oregonpartisanregistrationshift[i] <- ifelse(oregonregistrationshift[i]>0,"Democrat", "Republican")
}

#Through use of a confusion matrix, visualize and detail any patterns or trends

oregonregistrationandvoting <- table(Registration = oregonpartisanregistrationshift, Results = oregonpartisanvoteshift )

oregonregistrationandvoting 

registrationaccuracystatewide <- (19/24)*100

#Oregon breaks from the other states with regard to voter registration and election consistency. The voter roll only
#forecast the election trend in half of the past 4 election cycles. 

#Download the relevant figures for county level data

oregoncounty <- read.csv("oregoncounty.csv")

#Compute the both the electoral and registration figures for each county for all years featured

oregoncounty$margin2004 <- (oregoncounty$Democrat.Margin.2004- oregoncounty$Republican.Margin.2004)

oregoncounty$margin2008 <- (oregoncounty$Democrat.Margin.2008- oregoncounty$Republican.Margin.2008)

oregoncounty$margin2012 <- (oregoncounty$Democrat.Margin.2012- oregoncounty$Republican.Margin..2012)

oregoncounty$margin2016 <- (oregoncounty$Democrat.Margin.2016- oregoncounty$Republican.Margin.2016)

oregoncounty$margin2020 <- (oregoncounty$Democrat.Margin.2020- oregoncounty$Republican.Margin.2020)

oregoncounty$registration2004 <- (((oregoncounty$Democrat.Registration.2004-oregoncounty$Republican.Registration.2004)/ oregoncounty$Total.Registration2004) *100)

oregoncounty$registration2008 <- (((oregoncounty$Democrat.Registration.2008-oregoncounty$Republican.Registration.2008)/ oregoncounty$Total.Registration.2008) *100)

oregoncounty$registration2012 <- (((oregoncounty$Democrat.Registration..2012-oregoncounty$Republican.Registration.2012)/ oregoncounty$Total.Registration.2012) *100)

oregoncounty$registration2016 <- (((oregoncounty$Democrat.Registration.2016-oregoncounty$Republican.Registration.2016)/ oregoncounty$Total.Registration..2016) *100)

oregoncounty$registration2020 <- (((oregoncounty$Democrat.Registration.2020-oregoncounty$Republican.Registration.2020)/ oregoncounty$Total.Registration.2020) *100)

#By way of 2 loops, classifying each counties electoral and registration shifts in each cycle and compare
#Once the data has been assembled, organize it into a confusion matrix to assess outcomes

#2004 to 2008 

oregonmarginshiftcounty2004to2008 <- rep(NA, length(oregoncounty$margin2004))

oregonpartisanvoteshift2004to2008 <- rep(NA, length(oregoncounty$margin2004))

for(i in 1:length(oregonmarginshiftcounty2004to2008)){
  oregonmarginshiftcounty2004to2008[i] <- (oregoncounty$margin2008[i]-oregoncounty$margin2004[i])
  oregonpartisanvoteshift2004to2008[i] <- ifelse(oregonmarginshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

oregonregistrationshiftcounty2004to2008<- rep(NA, length(oregoncounty$registration2004))

oregonpartisanregistrationshift2004to2008<- rep(NA, length(oregoncounty$registration2004))

for(i in 1:length(oregonregistrationshiftcounty2004to2008)){
  oregonregistrationshiftcounty2004to2008[i] <- (oregoncounty$registration2008[i]-oregoncounty$registration2004[i])
  oregonpartisanregistrationshift2004to2008[i] <- ifelse(oregonregistrationshiftcounty2004to2008[i]>0,"Democrat", "Republican")
}

oregonregistrationandvoting2004to2008<- table(Registration = oregonpartisanregistrationshift2004to2008, Results =  oregonpartisanvoteshift2004to2008 )

oregonregistrationandvoting2004to2008

oregon2004to2008accuracy <- (32/36)*100

oregoncorrect <- 32

oregonwrong <- 4

#2008 to 2012

oregonmarginshiftcounty2008to2012 <- rep(NA, length(oregoncounty$margin2008))

oregonpartisanvoteshift2008to2012 <- rep(NA, length(oregoncounty$margin2008))

for(i in 1:length(oregonmarginshiftcounty2008to2012)){
  oregonmarginshiftcounty2008to2012[i] <- (oregoncounty$margin2012[i]-oregoncounty$margin2008[i])
  oregonpartisanvoteshift2008to2012[i] <- ifelse(oregonmarginshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

oregonregistrationshiftcounty2008to2012<- rep(NA, length(oregoncounty$registration2008))

oregonpartisanregistrationshift2008to2012<- rep(NA, length(oregoncounty$registration2008))

for(i in 1:length(oregonregistrationshiftcounty2008to2012)){
  oregonregistrationshiftcounty2008to2012[i] <- (oregoncounty$registration2012[i]-oregoncounty$registration2008[i])
  oregonpartisanregistrationshift2008to2012[i] <- ifelse(oregonregistrationshiftcounty2008to2012[i]>0,"Democrat", "Republican")
}

oregonregistrationandvoting2008to2012<- table(Registration = oregonpartisanregistrationshift2008to2012, Results =  oregonpartisanvoteshift2008to2012 )

oregonregistrationandvoting2008to2012

oregon2008to2012accuracy <- (36/36)*100

oregoncorrect +36 

oregoncorrect <-68

#2012 to 2016

oregonmarginshiftcounty2012to2016 <- rep(NA, length(oregoncounty$margin2012))

oregonpartisanvoteshift2012to2016 <- rep(NA, length(oregoncounty$margin2012))

for(i in 1:length(oregonmarginshiftcounty2012to2016)){
  oregonmarginshiftcounty2012to2016[i] <- (oregoncounty$margin2016[i]-oregoncounty$margin2012[i])
  oregonpartisanvoteshift2012to2016[i] <- ifelse(oregonmarginshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

oregonregistrationshiftcounty2012to2016<- rep(NA, length(oregoncounty$registration2012))

oregonpartisanregistrationshift2012to2016<- rep(NA, length(oregoncounty$registration2012))

for(i in 1:length(oregonregistrationshiftcounty2012to2016)){
  oregonregistrationshiftcounty2012to2016[i] <- (oregoncounty$registration2016[i]-oregoncounty$registration2012[i])
  oregonpartisanregistrationshift2012to2016[i] <- ifelse(oregonregistrationshiftcounty2012to2016[i]>0,"Democrat", "Republican")
}

oregonregistrationandvoting2012to2016<- table(Registration = oregonpartisanregistrationshift2012to2016, Results =  oregonpartisanvoteshift2012to2016 )

oregonregistrationandvoting2012to2016

oregon2012to2016accuracy <- (26/36)*100

oregoncorrect +26

oregonwrong +10

oregoncorrect <-94

oregonwrong <-14

#2016 to 2020

oregonmarginshiftcounty2016to2020 <- rep(NA, length(oregoncounty$margin2016))

oregonpartisanvoteshift2016to2020 <- rep(NA, length(oregoncounty$margin2016))

for(i in 1:length(oregonmarginshiftcounty2016to2020)){
  oregonmarginshiftcounty2016to2020[i] <- (oregoncounty$margin2020[i]-oregoncounty$margin2016[i])
  oregonpartisanvoteshift2016to2020[i] <- ifelse(oregonmarginshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

oregonregistrationshiftcounty2016to2020<- rep(NA, length(oregoncounty$registration2016))

oregonpartisanregistrationshift2016to2020<- rep(NA, length(oregoncounty$registration2016))

for(i in 1:length(oregonregistrationshiftcounty2016to2020)){
  oregonregistrationshiftcounty2016to2020[i] <- (oregoncounty$registration2020[i]-oregoncounty$registration2016[i])
  oregonpartisanregistrationshift2016to2020[i] <- ifelse(oregonregistrationshiftcounty2016to2020[i]>0,"Democrat", "Republican")
}

oregonregistrationandvoting2016to2020<- table(Registration = oregonpartisanregistrationshift2016to2020, Results =  oregonpartisanvoteshift2016to2020 )

oregonregistrationandvoting2016to2020

oregon2016to2020accuracy <- (13/36)*100

oregoncorrect+13

oregonwrong +23

oregoncorrect <- 107

oregonwrong <-37

overallcorrect +107

overallwrong +37

overallcorrect <-996

overallwrong <-400

oregoncountylevelaccuracy <- (107/144)

oregoncountylevelaccuracy

#Oregon's county level registration trends tracked with election results in 74.30% of cases. The trend remained
#perfect through 2016 before experiencing a sudden deviation in 2020. Overall, the states behavior bears
#similarities to Pennsylvania and Florida. 

overallcountylevelaccuracy <-(996/1396)*100

overallcountylevelaccuracy

registrationaccuracystatewide 

#In totality, the counties in these 6 states adhered to the voter registration trends in 71.35% of cases.
#At the state level, the predictor proved slightly more reliable. The state registration shift tracked
#with electoral outcomes at a slightly higher rate, anticipating 79.17% of iterations. 

#Separate the states with semi-closed and closed primaries and observe differences

semiclosedprimarystatewide <- (14/16)*100

semiclosedprimarystatewide

closedprimarystatewide <- (5/8)*100

closedprimarystatewide

semiclosedprimarycounty <- (653/939)*100

semiclosedprimarycounty

closedprimarycounty <- (312/412)*100

closedprimarycounty

#Commission a bar plot and scatter plot to visualize the data

eachstateclosedandsemiclosedprimarycountylevelaccuracy <- c(semiclosedprimarycounty, closedprimarycounty )

barplot(eachstateclosedandsemiclosedprimarycountylevelaccuracy, 
        ylim= c(0,100),
        names= c( "Semiclosed", "Closed"),
        col= c("red2", "darkslateblue"),
        main = "Graph 1: The Proportion of Counties where Voter Registration \n\ and Election Results Correlated by Primary Type",
        xlab= "Primary Type",
        ylab= "Proportion Correct",
)

all2004to2008 <- c(florida2004to2008accuracy,arizona2004to2008accuracy,northcarolina2004to2008accuracy,
                   pennslyvania2004to2008accuracy,oregon2004to2008accuracy, colorado2004to2008accuracy)

all2008to2012 <- c(florida2008to2012accuracy,arizona2008to2012accuracy,northcarolina2008to2012accuracy,
                   pennslyvania2008to2012accuracy,oregon2008to2012accuracy, colorado2008to2012accuracy)

all2012to2016 <- c(florida2012to2016accuracy,arizona2012to2016accuracy,northcarolina2012to2016accuracy,
                   pennslyvania2012to2016accuracy,oregon2012to2016accuracy, colorado2012to2016accuracy)

all2016to2020 <- c(florida2016to2020accuracy,arizona2016to2020accuracy,northcarolina2016to2020accuracy,
                   pennslyvania2016to2020accuracy,oregon2016to2020accuracy, colorado2016to2020accuracy)

floridaall <-c(florida2004to2008accuracy,florida2008to2012accuracy,florida2012to2016accuracy,
               florida2016to2020accuracy)

arizonaall <- c(arizona2004to2008accuracy, arizona2008to2012accuracy, arizona2012to2016accuracy,
                arizona2016to2020accuracy)

northcarolinaall <-c(northcarolina2004to2008accuracy,northcarolina2008to2012accuracy,
                     northcarolina2012to2016accuracy,northcarolina2016to2020accuracy)

pennslyvaniaall <- c(pennslyvania2004to2008accuracy,pennslyvania2008to2012accuracy,
                     pennslyvania2012to2016accuracy,pennslyvania2016to2020accuracy)

oregonall <- c(oregon2004to2008accuracy,oregon2008to2012accuracy,
               oregon2012to2016accuracy,oregon2016to2020accuracy)

coloradoall <- c(colorado2004to2008accuracy,colorado2008to2012accuracy,
                 colorado2012to2016accuracy,colorado2016to2020accuracy)

years <- c(2008,2008,2008,2008,2008,2008, 2012,2012,2012,2012,2012,2012, 2016, 2016, 2016, 2016, 2016, 2016, 2020,2020,2020,2020,2020,2020)


registrationfigures <- c(all2004to2008, all2008to2012, all2012to2016, all2016to2020)

plot(x=years,
     y= registrationfigures,
     main= "County Level Voter Registration Accuracy in each Election Cycle",
     xlab= "Election Year",
     ylab= "Proportion Correct",
     col= c("red2","orange2","yellow1","black",  "lightblue", "darkslateblue","red2","orange2","yellow1","black",  "lightblue", "darkslateblue",
            "red2","orange2","yellow1","black",  "lightblue", "darkslateblue","red2","orange2","yellow1","black",  "lightblue", "darkslateblue"),
     )
lines(x=c(2008,2012,2016,2020), y=floridaall,
      col="red2",lwd=3)
lines(x=c(2008,2012,2016,2020), y=arizonaall, 
      col="orange2",lwd=3)
lines(x=c(2008,2012,2016,2020), y=northcarolinaall, 
      col="yellow1",lwd=3)
lines(x=c(2008,2012,2016,2020), y=pennslyvaniaall, 
      col="black",lwd=3)
lines(x=c(2008,2012,2016,2020), y=oregonall, 
      col="lightblue",lwd=3)
lines(x=c(2008,2012,2016,2020), y=coloradoall, 
      col="darkslateblue",lwd=3)

legend("bottom", box.lty=0, col=c("red2","orange2","yellow1","black",  "lightblue", "darkslateblue"),
       legend= c("Florida", "Arizona","North Carolina", "Pennslyvania", "Oregon", "Colorado"), lwd=3,
       cex= 0.7)

coloradoall

oregonall 



