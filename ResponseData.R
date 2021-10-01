source("InstallPackages.R")
source("ReadData.R")
source("ReadCOVIDVaccine.R")

# File exploring and testing models to incorporate government response data.

# Download government response data and format date.
responses <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
responses$Date <- gsub("(.{6})(.*)","\\1/\\2",responses$Date)
responses$Date <- gsub("(.{4})(.*)","\\1/\\2",responses$Date)
responses$Date <- ymd(responses$Date)

# A lot of the names don't match, so this is how I manually compared lists.
countrylist2 <- unique(responses$CountryName)
setdiff(countrylist,countrylist2)
setdiff(unique(testdata$Country_Region),countrylist2)

# 
testdata <- read.csv("Test/testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date)
testdata$Country_Region <- as.character(testdata$Country_Region)
testdata[testdata$Country_Region=="Congo (Brazzaville)",]$Country_Region <- "Republic of Congo"
testdata[testdata$Country_Region=="Congo (Kinshasa)",]$Country_Region <- "Democratic Republic of the Congo"
testdata[testdata$Country_Region=="Cote d'Ivoire",]$Country_Region <- "Ivory Coast"
testdata[testdata$Country_Region=="Eswatini",]$Country_Region <- "Swaziland"
testdata[testdata$Country_Region=="Guinea-Bissau",]$Country_Region <- "Guinea Bissau"
testdata[testdata$Country_Region=="Tanzania",]$Country_Region <- "United Republic of Tanzania"


popdata <- read.csv("Test/popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="Congo",]$Location <- "Republic of Congo"
popdata[popdata$Location=="Côte d'Ivoire",]$Location <- "Ivory Coast"

# Join tables together, calculate for adjusted metrics.
testdata <- left_join(testdata,popdata,by=c("Country_Region"="Location"))
testdata <- testdata %>% group_by(Country_Region) %>% 
  mutate(Active = Confirmed - Recovered - Deaths,
         `New Active` = Active - lag(Active, default=0),
         `Case Fatalities` = Deaths/Confirmed,
         `Deaths per 100k pop` = Deaths/PopTotal*100000,
         `Cases per 100k pop` = Confirmed/PopTotal*100000,
         `Active per 100k pop` = Active/PopTotal*100000,
         `Recoveries per 100k pop` = Recovered/PopTotal*100000,
         `New Cases` = Confirmed-lag(Confirmed,default=0),
         `New Deaths` = Deaths-lag(Deaths,default=0),
         `New Recoveries` = Recovered-lag(Recovered,default=0),
         `New Deaths per 100k pop` = `New Deaths`/PopTotal*100000,
         `New Cases per 100k pop` = `New Cases`/PopTotal*100000,
         `log Cases` = log(Confirmed),
         `log Active` = log(Confirmed),
         `log Recovered` = log(Confirmed),
         `log Deaths` = log(Deaths),
         Exposure = Date - first(Date) + 1) %>% ungroup()

# Correct for a few country names.
responses[responses$CountryName=="Cote d'Ivoire",]$CountryName = "Ivory Coast"
responses[responses$CountryName=="Democratic Republic of Congo",]$CountryName = "Democratic Republic of the Congo"
responses[responses$CountryName=="Eswatini",]$CountryName = "Swaziland"
responses[responses$CountryName=="Tanzania",]$CountryName = "United Republic of Tanzania"
responses[responses$CountryName=="Congo",]$CountryName = "Republic of Congo"

# Subset countries from Africa
responsedata <- subset(responses, responses$CountryName %in% unique(testdata$Country_Region))

# Save the data to read it in later.
write.csv(responsedata,"Test/responsedata.csv")

#Join COVID data and response data for modelling.
joinedData <- left_join(testdata,responsedata,by=c("Country_Region" = "CountryName","Date"))

# Compare and fix countries
setdiff(unique(joinedData$Country_Region),unique(covacc$location))

covacc[covacc$location=="Cote d'Ivoire",]$location = "Ivory Coast"
covacc[covacc$location=="Eswatini",]$location = "Swaziland"
covacc[covacc$location=="Cape Verde",]$location = "Cabo Verde"
covacc$date = ymd(covacc$date) # Format date as date

joinedData <- left_join(joinedData,covacc,by=c("Country_Region" = "location","Date"="date"))

# Subset columns for model. Can be adjusted.
modeldata <- joinedData %>% 
  select(c(1:24,seq(29,43,2),44,46:48,49,51:55,57,59,64,68,70,72,74))

# Save data for easier use.
write.csv(modeldata,"Test/responsemodeldata.csv")

