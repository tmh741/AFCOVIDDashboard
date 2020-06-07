library(countrycode)
library(abind)

#Create List of Countries
initlist <- subset(codelist[,5],codelist$continent=="Africa")
countrylist <- c(initlist,"Congo (Brazzaville)","Congo (Kinshasa)",
                 "Sao Tome and Principe","Cote d'Ivoire","Eswatini")

## Read in everything from Jan 22 to March 21
startdate <- as.Date("2020-01-22")
enddate <- as.Date("2020-03-21")
date <- as.Date(startdate:enddate,origin="1970-01-01")
date <- strftime(date,"%m-%d-%Y")
url <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", 
             date, ".csv",sep="")

nfiles <- length(url)
resultlist <- vector("list",length=nfiles)
for (i in 1:nfiles){
  resultlist[[i]] <- read.csv(url[i])[,c("Country.Region","Confirmed","Deaths","Recovered")]
  resultlist[[i]]$Date <- date[i]
}
aggregate2 <- abind(resultlist,along=1,force.array=F)
aggregate2 <- subset(aggregate2, aggregate2$Country.Region %in% countrylist)
colnames(aggregate2)[1] <- "Country_Region"

#The first recorded date is February 14th. You can use this for convenience.



## Read in everything from March 22 to June 03
startdate <- as.Date("2020-03-22")
enddate <- as.Date("2020-06-05")
date <- as.Date(startdate:enddate,origin="1970-01-01")
date <- strftime(date,"%m-%d-%Y")
url <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", 
             date, ".csv",sep="")

nfiles <- length(url)
resultlist <- vector("list",length=nfiles)
for (i in 1:nfiles){
  resultlist[[i]] <- read.csv(url[i])[,c("Country_Region","Confirmed","Deaths","Recovered")]
  resultlist[[i]] <- subset(resultlist[[i]], resultlist[[i]]$Country_Region %in% countrylist)
  resultlist[[i]]$Date <- date[i]
}
aggregate <- abind(resultlist,along=1,force.array=F)

alldata <- rbind(aggregate2,aggregate)
includedcountries <- unique(alldata$Country_Region)

write.csv(alldata, "DashBoardProto/testfile.csv")

