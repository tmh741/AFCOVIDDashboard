library(countrycode)
library(abind)

#Create List of Countries to filter out.
initlist <- subset(codelist[,5],codelist$continent=="Africa")
#Adjust to include countries based on how they're listed in the COVID data.
countrylist <- c(initlist,"Congo (Brazzaville)","Congo (Kinshasa)",
                 "Sao Tome and Principe","Cote d'Ivoire","Eswatini")

## Read in everything from Jan 22 to March 21.
startdate <- as.Date("2020-01-22")
enddate <- as.Date("2020-03-21")
date <- as.Date(startdate:enddate,origin="1970-01-01")
date <- strftime(date,"%m-%d-%Y")
url <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", 
             date, ".csv",sep="")

#Basic idea: Read from URL, attaching the date as a CSV.
nfiles <- length(url)
resultlist <- vector("list",length=nfiles)
for (i in 1:nfiles){
  resultlist[[i]] <- read.csv(url[i])[,c("Country.Region","Confirmed","Deaths","Recovered")]
  resultlist[[i]]$Date <- date[i]
}
#Aggregate lists into one data frame using abind.
aggregate2 <- abind(resultlist,along=1,force.array=F)
aggregate2 <- subset(aggregate2, aggregate2$Country.Region %in% countrylist)
colnames(aggregate2)[1] <- "Country_Region"



## Read in everything from March 22 to June 03. Process is the same as above.
startdate <- as.Date("2020-03-22")
enddate <- as.Date(Sys.Date()-1)
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

#Combine data and save it.
alldata <- rbind(aggregate2,aggregate)
write.csv(alldata, "Test/testfile.csv")

#Write Population Data as a smaller .csv to decrease run time of app.
popdata <- read.csv("WPP2019_TotalPopulationBySex.csv") %>% filter(Time==2020) %>% 
  select(Location,PopTotal) %>% distinct()
write.csv(popdata, file="Test/popdata.csv",row.names = F)

