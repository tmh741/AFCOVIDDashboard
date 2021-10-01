## Set up model files for display and analysis.

source("InstallPackages.R")

# Read in data and calculate daily metrics.
testdata <- read.csv("ModelData/testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date) #Ensures dates are dates and not characters.
testdata <- testdata %>% group_by(Country_Region) %>% 
  arrange(Date) %>% # Sorts data.
  mutate(`New Cases` = Confirmed - lag(Confirmed, default=0),# Calculates metrics.
         `New Deaths` = Deaths - lag(Deaths, default = 0),
         `New Recoveries` = Recovered - lag(Recovered, default=0),
         DaysSince = Date - ymd("2020-03-31"),
         Exposure = Date - first(Date) + 1) %>%
  ungroup()

#Filter the data to remove repeated imputations on countries.
#Select variables, calculate the means for each one by country and by region (to reduce to one value)
filterdata <- read.csv("ModelData/covid_data.csv")

filterdata <- filterdata %>% group_by(countryorarea, region) %>% 
  summarize(lnrchange = mean(lnrchange, na.rm=T),
            lncaseload_lastobs = mean(lncaseload_lastobs, na.rm=T),
            lnexpo = mean(lnexpo, na.rm=T),
            lnsdi = mean(lnsdi, na.rm=T),
            lnurban = mean(lnurban, na.rm=T),
            lnp70p = mean(lnp70p, na.rm=T),
            lnhhsn = mean(lnhhsn, na.rm=T),
            lnihr2018 = mean(lnihr2018, na.rm=T),
            lnsqualty = mean(lnsqualty, na.rm=T),
            lnhiv = mean(lnhiv, na.rm=T),
            lnasthma = mean(lnasthma, na.rm=T),
            lntraffic = mean(lntraffic, na.rm=T),
            nrows = n())
filterdata[is.na(filterdata)] <- NA # Take out any weird text NA's.

## Model Files
# The next section sets up data for the model files.

filteradj <- filterdata #Made a new df in case the process went wonky. Can probably be adjusted.

# Adjust country names in order to link COVID data and country data.
filteradj[filteradj$countryorarea == "Cabo Verde",]$countryorarea <- "Cape Verde"
filteradj[filteradj$countryorarea == "Congo",]$countryorarea <- "Congo (Brazzaville)"
filteradj[filteradj$countryorarea == "Côte d'Ivoire",]$countryorarea <- "Cote d'Ivoire"
filteradj[filteradj$countryorarea == "Dem. Republic of the Congo",]$countryorarea <- "Congo (Kinshasa)"
filteradj[filteradj$countryorarea == "Swaziland",]$countryorarea <- "Eswatini"
filteradj[filteradj$countryorarea == "United Republic of Tanzania",]$countryorarea <- "Tanzania"

# Read in population and adjust countries.
popdata <- read.csv("Test/popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="United Republic of Tanzania", ]$Location <- "Tanzania"

# Join all datasets.
lmedata <- left_join(testdata,filteradj[,-c(2:5)], by = c("Country_Region" = "countryorarea")) %>%
  left_join(popdata,by=c("Country_Region" = "Location")) %>% 
  mutate(ConfirmedScaled = Confirmed/PopTotal)

# Set up blank forecasting dataframe to set up model predictions.
#  For now, we use 21 days (3 weeks) of training data. Can be adjusted.
# Used this number because we wanted data to train the model, but didn't want to capture effects too far into the past.
# Since COVID-19 and its cases are dynamic, going too far back would capture patterns that may not apply in the future.

forecastdatainit <- lmedata %>% group_by(Country_Region) %>% 
  mutate(LastExposure = max(Exposure), LastDate = max(Date)) %>%
  filter(Exposure <= 21) %>% 
  arrange(by = Country_Region) %>% 
  mutate(Date = LastDate + Exposure, Exposure = Exposure + LastExposure)

# Reduce the number of columns and save it as a .csv.
forecastdatainit <- forecastdatainit %>% 
  select(Country_Region, Exposure, Date, Region, LastExposure, LastDate,
         lnsdi, lnurban, lnp70p, lnhhsn, lnihr2018, lnsqualty) %>% 
  distinct()
write.csv(forecastdatainit,"ModelData/forecaststart.csv")

# Set up different test data frames for assessing model performance (May remove)
# lmetestdata <- lmedata %>% filter(`New Cases` > 0) #Remove days with no new cases - each data point is a new COVID recording.
# lmetestdata2 <- lmetestdata %>% filter(Country_Region != "Mayotte" & Country_Region != "Cape Verde") #Removed these countries due to their weird nature.
lmetestdata3 <- lmetestdata2 %>% group_by(Country_Region) %>% top_n(21, Exposure) #Take the 21 most recent days.

# Save test data.
write.csv(lmetestdata3,"ModelData/modeldata.csv")
