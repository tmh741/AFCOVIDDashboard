source("InstallPackages.R")

# Read in vaccine data.
covacc =read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
covacc <- covacc %>% select(location,date,total_vaccinations) %>% na.omit()
covacc$date = ymd(covacc$date)
