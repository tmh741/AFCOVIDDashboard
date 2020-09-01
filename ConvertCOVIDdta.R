library(tidyverse)
library(haven)
library(AER)
library(lubridate)
library(GGally)
library(lme4)
library(rstanarm)
library(bayesplot)


modeldata <- read_dta("covid_data.dta")

filterdata <- modeldata %>% select(countryorarea, region,lnrchange,lncaseload_lastobs,lnexpo,
                                   lnsdi,lnurban,lnp70p,lnhhsn,lnihr2018,lnsqualty,lnasthma,lnhiv,lntraffic)
write.csv(filterdata,"covid_data.csv",row.names=F)

##covid_data.dta is a large file that includes the original countries, all the UN data initially considered,
# data transformations, and much more. The file I use uses two things: countries, and log transformed 
# versions of finally selected data from the UN Database.
# To lower the size for convenience, I made covid_data.csv include only the rows used in the code, as listed above.

# countryorarea and region are the Country Name and Region.

# lnrchange is a value calculated based on the log one-week change of COVID between two weeks. It's included mostly
# to be used as a reference.
# lnexpo is the ln of the number of days since the first recorded COVID case in the country.
# lncaseload_lastobs was the most recent COVID case when the dta file was made.
# lnsdi - I think this is "state disability insurance" but I'm not sure and will check!
# lnurban is the log of a number representing how much of the country has been urbanized.
# lnp70p - I don't know exactly what this is, but I think this has to do with internet coverage!
# lnhhsn - I think this has to do with health and human services, but I'm not quite sure.
# lnihr2018 is standardized and based on how they applied international health regulations.
# lnsqualty is the health service quality, standardized and on the log scale.
# lnasthma is the log of the standardized value of asthma counts.
# lnhiv is the log of the standardized value of hiv counts
# lntraffic is the log of a standardized valeu constructed to represent how active flights are in the country.

