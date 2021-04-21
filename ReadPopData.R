library(tidyverse)
library(magrittr)

#Writes Population Data as a csv to decrease run time of app.

# PopTotal is the main estimate I'm interested in for this app! So I'm just filtering out the other population estimates.
# I will also note, the different estimates are a little bit strange. 
# For some of the bigger groups, these numbers do have different values for the same year, but for a lot of the countries, they don't.
# Since this app is only focusing on the countries, I used distinct() to isolate any possible duplicates for the same year, and there were none. 
# So that's a thing!

# In the end, the three columns are: 
# Location (Country/Area of focus)
# Time, or year
# PopTotal, the population estimate. In the original file it's in thousands of people, so for one file I adjusted for this.


population <- read.csv("WPP2019_TotalPopulationBySex.csv") %>% 
  select(Location,Time,PopTotal) %>% 
  distinct()
population = population %>% mutate(PopTotal=PopTotal*1000) %>% filter(Time>=1960&Time<=2019)
write.csv(population,file="WorldPopEstimates.csv",row.names=F)


popdata = population %>% filter(Time=="2020") %>% select(Location,PopTotal)
write.csv(popdata ,file="popdata.csv",row.names = F)