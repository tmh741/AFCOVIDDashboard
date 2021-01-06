library(tidyverse)
library(lubridate)
library(magrittr)
library(maptools)
library(maps)
library(rgdal)

testdata <- read.csv("testfile.csv")[,-1]
countries <- unique(testdata$Country_Region)
today <- testdata %>% filter(Date=="06-03-2020")

data("wrld_simpl")
afr=wrld_simpl[wrld_simpl$REGION==2,]
afr@data$id <- afr@data$NAME
afr <- fortify(afr,region="id")
afr[afr$id=="Swaziland",]$id <- "Eswatini"
afr[afr$id=="Congo",]$id <- "Congo (Brazzaville)"
afr[afr$id=="Democratic Republic of the Congo",]$id <- "Congo (Kinshasa)"
afr[afr$id=="United Republic of Tanzania",]$id <- "Tanzania"
afr[afr$id=="Libyan Arab Jamahiriya",]$id <- "Libya"
afr <- left_join(afr, today, by=c("id"="Country_Region"))


ggplot() + geom_map(data=afr,map=afr,aes(map_id=id,x=long,y=lat,fill=Confirmed))


