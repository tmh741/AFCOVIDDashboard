library(tidyverse)
library(lubridate)
library(magrittr)
library(maptools)
library(maps)
library(gridExtra)
library(grid)
require(rgeos)
library(shinydashboard)
library(rstanarm)
require(scales)
library(plotly)
library(sf)
library(leaflet)

af <- st_read("africa/AfricanCountires.shp")
af$COUNTRY <- as.character(af$COUNTRY)
af[af$COUNTRY=="Congo",]$COUNTRY <- "Congo (Brazzaville)"
af[af$COUNTRY=="Côte d'Ivoire",]$COUNTRY <- "Cote d'Ivoire"
af[af$COUNTRY=="Congo DRC",]$COUNTRY <- "Congo (Kinshasa)"
af[af$COUNTRY=="Swaziland",]$COUNTRY <- "Eswatini"

countries <- unique(af$COUNTRY)
setdiff(countries,unique(testdata$Country_Region))
setdiff(unique(testdata$Country_Region),countries)

testdata <- read.csv("testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date)

current <- testdata %>% filter(Date==max(testdata$Date))

af <- left_join(af, current,by=c("COUNTRY" = "Country_Region")) %>% na.omit()

pal <- colorBin("Reds",domain=af$Confirmed) ## Color
pal <- colorBin("Reds",domain=testdata[,"Confirmed"]) ## Color



pal1 <- colorBin("Reds",domain=af$Deaths) ## Color


leaflet(af) %>%
  setView(20,10,2) %>%
  addProviderTiles("MapBox",options=providerTileOptions(
    id="mapbox.light",
    accessToken=Sys.getenv('MapBox_Access_Token')
  )) %>% addPolygons(
    fillColor = ~pal(Confirmed), ### Variable
    weight=1,
    opacity=1,
    color="black",
    dashArray = "",
    fillOpacity=0.7,
    highlight = highlightOptions(
      weight=5,
      color="#666",
      dashArray="",
      fillOpacity=0.7,
      bringToFront=T),
    label= ~as.character(paste0(COUNTRY, ": ", format(Confirmed, big.mark=",",scientific=F,trim=T), " cases",sep=" ")),
    labelOptions = labelOptions(
      style=list("font-weight"="normal",padding="3px 8px"),
      textsize="15px",
      direction="auto")
    ) %>% 
  addLegend(pal = pal, values = ~Confirmed, opacity=0.7,title=NULL,position="bottomright")


