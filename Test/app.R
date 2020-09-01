library(shiny)
library(tidyverse)
library(lubridate)
library(magrittr)
library(shinydashboard)
library(scales)
library(highcharter)
library(jsonlite)

#Read model data and set colors.
modelproj <- read.csv("modelprojections.csv") %>% mutate(Date = ymd(Date))
modeldate <- modelproj[modelproj$DaysSince==0,]$Date[1]

#Read the COVID data and clean it.
testdata <- read.csv("testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date)
testdata$Country_Region <- as.character(testdata$Country_Region)
testdata[testdata$Country_Region=="Congo (Brazzaville)",]$Country_Region <- "Republic of Congo"
testdata[testdata$Country_Region=="Congo (Kinshasa)",]$Country_Region <- "Democratic Republic of the Congo"
testdata[testdata$Country_Region=="Cote d'Ivoire",]$Country_Region <- "Ivory Coast"
testdata[testdata$Country_Region=="Eswatini",]$Country_Region <- "Swaziland"
testdata[testdata$Country_Region=="Guinea-Bissau",]$Country_Region <- "Guinea Bissau"
testdata[testdata$Country_Region=="Tanzania",]$Country_Region <- "United Republic of Tanzania"


#Read Population Data and clean country names.
popdata <- read.csv("popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="Congo",]$Location <- "Republic of Congo"
popdata[popdata$Location=="Côte d'Ivoire",]$Location <- "Ivory Coast"


# Read in forecast data.
forecast <- read.csv("forecastlmenoregion.csv")
forecast$Date <- ymd(forecast$Date)
forecast$Country <- as.character(forecast$Country)
forecast[forecast$Country=="Congo (Brazzaville)",]$Country <- "Republic of Congo"
forecast[forecast$Country=="Congo (Kinshasa)",]$Country <- "Democratic Republic of the Congo"
forecast[forecast$Country=="Cote d'Ivoire",]$Country <- "Ivory Coast"
forecast[forecast$Country=="Eswatini",]$Country <- "Swaziland"
forecast[forecast$Country=="Guinea-Bissau",]$Country <- "Guinea Bissau"
forecast[forecast$Country=="Tanzania",]$Country <- "United Republic of Tanzania"

forecastw <- read.csv("forecastlmewithregion.csv")
forecastw$Date <- ymd(forecastw$Date)
forecastw$Country <- as.character(forecastw$Country)
forecastw[forecastw$Country=="Congo (Brazzaville)",]$Country <- "Republic of Congo"
forecastw[forecastw$Country=="Congo (Kinshasa)",]$Country <- "Democratic Republic of the Congo"
forecastw[forecastw$Country=="Cote d'Ivoire",]$Country <- "Ivory Coast"
forecastw[forecastw$Country=="Eswatini",]$Country <- "Swaziland"
forecastw[forecastw$Country=="Guinea-Bissau",]$Country <- "Guinea Bissau"
forecastw[forecastw$Country=="Tanzania",]$Country <- "United Republic of Tanzania"

# Left join population data and test data. Calculated values of interest to be displayed.
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
#Filter out the most recent date.
current <- testdata %>% filter(Date==max(testdata$Date)) %>%
  mutate(
    New.Cases = `New Cases`,
    New.Active = `New Active`,
    New.Recoveries = `New Recoveries`,
    New.Deaths = `New Deaths`
  )

#Get country list.
countrylist <- sort(unique(testdata$Country_Region))

#Set colors for graphics.
modelcolors <- c("Projection Range" = "skyblue", "Model Projection" = "black", "Confirmed Cases" = "darkorchid")

#Get list of cumulative and daily variables.
#This lets me create menus for daily and cumulative graphics separately more easily.
tot = colnames(testdata)[c(2:4,7:9)]
daily = colnames(testdata)[c(10:14)]

#Get map of Africa.

map <- jsonlite::fromJSON("https://code.highcharts.com/mapdata/custom/africa.geo.json",simplifyVector=F)


#Get country-level data from UN
displaydata <- read.csv("filtereddata.csv")
displaydata$countryorarea <- as.character(displaydata$countryorarea)
displaydata[displaydata$countryorarea=="Congo (Brazzaville)",]$countryorarea <- "Republic of Congo"
displaydata[displaydata$countryorarea=="Congo (Kinshasa)",]$countryorarea <- "Democratic Republic of the Congo"
displaydata[displaydata$countryorarea=="Cote d'Ivoire",]$countryorarea <- "Ivory Coast"
displaydata[displaydata$countryorarea=="Eswatini",]$countryorarea <- "Swaziland"
displaydata[displaydata$countryorarea=="Guinea-Bissau",]$countryorarea <- "Guinea Bissau"
displaydata[displaydata$countryorarea=="Tanzania",]$countryorarea <- "United Republic of Tanzania"


## Cumulate data for all of Africa and use pivot_longer to put it in graphic-ready form.
alldata <- testdata %>% group_by(Date) %>% 
  summarize(Confirmed = sum(Confirmed),
            Recovered = sum(Recovered),
            Deaths = sum(Deaths)) %>%
  mutate(`New Cases` = Confirmed - lag(Confirmed,default=0),
         `New Recoveries` = Recovered - lag(Recovered,default=0),
         `New Deaths` = Deaths - lag(Deaths,default=0),
         Active = Confirmed - Recovered - Deaths,
         `New Active` = Active - lag(Active,default=0),
         `log Cases` = log(Confirmed),
         `log Active` = log(Active),
         `log Recovered` = log(Recovered),
         `log Deaths` = log(Deaths),
         New.Cases = `New Cases`,
         New.Active = `New Active`,
         New.Recoveries = `New Recoveries`,
         New.Deaths = `New Deaths`)

plotColors <- data.frame(Var = c("Confirmed","Active","Recovered","Deaths",
                                 "New.Cases","New.Active","New.Recoveries","New.Deaths"),
                         maxCol = c("#645cac", "#449c6c", "#5dbcd2","#dc4c3c",
                                    "#645cac", "#449c6c", "#5dbcd2","#dc4c3c"),
                         minCol = c("#cdcaed","#b9edd1","#c9f5ff","#f7d2cd",
                                    "#cdcaed","#b9edd1","#c9f5ff","#f7d2cd"))

## UI

ui <- dashboardPage(skin = "green", # Set color and modify the header.
                    dashboardHeader(title = "COVID-19 in Africa - Data Visualizer",
                                    titleWidth = 350),
                    
                    #Modify the sidebar menu.
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("COVID Exploration", tabName = "covid", icon = icon("chart-bar"),
                                 menuSubItem("COVID Africa Summary",tabName="summary"),
                                 menuSubItem("Country Analysis",tabName="country"),
                                 menuSubItem("Dynamic Map Explorer",tabName="mapexp")),
                        # menuItem("Model Projections", tabName = "bigtab", icon = icon("chart-line"),
                        #          # menuSubItem("IV Regression Projection",tabName="model"),
                        #          menuSubItem("LME Projection", tabName = "lmer"),
                        # ),
                        menuItem("COVID Forecasts",tabName="lmer",icon=icon("chart-line")),
                        menuItem("Data Table", tabName = "data", icon = icon("table")),
                        menuItem("About", tabName = "about", icon = icon("question-circle")),
                        menuItem("Contact Us",
                                 href="https://sites.bu.edu/covid-19-in-africa/partner-with-us/",
                                 icon = icon("address-card")),
                        menuItem(" GitHub",
                                 href="https://github.com/tmh741/AFCOVIDDashboard/tree/master/Test",
                                 icon = icon("github-square"))
                        
                      )
                    ),
                    
                    ## This is the body. Content is separated into different tags.
                    dashboardBody(
                      
                      ## First page - COVID Summary for all of Africa.  
                      tabItems(
                        # COVID
                        tabItem(tabName = "summary",
                                
                                #Row 1 & 2 - summary content.
                                
                                fluidRow(HTML("<h1><center>Summary</center></h1>")),
                                
                                fluidRow(
                                  valueBox(color="purple",
                                           value=tags$p(paste(sum(current$Confirmed), " (+", sum(current$`New Cases`), ")",sep=""),style="font-size:50%;"),
                                           tags$p("Confirmed",style="font-size:100%;")),
                                  valueBox(color="olive",
                                           value=tags$p(paste(sum(current$Active), " (+", sum(current$`New Active`), ")",sep=""),style="font-size:50%;"),
                                           tags$p("Active",style="font-size:100%;")),
                                  valueBox(color="light-blue",
                                           value=tags$p(paste(sum(current$Recovered), " (+", sum(current$`New Recoveries`), ")",sep=""),style="font-size:50%;"),
                                           tags$p("Recovered",style="font-size:100%;")),
                                  valueBox(color="red",
                                           value=tags$p(paste(sum(current$Deaths), " (+", sum(current$`New Deaths`), ")",sep=""),style="font-size:50%;"),
                                           tags$p("Deaths",style="font-size:100%;")),
                                  valueBox(color="aqua",
                                           value=tags$p(max(testdata$Date),style="font-size:50%"),
                                           tags$p("Last Update")),
                                  valueBox(color="maroon",
                                           value=tags$p(interval(min(testdata$Date),max(testdata$Date)) %/% days(1),
                                                        style="font-size:50%"),
                                           tags$p("Days of Exposure"))
                                ),
                                
                                
                                
                                fluidRow(
                                  tabBox(title="Cumulative", side = "right",
                                         height=700,selected = "Scaled",
                                         tabPanel("Log",highchartOutput("plotall1log",height=650)),
                                         tabPanel("Uniform",highchartOutput("plotall1same",height=650)),
                                         tabPanel("Scaled",highchartOutput("plotall1",height=650))),
                                  tabBox(title="Daily",height=700,side="right",
                                         selected = "Scaled",
                                         tabPanel("Uniform",highchartOutput("plotall2same",height=650)),
                                         tabPanel("Scaled",highchartOutput("plotall2",height=650)))
                                ),
                                
                                #Row 4 - Map of Africa for cumulative variables.
                                fluidRow(
                                  # box(title="COVID in Africa Map - Cumulative",width=8,height=700,
                                  #     leafletOutput("summap1",height=600)),
                                  
                                  tabBox(title = "Africa", side="right",height=700,width=9,
                                         selected = "Cumulative",
                                         tabPanel("Cumulative", highchartOutput("summap1",height=600)),
                                         tabPanel("Daily",highchartOutput("summap2",height=600))),
                                  box(title="Select Variable",
                                      radioButtons("var3","Cumulative Variable:",
                                                   choices=c("Confirmed Cases" = "Confirmed",
                                                             "Recovered Patients" = "Recovered",
                                                             "Active" = "Active",
                                                             "Deaths" = "Deaths")),
                                      radioButtons("var4","Daily Variable:",
                                                   choices=c("New Cases" = "New.Cases",
                                                             "New Recoveries" = "New.Recoveries",
                                                             "New Deaths" = "New.Deaths")),
                                      width=3)
                                )
                                
                        ),
                        
                        ## In-depth country exploration
                        tabItem(tabName = "country",
                                #Row 1 -Variable selection and summary
                                fluidRow(
                                  
                                  #This box lets you pick country and date.
                                  box(title = span(icon("globe-africa"),"Select Country"),style="font-size:150%",solidHeader=T,status="primary",collapsible=T,
                                      selectInput("country", label = "",
                                                  choices=sort(unique(testdata$Country_Region)),selected= sort(unique(testdata$Country_Region))[1]),
                                      downloadButton("downloadCountry","Download csv for this Country")
                                  ),
                                  box(title = span(icon("calendar"),"Change Date"),style="font-size:150%",solidHeader=T,status="primary",collapsible=T,
                                      sliderInput("date", label= "", 
                                                min = min(testdata$Date),max = max(testdata$Date),
                                                value = max(testdata$Date),width="90%")
                                  )
                                ),
                                fluidRow(
                                  valueBoxOutput("countryconfirmed"),
                                  valueBoxOutput("countryactive"),
                                  valueBoxOutput("countryrecovered"),
                                  valueBoxOutput("countrydeaths"),
                                  valueBoxOutput("countrydate"),
                                  valueBoxOutput("countryexposure")
                                ),
                                
                                #Row 2 - Plot
                                fluidRow(
                                  # Box with plot.
                                  tabBox(title = "COVID by Country",width=12,height=700,
                                         side="right",selected="Cumulative",
                                         tabPanel("Daily",highchartOutput("ploteachdaily",height=650)),
                                         tabPanel("Cumulative per 100k", highchartOutput("ploteachpop",height=650)),
                                         tabPanel("Cumulative (log)",highchartOutput("ploteachlog",height=650)),
                                         tabPanel("Cumulative",highchartOutput("ploteach",height=650))
                                  )
                                )
                                
                        ),
                        
                        ## Dynamic Maps tab.
                        tabItem(tabName="mapexp",
                                
                                #Row 1 - Date Selection
                                fluidRow(
                                  box(title = span(icon("calendar"),"Change Date"),style="font-size:150%",solidHeader=T,status="primary",collapsible=T,
                                      sliderInput("date1", label= "", 
                                                  min = min(testdata$Date),max = max(testdata$Date),
                                                  value = max(testdata$Date),width="90%"))
                                ),
                                
                                # Row 2 - Cumulative Variable map.
                                fluidRow(
                                  tabBox(title="COVID in Africa Map - Cumulative",width=8, height=700,
                                         side="right",selected="Cumulative",
                                         tabPanel("Population",highchartOutput("map3",height=600)),
                                         tabPanel("Daily",highchartOutput("map2",height=600)),
                                         tabPanel("Cumulative",highchartOutput("map1",height=600))),
                                  
                                  box(title="Select Variable",
                                      radioButtons("var5","Cumulative Variable:",
                                                   choices=c("Confirmed Cases" = "Confirmed",
                                                             "Active Cases" = "Active",
                                                             "Recovered Patients" = "Recovered",
                                                             "Deaths" = "Deaths")),
                                      radioButtons("var6","Daily Variable:",
                                                   choices=c("New Cases" = "New.Cases",
                                                             "New Recoveries" = "New.Recoveries",
                                                             "New Deaths" = "New.Deaths")),
                                      radioButtons("var7","Population Variable",
                                                   choices = c("Asthma (Standardized)" = "asthma_standardised",
                                                               "HIV (Standardized)" = "hiv_standardised",
                                                               "Air Traffic" = "airtraffic",
                                                               "Service Quality" = "service_quality2018",
                                                               "Urbanization" = "urbanization")),width=3)
                                ),
                        ),
                        
                        ## Model Projections for IV Regression
                        tabItem(tabName = "model",
                                
                                ## Row 1 - plot projections and select country.
                                fluidRow(                      
                                  box(title = tags$p("Model Projections",style="font-size:150%"),
                                      plotOutput("plotmodel"),width=8),
                                  
                                  box(title = tags$p("Select Country",style="font-size:150%"),
                                      selectInput("modelcountry", label = "",
                                                  choices=sort(unique(modelproj$Country_Region)),
                                                  selected= sort(unique(modelproj$Country_Region))[1]),
                                      width=4
                                  )
                                ),
                                
                                # Row 2 - About information for the model. This is all just text in boxes.
                                fluidRow(
                                  box(title= "About The Model",
                                      div(
                                        HTML(paste("<h5> These projections were created using an 
                             instrumental variable regression model. Data was trained 
                             using COVID case numbers and exposure time on ", modeldate, 
                                                   " as well as other information from the country, including 
                             air traffic, population, and other factors, provided by United Nations' 
                             World Population Prospects. 
                             This model will continue to be updated and improved to construct 
                             more accurate projections for each country. </h5>"))
                                      )),
                                  box(title = "Reading the Graphic",
                                      div(
                                        HTML("<h5> The model's projections are displayed 
                                 with the line provided in black, as well as a 
                                 prediction interval displayed in blue. 
                                 Real-time COVID data will be collected and shown in purple 
                                 to compare these projections. </h5>")
                                      ))
                                )
                        ),
                        
                        ## Model Projections for Linear Mixed Effects Model.
                        tabItem(tabName = "lmer",
                                fluidRow(
                                  tabBox(title="COVID Forecasts", side = "right",
                                         height=700,selected = "Standard", width=8,
                                         tabPanel("Standard",highchartOutput("lmeplot",height=650)),
                                         tabPanel("Regional",highchartOutput("lmeregionplot",height=650))),
                                  
                                  column(box(title="Select Country",
                                      selectInput("lmecountry", label = "",
                                                  choices=sort(unique(forecast$Country)),
                                                  selected= sort(unique(forecast$Country))[1]),width="100%"),
                                      box(title = span(icon("question-circle"),"What is this?"),
                                          solidHeader=T,status="primary",collapsible=T, collapsed=T, width="100%",
                                        "As COVID-19 continues to spread worldwide, different nations have been 
                                        taking measures to counteract them. However, some areas will be affected 
                                        differently than others, and will need to prepare in different ways. 
                                        These forecasting models were developed to predict what the COVID cases may be in the near future."),
                                      box(title = span(icon("question-circle"),"How were these projections made?"),
                                          solidHeader=T,status="primary",collapsible=T, collapsed=T, width="100%",
                                          "With access to data from the JHU COVID-19 Dashboard and Github database, we wanted to make projections 
                                          using statistical modeling, in order to understand how each aspect of each country could influence the spread of COVID-19. 
                                          However, we had to keep two things in mind. First, we also had to consider the innate differences of each country, 
                                          like population. Second, we had to recognize that some countries are more reliable with how frequently they update their data. 
                                          As such, we wanted to figure out a way to account for the unreliability of some countries into our projections.

                                          Keeping these challenges in mind, we decided to use a Linear Mixed Effects model. The main advantage of this model is that it uses partial pooling. 
                                          In short, it treats each country separately, but also considers how much data is in each country. If a country frequently updates its COVID Confirmed Cases, 
                                          our model will use that to drive its predictions. However, if a country doesn’t update as frequently, our model will use other similar countries to govern its predictions. 
                                          This allows us to still make projections for countries with less counts.

                                          When constructing our model, we also chose to remove the data where no new data was recorded for a country. Given the trends occurring in South Africa, Nigeria, and Egypt, 
                                          we thought it was unlikely that a country would have no COVID cases on a single day. 
                                          On top of this, we found over time that many countries would have a long time period where no data was being recorded, only to have a huge “spike” afterwards.

                                          This model was constructed in R, using the lme4 package. We also used the rstanarm package in R to apply simulation to our model, allowing us to predict a range of possible values. 
                                          We’re also continuing to work on the model to take more country-level data into account, as well as continue to update it for new data. 
                                          We’re also going to continue to attempt to make models to project case mortality."),
                                      box(title = span(icon("question-circle"),"How does this graph work?"),
                                          solidHeader=T,status="primary",collapsible=T, collapsed=T, width="100%",
                                          "This graph showcases the current COVID trends as well as our models’ forecasted predictions. The purple line is the current COVID trends. 
                                          Our model predictions are stuck at the end of the X axis (date), and has two parts. The first is a black line, 
                                          which is the average prediction for our model. We also developed a predictive range where our data is most likely to fall, which is the blue area. 
                                          Ideally, the COVID data should fall somewhere within that blue range.

                                          You can select which country you want to look at in the top of the menu! Once you do, the graph below will change. 
                                          You can also mouse over the lines to view the case numbers for each country."),
                                      box(title = span(icon("question-circle"),"The model seems more off for some countries than others. Why?"),
                                          solidHeader=T,status="primary",collapsible=T, collapsed=T, width="100%",
                                          "Ideally, the COVID data will fall within the blue lines. However, COVID is relatively unpredictable, 
                                          so you may find that the recorded cases fall outside the line. 
                                          Depending on how well the people are able to accommodate or respond to the virus, 
                                          cases can either suddenly rise or fall. While we can’t predict how people are going to act on a given day, 
                                          we’re working hard to try to find the overall trends and predict with accuracy. \n \n

                                          We try very hard to avoid cases where our model underpredicts COVID cases and guesses too low. 
                                          When this happens, we usually study the model to see what went wrong and attempt to correct it for the future. 
                                          Our model can also overpredict data, and there are usually two reasons behind this. 
                                          Some countries seem to have time periods where they don’t update their COVID data, 
                                          and seem to flatline on a number. Our model usually continues to go up, which is what we want it to do, 
                                          because usually the flatlines are followed by a big spike that we want to predict. 
                                          On the other hand, if a country is continuing to update its better and has fewer cases than we predict, 
                                          that means it’s doing better than predicted."),
                                      
                                      width=4)
                                )
                        ),
                        
                        ## Data Table for viewing data.
                        tabItem(tabName = "data",
                                DT::dataTableOutput("data"),
                                downloadButton("downloadData","Download")
                        ),
                        
                        ## About Us. All of this is text.
                        tabItem(tabName = "about",
                                fluidRow( 
                                  column(2),
                                  column(7,
                                         div(
                                           HTML("<br><center><h2> About </h2></center>"),
                                           HTML("<h5>This is an interactive tool that visualizes 
                                   developing COVID data in Africa, and can help
                                   users explore patterns more closely in African countries.<h5>"),
                                           HTML("<h5> It is part of the "),
                                           tags$a(href="https://sites.bu.edu/covid-19-in-africa/",
                                                  "COVID-19 in Africa Data Science Initiative"),
                                           HTML(" that seeks to model and map the trajectory of the novel corona 
                                        virus [SARS-Cov-2, COVID-19] on the African continent, its economic impact, 
                                        and the capacity of country level healthcare systems to adequately respond to 
                                        the pandemic over time. </h5>"),
                                           HTML("<h5>This App is currently under construction and will
                                  receive changes to UI and additional features.</h5>"),
                                           HTML("<br><center><h3> How to Use </h3></center>"),
                                           HTML("<h5>You can explore different graphics using the menus on the left! </h5>"),
                                           HTML("<h5> <b>COVID Exploration</b> is split into three sections: <b>COVID Africa Summary</b>,
                                        <b>Country Analysis</b>, and <b>Dynamic Maps</b>.</h5>"),
                                           HTML("<h5> <b>COVID Africa Summary</b> summarizes the most recent COVID data with texts 
                                   and graphics. You can change the variables in the map using the menus on the right.</h5>"),
                                           HTML("<h5> <b>Country Analysis</b> lets you more freely explore COVID patterns over time 
                                        in each country. You can change the country on the top, and look more closely at some 
                                        basic information for some dates. You can also choose which variables to view in the 
                                        menu on the right.</h5>"),
                                           HTML("<h5> <b>Dynamic Maps</b> lets you visualize more data with the maps. You can 
                                        change date using the bar above, and like before, select variables with the 
                                        menus on the left.</h5>"),
                                           HTML("<h5> <b>Model Projections</b> will include projections made by various models.</h5>"),
                                           HTML("<h5> <b>Data Table</b> will let you freely explore the raw, tabular data. </h5>"),
                                           
                                           HTML("<br><center><h3> About the Data </h3></center>"),
                                           HTML("The data used for this tool was downloaded from the "),
                                           tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6",
                                                  "John Hopkin's University COVID-19 Dashboard."),
                                           HTML("<h4><center> Variables </center></h4>"),
                                           HTML("<h5><b>Confirmed</b>, <b>Recoveries</b>, and, <b>Deaths</b> are the listed 
                                        confirmed COVID-19 cases, recovered patients, and deaths due to COVID-19 according
                                        to the JHU data. <b>New Cases</b>, <b>New Recoveries</b>, and <b>New Deaths</b> are 
                                        how many new cases, recoveries, and deaths were added to the data on a given day.</h5>" ),
                                           HTML("<h5><b>Case Fatalities</b> is the number of deaths divided by 
                                        by the number of confirmed cases. </h5>"),
                                           HTML("<h5><b>Cases per 100k pop</b> and <b>Deaths per 100k pop</b> 
                                        are the number of Confirmed Cases and Deaths per 100k population. 
                                        Measurements were collected from the "),
                                           tags$a(href="https://population.un.org/wpp/","UN World Population Prospects."),
                                           br(), br(),
                                           HTML("<br><center><h5> Maintained by Timothy Hogan </h5></center>")
                                         )),
                                  column(2)
                                )     
                                
                        ),
                        
                        ## Contact Us Tab.
                        tabItem(tabName = "contact",
                                h2("Contact Info")
                        )
                      )
                    )
)

##Server. Shiny Servers take inputs from the UI and then use them to create plots.
## input$country for example is an input from one of the menus.

server <- function(input, output) {
  
  #Plot cumulative information for all of Africa.
  output$plotall1 <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Summary",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Cumulative",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(alldata, "line",yAxis=0,name="Confirmed", hcaes(Date,Confirmed),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(alldata, "line",yAxis=1,name="Active", hcaes(Date,Active),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(alldata,"line",yAxis=2,name="Recovered",hcaes(Date,Recovered),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(alldata,"line",yAxis=3,name="Deaths",hcaes(Date,Deaths),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T) %>%
      hc_size(height=650)
  })
  
  
  output$plotall1same <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Summary",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Cumulative",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),max = max(alldata$Confirmed),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(alldata, "line",yAxis=0,name="Confirmed", hcaes(Date,Confirmed),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(alldata, "line",yAxis=1,name="Active", hcaes(Date,Active),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(alldata,"line",yAxis=2,name="Recovered",hcaes(Date,Recovered),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(alldata,"line",yAxis=3,name="Deaths",hcaes(Date,Deaths),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T)   %>%
      hc_size(height=650)
  })
  
  output$plotall1log <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Summary",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Cumulative (log scale)",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(alldata, "line",yAxis=0,name="log Cases", hcaes(Date,round(`log Cases`,2)),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(alldata, "line",yAxis=1,name="log Active", hcaes(Date,round(`log Active`,2)),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(alldata,"line",yAxis=2,name="log Recovered",hcaes(Date,round(`log Recovered`,2)),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(alldata,"line",yAxis=3,name="log Deaths",hcaes(Date,round(`log Deaths`,2)),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T)   %>%
      hc_size(height=650)
  })
  
  
  
  #Plot daily information for all of Africa.
  output$plotall2 <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Summary",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Daily",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(alldata, "line",yAxis=0,name="New Cases", hcaes(Date,`New Cases`),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(alldata, "line",yAxis=1,name="New Active", hcaes(Date,`New Active`),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(alldata,"line",yAxis=2,name="New Recoveries",hcaes(Date,`New Recoveries`),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(alldata,"line",yAxis=3,name="New Deaths",hcaes(Date,`New Deaths`),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T)   %>%
      hc_size(height=650)
  }) 
  
  output$plotall2same <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Summary",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Daily",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),max=max(alldata$`New Cases`),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(alldata, "line",yAxis=0,name="New Cases", hcaes(Date,`New Cases`),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(alldata, "line",yAxis=1,name="New Active", hcaes(Date,`New Active`),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(alldata,"line",yAxis=2,name="New Recoveries",hcaes(Date,`New Recoveries`),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(alldata,"line",yAxis=3,name="New Deaths",hcaes(Date,`New Deaths`),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T)   %>%
      hc_size(height=650)
  }) 
  
  # Plot cumulative map for summary page.
  output$summap1 <- renderHighchart({ 
    highchart() %>%
      hc_add_series_map(map,current,value=input$var3,joinBy=c("name","Country_Region"),name=input$var3,
                        borderColor="#000000") %>%
      hc_colorAxis(minColor = plotColors[plotColors$Var==input$var3,"minCol"], maxColor = plotColors[plotColors$Var==input$var3,"maxCol"]) 
    
  })
  
  
  
  # Plot daily map for summary page.
  output$summap2 <- renderHighchart({
    highchart() %>%
      hc_add_series_map(map,current,value=input$var4,joinBy=c("name","Country_Region"),name=input$var4,
                        borderColor="#000000") %>%
      hc_colorAxis(minColor = plotColors[plotColors$Var==input$var4,"minCol"], maxColor = plotColors[plotColors$Var==input$var4,"maxCol"]) 
  })
  
  
  datedata <- reactive({
    testdata %>% filter(Date==input$date&Country_Region==input$country)
  })
  
  output$countryconfirmed <- renderValueBox({
    valueBox(color="purple",
             value=tags$p(paste(datedata()$Confirmed, " (", datedata()$`New Cases`, " new)",sep=""),
                          style="font-size:50%;"),
             tags$p("Confirmed",style="font-size:100%;"))
  })
  
  output$countryactive <- renderValueBox({
    valueBox(color="olive",
             value=tags$p(paste(datedata()$Active, " (", datedata()$`New Active`, " new)",sep=""),
                          style="font-size:50%;"),
             tags$p("Active",style="font-size:100%;"))
  })
  
  output$countryrecovered <- renderValueBox({
    valueBox(color="light-blue",
             value=tags$p(paste(datedata()$Recovered, " (", datedata()$`New Recoveries`, " new)",sep=""),style="font-size:50%;"),
             tags$p("Recovered",style="font-size:100%;"))
  })
  
  output$countrydeaths <- renderValueBox({
    valueBox(color="red",
             value=tags$p(paste(datedata()$Deaths, " (", datedata()$`New Deaths`, " new)",sep=""),style="font-size:50%;"),
             tags$p("Deaths",style="font-size:100%;"))
  })
  
  output$countrydate <- renderValueBox({
    valueBox(color="aqua",
             value=tags$p(datedata()$Date,style="font-size:50%;"),
             tags$p("Selected Date",style="font-size:100%;"))
  })
  
  output$countryexposure <- renderValueBox({
    min <- min(testdata[testdata$Country_Region==input$country,]$Date)
    
    valueBox(color="maroon",
             value=tags$p(interval(min,datedata()$Date) %/% days(1),style="font-size:50%;"),
             tags$p("Days Since Case 1",style="font-size:100%;"))
  })
  
  countrydata <- reactive({
    testdata %>% filter(Country_Region==input$country)})
  
  
  ## Plot adjustable COVID data.
  output$ploteach <-  renderHighchart({ 
    highchart() %>%
      hc_title(text=input$country,align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Cumulative",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(countrydata(), "line",yAxis=0,name="Confirmed", hcaes(Date,Confirmed),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(), "line",yAxis=1,name="Active", hcaes(Date,Active),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(),"line",yAxis=2,name="Recovered",hcaes(Date,Recovered),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(countrydata(),"line",yAxis=3,name="Deaths",hcaes(Date,Deaths),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T) %>%
      hc_size(height=650)
  })
  
  output$ploteachlog <-  renderHighchart({ 
    highchart() %>%
      hc_title(text=input$country,align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Cumulative (log scale)",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(countrydata(), "line",yAxis=0,name="log Cases", hcaes(Date,round(`log Cases`,2)),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(), "line",yAxis=1,name="log Active", hcaes(Date,round(`log Active`,2)),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(),"line",yAxis=2,name="log Recovered",hcaes(Date,round(`log Recovered`,2)),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(countrydata(),"line",yAxis=3,name="log Deaths",hcaes(Date,round(`log Deaths`,2)),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T) %>%
      hc_size(height=650)
  })
  
  output$ploteachpop <-  renderHighchart({ 
    highchart() %>%
      hc_title(text=input$country,align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Cumulative",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(countrydata(), "line",yAxis=0,name="Cases per 100k pop", hcaes(Date,round(`Cases per 100k pop`,2)),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(), "line",yAxis=1,name="Active per 100k pop", hcaes(Date,round(`Active per 100k pop`,2)),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(),"line",yAxis=2,name="Recoveries per 100k pop",hcaes(Date,round(`Recoveries per 100k pop`,2)),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(countrydata(),"line",yAxis=3,name="Deaths per 100k pop",hcaes(Date,round(`Deaths per 100k pop`,2)),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T) %>%
      hc_size(height=650)
  })
  
  output$ploteachdaily <-  renderHighchart({ 
    highchart() %>%
      hc_title(text=input$country,align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = "Daily",align="left") %>%
      hc_yAxis_multiples(
        create_yaxis(4,height=c(1,1,1,1),turnopposite=F,title=list(text=NULL),sep=0.1)
      ) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(countrydata(), "line",yAxis=0,name="Confirmed", hcaes(Date,`New Cases`),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(), "line",yAxis=1,name="Active", hcaes(Date,`New Active`),color="#449c6c",marker=list(symbol="circle")) %>%
      hc_add_series(countrydata(),"line",yAxis=2,name="Recovered",hcaes(Date,`New Recoveries`),color="#5dbcd2",marker=list(symbol="circle")) %>% 
      hc_add_series(countrydata(),"line",yAxis=3,name="Deaths",hcaes(Date,`New Deaths`),color="#dc4c3c",marker=list(symbol="circle")) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_tooltip(table = T,
                 sort = T,
                 crosshairs=T,
                 pointFormat = paste0('<br><span style="color:{point.color}">\u25CF</span>',
                                      "{series.name}: {point.y}")
      ) %>% 
      hc_exporting(enabled=T) %>%
      hc_size(height=650)
  })
  
  # Plot cumulative map for dynamic map page.
  
  
  output$map1 <- renderHighchart({ 
    day <- testdata %>% filter(Date==input$date1)
    
    highchart() %>%
      hc_add_series_map(map,day,value=input$var5,joinBy=c("name","Country_Region"),name=input$var5,
                        borderColor="#000000") %>%
      hc_colorAxis(minColor = plotColors[plotColors$Var==input$var5,"minCol"], maxColor = plotColors[plotColors$Var==input$var5,"maxCol"]) 
  })
  
  output$map2 <- renderHighchart({
    day <- testdata %>% filter(Date==input$date1) %>% 
      mutate(New.Cases = `New Cases`,
             New.Active = `New Active`,
             New.Recoveries = `New Recoveries`,
             New.Deaths = `New Deaths`)
    
    highchart() %>%
      hc_add_series_map(map,day,value=input$var6,joinBy=c("name","Country_Region"),name=input$var6,
                        borderColor="#000000") %>%
      hc_colorAxis(minColor = plotColors[plotColors$Var==input$var6,"minCol"], maxColor = plotColors[plotColors$Var==input$var6,"maxCol"]) 
  })
  
  
  output$map3 <- renderHighchart({
    highchart() %>%
      hc_add_series_map(map,displaydata,value=input$var7,joinBy=c("name","countryorarea"),name=input$var7,
                        borderColor="#000000") %>%
      hc_colorAxis(minColor = "#edf8e8", maxColor = "#065a2f") 
  })
  
  ## Plot model for LMER
  output$lmeplot <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Model Projections",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = input$lmecountry,align="left") %>%
      hc_yAxis(title="Cases",min=0) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(testdata[testdata$Country_Region==input$lmecountry,], "line",name="Confirmed Cases", hcaes(Date,Confirmed),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(forecast[forecast$Country==input$lmecountry,],"line",
                    name="Forecasted Cases",hcaes(Date,round(exp(Est))),color="#000000",
                    marker=list(symbol="circle"),id="forecast") %>%
      hc_add_series(forecast[forecast$Country==input$lmecountry,],"arearange",name="Range",
                    hcaes(x=Date,low=round(exp(Lower)),high=round(exp(Upper))),color=hex_to_rgba("skyblue",0.2),showInLegend=F,zIndex=-3,
                    marker=list(symbol="circle"),linkedTo="forecast",lineWidth=1) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_exporting(enabled=T) %>%
      hc_size(height=650)    
  })
  
  output$lmeregionplot <- renderHighchart({
    highchart() %>%
      hc_title(text="COVID 19 Model Projections",align="left",margin=20,useHTML=T) %>%
      hc_subtitle(text = input$lmecountry,align="left") %>%
      hc_yAxis(title="Cases",min=0) %>% 
      hc_xAxis(type = "datetime") %>%
      hc_add_series(testdata[testdata$Country_Region==input$lmecountry,], "line",name="Confirmed Cases", hcaes(Date,Confirmed),color="#645cac",marker=list(symbol="circle")) %>%
      hc_add_series(forecastw[forecastw$Country==input$lmecountry,],"line",
                    name="Forecasted Cases",hcaes(Date,round(exp(Est))),color="#000000",
                    marker=list(symbol="circle"),id="forecast") %>%
      hc_add_series(forecastw[forecastw$Country==input$lmecountry,],"arearange",name="Range",
                    hcaes(x=Date,low=round(exp(Lower)),high=round(exp(Upper))),color=hex_to_rgba("skyblue",0.2),showInLegend=F,zIndex=-3,
                    marker=list(symbol="circle"),linkedTo="forecast",lineWidth=1) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMousTracking = T)
      ) %>%
      hc_exporting(enabled=T) %>%
      hc_size(height=650)    
  })
  
  
  ## Put up data table.
  output$data <- DT::renderDataTable(testdata[,c("Country_Region","Date",
                                                 "Confirmed","Active","Recovered","Deaths",
                                                 "New Cases","New Active", "New Recoveries", "New Deaths")])
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("covidafricadata",".csv",sep="")
    },
    content= function(file) {
      write.csv(testdata[,c("Country_Region","Date","Confirmed","Active","Recovered","Deaths","New Cases", 
                            "New Active", "New Recoveries", "New Deaths")],file,row.names=F)
    }
  )
  
  output$downloadCountry <- downloadHandler(
    filename = function() {
     paste(input$country,".csv",sep="")
    },
    content = function(file) {
      write.csv(countrydata(),file,row.names=F)
    }
  )
  
}


shinyApp(ui, server)
