library(shiny)
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

## This app as 3 parts: pre-prep, UI, and Server.
## The pre-prep includes data reading and setting some numbers from the data.
## The UI is the code for the app UI.
## The server is the code for the interactive functions.

#Read model data and set colors.
modelproj <- read.csv("modelprojections.csv") %>% mutate(Date = ymd(Date))
modeldate <- modelproj[modelproj$DaysSince==0,]$Date[1]

#Read the COVID data and clean it.
testdata <- read.csv("testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date)
testdata$Country_Region <- as.character(testdata$Country_Region)

#Read Population Data and clean country names.
popdata <- read.csv("popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="Congo",]$Location <- "Congo (Brazzaville)"
popdata[popdata$Location=="Democratic Republic of the Congo",]$Location <- "Congo (Kinshasa)"
popdata[popdata$Location=="Côte d'Ivoire",]$Location <- "Cote d'Ivoire"
popdata[popdata$Location=="United Republic of Tanzania",]$Location <- "Tanzania"
popdata[popdata$Location=="Cabo Verde",]$Location <- "Cape Verde"

# Read in forecast data.
forecast <- read.csv("forecast.csv")[,-1]

# Left join population data and test data. Calculated values of interest to be displayed.
testdata <- left_join(testdata,popdata,by=c("Country_Region"="Location"))
testdata <- testdata %>% group_by(Country_Region) %>% 
    mutate(`Case Fatalities` = Deaths/Confirmed,
           `Deaths per 100k pop` = Deaths/PopTotal*100000,
           `Cases per 100k pop` = Confirmed/PopTotal*100000,
           `New Cases` = Confirmed-lag(Confirmed,default=0),
           `New Deaths` = Deaths-lag(Deaths,default=0),
           `New Recoveries` = Recovered-lag(Recovered,default=0),
           `New Deaths per 100k pop` = `New Deaths`/PopTotal*100000,
           `New Cases per 100k pop` = `New Cases`/PopTotal*100000,
           `Log(Cases)` = log(Confirmed),
           `log(Deaths)` = log(Deaths),
           Exposure = Date - first(Date) + 1) %>% ungroup()
#Filter out the most recent date.
current <- testdata %>% filter(Date==max(testdata$Date))

#Get country list.
countrylist <- sort(unique(testdata$Country_Region))

#Set colors for graphics.
group.colors = c(Deaths="red4",`New Deaths`="red4",
                 Recovered="deepskyblue",
                 `Case Fatalities` = "red4", `Deaths per 100k pop` = "red4",
                 `Cases per 100k pop` = "darkorchid",
                 `New Deaths per 100k pop` = "red4",
                 `New Cases per 100k pop` = "darkorchid",
                 `New Recoveries`="deepskyblue",
                 Confirmed="darkorchid",`New Cases`="darkorchid")
modelcolors <- c("Projection Range" = "skyblue", "Model Projection" = "black", "Confirmed Cases" = "darkorchid")

#Get list of cumulative and daily variables.
#This lets me create menus for daily and cumulative graphics separately more easily.
tot = colnames(testdata)[c(2:4,7:9)]
daily = colnames(testdata)[c(10:14)]

#Get map of Africa, and correct country names.
#I need a newer shape file.
data("wrld_simpl")
afr=wrld_simpl[wrld_simpl$REGION==2,]
afr@data$id <- afr@data$NAME
afr <- fortify(afr,region="id")
afr[afr$id=="Swaziland",]$id <- "Eswatini"
afr[afr$id=="Congo",]$id <- "Congo (Brazzaville)"
afr[afr$id=="Democratic Republic of the Congo",]$id <- "Congo (Kinshasa)"
afr[afr$id=="United Republic of Tanzania",]$id <- "Tanzania"
afr[afr$id=="Libyan Arab Jamahiriya",]$id <- "Libya"

## Cumulate data for all of Africa and use pivot_longer to put it in graphic-ready form.
alldata <- testdata %>% group_by(Date) %>% 
    summarize(Confirmed = sum(Confirmed),
              Recovered = sum(Recovered),
              Deaths = sum(Deaths)) %>%
    mutate(`New Cases` = Confirmed - lag(Confirmed,default=0),
           `New Recoveries` = Recovered - lag(Recovered,default=0),
           `New Deaths` = Deaths - lag(Deaths,default=0)) %>% 
    pivot_longer(cols=c("Confirmed","Deaths","Recovered",
                        "New Cases","New Recoveries","New Deaths"),
                 names_to="Type",values_to="Value")

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
            # menuItem("COVID Africa Summary",tabName="summary"),
            # menuItem("COVID Country Analysis",tabName="country"),
            # menuItem("Dynamic Map Explorer",tabName="mapexp"),
            menuItem("Model Projections", tabName = "bigtab", icon = icon("chart-line"),
                     menuSubItem("IV Regression Projection",tabName="model"),
                     menuSubItem("LME Projection", tabName = "lmer")
                     ),
            menuItem("Data Table", tabName = "data", icon = icon("table")),
            menuItem("About", tabName = "about", icon = icon("question-circle")),
            menuItem("Contact Us",
                       href="https://sites.bu.edu/covid-19-in-africa/partner-with-us/",
                     icon = icon("address-card"))
            
        )
    ),
    
    ## This is the body. Content is separated into different tags.
    dashboardBody(
        
      #Sets background color to the green it is.
        tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #f3faf0;
                                }        }
      '))),
        
## First page - COVID Summary for all of Africa.  
        tabItems(
            # COVID
            tabItem(tabName = "summary",
                    
                    #Row 1 - summary content.
                    fluidRow(
                        
                        box(
                            title=tags$p("Summary",style="font-size:150%"),
                            h4(paste("In Africa, as of ", max(testdata$Date),", there are:",sep="")),
                            tags$div(
                                HTML(paste("<font style=color:#9932cc> <b>",
                                           sum(current$Confirmed)," total COVID cases </b> (", sum(current$`New Cases`)," new)</font>", sep="")),
                                tags$br(),
                                HTML(paste("\n<font style=color:#00bfff> <b>",
                                           sum(current$Recovered)," total recoveries </b> (", sum(current$`New Recoveries`), " new) </font>", sep="")),
                                tags$br(),
                                HTML(paste("\n<font style=color:#8b0000> <b>",
                                           sum(current$Deaths)," total deaths </b> (", sum(current$`New Deaths`), " new)</font>", sep=""))
                                
                            )
                            
                        )
                        

                    ),
                    
                    #Row 2 - Plot of cumulative cases.
                    fluidRow(
                        box(title="COVID in Africa - Cumulative",
                            plotOutput("plotall1"),width=8)
                    ),
                    
                    # Row 3 - Plot of daily cases.
                    fluidRow(
                      box(title="COVID in Africa - Daily",
                              plotOutput("plotall2"),width=8)
                    ),
                    
                    #Row 4 - Map of Africa for cumulative variables.
                    fluidRow(
                        box(title="COVID in Africa Map - Cumulative",
                            plotOutput("summap1"),width=8),
                        box(title="Select Variable",
                            radioButtons("var3","Cumulative Variable:",
                                         choices=c("Confirmed Cases" = "Confirmed",
                                                   "Recovered Patients" = "Recovered",
                                                   "Deaths" = "Deaths",
                                                   "Case Fatalities" = "Case Fatalities",
                                                   "Deaths per 100k pop" = "Deaths per 100k pop",
                                                   "Cases per 100k pop" = "Cases per 100k pop")),
                            width=3)
                    ),
                    
                    #Row 4 - Map of Africa for Daily cases.
                    fluidRow(
                        box(title="COVID in Africa Map - Daily",
                            plotOutput("summap2"),width=8),
                        box(width=3, title="Select Variable",
                            radioButtons("var4","Daily Variable:",
                                         choices=c("New Cases" = "New Cases",
                                                   "New Recoveries" = "New Recoveries",
                                                   "New Deaths" = "New Deaths",
                                                   "New Deaths per 100k pop" = "New Deaths per 100k pop",
                                                   "New Cases per 100k pop" = "New Cases per 100k pop")))
                    )
            ),
            
## In-depth country exploration
            tabItem(tabName = "country",
                    #Row 1 -Variable selection and summary
                    fluidRow(
                        
                      #This box lets you pick country and date.
                        box(title = tags$p("Change Country and Date",style="font-size:150%"),
                            selectInput("country", label = "Select Country",
                                        choices=sort(unique(testdata$Country_Region)),selected= sort(unique(testdata$Country_Region))[1]),
                            dateInput("date", label= "Select Date", 
                                      min = min(testdata$Date),max = max(testdata$Date),
                                      value = max(testdata$Date))
                            ),

                      #This box displays summary information.
                        box(tags$p("Summary",style="font-size:170%"),
                            htmlOutput("countrysum"))
                        
                        ),

                    #Row 2 - Plot
                    fluidRow(
                        # Box with plot.
                      box(title = "COVID by Country",
                               plotOutput("ploteach"),
                               width=8),
                        #Variable selection menus.
                      box(title = tags$p("Select Variable",style="font-size:150%"),
                                radioButtons("var","Cumulative:",
                                             choices=c("Confirmed Cases" = "Confirmed",
                                                       "Recovered Patients" = "Recovered",
                                                       "Deaths" = "Deaths",
                                                       "Case Fatalities" = "Case Fatalities",
                                                       "Deaths per 100k pop" = "Deaths per 100k pop",
                                                       "Cases per 100k pop" = "Cases per 100k pop")),
                                radioButtons("var2","Daily Variable:",
                                             choices=c("New Cases" = "New Cases",
                                                       "New Recoveries" = "New Recoveries",
                                                       "New Deaths" = "New Deaths",
                                                       "New Deaths per 100k pop" = "New Deaths per 100k pop",
                                                       "New Cases per 100k pop" = "New Cases per 100k pop")),
                            width= 4
                            
                        )
                        )
                    
                    ),
            
## Dynamic Maps tab.
            tabItem(tabName="mapexp",
                    
                    #Row 1 - Date Selection
                    fluidRow(
                    box(tags$p("Select Date",style="font-size:150%"),
                        dateInput("date1", label= "Select Date", 
                                  min = min(testdata$Date),max = max(testdata$Date),
                                  value = max(testdata$Date)))
                    ),
                    
                    # Row 2 - Cumulative Variable map.
                    fluidRow(
                        box(title="COVID in Africa Map - Cumulative",
                            plotOutput("map1"),width=8),
                        box(title="Select Variable",
                            radioButtons("var5","Cumulative Variable:",
                                         choices=c("Confirmed Cases" = "Confirmed",
                                                   "Recovered Patients" = "Recovered",
                                                   "Deaths" = "Deaths",
                                                   "Case Fatalities" = "Case Fatalities",
                                                   "Cases per 100k pop" = "Cases per 100k pop",
                                                   "Deaths per 100k pop" = "Deaths per 100k pop")),width=3)
                    ),
                    
                    #Row 3 - Daily variable map.
                    fluidRow(
                        box(title="COVID in Africa Map - Daily",
                            plotOutput("map2"),width=8),
                        box(width=3, title="Select Variable",
                            radioButtons("var6","Daily Variable:",
                                         choices=c("New Cases" = "New Cases",
                                                   "New Recoveries" = "New Recoveries",
                                                   "New Deaths" = "New Deaths",
                                                   "New Cases per 100k pop" = "New Cases per 100k pop",
                                                   "New Deaths per 100k pop" = "New Deaths per 100k pop")))
                    )
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
                      box(title="Model Projection",
                          plotOutput("lmeplot"),width=8),
                      box(title="Select Country",
                          selectInput("lmecountry", label = "",
                                      choices=sort(unique(forecast$Country)),
                                      selected= sort(unique(forecast$Country))[1]),width=4)
                    )),
            
## Data Table for viewing data.
            tabItem(tabName = "data",
                    DT::dataTableOutput("data")
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
  output$plotall1 <- renderPlot({
    alldata[which(alldata$Type%in%tot),] %>%  #Filter for cumulative variables.
      ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() + 
      scale_color_manual(values=group.colors) + ylab("Number of People") + 
      labs(title = "Cumulative")
  })
  
  #Plot daily information for all of Africa.
  output$plotall2 <- renderPlot({
    alldata[which(alldata$Type %in% daily),] %>% #Filter daily variables.
      ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
      scale_color_manual(values=group.colors) + ylab("Number of People") + 
      labs(title = "Daily")
  })
  
  # Plot cumulative map for summary page.
  output$summap1 <- renderPlot({ 
    afr %>% 
      left_join(testdata[testdata$Date==max(testdata$Date),],by=c("id"="Country_Region")) %>%
      ggplot(aes_string(fill=paste("`",input$var3,"`",sep=""))) + 
      geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + 
      coord_fixed(ratio=1) +
      xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette = "Reds",direction=1) +
      labs(title = paste("Cumulative Data - ", max(testdata$Date)),subtitle=input$var3)
  })
  
  # Plot daily map for summary page.
  output$summap2 <- renderPlot({
    afr %>% 
      left_join(testdata[testdata$Date==max(testdata$Date),],by=c("id"="Country_Region")) %>%
      ggplot(aes_string(fill=paste("`",input$var4,"`",sep=""))) + 
      geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + coord_fixed(ratio=1) +
      xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette="Reds",direction=1)    +
      labs(title = paste("Daily Changes - ", max(testdata$Date)), subtitle=input$var4)
  })
  
## Output summary text
    output$countrysum <- renderText({
        end <- testdata %>% filter(Date==max(testdata[testdata$Country_Region==input$country,]$Date)&Country_Region==input$country)
        selected <- testdata %>% filter(Date==input$date&Country_Region==input$country)
        
        paste("<h4> ", input$country, "</h3>",
              "<b><h5>On ", max(testdata$Date), ", there have been: \n </h5></b>",
              "<b><h5><font style=color:#9932cc>", end$Confirmed, " Confirmed Cases (", end$`New Cases`," new) </font></h5></b>",
              "<b><h5><font style=color:#00bfff>", end$Recovered, " Recoveries (", end$`New Recoveries`," new) </font></h5></b>",
              "<b><h5><font style=color:#8b0000>", end$Deaths," Deaths (", end$`New Recoveries`," new) </font></h5></b>",
              "<b><h5> In comparison, on ", input$date, ", there were:</h5></b>",
              "<b><h5><font style=color:#9932cc>", selected$Confirmed, " Confirmed Cases (", selected$`New Cases`," new)</font></h5></b>",
              "<b><h5><font style=color:#00bfff>", selected$Recovered, " Recoveries (", selected$`New Recoveries`," new)</font></h5></b>",
              "<b><h5><font style=color:#8b0000>", selected$Deaths," Deaths (", selected$`New Deaths`," new)</font></h5></b>", sep = "")
        
    })

## Plot adjustable COVID data.
    output$ploteach <-  renderPlot({ 
      #Adjust structure of the COVID data.
      plotdata <- testdata %>% pivot_longer(cols=c(all_of(tot),all_of(daily)),
                                        names_to="Type",values_to="Value") 
      
      p1 <-plotdata %>% 
        filter(Country_Region==input$country&Type==input$var) %>% #Filter country and selected cumulative variable.
        ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
        scale_color_manual(values=group.colors) + ylab("Number of People")  + 
        labs(title = "Cumulative Data", subtitle= input$var) + theme(legend.position="none") +
        geom_vline(xintercept=input$date,linetype="dotted")
      
      p2 <- plotdata %>% 
        filter(Country_Region==input$country&Type==input$var2) %>% #Filter country and selected daily variable.
        ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
        scale_color_manual(values=group.colors) + ylab("Number of People") + 
        labs(title = "Daily Data", subtitle = input$var2) + theme(legend.position="none") +
        geom_vline(xintercept=input$date,linetype="dotted")
      
      #Stick both plots together and show the selected country's name in the title.
      grid.arrange(p1,p2,nrow=1,
                   top = textGrob(
                     paste("\nCOVID Over Time in", input$country, "- Individual Variables\n"),
                     gp=gpar(fontsize=20,font=2)))
    })

# Plot cumulative map for dynamic map page.
    output$map1 <- renderPlot({ 
      
      afr %>% 
        left_join(testdata[testdata$Date==input$date1,],by=c("id"="Country_Region")) %>%
        ggplot(aes_string(fill=paste("`",input$var5,"`",sep=""))) + 
        geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + 
        coord_fixed(ratio=1) +
        xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette="Reds",direction=1) +
        labs(title = paste("Cumulative Data - ", input$date1),subtitle=input$var5)
      
    })

## Plot map for daily data on dynamic map page.
    output$map2 <- renderPlot({
      afr %>% 
        left_join(testdata[testdata$Date==input$date1,],by=c("id"="Country_Region")) %>%
        ggplot(aes_string(fill=paste("`",input$var6,"`",sep=""))) + 
        geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + coord_fixed(ratio=1) +
        xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette="Reds",direction=1) +
        labs(title = paste("Daily Changes - ", input$date1), subtitle=input$var6)
    })

## Plot model for IVREG.
    output$plotmodel <- renderPlot({
      modelproj %>% 
        filter(Country_Region==input$modelcountry) %>%
        ggplot() + aes(x=Date) + 
        geom_ribbon(aes(ymin=lower,ymax=upper, fill="Projection Range"),alpha=0.3) +
        geom_line(aes(y=prediction,color="Model Projection"), size = 1.5,linetype="dotted") +
        geom_point(data=testdata[testdata$Country_Region==input$modelcountry,],
          aes(y=Confirmed,color="Confirmed Cases")) + 
        geom_line(aes(y=upper,color="Projection Range")) +
        geom_line(aes(y=lower,color="Projection Range")) + 
        theme_bw() +
        ylab("Cases") + 
        ggtitle("COVID-19 Model Projections",subtitle = input$modelcountry) +
        scale_color_manual(values=modelcolors) + 
        scale_fill_manual(values=modelcolors) +
        labs(color = "Legend") +
        guides(fill=F)
    })

## Plot model for LMER
    output$lmeplot <- renderPlot({
      ggplot() + aes(x=Exposure) +
        geom_ribbon(data=forecast[forecast$Country==input$lmecountry,],
                    aes(ymin=exp(Lower),ymax=exp(Upper),fill="Projection Range"),alpha=0.3) +
        geom_point(data=testdata[testdata$Country_Region==input$lmecountry,],
                   aes(y=Confirmed,color="Confirmed Cases"),color="darkorchid") +
        geom_point(data=forecast[forecast$Country==input$lmecountry,],
                   aes(x=Exposure,y=exp(Est),color="Model Projection")) +
        geom_line(data=forecast[forecast$Country==input$lmecountry,],
                  aes(y=exp(Upper),color="Projection Range")) +
        geom_line(data=forecast[forecast$Country==input$lmecountry,],
                  aes(y=exp(Lower),color="Projection Range")) + 
        ggtitle("COVID-19 Model Projections",subtitle = input$lmecountry) +
        theme_bw() +
        scale_color_manual(values=modelcolors) + 
        scale_fill_manual(values=modelcolors) +
        labs(color = "Legend") + 
        guides(fill=F)
        
    })

## Put up data table.
    output$data <- DT::renderDataTable(testdata)
    
}


shinyApp(ui, server)
