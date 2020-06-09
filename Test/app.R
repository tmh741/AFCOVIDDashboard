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

testdata <- read.csv("testfile.csv")[,-1]
testdata$Date <- mdy(testdata$Date)
testdata$Country_Region <- as.character(testdata$Country_Region)

#Read Population Data
popdata <- read.csv("popdata.csv")
popdata$Location <- as.character(popdata$Location)
popdata[popdata$Location=="Congo",]$Location <- "Congo (Brazzaville)"
popdata[popdata$Location=="Democratic Republic of the Congo",]$Location <- "Congo (Kinshasa)"
popdata[popdata$Location=="Côte d'Ivoire",]$Location <- "Cote d'Ivoire"
popdata[popdata$Location=="United Republic of Tanzania",]$Location <- "Tanzania"
popdata[popdata$Location=="Cabo Verde",]$Location <- "Cape Verde"


testdata <- left_join(testdata,popdata,by=c("Country_Region"="Location"))

testdata <- testdata %>% group_by(Country_Region) %>% 
    mutate(`Case Fatalities` = Deaths/Confirmed,
           `Deaths per 100k pop` = Deaths/PopTotal*100000,
           `Cases per 100k pop` = Confirmed/PopTotal*100000,
           `New Cases` = Confirmed-lag(Confirmed,default=0),
           `New Deaths` = Deaths-lag(Deaths,default=0),
           `New Recoveries` = Recovered-lag(Recovered,default=0),
           `New Deaths per 100k pop` = `New Deaths`/PopTotal*100000,
           `New Cases per 100k pop` = `New Cases`/PopTotal*100000) %>% ungroup()
current <- testdata %>% filter(Date==max(testdata$Date))

countrylist <- sort(unique(testdata$Country_Region))
group.colors = c(Deaths="red4",`New Deaths`="red4",
                 Recovered="deepskyblue",
                 `Case Fatalities` = "red4", `Deaths per 100k pop` = "red4",
                 `Cases per 100k pop` = "darkorchid",
                 `New Deaths per 100k pop` = "red4",
                 `New Cases per 100k pop` = "darkorchid",
                 `New Recoveries`="deepskyblue",
                 Confirmed="darkorchid",`New Cases`="darkorchid")

tot = colnames(testdata)[c(2:4,7:9)]
daily = colnames(testdata)[c(10:14)]

data("wrld_simpl")
afr=wrld_simpl[wrld_simpl$REGION==2,]
afr@data$id <- afr@data$NAME
afr <- fortify(afr,region="id")
afr[afr$id=="Swaziland",]$id <- "Eswatini"
afr[afr$id=="Congo",]$id <- "Congo (Brazzaville)"
afr[afr$id=="Democratic Republic of the Congo",]$id <- "Congo (Kinshasa)"
afr[afr$id=="United Republic of Tanzania",]$id <- "Tanzania"
afr[afr$id=="Libyan Arab Jamahiriya",]$id <- "Libya"

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

ui <- dashboardPage(skin = "green",
    dashboardHeader(title = "COVID-19 in Africa - Data Visualizer",
                    titleWidth = 350),
    dashboardSidebar(
        sidebarMenu(
            menuItem("COVID Exploration", tabName = "covid", icon = icon("chart-bar"),
                     menuSubItem("All of Africa",tabName="summary"),
                     menuSubItem("Country Analysis",tabName="country"),
                     menuSubItem("Dynamic Map Explorer",tabName="mapexp")),
            menuItem("Model Projections", tabName = "model", icon = icon("chart-line")),
            menuItem("Data Table", tabName = "data", icon = icon("table")),
            menuItem("About", tabName = "about", icon = icon("question-circle")),
            menuItem("Contact Us",
                       href="https://sites.bu.edu/covid-19-in-africa/partner-with-us/",
                     icon = icon("address-card"))
            
        )
    ),
    dashboardBody(
        tabItems(
            # COVID
            tabItem(tabName = "summary",
                    
                    #Row 1
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
                                           sum(current$Deaths)," total deaths </b> (", sum(current$`New Deaths`), " new)</font>", sep="")),
                                htmlOutput("summarytext")
                                
                            )
                            
                        )
                        

                    ),
                    
                    #Row 2
                    fluidRow(
                        box(title="COVID in Africa - Cumulative",
                            plotOutput("plotall1")),
                        box(title="COVID in Africa - Daily",
                            plotOutput("plotall2"))
                    ),
                    
                    #Row 3
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
                                                   "Cases per 100k pop" = "Cases per 100k pop")),width=3)
                    ),
                    
                    #Row 4
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
            
            tabItem(tabName = "country",
                    fluidRow(
                        
                        box(title = tags$p("Change Country and Date",style="font-size:150%"),
                            selectInput("country", label = "Select Country",
                                        choices=sort(unique(testdata$Country_Region)),selected= sort(unique(testdata$Country_Region))[1]),
                            dateInput("date", label= "Select Date", 
                                      min = min(testdata$Date),max = max(testdata$Date),
                                      value = max(testdata$Date))
                            ),

                        box(tags$p("Summary",style="font-size:170%"),
                            htmlOutput("countrysum"))
                        
                        ),

                        fluidRow(
                        box(title = "COVID by Country",
                               plotOutput("ploteach"),
                               width=8),
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
            
            #Dynamic Maps
            
            tabItem(tabName="mapexp",
                    fluidRow(
                    box(tags$p("Select Date",style="font-size:150%"),
                        dateInput("date1", label= "Select Date", 
                                  min = min(testdata$Date),max = max(testdata$Date),
                                  value = max(testdata$Date)))
                    ),
                    
                    # Row 2
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
                    
                    #Row 3
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
            
            # Model Projections
            tabItem(tabName = "model",
                    h5("This page will include COVID projections using models. Will be updated soon!")
            ),
            
            # Data Table
            tabItem(tabName = "data",
                    DT::dataTableOutput("data")
            ),
            
            # About Us
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
                                   HTML("<h5> <b>'COVID Africa - Summary'</b> summarizes the most recent COVID data with texts 
                                   and graphics. You can change the variables in the map using the menus on the right.</h5>"),
                                   HTML("<h5> <b>'Country Analysis'</b> lets you more freely explore COVID patterns over time 
                                        in each country. You can change the country on the top, and look more closely at some 
                                        basic information for some dates. You can also choose which variables to view in the 
                                        menu on the right.</h5>"),
                                   HTML("<h5> <b>'Dynamic Maps'</b> lets you visualize more data with the maps. You can 
                                        change date using the bar above, and like before, select variables with the 
                                        menus on the left.</h5>"),
                                   HTML("<br><center><h3> About the Data </h3></center>"),
                                   HTML("The used for this tool was downloaded from the "),
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
            
            #Contact Us
            tabItem(tabName = "contact",
                    h2("Contact Info")
            )
        )
    )
)

server <- function(input, output) {
    
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
    
    
    output$plotall1 <- renderPlot({
        
      alldata[which(alldata$Type%in%tot),] %>% 
            ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
            scale_color_manual(values=group.colors) + ylab("Number of People") + 
            labs(title = "Cumulative")
       
    })
    
    output$plotall2 <- renderPlot({
        alldata[which(alldata$Type %in% daily),] %>%
            ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
            scale_color_manual(values=group.colors) + ylab("Number of People") + 
            labs(title = "Daily")
    })
    
    output$plot <-  renderPlot({ 
        test <- testdata %>% pivot_longer(cols=c(tot[1:3],daily[1:3]),
                                          names_to="Type",values_to="Value") 
        
        
        p1 <- test[which(test$Type%in%tot),] %>% filter(Country_Region==input$country) %>%
            ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
            scale_color_manual(values=group.colors) + ylab("Number of People") + 
            labs(title = "Cumulative Data") + geom_vline(xintercept=input$date,linetype="dotted")
        
        p2 <- test[which(test$Type%in%daily),] %>% filter(Country_Region==input$country) %>%
            ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
            scale_color_manual(values=group.colors) + ylab("Number of People") + 
            labs(title = "Daily Data") + geom_vline(xintercept=input$date,linetype="dotted")
        
        grid.arrange(p1,p2,nrow=1, top = textGrob(paste("\nCOVID Over Time in", input$country, "- All Variables \n"),gp=gpar(fontsize=20,font=2)))
    })
    
    output$ploteach <-  renderPlot({ 
        test <- testdata %>% pivot_longer(cols=c(all_of(tot),all_of(daily)),
                                          names_to="Type",values_to="Value") 
        
        p1 <-test %>% 
            filter(Country_Region==input$country&Type==input$var) %>%
            ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
            scale_color_manual(values=group.colors) + ylab("Number of People")  + 
            labs(title = "Cumulative Data", subtitle= input$var) + theme(legend.position="none") +
            geom_vline(xintercept=input$date,linetype="dotted")
        
        p2 <- test %>% 
            filter(Country_Region==input$country&Type==input$var2) %>%
            ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
            scale_color_manual(values=group.colors) + ylab("Number of People") + 
            labs(title = "Daily Data", subtitle = input$var2) + theme(legend.position="none") +
            geom_vline(xintercept=input$date,linetype="dotted")
        
        grid.arrange(p1,p2,nrow=1,top = textGrob(paste("\nCOVID Over Time in", input$country, "- Individual Variables\n"),gp=gpar(fontsize=20,font=2)))
    })
    
    output$summap1 <- renderPlot({ 
        
        afr %>% 
            left_join(testdata[testdata$Date==max(testdata$Date),],by=c("id"="Country_Region")) %>%
            ggplot(aes_string(fill=paste("`",input$var3,"`",sep=""))) + 
            geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + 
            coord_fixed(ratio=1) +
            xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette = "Reds",direction=1) +
            labs(title = paste("Cumulative Data - ", max(testdata$Date)),subtitle=input$var3)
        
    })
    
    output$summap2 <- renderPlot({
        
        afr %>% 
            left_join(testdata[testdata$Date==max(testdata$Date),],by=c("id"="Country_Region")) %>%
            ggplot(aes_string(fill=paste("`",input$var4,"`",sep=""))) + 
            geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + coord_fixed(ratio=1) +
            xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette="Reds",direction=1)    +
            labs(title = paste("Daily Changes - ", max(testdata$Date)), subtitle=input$var4)
        
        
    })
    
    
    output$map1 <- renderPlot({ 
        
        afr %>% 
            left_join(testdata[testdata$Date==input$date1,],by=c("id"="Country_Region")) %>%
            ggplot(aes_string(fill=paste("`",input$var5,"`",sep=""))) + 
            geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + 
            coord_fixed(ratio=1) +
            xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette="Reds",direction=1) +
            labs(title = paste("Cumulative Data - ", input$date1),subtitle=input$var5)
        
    })
    
    output$map2 <- renderPlot({
        
       afr %>% 
            left_join(testdata[testdata$Date==input$date1,],by=c("id"="Country_Region")) %>%
            ggplot(aes_string(fill=paste("`",input$var6,"`",sep=""))) + 
            geom_map(map=afr,aes(map_id=id,x=long,y=lat),color="black",size=0.3) + coord_fixed(ratio=1) +
            xlab("") + ylab("") + theme_bw() + scale_fill_distiller(palette="Reds",direction=1) +
            labs(title = paste("Daily Changes - ", input$date1), subtitle=input$var6)
      
        
    })
    
    output$data <- DT::renderDataTable(testdata)
    
}

shinyApp(ui, server)
