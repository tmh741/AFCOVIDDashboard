library(shiny)
library(tidyverse)
library(lubridate)
library(magrittr)
library(maptools)
library(maps)
library(gridExtra)
library(grid)
require(rgeos)

#Read COVID Data
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



ui <- fluidPage(
    
    titlePanel("Africa COVID-19 - Data Visualization Prototype"),

    sidebarLayout(
        sidebarPanel(
            h2("Variables"),
            h5("For Country Data (Selected and All Variables) and Maps"),
            selectInput("country", label = "Select Country",
                        choices=sort(unique(testdata$Country_Region)),selected= sort(unique(testdata$Country_Region))[1]),
            sliderInput("date", label= "Select Date", 
                        min = min(testdata$Date),max = max(testdata$Date),
                        value = max(testdata$Date)),
            
            
            h5("For Country Data - Selected Variables and Maps"),
            radioButtons("var","Cumulative Variable:",
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
            h2("Summary"),
            h4("Africa Total"),
            h5(paste("As of ", max(testdata$Date),", there are:",sep="")),
            tags$div(
                HTML(paste("<font style=color:#9932cc> <b>",
                           sum(current$Confirmed),"total COVID cases </b> </font>")),
                tags$br(),
                HTML(paste("\n<font style=color:#00bfff> <b>",
                           sum(current$Recovered),"total recoveries </b> </font>")),
                tags$br(),
                HTML(paste("\n<font style=color:#8b0000> <b>",
                           sum(current$Deaths),"total deaths </b> </font><br>")),
                htmlOutput("textall")
                
            )
        , width=3),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Country Data - Selected Variables", plotOutput("ploteach"),
                         HTML("<br><center><h3> Usage </h3></center>"),
                         HTML("These graphs displays cumulative and daily COVID-19 metrics 
                               for a selected country, from the first day until now. 
                               You can change date, country, and displayed variables in the menu on the left.
                               Some basic numbers will also be displayed at the bottom of the bar!")
                         ),
                tabPanel("Country Data - All Variables",plotOutput("plot"),
                         HTML("<br><center><h3> Usage </h3></center>"),
                         HTML("These graphs displays cumulative and daily COVID-19 metrics 
                               for a selected country, from the first day until now.
                               You can change country and date selected in the menu on the left.
                               Some basic numbers will also be displayed at the bottom of the bar!")
                         ),
                tabPanel("COVID Across Africa", plotOutput("plotall"),
                         HTML("<br><center><h3> Usage </h3></center>"),
                         HTML("These graphs displays cumulative and daily COVID-19 metrics 
                               for all of Africa. All metrics are displayed. You can use the bar
                               on the left to change the date.")
                         ),
                
                tabPanel("Maps",plotOutput("map"),
                         HTML("<br><center><h3> Usage </h3></center>"),
                         HTML("This maps displays COVID metrics in Africa on a specific date. The left
                         is colored by a cumulative metric, and the right is colored by a daily metric.
                               You can change the selected metrics and date using the menu on the left.
                               Some basic numbers will also be displayed at the bottom of the bar!")
                         ),
                
                tabPanel("Data Table",DT::dataTableOutput("data")),
                
                tabPanel("About",
                         fluidRow( 
                             column(2),
                             column(7,
                                    div(
                                        HTML("<br><center><h2> About </h2></center>"),
                                        HTML("<h5>This is an interactive tool that visualizes 
                                   developing COVID data in Africa, and can help
                                   users explore patterns more closely in African countries.<h5>"),
                                        HTML("<h5>This App is currently under construction and will
                                  receive changes to UI and additional features.</h5>"),
                                        HTML("<br><center><h3> How to Use </h3></center>"),
                                        HTML("<h5>Use the above tabs to look at various graphical information.
                                  Variables can be changed using the sidebar on the left, and will
                                  influence the information displayed. </h5>"),
                                        HTML("<br><center><h3> Data </h3></center>"),
                                        tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6",
                                               "John Hopkin's University COVID-19 Dashboard"),
                                        HTML("<br><center><h4> Contact </h4></center>"),
                                        HTML("tmh741@bu.edu")
                                        )),
                             column(1)
                         )     
                )
                
            ),
            width = 9
        )
    )
)




server <- function(input, output) {

    #TEXT
    output$textall <- renderText({
        end <- testdata %>% filter(Date==max(testdata[testdata$Country_Region==input$country,]$Date)&Country_Region==input$country)
        selected <- testdata %>% filter(Date==input$date&Country_Region==input$country)
                                   
                                   paste("<br><h4> ", input$country, "</h3>",
                                         "<b><h5>On", max(testdata$Date), "there have been: \n </h5></b>",
                                         "<b><h5><font style=color:#9932cc>", end$Confirmed, "Confirmed Cases </font></h5></b>",
                                         "<b><h5><font style=color:#00bfff>", end$Recovered, "Recoveries </font></h5></b>",
                                         "<b><h5><font style=color:#8b0000>", end$Deaths,"Deaths </font></h5></b>",
                                         "<b><h5> In comparison, on", input$date, "there were:</h5></b>",
                                         "<b><h5><font style=color:#9932cc>", selected$Confirmed, "Confirmed Cases </font></h5></b>",
                                         "<b><h5><font style=color:#00bfff>", selected$Recovered, "Recoveries </font></h5></b>",
                                         "<b><h5><font style=color:#8b0000>", selected$Deaths,"Deaths </font></h5></b><br>")
                                   
                                   })
    
    output$plotall <- renderPlot({
        
       p1 <- alldata[which(alldata$Type%in%tot),] %>% 
           ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
           scale_color_manual(values=group.colors) + ylab("Number of People") + 
           labs(title = "Cumulative") + geom_vline(xintercept=input$date,linetype="dotted")
       
       p2 <- alldata[which(alldata$Type %in% daily),] %>%
           ggplot() + aes(x=Date,y=Value,color=Type) + geom_point()  + geom_line() +
           scale_color_manual(values=group.colors) + ylab("Number of People") + 
           labs(title = "Daily") + geom_vline(xintercept=input$date,linetype="dotted")
       
       grid.arrange(p1,p2,nrow=1,top= textGrob("\nCOVID across Africa \n",gp=gpar(fontsize=20,font=2)))
        
    })

    output$plot <-  renderPlot({ 
        test <- testdata %>% pivot_longer(cols=c(all_of(tot),all_of(daily)),
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
    
    output$map <- renderPlot({ 
        
        p1 <- afr %>% 
            left_join(testdata[testdata$Date==input$date,],by=c("id"="Country_Region")) %>%
            ggplot(aes_string(fill=paste("`",input$var,"`",sep=""))) + 
            geom_map(map=afr,aes(map_id=id,x=long,y=lat)) + coord_fixed(ratio=1) +
            xlab("") + ylab("") + theme_bw() + scale_fill_viridis_c() +
            labs(title = paste("Cumulative Data"),subtitle=input$var)
        
        p2 <- afr %>% 
            left_join(testdata[testdata$Date==input$date,],by=c("id"="Country_Region")) %>%
            ggplot(aes_string(fill=paste("`",input$var2,"`",sep=""))) + 
            geom_map(map=afr,aes(map_id=id,x=long,y=lat)) + coord_fixed(ratio=1) +
            xlab("") + ylab("") + theme_bw() + scale_fill_viridis_c()    +
            labs(title = paste("Daily Changes"), subtitle=input$var2)
            
            grid.arrange(p1,p2,nrow=1, top = textGrob(paste("\n COVID Across Africa as of", input$date, "\n"),gp=gpar(fontsize=20,font=2)))
        
  
    })
    
    output$data <- DT::renderDataTable(testdata)
    
    }

shinyApp(ui = ui, server = server)
