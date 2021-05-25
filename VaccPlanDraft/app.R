library(shiny)
library(shinydashboard)
library(DT)

popdata = read.csv("popdata.csv")
staffdata = read.csv("Book1.csv")
colnames(staffdata)[2:5]  = c("Shift_4Station","Day_4Station","Shift_8Station","Day_8Station")
staffdata[staffdata$Position=="Supply <anager",]$Position = "Supply Manager"
staffdata[14,c("Shift_4Station")] =1
staffdata[15,c("Shift_4Station")] =1
staffdata[15,c("Shift_8Station")] =1



body <- dashboardBody(
    fluidRow(
        column(width=12,
               box(
                   title = "Target Number", width = NULL, solidHeader = FALSE, status = "primary",
                selectInput("country",label="Select Country:",choices=unique(popdata$Location)),
                sliderInput("percent",label="Target % (of total population):",min=70,max=100,value=80,step=1),
                textOutput("target")
               )
        )
    ),
    
    fluidRow(
        column(width=6,
               box(
                   title = "Inputs", width = NULL, solidHeader = T, status="primary",
                   numericInput("workers", label= "Number of vaccine workers:", value=1000),
                   numericInput("hospitals",label = "Number of Hospitals:",value=10)
               )
        ),
        column(width=6,
               box(
                   title = "Time Calculator", width=NULL, solidHeader=T, status = "primary",
                   numericInput("rate",label="Vaccines per Nurse per Day", value = 15, max = 20),
                   textOutput("time")
               )
        )
    ),
    
    fluidRow(
        column(width=4,
               box(title = "Finance Inputs", width=NULL,solidHeader=T,status="primary",
               numericInput("unitsneeded",label= "Storage Units Needed per Hospital",value=10,max = 20),
               numericInput("workersneeded",label = "Workers Needed per Hospital",value=100),
               numericInput("current",label="Current Deployable Vaccine Workers",value=80),
               )),
        column(width=4,
               box(title="Price Inputs", width=NULL,solidHeader=T,status="primary",
               numericInput("storeprice",label="Storage Price",value=10000),
               numericInput("workerwage",label="Worker Wage per hour",value=20),
               numericInput("transportcost",label= "Transport Cost",value=40000),
               numericInput("order",label="Vaccine Order Cost",value=20000)
               )),
        column(width=4,
               box(title="Finance Outputs",width=NULL,solidHeader=T,status="primary",
               textOutput("storecost"),
               textOutput("workneed"),
               textOutput("wageprice"),
               textOutput("totalprice")))
    )
    
)

body2 = dashboardBody(
    fluidRow(
        column(width=12,
               box(
                   title = "Target Number", width = NULL, solidHeader = FALSE, status = "primary",
                   selectInput("country",label="Select Country:",choices=unique(popdata$Location)),
                   sliderInput("percent",label="Target %:",min=70,max=100,value=80,step=1),
                   textOutput("target")
               )
        )
    ),
    
    fluidRow(
        column(width=12,
            box(title = "Station Layout", width=NULL,solidHeader=T,status="primary",collapsible=T,
               dataTableOutput("staff")))
    ),
    
    fluidRow(
        column(width=12,
               tabBox(title = "", width=12,
                   tabPanel(title = "Input Clinic Number",
                            numericInput("numFour", label = "Number of 4-Station Clinics: ", value = 10),
                            numericInput("numEight", label = "Number of 8-Station Clinics: ",value = 10)),
                   tabPanel(title = "Adjust Translators", 
                   radioButtons(inputId = "StationsPerClinic",
                                label = "Stations per Clinic",
                                choices = c("4 Stations" = "Shift_4Station",
                                            "8 Stations" = "Shift_8Station")),
                   numericInput("numTranslators4",
                                label = "Number of Translators at a 4 Station Clinic (recommended: one per language)",
                                value = 1),
                   numericInput("numTranslators8",
                                label = "Number of Translators at an 8 Station Clinic (recommended: one per language)",
                                value = 2)),
                   tabPanel(title="Input Worker Wages",
                            numericInput("wageForm", label = "Form Distributor Wage",value = 20),
                             numericInput("wageOrient", label = "Orientation/Information Wage",value = 20),
                             numericInput("wageScreener", label = "Medical Screener Wage",value = 20),
                             numericInput("wageFlow", label = "Clinic Flow/Form Helper Wage",value = 20),
                             numericInput("wageVacc", label = "Vaccinator Wage",value = 20),
                             numericInput("wagePrep", label = "Vaccine Preparer/Supplier Wage",value = 20),
                             numericInput("wageExit", label = "Exit Review Wage",value = 20),
                             numericInput("wageEntry", label = "Records/Data Entry Wage",value = 20),
                             numericInput("wageClinMan", label = "Clinic Manager Wage",value = 20),
                             numericInput("wageSuppMan", label = "Supply Manager Wage",value = 20),
                             numericInput("wageSecurity", label = "Security Wage",value = 20),
                             numericInput("wageTranslator", label = "Translator Wage",value = 20),
                             numericInput("wageFloat", label = "Float Staff Wage",value = 20),
                             numericInput("wageEMT", label = "EMT Wage",value = 20),
                             numericInput("wageIT", label = "IT Wage",value = 20)),
                   
                             tabPanel(title = "Additional Costs",
                             numericInput("costTransport", label = "Transport Cost", value= 10000),
                             numericInput("costVacc", label = "Vaccine Cost",value=20),
                             numericInput("costMat",label = "Material Cost", value = 12),
                             numericInput("numVacc",label = "Vaccine Number", value = 1000)
                             
                   )
               )
        )

                  
    ),
    
    fluidRow(
        valueBoxOutput("fourDistribution"),
            
        valueBoxOutput("eightDistribution"),
            
        valueBoxOutput("totalDistribution")
    ),
    fluidRow(
        
        valueBoxOutput("transportCost"),
        valueBoxOutput("vaccCost"),
        valueBoxOutput("personellCost"),
        valueBoxOutput("totalCost"),
        infoBox(numericInput(inputId="vaccGiven",label="Vaccines Distributed",value=0)),
        valueBoxOutput("timeTo")
        
    )
)

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
    dashboardHeader(title = "Column layout"),
    dashboardSidebar(),
    body2
)
server = function(input, output) {
    targetVacc = reactive({
        popdata[popdata$Location==input$country,2]*input$percent*10
    })
    output$target = renderText(targetVacc())
     
    # output$time = renderText(targetVacc()/(input$rate*input$workers))
    # output$storecost = renderText(input$storeprice*input$unitsneeded)
    # output$workneed = renderText(input$workersneeded-input$current)
    # output$wageprice = renderText((input$workersneeded-input$current)*input$workerwage)
    # output$totalprice = renderText(input$storeprice*input$unitsneeded + 
    #                                    (input$workersneeded-input$current)*input$workerwage + 
    #                                    input$transportcost + 
    #                                    input$order)
    # 
    
     staffTable = reactive({
         stafftable = staffdata
         stafftable[12,]$Shift_4Station = input$numTranslators4
         stafftable[12,]$Shift_8Station = input$numTranslators8
         return(stafftable)
     })

    
    displayTable = reactive({
        display = staffTable()[,c("Position",input$StationsPerClinic)]
        colnames(display)[2] = "Workers"
        return(display)
    })
    
    output$staff = renderDataTable(datatable(displayTable(), options = list(dom = 't',pageLength=15))
)
    
    fourNumber = reactive({
        input$numFour*1900
    })
    
    eightNumber = reactive({
        input$numEight*5000
    })
    
    
    output$fourDistribution = renderValueBox({
        infoBox(
            "4-Station", paste("~", fourNumber()," Vaccines/Day"),
            color="purple"
                )
                                            })
    output$eightDistribution = renderValueBox({
        infoBox(
            "8-Station", paste("~", eightNumber()," Vaccines/Day"), 
            color="yellow"
        )
        })
    output$totalDistribution = renderValueBox({
        infoBox(
        "Total", paste("~",fourNumber() + eightNumber()," Vaccines per Day"),icon =icon("plus",lib="glyphicon"),
        color="blue"
        )
        })
    
    output$transportCost = renderValueBox({
        infoBox(
        "Transport", paste(input$costTransport), icon =icon("usd",lib="glyphicon"),
        color="red"
        )
        })
    output$vaccCost = renderValueBox({
        infoBox(
        "Materials + Vaccine",paste(input$numVacc*(input$costVacc+input$costMat)), icon =icon("usd",lib="glyphicon"),
        color="red"
        )
        })
    
    totalPeople = reactive({
        as.numeric(staffTable()$Shift_4Station)*input$numFour + as.numeric(staffTable()$Shift_8Station)*input$numEight
    })
    
    output$personellCost = renderValueBox({
        infoBox("Clinic Staff",paste(sum(totalPeople()*c(input$wageForm,
                                                      input$wageOrient,
                                                      input$wageScreener,
                                                      input$wageFlow,
                                                      input$wageVacc,
                                                      input$wagePrep,
                                                      input$wageExit,
                                                      input$wageEntry,
                                                      input$wageClinMan,
                                                      input$wageSuppMan,
                                                      input$wageSecurity,
                                                      input$wageTranslator,
                                                      input$wageFloat,
                                                      input$wageEMT,
                                                      input$wageIT))*8), icon =icon("usd",lib="glyphicon"),
                                                      color="red")
        })
    
    output$totalCost = renderValueBox({
        infoBox(
            "Total Cost", paste(input$costTransport + input$numVacc*(input$costVacc+input$costMat) + sum(totalPeople()*c(input$wageForm,
                                                                                                                                                  input$wageOrient,
                                                                                                                                                  input$wageScreener,
                                                                                                                                                  input$wageFlow,
                                                                                                                                                  input$wageVacc,
                                                                                                                                                  input$wagePrep,
                                                                                                                                                  input$wageExit,
                                                                                                                                                  input$wageEntry,
                                                                                                                                                  input$wageClinMan,
                                                                                                                                                  input$wageSuppMan,
                                                                                                                                                  input$wageSecurity,
                                                                                                                                                  input$wageTranslator,
                                                                                                                                                  input$wageFloat,
                                                                                                                                                  input$wageEMT,
                                                                                                                                                  input$wageIT))),
        icon =icon("usd",lib="glyphicon"), color="red"
        )
        })
    
    output$timeTo = renderValueBox({
        infoBox(
            "Days to Goal:",
            paste(round(targetVacc()/(fourNumber() + eightNumber()-input$vaccGiven))), icon = icon("plus",lib="glyphicon"),color="fuchsia"
        )
    })
}

# Preview the UI in the console
shinyApp(ui = ui, server = server)

