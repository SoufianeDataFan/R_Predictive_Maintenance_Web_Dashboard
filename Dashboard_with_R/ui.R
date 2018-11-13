setwd("Path/to/Shiny Interface Beta 06_04_2017/data")
library(ggplot2)
library(xts)
library(dygraphs)
library(DT)
library(shiny)
library(shinydashboard)
library(bubbles)
# devtools::install_github("jcheng5/bubbles")

# Liste des equipements 
Equipment=read.csv("ID Equipement.csv",header = F,  sep = ";")

# Tableau des donnees capteurs dpour ces equipements 

Data_Sensors=read.csv("Data_sensors.csv", sep = ",")
Data_Sensors=Data_Sensors[,-1]
Data_Sensors$Date=as.POSIXct(Data_Sensors$Date, format = "%Y-%m-%d %H:%M:%S")
TTAF=read.csv("TTF.csv",header = T,  sep = ";")
anchor <- tags$a(href='http://http://www.groupLink.ma',
                 tags$img(src='Logo.png', height='60', width='50'),
                 'project name')
dashboardPage(skin = "blue", 
              
             
####################### Header setting #########################################
              dashboardHeader(title = "Maitenance Solution", titleWidth = 350,
                              tags$li(a(href = 'www.groupLink.ma',
                                        img(src = 'Logo.png',
                                            title = "Company Home", height = "30px"),
                                        style = "padding-top:10px; padding-bottom:10px;"),
                                      class = "dropdown")
                            
                              ), 
              dashboardSidebar(
####################### Sidebar / Menu items #########################################
                sidebarMenu(
                  sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
                  h5("Main Navigation"),
                  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                  h5("Rooms"),
                  menuItem("Analysis", icon = icon("pie-chart"), tabName = "stats",
                           
                    
                           selectInput("machine",
                                       "Equipment name:",
                                       c("",
                                         unique(as.character(Data_Sensors$Equipment))), multiple = F), 
                           menuSubItem('Data Stream',
                                    tabName = 'a',
                                    icon = icon('line-chart')
                                    ), 
                           menuSubItem("Data raw",
                                       tabName = "DataRaw",
                                       icon = icon("bars" )), 
                           menuSubItem('Equipment overview',
                                                   tabName = 'overview',
                                                   icon = icon('folder-open-o')
                           )),
                  br(),
                  menuItem("Maroc CHIMIE- General KPI", icon = icon("industry"), tabName = "Charts"),
                  br(), 
                  br(),
                  h5("More information"),
                  menuItem("User Guide", icon = icon("info-circle"), tabName = "info",
                           badgeLabel = "new", badgeColor = "green"),
                  br(),
                  br(),
                  br(),
                  br(),
                  actionButton("count", "Refresh Data")
                )
              ),

####################### Dashb.Body#########################################

              dashboardBody(
      
                includeCSS("Pth/to/Shiny Interface Beta 06_04_2017/Dahsborad/www/custom.css"),
                tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ), 
                
                
                tabItems(
                    tabItem("dashboard",
                            fluidRow(
                              valueBoxOutput("nextFailure"),
                              valueBoxOutput("efficiency"),
                              valueBoxOutput("CriticalEquips")
                            ),
                            fluidRow(
                              
                              box(
                                width = 8, status = "primary", solidHeader = TRUE,
                                title = "Equipment Criticality Overview",
                                value = tags$p(style = "font-size: 10px;", "Bubbles"),
                                bubblesOutput("packagePlot", width = "100%", height = 600),
                                collapsible = T
                              ),
                              box(
                                width = 4, status = "info", collapsible = T , solidHeader = TRUE,
                                title = "Recommendations for maintenance actions ",
                                tableOutput("packageTable")
                              )
                            )
                    )
                    ,
                    tabItem(tabName = "overview",
                          h2("Equipment overview"),
                          fluidRow(
                            valueBoxOutput("equipment"),
                            valueBoxOutput("MTBF"),
                            valueBoxOutput("TTR")),
                        #   fluidRow(
                        #     
                        #     valueBoxOutput("TurnOff"), 
                        #     valueBoxOutput("Date de la mise en service"),
                        #     valueBoxOutput("TurnOn")
                        # ),

                          
                          fluidRow(
                            valueBoxOutput("VitesseRotation"),
                            valueBoxOutput("MachineClass"),
                            valueBoxOutput("Diametre")), 
                            fluidRow(
                              valueBoxOutput("UpTime"),
                              valueBoxOutput("TTF"),
                              infoBoxOutput("Temperature"))
                        # box(width = 4, status = "info", collapsible = T , solidHeader = TRUE,
                        #     title = "Choisir l'horizon du temps  ",
                        #   numericInput("n", "Prevoir les pannes dans", 5),
                        #   actionButton("go", "Go")
                        #  )
                        
                          # ,
                          # valueBoxOutput("TurnOnOff")

                          ),
                          # ),
                  
                  tabItem(tabName = 'a',
                          h2("DATA STREAM"),
                    fluidRow(
                        box(dygraphOutput("dygraph"), width = 9),
                        fluidRow(
                          h3("Equipement settings"),
                          box(selectInput("Paramerter", "Indicator",  names(Data_Sensors)[-c(1,2)]),width = 3, status = "success",solidHeader = TRUE,
                              
                              collapsible = T, title = "Variable"), 
                          # br(),
                          box(
                            
                            textInput("fromDatetime", "From:", value = "2014-12-01 00:00:00" ),
                              br(),
                              textInput("ToDatetime", "To:", value = "2014-12-30 00:00:00" ), 
                              width = 3, status = "success",solidHeader = TRUE,
                              collapsible = T, title = "Time frame"), 
                          # br(),
                          # box(textInput("VitesseRotation", "Vitesse de rotation", value = "000" ),
                          #     # br(),
                          #     textInput("ToDatetime", "Diameter", value = "50" ), 
                          #     width = 3, status = "success",solidHeader = TRUE,
                          #     collapsible = T, title = "Reglages"),
                          
                          # 
                          # dateRangeInput('dateRange',
                          #                label = 'Date range input: yyyy-mm-dd',
                          #                start = Sys.Date() - 2, end = Sys.Date() + 2
                          # ),
                          
                          
                          br(), width = 3)
                          )),
                  
                  
                  tabItem(tabName = "DataRaw", 
                          h2("Data raw from sensors"), 
                          fluidRow(dataTableOutput("dt"))
                          
                          ), 
                  tabItem(tabName ="info", 
                          h2("How this App works ?"))
                  
                )
              )
)