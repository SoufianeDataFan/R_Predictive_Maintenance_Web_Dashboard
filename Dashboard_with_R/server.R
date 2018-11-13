
  #####################################################################################################################
  ########################################## R-shiny App for Predicive Maintenance ####################################
  #####################################################################################################################
  ########################################## Authors  Soufiane CHAMI ###############################
  #####################################################################################################################
  ########################################## Date : 06/April/2017 #####################################################
  #####################################################################################################################




######################################### check packages and Install the missing one #########################################
# devtools::install_github("jcheng5/bubbles")
# install.packages(c("ggplot2", "xts","dygraphs","DT","zoo","data.table","lubridate","dplyr","dtplyr", "reshape2",
#                    "dataQualityR", "caret", "compare", "psych", "randomForest", "nnet", "e1071", "rpart","tree",
#                    "xgboost",
#                    "plyr", 
#                    "gbm", "party", 
#                    "neuralnet", 
#                    "survival", 
#                    "randomForestSRC",
#                    "shinydashboard",
#                    "shiny", 
#                    "xts"
#                    ))
#####################################################################################################################

function(input, output) {
  set.seed(122)
  library(bubbles)
  library(ggplot2)
  library(xts)
  library(dygraphs)
  library(DT)
  library(zoo)
  library(data.table)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(dtplyr)
  library(reshape2)
  library(dataQualityR)
  library(caret)
  library(compare)
  library(psych) 
  library(randomForest)
  library(nnet)
  library(e1071)
  library(rpart)
  library(tree)
  library(xgboost)
  library(plyr)
  library(gbm)
  library(party)
  library(neuralnet)
  library(survival)
  library(rms)
  library(randomForestSRC)
  library(party)
  library(prodlim)
  library(xts)
  library(dygraphs)
  library(DT)

  ####################################### Working directories#####################
  
  path_parent="Path/to/Shiny Interface Beta 06_04_2017"
  path_data=paste(path_parent,"/data/",sep="")
  path_scripts=paste(path_parent,"/Scripts/",sep="")
  path_results=paste(path_parent,"/Results/",sep="")
  setwd("Path/to/Shiny Interface Beta 06_04_2017/data") 
  
  ######################################### Importing the equipement failures history#####################
  # Liste des equipements 
  TTAF=read.csv("TTF.csv",header = T,  sep = ";")
  TTF=TTAF$TTF
  Equipment=read.csv("ID Equipement.csv",header = T,  sep = ";")
  PanneID=character(nrow(Equipment))
  for(i in 1:nrow(Equipment)){
    PanneID[i]=paste(Equipment$Equipment[i],"_Panne.csv", sep = "")
  }  
  TTF_date=TTF+Sys.Date()
  Equipment=cbind(Equipment,PanneID, TTF, TTF_date) 
  Equipment$TTF_date=format(Equipment$TTF_date)
  Equipment=Equipment[order(Equipment$TTF_date, decreasing = F),, drop=F]
  S=rep(0, length(Equipment$TTF))
  for(i in 1:length(Equipment$TTF)){
    S[i]=1-(Equipment$TTF[i]/100)
  }
  Equipment=cbind(Equipment, S)
  # colnames(Equipment)=c("Equipment", "ID", "PanneID", "TTF", "TTF_date","S")
  Panne=read.csv(as.character(first(as.character(Equipment$PanneID[1]))),header = T,  sep = ",")
  for (i in 2:nrow(Equipment)){
    if (file.exists(first(as.character(Equipment$PanneID[i])))){
      Tab=read.csv(first(as.character(Equipment$PanneID[i])),header = T,  sep = ",")
      Panne=rbind(Panne,Tab) 
    }
  }
  Panne=Panne[,-1]
  colnames(Panne)=c("Equipement", "Ligne", "Date", "Maintenance", "Duree" )
  Panne$Date=as.POSIXct(as.character(Panne$Date),  format = "%d/%m/%Y ")
  levels(Panne$Maintenance)
  levels(Panne$Maintenance)[1]="Electrical"
  levels(Panne$Maintenance)
  levels(Panne$Maintenance)[2]="Civil"
  levels(Panne$Maintenance)
  levels(Panne$Maintenance)[3]="Mechanical"
  levels(Panne$Maintenance)
  levels(Panne$Maintenance)[4]="Civil"
  levels(Panne$Maintenance)
  levels(Panne$Maintenance)[4]="Regulation"
  levels(Panne$Maintenance)
  Panne$Maintenance=as.character(Panne$Maintenance)
  Panne$Equipement=as.character(Panne$Equipement)
  
  ######################################### Importing Data sensors#####################
  Data_Sensors=read.csv("Data_sensors.csv", sep = ",")
  Data_Sensors=Data_Sensors[,-1] # to remove the column X (it's unused !)
  Data_Sensors$Date=as.POSIXct(Data_Sensors$Date, format = "%Y-%m-%d %H:%M:%S")
  
  ######################################### Artificial outputs of the interface #####################
  
  FromDate= reactive({
    as.POSIXct(input$fromDatetime, format = "%Y-%m-%d %H:%M:%S")
  })
  
  ToDate= reactive({
    as.POSIXct(input$ToDatetime, format = "%Y-%m-%d %H:%M:%S")
  })
  Xable=reactive({
    Data_Sensors[Data_Sensors$Date>= FromDate() & Data_Sensors$Date<=ToDate()& Data_Sensors$Equipment==input$machine,]
  })
  Yable=reactive({
    Xable()[,input$Paramerter]
  })
  output$dt = DT::renderDataTable(DT::datatable({
    data <- Xable()
    data
  }))
  
  X1=reactive({
    na.approx(xts(Yable(), as.POSIXct(Xable()$Date, format = "%Y-%m-%d %H:%M:%S"))) #I used appox in order to replace missing values
    # by interpolation 
  })
  
  output$ts= renderPrint({
    # TAB_data=Data_Sensors[Data_Sensors$Equipment==input$machine,]
    summary(X1())
  })
  output$summ= renderPrint({
    # TAB_data=Data_Sensors[Data_Sensors$Equipment==input$machine,]
    summary(Xable())
  })
  
  output$alarm =DT::renderDataTable(DT::datatable({
    data1 <- X1()
    data1
  }))
  # 
  #  dg= reactive({
  #    dygraph(X1(), main ="Vibrations records",  
  #            xlab = "time frame",
  #            ylab = " records" )  %>% dyRangeSelector()
  #    
  # }) 
  
  shade_tab=reactive({
    Panne[Panne$Date>= FromDate() & Panne$Date<=ToDate() & Panne$Equipement==input$machine,] 
  }) 
  
  output$Shadee = DT::renderDataTable(DT::datatable({
    data_shade <- shade_tab()
    data_shade
  }))
  
  # output$shadde= renderPrint({
  #   
  #     # shade_tab()$Date[1]
  #    head(shade_tab())
  # })
  output$str= renderPrint({
    # TAB_data=Data_Sensors[Data_Sensors$Equipment==input$machine,]
    summary(shade_tab())
    
  })
  
  
  dg= reactive ({ 
    ddgg=dygraph(X1(), main ="interactive graph",
                 xlab = "time frame",
                 ylab = "records" ) %>% dyRangeSelector()
    for( i in 1:nrow(shade_tab())) 
    { 
      ddgg=dyShading(ddgg, from= shade_tab()$Date[i],  to  = shade_tab()$Date[i] + 24*60*60 ,
                     color = ifelse(shade_tab()$Maintenance[i]== 'Mechanical' , 'gold', 
                                    ifelse(shade_tab()$Maintenance[i]== 'Electrical' , 'springgreen' ,
                                           ifelse(shade_tab()$Maintenance[i]== 'Civil' , 'royalblue' , 'red' ) ) ))
    }
    ddgg
  })
  
  output$dygraph <- renderDygraph({ 
    dg()%>% dyLegend( show = "auto" ,
                      width = 250, showZeroValues = TRUE,
                      labelsDiv = NULL, 
                      labelsSeparateLines = T, 
                      hideOnMouseOut = TRUE) %>% 
      dyLimit( 0.04, 
               label = 'Lower Control Limit', 
               labelLoc = "right", 
               color = "red",
               strokePattern = "dashed") %>% 
      dyLimit( 0.5, 
               label = 'Upper Control Limit', 
               labelLoc = "right", 
               color = "red",
               strokePattern = "dashed")%>% 
      dyLimit( 0.27, 
               label = 'Normal', 
               labelLoc = "right", 
               color = "black",
               strokePattern = "dashed")
  })
  
  
  #################################### Dashboard ###########################
  
  
  output$packagePlot <- renderBubbles({

    bubbles(Equipment$S, Equipment$Equipment,  color =ifelse(Equipment$TTF>25,"#40E354",
                                                             ifelse(Equipment$TTF>15, "#FABC3C",
                                                                    ifelse(Equipment$TTF>7, "#F55536", "#F55536"))),
            width = "100%", height = 600)
  })
  
  output$packageTable<- renderTable({
   Equipment[,c(1,5,4)]
  })
  
  output$nextFailure <- renderValueBox({
    valueBox("Next failure:", 
      as.character(paste( min(Equipment[,5]), Equipment$Equipment[Equipment$TTF_date==min(min(Equipment[,5]))], sep = " : ")) ,
                   icon = icon("glyphicon glyphicon-warning-sign", lib = "glyphicon"),
                   color = "red"
    )
  })
  
  output$CriticalEquips <- renderValueBox({
    valueBox( paste(length(Equipment$Equipment[Equipment$TTF<20]), "Failures", sep = " ")
      ,
      "This week" ,
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  output$efficiency <- renderValueBox({
    valueBox("80%", "Probabilty",
      icon = icon("percent"),
      color = "black"
    )
  })
 
  output$MTBF=renderValueBox({
    valueBox( "MTBF" ,paste("30", "jours", sep=" "),
             icon = icon("glyphicon glyphicon-send", lib ="glyphicon" ),
             color = "red"
    )
  })

  output$TTF =renderValueBox({
    valueBox( "Next failure date " , paste(max(Equipment$TTF[Equipment$Equipment== input$machine], 0), "jours", sep=" "),
              icon = icon("glyphicon glyphicon-hourglass"),
              color = "red"
    )
  })

  output$TTR=renderValueBox({
    valueBox("MTTR", paste(max(Equipment$TTF[Equipment$Equipment==input$machine]+2, 0), "heures", sep=" "),
             icon = icon("glyphicon glyphicon-wrench", lib="glyphicon"), color = "orange")
  })
  
  
 
  output$Temperature=renderValueBox({
    valueBox("TEMPERATURE",paste(signif(mean(Data_Sensors$Temp[(length(Data_Sensors$Equipment[Equipment$Equipment== input$machine])-15)]), digits = 3)
                                 ,"degree", sep = " " ),
             icon = icon("glyphicon glyphicon-tint", lib="glyphicon"), color = "orange")
  })
  output$equipment=renderInfoBox({
    infoBox("Equipment",input$machine, icon("glyphicon glyphicon-tags", lib="glyphicon"), color = "orange" )
  })
  
  output$MachineClass=renderValueBox({
    valueBox("Equipement Class ","AA", icon("glyphicon glyphicon-oil", lib="glyphicon"), color = "red" )
  })
  
  output$UpTime=renderValueBox({
    valueBox("Up time ","100 jours 20 heures 30 min", icon("glyphicon glyphicon-oil", lib="glyphicon"), color = "green" )
  })
  
  randomVals <- eventReactive(input$go, {
    runif(input$n)
  })
  
  plotInput <- function(){hist(randomVals())}
  
  output$plot <- renderPlot({
    hist(randomVals())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      plotInput()
      dev.off()
    })
 
  }
