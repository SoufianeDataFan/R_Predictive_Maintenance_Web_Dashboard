################ Preditive Maintenance#############################################
################ Data Exploration ########################################
############### Author : CHAMI Soufiane #############
################Creation date : 14/02/2017###################

path_parent="C:/path/to/Shiny Interface Beta 06_04_2017"
path_data=paste(path_parent,"/data/",sep="")
path_scripts=paste(path_parent,"/Scripts/",sep="")
path_results=paste(path_parent,"/Results/",sep="")
setwd(path_scripts)

library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)


Telemetry_data<-fread(paste(path_data,"Sensors_data.csv",sep=""),sep = ",",header = F)
# Telemetry_data$Tachometer=NULL
# Telemetry_data$V10=NULL
colnames(Telemetry_data)=c("Date", "G.pk", "mm.s.RMS", "Temp")

Telemetry_data$mm.s.RMS<- as.numeric(as.character(gsub(",",".",Telemetry_data$mm.s.RMS)))

Telemetry_data$Date <- as.POSIXct(Telemetry_data$Date,format="%d/%m/%Y %H:%M",tz="UTC")
Telemetry_data$Dt=date(Telemetry_data$Date)
Telemetry_data$Hour=hour(Telemetry_data$Date)
Telemetry_data$Minute=minute(Telemetry_data$Date)




######List des équipement par point of measure######################

Pt_of_mesure=summarise(group_by(Telemetry_data, `Point of measure`), mean(Temp))[,1]
Equipment_lst=summarise(group_by(Telemetry_data, Equipment), mean(Temp))[,1]
###### Visualisation  par Equipment########

theme_set(theme_bw())  # type de graphique
options(repr.plot.width = 8, repr.plot.height = 6)
data_plot1 = Telemetry_data %>% filter(Equipment %in% t(Equipment_lst[1:5]), 
                                 Date > min(Telemetry_data$Date),
                                 Date < max(Telemetry_data$Date))
ggplot(data_plot1, aes(x = Date, y = `mm/s (RMS)`, colour = Equipment)) +
  geom_point() + facet_grid(Equipment ~ . )


##### Graphique par Capteur(Point of mesure)
theme_set(theme_bw())  # type de graphique
options(repr.plot.width = 8, repr.plot.height = 6)
data_plot2 = Telemetry_data %>% filter(`Point of measure` %in% t(Pt_of_mesure[1:5,1]), 
                                      Date > min(Telemetry_data$Date),
                                      Date < max(Telemetry_data$Date))
ggplot(data_plot2, aes(x = Date, y = `mm/s (RMS)`, colour = `Point of measure`)) +
  geom_point() + facet_grid(`Point of measure` ~ . )

#############################Creation d'un enveloppe de confiance #####################
###### Calculer la moyenne mobile  et l'ecart type  pour les variables####################################

telemetrymean <-  Telemetry_data %>% arrange(Site,Area,Equipment,`Point of measure`) %>% 
  arrange(`Point of measure`,Date) %>% 
  group_by(`Point of measure`) %>% 
  mutate(RMS_mean=rollapplyr(`mm/s (RMS)`, width=3, FUN=mean, partial=TRUE),
         g_mean = rollapplyr( `g (pk)`, width = 3, FUN = mean,partial=TRUE),
         Temp_mean = rollapplyr(Temp, width = 3, FUN = mean, partial=TRUE), 
         RMS_sd=rollapplyr(`mm/s (RMS)`, width=3, FUN=sd, partial=TRUE),
         g_sd = rollapplyr( `g (pk)`, width = 3, FUN =sd,partial=TRUE),
         Temp_sd = rollapplyr(Temp, width = 3, FUN = sd, partial=TRUE))


telemetrymean$RMS_LB=telemetrymean$RMS_mean-1.96*telemetrymean$RMS_sd
telemetrymean$RMS_UB=telemetrymean$RMS_mean+1.96*telemetrymean$RMS_sd

telemetrymean$g_LB=telemetrymean$g_mean-1.96*telemetrymean$g_sd
telemetrymean$g_UB=telemetrymean$g_mean+1.96*telemetrymean$g_sd

telemetrymean$Temp_LB=telemetrymean$Temp_mean-1.96*telemetrymean$Temp_sd
telemetrymean$Temp_UB=telemetrymean$Temp_mean+1.96*telemetrymean$Temp_sd
  
  
#############################Creation graphiques par Point de mesure  #####################
#################Graphique par RMS 
theme_set(theme_bw())  # type de graphique
options(repr.plot.width = 8, repr.plot.height = 6)
data_plot3 = telemetrymean %>% filter(`Point of measure` %in% t(Pt_of_mesure[1:2,1]), 
                                       Date > min(telemetrymean$Date),
                                       Date < max(telemetrymean$Date))
ggplot(data_plot3, aes(x = Date, y = `mm/s (RMS)`)) +
  geom_point() +
  geom_line(data=data_plot3,aes(x = Date, y = RMS_UB ), na.rm = TRUE,colour = "Blue")+
  geom_line(data=data_plot3,aes(x = Date, y = RMS_mean),na.rm = TRUE,colour = "Red")+
  geom_line(data=data_plot3,aes(x = Date, y = RMS_LB),na.rm = TRUE,colour = "Green")+
  facet_grid(`Point of measure` ~ . )


#################Graphique par `g (pk)`

theme_set(theme_bw())  # type de graphique
options(repr.plot.width = 8, repr.plot.height = 6)
data_plot4 = telemetrymean %>% filter(`Point of measure` %in% t(Pt_of_mesure[1:5,1]), 
                                      Date > min(telemetrymean$Date),
                                      Date < max(telemetrymean$Date))
ggplot(data_plot4, aes(x = Date, y = `g (pk)`)) +
  geom_point() +
  geom_line(data=data_plot4,aes(x = Date, y = g_UB ), na.rm = TRUE,colour = "Blue")+
  geom_line(data=data_plot4,aes(x = Date, y = g_mean),na.rm = TRUE,colour = "Red")+
  geom_line(data=data_plot4,aes(x = Date, y = g_LB),na.rm = TRUE,colour = "Green")+
  facet_grid(`Point of measure` ~ . )


#################Graphique par `g (pk)`

theme_set(theme_bw())  # type de graphique
options(repr.plot.width = 8, repr.plot.height = 6)
data_plot5 = telemetrymean %>% filter(`Point of measure` %in% t(Pt_of_mesure[1:5,1]), 
                                      Date > min(telemetrymean$Date),
                                      Date < max(telemetrymean$Date))
ggplot(data_plot5, aes(x = Date, y = Temp)) +
  geom_point() +
  geom_line(data=data_plot5,aes(x = Date, y = Temp_UB ), na.rm = TRUE,colour = "Blue")+
  geom_line(data=data_plot5,aes(x = Date, y = Temp_mean),na.rm = TRUE,colour = "Red")+
  geom_line(data=data_plot5,aes(x = Date, y = Temp_LB),na.rm = TRUE,colour = "Green")+
  facet_grid(`Point of measure` ~ . )





