##################################### Packages #############################
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

##################################### Working directories  #############################

path_parent="C:/Users/Curiosity/Desktop/OCP/Shiny Interface Beta 06_04_2017"
path_data=paste(path_parent,"/data/",sep="")
path_scripts=paste(path_parent,"/Scripts/",sep="")
path_results=paste(path_parent,"/Results/",sep="")
setwd(path_data)


##################################### ID Equipment   #############################

EquipmentID=read.csv("ID Equipement.csv",header = T,  sep = ";")
EquipmentID$Equipment=as.character(EquipmentID$Equipment)
EquipmentID$ID <- paste(EquipmentID$ID, "csv" , sep=".")

#####################################Merging Sensors Data#############################

Ytable=NULL
n=0
# Importing the data 
for(i in 1:nrow(EquipmentID)){

  Tab_Table=read.csv(EquipmentID$ID[i], header = F, sep = ',') 
  Tab_Table=cbind(Equipment=EquipmentID$Equipment[i], Tab_Table)
  colnames(Tab_Table)=c("Equipment", "Date","G.pk","mm.s.RMS", "Temp")
  Ytable=rbind(Ytable, Tab_Table)
}
Data_Sensors=Ytable
write.csv(Data_Sensors,"Data_sensors.csv")

################################################### Import all Failures data history  ###########################################
Tab_2015=read.csv("Histo_2015.csv", header = T, sep = ';') 
Tab_2016=read.csv("Histo_2016.csv", header = T, sep = ';') 
Tab_2014=read.csv("Histo_2014.csv", header = T, sep = ';') 


# Cleaning empty columns
Tab_2016$X=NULL
Tab_2016$X.10=NULL
Tab_2016$X.9=NULL
Tab_2016$X.8=NULL
Tab_2016$X.7=NULL
Tab_2016$X.6=NULL
Tab_2016$X.5=NULL
Tab_2016$X.4=NULL
Tab_2016$X.3=NULL
Tab_2016$X.2=NULL
Tab_2016$X.1=NULL

Tab_2014$X=NULL
Tab_2014$X.10=NULL
Tab_2014$X.9=NULL
Tab_2014$X.8=NULL
Tab_2014$X.7=NULL
Tab_2014$X.6=NULL
Tab_2014$X.5=NULL
Tab_2014$X.4=NULL
Tab_2014$X.3=NULL
Tab_2014$X.2=NULL
Tab_2014$X.1=NULL
Tab_2014$Cumul=NULL
Tab_2014$Durée.cumul=NULL
Tab_2014$Subi=NULL
Tab_2014$Programmé=NULL

# Let's merge tables into one 

TAB_y=Tab_2014[,c("Équipement","Ligne","Date","Maintenance","Durée")]

newrow=Tab_2015[,c("Équipement","Ligne","Date","Maintenance","Durée")]

TAB_y=rbind(TAB_y,newrow)

newrow1=Tab_2016[,c("Équipement","Ligne","Date","Maintenance","Durée")]

TAB_y=rbind(TAB_y,newrow1)

# finish !

################################################ Failures History Selection  #############################################
levels(VNV$Équipement)
VNV=TAB_y[grep("entilateur",TAB_y$Équipement),]
PMP=TAB_y[grep("culation" ,TAB_y$Équipement),]
ELV=TAB_y[grep("levateur", TAB_y$Équipement),]
SEP=TAB_y[grep("parateur", TAB_y$Équipement),]

PMP$Équipement= EquipmentID$Equipment[6]
for (i in 1:nrow(PMP)){
  if (as.character(PMP$Ligne[i]) =="AP3/4") PMP$Équipement[i]=EquipmentID$Equipment[5]
  if (as.character(PMP$Ligne[i]) =="AP") PMP$Équipement[i]="PMP Circulation AP"
}
VNV$Équipement=EquipmentID$Equipment[2]
for (i in 1:nrow(VNV)){
  if (as.character(VNV$Ligne[i]) =="AP3/4") VNV$Équipement[i]=EquipmentID$Equipment[1]
  if (as.character(VNV$Ligne[i]) =="AP") VNV$Équipement[i]=EquipmentID$Equipment[3]
  if (as.character(VNV$Ligne[i]) =="LS") VNV$Équipement[i]="Vtl Assainissement LS"
  if (as.character(VNV$Ligne[i]) =="LN") VNV$Équipement[i]="Vtl Assainissement LN"
}
ELV$Équipement=as.character(ELV$Équipement)
ELV$Ligne=as.character(ELV$Ligne)
for (i in 1:nrow(ELV)) {
  ELV$Équipement[i]=paste(as.character(ELV$Équipement[i]), as.character(ELV$Ligne[i]), sep = "____")
}

SEP$Équipement= EquipmentID$Equipment[6]
for (i in 1:nrow(PMP)){
  if (as.character(PMP$Ligne[i]) =="AP3/4") PMP$Équipement[i]=EquipmentID$Equipment[5]
  if (as.character(PMP$Ligne[i]) =="AP") PMP$Équipement[i]="PMP Circulation AP"
}







# library(tidyr) 
# PMP= unite(PMP, Équipement, c(Équipement, Ligne), remove=T) 
# VNV=unite(VNV, Équipement, c(Équipement, Ligne), remove=T)

VNV_AB=subset(VNV, VNV$Ligne=="AP1/2" )
VNV_CD=subset(VNV, VNV$Ligne=="AP3/4" )
VNV_PII=subset(VNV, VNV$Ligne=="AP")


###########################save the failure ID (Panne ID) into equipment ID  table ##############

PanneID=character(nrow(EquipmentID))
for(i in 1:nrow(EquipmentID)){
  PanneID[i]=paste(EquipmentID$Equipment[i],"_Panne.csv", sep = "")
}
EquipmentID=cbind(EquipmentID,PanneID)
#finish !

################### Merge selected failures tables #######################
Panne=read.csv(as.character(first(as.character(EquipmentID$PanneID[1]))),header = T,  sep = ",")
for (i in 2:nrow(EquipmentID)){
  if (file.exists(first(as.character(EquipmentID$PanneID[i])))){
    Tab=read.csv(first(as.character(EquipmentID$PanneID[i])),header = T,  sep = ",")
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
write.csv(VNV_AB, file = as.character(EquipmentID[EquipmentID$Equipment=="Vtl Assainissement AB", 3]))
write.csv(VNV_CD, file = as.character(EquipmentID[EquipmentID$Equipment=="Vtl Assainissement CD", 3]))
write.csv(VNV_PII, file = as.character(first(as.character(EquipmentID[EquipmentID$Equipment=="Vtl Assainissement AP", 3]))))
######################################Multiple shades################################
# FromDate= as.POSIXct("2016-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
# 
# ToDate=as.POSIXct("2017-12-30 00:00:00", format = "%Y-%m-%d %H:%M:%S")
# 
# input_machine= "Vtl Assainissement CD"
# 
# input_Paramerter="G.pk"
# 
# Data_Sensors$Date=as.POSIXct(Data_Sensors$Date, format = "%Y-%m-%d %H:%M:%S")
# 
# Yable=Data_Sensors[Data_Sensors$Date>= FromDate & Data_Sensors$Date<=ToDate & Data_Sensors$Equipment==input_machine ,]
# 
# 
# 
# Yable=Yable[,c(2,3)]
# 
# 
# 
# Yable= xts(Yable[,2], as.POSIXct(Yable[,1], format = "%Y-%m-%d %H:%M:%S"))

# 
# 
# library(dygraphs)
# dg=dygraph(Yable, 
#         main ="Vibrations records",  
#         xlab = "time frame",
#         ylab = "G.pk records"
#         )  %>% dyRangeSelector()
# 
# # dg %>% dyShading( from= shade_tab$Date[15], to= shade_tab$Date[15] + 24*60*60 , color = '#FFE6E6')
# 
# shade_tab=read.csv("VNVCD.csv", sep=",")
# colnames(shade_tab)[1]="TBF"
# Prd=shade_tab[,c(2,4,1)]
# Prd$Équipement="Vtl Assainissement CD"
# shade_tab$Date=as.POSIXct(as.character(shade_tab$Date),  format = "%d/%m/%Y ")
# shade_tab$Maintenance=as.character(shade_tab$Maintenance)
# 
# # #add shades
# # for( i in 1:nrow(shade_tab)) {
# #   dg = dyShading(dg, from= shade_tab$Date[i], to= shade_tab$Date[i] + 24*60*60 , color = 'black')
# # }
# # 
# # 
# # #show graph
# # dg
# 
# dyShading(dg, from= shade_tab$Date[15], to= shade_tab$Date[15] + 24*60*60 , color = 'black')
# 
################################## Data Preparation for ML models ##############################################

IO_tab=rep("", nrow(EquipmentID))

for(compteur in 1:3){ #nrow(EquipmentID)

Sensor_data<-fread(paste(path_data,as.character(EquipmentID$ID[compteur]),sep=""),sep = ",",header = F)
colnames(Sensor_data)=c("Date", "G.pk", "mm.s.RMS", "Temp")

Sensor_data$G.pk<- as.numeric(as.character(gsub(",",".",Sensor_data$G.pk)))
Sensor_data$mm.s.RMS<- as.numeric(as.character(gsub(",",".",Sensor_data$mm.s.RMS)))
Sensor_data$Temp<- as.numeric(as.character(gsub(",",".",Sensor_data$Temp)))

Sensor_data <- as.data.frame(Sensor_data) 
Sensor_data$Date=substr(Sensor_data$Date,1,19)


Sensor_data$Date<-as.POSIXct(strptime(Sensor_data$Date, "%Y-%m-%d %H:%M:%S"))


Sensor_data$Dt=date(Sensor_data$Date)
Sensor_data$Hour=hour(Sensor_data$Date)
Sensor_data$Minute=minute(Sensor_data$Date)
###############Normaisation of data#################

##############fin de scaling#######################

Sensor_data_agg=as.data.frame(Sensor_data[,2:5])
Sensor_data_agg <- Sensor_data_agg %>% group_by(Dt) %>% summarise_each(funs(mean=mean(.,na.rm=TRUE)
                                                                            ,rg = diff(range(.))
                                                                            ,med=median(.,na.rm=TRUE)
                                                                            ,stdev=sd(.,na.rm=TRUE)))

source(paste(path_scripts,"OutliersFn.R",sep=""))

Sensor_data_ot <- data.frame(Sensor_data[2:4], lapply(Sensor_data[2:4], outlier.fun) )
Sensor_data_ot$Date=Sensor_data$Dt

######################COMPARE RAW DATA BEFORE AND AFTER OUTLIER TREATMENT Unaggregated######################
### These folders need to be created before this part of script is executed
dir.create(paste(path_scripts, "/Results/Outliers", sep = ""), showWarnings = F, recursive = FALSE) # I added thisline because
# the next one will return error because it cant find the folder 
setwd(paste(path_scripts,"/Results/Outliers",sep=""))
num.file <- paste(getwd(), "/dq_numOrig.csv", sep= "")
checkDataQuality(data= Sensor_data, out.file.num= num.file)

num.file <- paste(getwd(), "/dq_numOutlieR.csv", sep= "")
checkDataQuality(data= Sensor_data_ot, out.file.num= num.file)
###########################################################################################

Sensor_data_Agg_ot <- data.frame(Sensor_data_agg[2:13], lapply(Sensor_data_agg[2:13], outlier.fun) )
Sensor_data_Agg_ot$Date<-Sensor_data_agg$Dt

######################COMPARE RAW DATA BEFORE AND AFTER OUTLIER TREATMENT Unaggregated######################
### These folders need to be created before this part of script is executed
setwd(paste(path_scripts,"/Results/Outliers",sep=""))
num.file <- paste(getwd(), "/dq_numAgg.csv", sep= "")
checkDataQuality(data= Sensor_data_agg, out.file.num= num.file)

num.file <- paste(getwd(), "/dq_numOutlieR_Aggr.csv", sep= "")
checkDataQuality(data= Sensor_data_Agg_ot, out.file.num= num.file)
###########################################################################################

Sensor_data_Agg_ot_MA <-  Sensor_data_Agg_ot %>%
  mutate(RMS_mean=rollapplyr(Sensor_data_Agg_ot$mm.s.RMS_mean.1, width=3, FUN=mean, partial=TRUE,na.rm=TRUE),
         g_mean = rollapplyr(Sensor_data_Agg_ot$G.pk_mean.1, width = 3, FUN = mean,partial=TRUE,na.rm=TRUE),
         Temp_mean = rollapplyr(Sensor_data_Agg_ot$Temp_mean.1, width = 3, FUN = mean, partial=TRUE,na.rm=TRUE), 
         RMS_sd=rollapplyr(Sensor_data_Agg_ot$mm.s.RMS_stdev.1, width=3, FUN=sd, partial=TRUE,na.rm=TRUE),
         g_sd = rollapplyr( Sensor_data_Agg_ot$G.pk_stdev.1, width = 3, FUN =sd,partial=TRUE,na.rm=TRUE),
         Temp_sd = rollapplyr(Sensor_data_Agg_ot$Temp_stdev.1, width = 3, FUN = sd, partial=TRUE,na.rm=TRUE))

Sensor_data_Agg_ot_MA$Date<-Sensor_data_agg$Dt


#####Traitement des Pannes  
#######
#######
failure_data<-fread(paste(path_data,as.character(EquipmentID$PanneID[compteur]),sep=""),sep = ",",header = T)
failure_data$Durée<- as.numeric(as.character(gsub(",",".",failure_data$Durée)))

failure_data$Date<-as.POSIXct(strptime(failure_data$Date, "%d/%m/%Y"))
failure_data$V1=NULL


TBF=numeric(length(failure_data$Ligne))

failure_data$Date=sort(as.Date(((failure_data$Date)), format="%d/%m/%Y"),decreasing = F)
# WARNING !! after applying the sort gunction the format of the date becomes "%Y-%m-%d" instead of "%d/%m/%Y"

for (i in 1:(nrow(failure_data)-1)) TBF[i]=as.numeric(as.Date((failure_data$Date[i+1]), format="%Y-%m-%d")-as.Date((failure_data$Date[i]), forma="%Y-%m-%d"))
failure_data<- cbind(failure_data,TBF)


En=as.Date((failure_data$Date[nrow(failure_data)]))
start=as.Date((failure_data$Date[1]))
TTF=data.frame(Equipment=as.character(EquipmentID$Equipment[compteur]), Date_Record=seq.Date(start,En, by="days"),TTF_days=0)

# Filling the column TTF_days
for(j in 1:(nrow(failure_data)-1)){
  start=sum(failure_data$TBF[1:(j-1)])+1
  if(j==1)start=1
  end=sum(failure_data$TBF[1:j])
  for(i in start:end){
    if(TTF$Date_Record[i]==failure_data$Date[j]){ 
      TTF$TTF_days[i]=failure_data$TBF[j]
      TTF$delta[i]=1
      TTF$TTF_Rul[i]=1
      TTF$TTF_Mtnce[i]=failure_data$Maintenance[j]
      
    }
    if(TTF$Date_Record[i]!=failure_data$Date[j]){
      n=as.numeric(as.Date((TTF$Date_Record[i]), format="%Y-%m-%d")-as.Date((failure_data$Date[j]), format ="%Y-%m-%d"))
      TTF$TTF_days[i]= n ###failure_data$Date[j]-n
      TTF$delta[i]=0
      TTF$TTF_Rul[i]=TTF$TTF_days[i]/failure_data$TBF[j]
      TTF$TTF_Mtnce[i]=failure_data$Maintenance[j]
    }
  }
}
TTF<-plyr::rename(TTF,c("Date_Record"="Date"))
Sensor_data_agg<-plyr::rename(Sensor_data_agg,c("Dt"="Date"))



dir.create(paste(path_data, "/Prepared_Data", sep = ""), showWarnings = F, recursive = FALSE) # I added thisline because
# the next one will return error because it cant find the folder 
setwd(paste(path_data, "/Prepared_Data", sep = ""))

IO<- as.data.table(merge(TTF,Sensor_data_agg[,1:4],by="Date"))
write.csv(IO,as.character(paste(EquipmentID$Equipment[compteur],"IO.csv", sep = "_")))
IO_tab[compteur]=paste(EquipmentID$Equipment[compteur],"IO.csv", sep = "_")
}

