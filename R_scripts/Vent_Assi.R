################ Pred Maintenance  #############################################
############### Author: CHAMI Soufiane ################
################ Creation date : 14/02/2017###########################################################

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
library(subgraphMining)
# install.packages("subgraphMining")
# install.packages("mnormt")


path_parent="/path/to/Shiny Interface Beta 06_04_2017"
path_data=paste(path_parent,"/data/",sep="")
path_scripts=paste(path_parent,"/Scripts/",sep="")
path_results=paste(path_parent,"/Results/",sep="")
setwd(path_scripts)


Sensor_data<-fread(paste(path_data,"Sensor_data.csv",sep=""),sep = ",",header = F)
colnames(Sensor_data)=c("Date", "G.pk", "mm.s.RMS", "Temp")
str(Sensor_data)
Sensor_data$G.pk<- as.numeric(as.character(gsub(",",".",Sensor_data$G.pk)))
Sensor_data$mm.s.RMS<- as.numeric(as.character(gsub(",",".",Sensor_data$mm.s.RMS)))
Sensor_data$Temp<- as.numeric(as.character(gsub(",",".",Sensor_data$Temp)))

Sensor_data <- as.data.frame(Sensor_data) 
Sensor_data$Date=substr(Sensor_data$Date,1,19)


Sensor_data$Date<-as.POSIXct(strptime(Sensor_data$Date, "%Y-%m-%d %H:%M:%S"))
           

Sensor_data$Dt=date(Sensor_data$Date)
Sensor_data$Hour=hour(Sensor_data$Date)
Sensor_data$Minute=minute(Sensor_data$Date)
###############Normalisation of data#################

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
setwd(paste(path_scripts, "/Results/Outliers", sep = ""))
num.file <- paste(getwd(), "/dq_numOrig.csv", sep= "")
checkDataQuality(data= Sensor_data, out.file.num= num.file)

num.file <- paste(getwd(), "/dq_numOutlieR.csv", sep= "")
checkDataQuality(data= Sensor_data_ot, out.file.num= num.file)
###########################################################################################

Sensor_data_Agg_ot <- data.frame(Sensor_data_agg[2:13], lapply(Sensor_data_agg[2:13], outlier.fun) )
Sensor_data_Agg_ot$Date<-Sensor_data_agg$Dt

######################COMPARE RAW DATA BEFORE AND AFTER OUTLIER TREATMENT Unaggregated######################
### These folders need to be created before this part of script is executed
dir.create(file.path(paste(path_scripts,"/Results/Outliers",sep="")), showWarnings = FALSE)
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
failure_data<-fread(paste(path_data,"VNVCD.csv",sep=""),sep = ",",header = T)
failure_data$Dur?e<- as.numeric(as.character(gsub(",",".",failure_data$Dur?e)))

failure_data$Date<-as.POSIXct(strptime(failure_data$Date, "%d/%m/%Y"))
failure_data$V1=NULL


TBF=numeric(length(failure_data$Ligne))

failure_data$Date=sort(as.Date(((failure_data$Date)), format="%d/%m/%Y"),decreasing = F)
# WARNING !! after applying the sort gunction the format of the date becomes "%Y-%m-%d" instead of "%d/%m/%Y"

for (i in 1:(nrow(failure_data)-1)) TBF[i]=as.numeric(as.Date((failure_data$Date[i+1]), format="%Y-%m-%d")-as.Date((failure_data$Date[i]), forma="%Y-%m-%d"))
failure_data<- cbind(failure_data,TBF)


En=as.Date((failure_data$Date[nrow(failure_data)]))
start=as.Date((failure_data$Date[1]))
TTF=data.frame(Equipment="Vntl Assainissement AP3/4", Date_Record=seq.Date(start,En, by="days"),TTF_days=0)

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

IO<- as.data.table(merge(TTF,Sensor_data_agg[,1:4],by="Date"))

#############################Creation graphiques par Point de mesure  #####################









