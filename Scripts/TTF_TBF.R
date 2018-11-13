
setwd("C:/Users/Curiosity/Desktop/OCP/Shiny Interface Beta 06_04_2017")
path_parent="C:/Users/Curiosity/Desktop/OCP/Shiny Interface Beta 06_04_2017"
path_data=paste(path_parent,"/data/",sep="")
path_scripts=paste(path_parent,"/Scripts/",sep="")
path_results=paste(path_parent,"/Results/",sep="")
setwd(path_data)

#Import virgin data 
##############################################################################################
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

# summary(TAB_y)
# nrow(newrow1)+nrow(newrow)+nrow(Tab_2014)
# summary(VNV_AB)
class(VNV_AB$Durée)

as.numeric(levels(VNV_AB$Durée))[VNV_AB$Durée[]]
# Failures History 
##############################################################################################

levels(TAB_y$Équipement)
VNV=TAB_y[grep("entilateur",TAB_y$Équipement),]
VNV_AB=subset(VNV, VNV$Ligne=="AP1/2" )
VNV_CD=subset(VNV, VNV$Ligne=="AP3/4" )
VNV_PII=subset(VNV, VNV$Ligne=="AP")

write.csv(VNV_AB, file = "VNVAB.csv")
write.csv(VNV_CD, file = "VNVCD.csv")
write.csv(VNV_CD, file = "VNVPII.csv")


# Let's create the TBF table 

TBF=numeric(length(VNV_CD$Ligne))

VNV_CD$Date=sort(as.Date((as.character(VNV_CD$Date)), format="%d/%m/%Y"),decreasing = F)
# WARNING !! after applying the sort gunction the format of the date becomes "%Y-%m-%d" instead of "%d/%m/%Y"

for (i in 1:(nrow(VNV_CD)-1)) TBF[i]=as.numeric(as.Date(as.character(VNV_CD$Date[i+1]), format="%Y-%m-%d")-as.Date(as.character(VNV_CD$Date[i]), forma="%Y-%m-%d"))
VNV_CD<- cbind(VNV_CD,TBF)






#  Now let\s build the function to create Time To failures Table (TTF)

# EMpty Table
En=as.Date(as.character(VNV_CD$Date[nrow(VNV_CD)]))
start=as.Date(as.character(VNV_CD$Date[1]))
TTF=data.frame(Equipment="Vntl Assainissement CD", Date_Record=seq.Date(start,En, by="days"),TTF_days=0)

# Filling the column TTF_days
for(j in 1:18){
  start=sum(VNV_CD$TBF[1:(j-1)])+1
  if(j==1)start=1
  end=sum(VNV_CD$TBF[1:j])
  for(i in start:end){
    if(TTF$Date_Record[i]==VNV_CD$Date[j]){ 
      TTF$TTF_days[i]=VNV_CD$TBF[j]
      }
    if(TTF$Date_Record[i]!=VNV_CD$Date[j]){
      n=as.numeric(as.Date(as.character(TTF$Date_Record[i]), format="%Y-%m-%d")-as.Date(as.character(VNV_CD$Date[j]), format ="%Y-%m-%d"))
      TTF$TTF_days[i]=VNV_CD$TBF[j]-n
    }
  }
}

write.csv(TTF, "TTF.csv")

