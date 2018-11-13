Models_fun=function(i){
  ###########Data Preparation #####################
  setwd(path_data)
  Sensor_data<-fread(paste(path_data,as.character(Equipment$ID[i]),sep=""),sep = ",",header = F)
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
  
  
  ##### Processing of Machine Failures history ##############
  #######
  #######
  
  failure_data<-fread(paste(path_data,as.character(Equipment$PanneID[i]),sep=""),sep = ",",header = T)
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
  TTF=data.frame(Equipment=Equipment$Equipment[i], Date_Record=seq.Date(start,En, by="days"),TTF_days=0)
  
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
  
  #### Data Partition #####################
  
  source(paste(path_scripts,"Metrics.R",sep=""))
  
  set.seed(2017)
  
  train_idx= sample(1:nrow(IO),round(nrow(IO))*.80)
  IO_train=as.data.frame(IO[train_idx,])
  IO_test=as.data.frame(IO[-train_idx,])
  
  CV_options=trainControl(method = "cv",number = 100) ### Cross validation
  tree_TTF=tree(TTF_days~.,data=IO_train[,3:6])
  
  summary(tree_TTF)
  
  plot(tree_TTF)
  text(tree_TTF,pretty = 0)
  cv.tree_TTF=cv.tree(tree_TTF)
  plot(cv.tree_TTF$size,cv.tree_TTF$dev,type = 'b')
  
  ###########################################################################################################################
  ###########################################################################################################################
  ############# Last updates   05/25/2017 ###################################################################################
  ###########################################################################################################################
  ###########################################################################################################################
  
  
  
  ###############"Random Forest#####################
  rf_opts = data.frame(.mtry=c(1:3))
  
  results_rf = train(TTF_days~., data =IO_train[,3:5], method="rf",
                     preProcess='knnImpute',trControl=CV_options, tuneGrid=rf_opts,
                     n.tree=100) 
  
  model_TTF_rf_pred=predict(object = results_rf,newdata =IO_test[,4:6],type="raw")
  model_TTF_rf_train=predict(object = results_rf,newdata =IO_train[,4:6],type="raw")
  
  plot(model_TTF_rf_train)  
  plot(model_TTF_rf_pred)
  
  model_TTF_rf_metrics <- evaluate_model(observed = IO_test$TTF_days, predicted = model_TTF_rf_pred) 
  model_TTF_rf_metrics_tr <- evaluate_model(observed = IO_train$TTF_days, predicted = model_TTF_rf_train) 
  
  Y=as.data.frame(model_TTF_rf_metrics)
  # 
  ########Extreme Boosting Gradien #####################
  xgb_grid_1 = expand.grid(
    nrounds = 100,
    eta = c(0.2,0.15,0.1),
    max_depth = c(1,6),
    gamma = c(2.5,3,3.5),
    min_child_weight = c(1,3),
    subsample=c(0.5,1),
    colsample_bytree = 1
  )
  
  # pack the training control parameters
  xgb_trcontrol_1 = trainControl(
    method = "cv",
    number = 100,
    verboseIter = TRUE,
    returnData = FALSE,
    returnResamp = "all",
    allowParallel = TRUE
  )
  
  set.seed(102116)
  model_TTF_xgb = train(
    x = as.matrix(IO_train[,4:5]),
    y = as.numeric(IO_train$TTF_days),
    trControl = xgb_trcontrol_1,
    tuneGrid = xgb_grid_1,
    method = "xgbTree"
  )
  
  Xgb_pred_TTF_pred <- predict(object=model_TTF_xgb,newdata =IO_test[,4:6],type="raw")
  Xgb_pred_TTF_train <- predict(object=model_TTF_xgb,newdata =IO_train[,4:6],type="raw")
  
  plot(Xgb_pred_TTF_pred)
  
  model_TTF_Xgb_metrics <- evaluate_model(observed = IO_test$TTF_days, predicted = Xgb_pred_TTF_pred)
  model_TTF_Xgb_metrics_tr <- evaluate_model(observed = IO_train$TTF_days, predicted = Xgb_pred_TTF_train)
  
  
  # ################ GLM : doesn't hurt if we try it :) ##############################################
  
  model_TTF_GLM=glm(TTF_days ~ .,data=IO_train[,3:5],family = poisson())
  
  model_TTF_GLM_pred=predict(object = model_TTF_GLM,newdata =IO_test[,4:6],type="response" )
  model_TTF_GLM_train=predict(object = model_TTF_GLM,newdata =IO_train[,4:6],type="response" )
  
  plot(model_TTF_GLM_pred)
  
  model_TTF_GLM_metrics <- evaluate_model(observed = IO_test$TTF_days, predicted = model_TTF_GLM_pred)
  model_TTF_GLM_metrics_tr <- evaluate_model(observed = IO_train$TTF_days, predicted = model_TTF_GLM_train)
  
  
  # #####################GBM###########################################
  
  # train the model for each parameter combination in the grid,
  #   using CV to evaluate
  set.seed(032017)
  
  model_TTF <- gbm(formula = TTF_days ~ .,
                   distribution=  "gaussian",
                   data = IO_train[,3:5],
                   n.trees = 70,
                   interaction.depth = 5,
                   shrinkage = 0.3,
                   bag.fraction = 0.5,
                   train.fraction = 1.0,
                   n.cores = NULL)
  
  model_TTF_GBM_pred=predict (object = model_TTF,newdata =IO_test[,4:5], n.trees =70,type="response")
  model_TTF_GBM_train=predict (object = model_TTF,newdata =IO_train[,4:5], n.trees =70,type="response")
  
  
  
  plot(model_TTF_GBM_pred)
  
  model_TTF_GBM_metrics <- evaluate_model(observed = IO_test$TTF_days, predicted = model_TTF_GBM_pred)
  model_TTF_GBM_metrics_tr <- evaluate_model(observed = IO_train$TTF_days, predicted = model_TTF_GBM_train)
  
  # ########### Neural Network : Focus on Decision Trees family for now ! ##################################
  # 
  # 
  #   n <- names(IO_train[,3:5])
  #   f <- as.formula(paste("TTF_days ~", paste(n[!n %in% "TTF_days"], collapse = " + ")))
  # 
  #   nn <- neuralnet(f,data=IO_train[,3:5],hidden=c(3,2),linear.output=T)
  # 
  # 
  # ############# Metrics for ALL Models ####################################
  
  metrics_df <- rbind(model_TTF_GBM_metrics, model_TTF_GLM_metrics, model_TTF_Xgb_metrics,model_TTF_rf_metrics)
  rownames(metrics_df) <- NULL
  Algorithms <- c("Genralized Boosting Model",
                  "Poisson Regression Model",
                  "Extreme Gradient Boosting",
                  "Random Forest")
  metrics_df <- cbind(Algorithms, metrics_df)
  
  metrics_df_train <- rbind(model_TTF_GBM_metrics_tr, model_TTF_GLM_metrics_tr, model_TTF_Xgb_metrics_tr,model_TTF_rf_metrics_tr)
  rownames(metrics_df_train) <- NULL
  
  metrics_df_train <- cbind(Algorithms, metrics_df_train)
  setwd(paste(path_scripts,"/Results/",sep=""))
  num.file <- paste(getwd(), "/Metrics_Test.csv", sep= "")
  write.csv(metrics_df,num.file)
  num.file <- paste(getwd(), "/Metrics_Train.csv", sep= "")
  write.csv(metrics_df_train,num.file)
  
  
  
  matplot (x=IO_train$Date, cbind (IO_train$TTF_days,Xgb_pred_TTF_train,model_TTF_rf_train),
           main = "Predicting Time To Failure  (Training Sample)",xlab = "Date",ylab = "Time To Failure", pch = 19)
  
  axis.Date(1, at=seq(IO_train$Date[1],IO_train$Date[nrow(IO_train)],"month"), format="%m/%Y")
  legend("topright", legend = c("Real","Extreme Gradient Boosting (R-square=99.34%)","Random Forest (R-square=93%)"),col =c("black","red","green"),pch=19)
  
  
  
  
  matplot (x=IO_test$Date, cbind (IO_test$TTF_days,Xgb_pred_TTF_pred,model_TTF_rf_pred),
           main = "Predicting Time To Failure  (Testing Sample)",xlab = "Date",ylab = "Time To Failure", pch = 19)
  
  axis.Date(1, at=seq(IO_test$Date[1],IO_test$Date[nrow(IO_test)],"month"), format="%m/%Y")
  legend("topright", legend = c("Real","Extreme Gradient Boosting (R-square=58.30%)","Random Forest(R-square=63.4%)"),col =c("black","red","green"),pch=19)
  
  
  # 
  #   
  #   
  ###############################The output of the model ##########
  
}
