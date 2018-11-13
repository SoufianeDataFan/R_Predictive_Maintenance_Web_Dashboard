library(data.table)
library(ggplot2)
library(survival)
library(rms)
library(randomForestSRC)
library(party)
library(prodlim)
library(data.table)


Model_data=IO[,c(1,3:4,7:9)]
set.seed(2017)
train_idx=sample(1:nrow(Model_data),round(nrow(Model_data))*.80)
train_data=Model_data[train_idx,]
test_data=Model_data[-train_idx,]

fit_form=Surv(time=TTF_days,event = delta,type="right")~G.pk_mean+mm.s.RMS_mean+Temp_mean

TTF_train<- rfsrc(fit_form, data = train_data[,2:6])
TTF_pred<-predict.rfsrc(TTF_train,test_data[,2:6])

Time=TTF_train$time.interest

matplot(Time,t(exp(-TTF_train$chf)[1:15,]),ylab = "Time To Fillure",col = 1,type = "b")
setwd("C:/path/to/Shiny Interface Beta 06_04_2017")
save.image(file = "Proj_PredMaint.RData")












