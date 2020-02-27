library(class)
library(dplyr)
library(dbscan)
library(caret)
library(ggplot2)
library(gridExtra)
library(psych)
library(scales)
library(memisc)
library(readxl)

setwd("Downloads/")
#read dataset into  a frame
data<-read.csv("cccc.csv",header=TRUE)
dataset<-data[1:20000,c(2,7:19)]
dataset$default.payment.next.month<-as.factor(dataset$default.payment.next.month)
tail(dataset)
#######################################
library(dplyr)
k_optimal=function(data, k_from, k_to){
  rentetan=seq(k_from, k_to)
  pakai=rentetan %% 2!=0
  acc=matrix(nrow=50,ncol=length(rentetan[pakai]))
  for(i in 1:50){
    intrain=createDataPartition(data %>% select_if(is.factor) %>% pull(1), p=0.65, list=FALSE)
    tr=data[intrain,] %>% select_if(is.numeric)
    ts=data[-intrain,] %>% select_if(is.numeric)
    tr_l=data[intrain,] %>% select_if(is.factor) %>% pull(1)
    ts_l=data[-intrain,] %>% select_if(is.factor) %>% pull(1)
    minmax<-apply(tr, 2,FUN=function(x){c('min'=min(x),'max'=max(x))})
    data.baku.tr <- (tr-minmax[1,])/( minmax[2,] -  minmax[1,])
    data.baku.ts<- (ts-minmax[1,])/( minmax[2,] -  minmax[1,])
    for(j in 1:length(rentetan[pakai])){
      model=knn(train=data.baku.tr, test=data.baku.ts, cl=tr_l, k=rentetan[pakai][j])
      acc_temp<-confusionMatrix(data=model, reference=ts_l)$overall[[1]]
      acc[i,j]<-acc_temp
    } 
  }
  out=acc %>% as.data.frame ()
  colnames(out)=paste0("k_", rentetan[pakai])
  out
}
k_opt=k_optimal(data=dataset, k_from=3, k_to=50)
k_opt
best_k=cbind(k=seq(3, 50,by=2),rata2_akurasi=sapply(k_opt,FUN=mean))
best_k