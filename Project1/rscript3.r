#importing final model
data=read.csv('finalData.csv')

dates<-unique(data$calendardate)

model_list<-vector()

for(i in 1:15){
  a<-paste("modelDate_",dates[i],sep="")
  model_list<-c(model_list, a)
  model<-lm(ln_returns~.,data=data)
  assign(a, model)
}

for(i in model_list){

}
  
