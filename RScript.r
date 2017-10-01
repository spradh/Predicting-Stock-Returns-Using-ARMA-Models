factorData<-read.csv('data_file_ARQ_mod.csv')
names(factorData)

View(factordata)

ln_returns<-vector()

for(i in 1:length(factorData$ticker)){
  if(identical(factorData$ticker[i+1],factorData$ticker[i])){
    ln_returns[i]=log(factorData$price[i+1]/factorData$price[i]);
  }else{
    ln_returns[i]=-9999;
  }
}

ln_returns

factorData<-cbind(ln_returns,factorData)


factorDataReduced<-subset(factorData,ln_returns>-1000)

View(factorDataReduced)

plot(factorDataReduced$ln_returns,factorDataReduced$eps)


factordata_reduced<-subset(factordata,calendardate==20110630)




plot(factordata_reduced$ln_returns,factordata_reduced$eps)
