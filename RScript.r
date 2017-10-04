factorData<-read.csv('data_file_ARQ_mod.csv')
names(factorData)

View(factordata)


#Calculating Log Returns
ln_returns<-vector()

for(i in 1:length(factorData$ticker)){
  if(identical(factorData$ticker[i+1],factorData$ticker[i])){
    ln_returns[i]=log(factorData$price[i+1]/factorData$price[i]);
  }else{
    ln_returns[i]=-9999;
  }
}

ln_returns

#Combining Log Returns to factor data 
factorData<-cbind(ln_returns,factorData)

#Taking out outliers
factorDataReduced<-subset(factorData,ln_returns>-1000)

View(factorDataReduced)

plot(factorDataReduced$ln_returns,factorDataReduced$eps)

#Selecting Specified date
factordata_reduced<-subset(factordata,calendardate==20110630)

plot(factordata_reduced$ln_returns,factordata_reduced$eps)


#selected variables

var=c('PE1', 'PB', 'ROIC','ROA','ASSETTURNOVER','EBITDA','MARKETCAP','DE','BVPS','CURRENTRATIO','FCFPS','ROIC','ASSETSC','PPNENETS','LIABILITIES','EQUITY','EBIT','REVENUE','RND','CONSOLINC')

models=c()

for(i in 1:length(var)){
  model[i]=lm(
  }else{
    ln_returns[i]=-9999;
  }
}
