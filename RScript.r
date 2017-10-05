
factorData<-read.csv('dataFileARQmod.csv')
names(factorData)

View(factorData)


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

#Selecting Specified date
factorDataReducedDate<-subset(factorDataReduced,calendardate==20110630)

plot(factorDataReducedDate$eps, factorDataReducedDate$ln_returns)


#selected variables

var=c('PE1', 'PB', 'ROIC','ROA','ASSETTURNOVER','EBITDA','MARKETCAP','DE','BVPS','CURRENTRATIO','FCFPS','ROIC','ASSETSC','PPNENETS','LIABILITIES','EQUITY','EBIT','REVENUE','RND','CONSOLINC')

models=c()

for(i in 1:length(var)){
  model[i]=lm(ln_returns~var[i], data=factorDataReduced)
}
