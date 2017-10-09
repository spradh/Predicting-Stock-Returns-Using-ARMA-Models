#importing data
factorData<-read.csv('dataFileARQmod.csv')
header<-names(factorData)

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
factorData<-cbind(factorData, ln_returns)

#Taking out missing values/outliers
factorDataReduced<-subset(factorData,ln_returns>-1000)

#writing modified file onto refinedData.csv
View(factorDataReduced)
write.csv(factorDataReduced,file="refinedData")
