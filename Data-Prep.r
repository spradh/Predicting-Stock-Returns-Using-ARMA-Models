
factorData<-read.csv('dataFileARQmod.csv')
names<-names(factorData)
sort(names)
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
View(factorDataReducedDate)
plot(factorDataReducedDate$eps, factorDataReducedDate$ln_returns)


#selected variables

(factorDataReducedDate[,var[1]]-mean(factorDataReducedDate[,var[1]]))/sd(factorDataReducedDate[,var[1]])

mean(factorDataReducedDate[,var[1]])

#PE1
plot(factorDataReducedDate$pe1, factorDataReducedDate$ln_returns)
model_pe1=lm(ln_returns~pe1, data=factorDataReducedDate)
summary(model_pe1)

#marketcap *
plot(factorDataReducedDate$marketcap, factorDataReducedDate$ln_returns)
model_marketcap=lm(ln_returns~marketcap, data=factorDataReducedDate)
summary(model_marketcap)

#pb
model_pb=lm(ln_returns~pb, data=factorDataReducedDate)
summary(model_pb)

#pb
model_assetturnover=lm(ln_returns~assetturnover, data=factorDataReducedDate)
summary(model_assetturnover)

#ebitda *
plot(factorDataReducedDate$ebitda, factorDataReducedDate$ln_returns)
model_ebitda=lm(ln_returns~ebitda, data=factorDataReducedDate)
summary(model_ebitda)

#de
model_de=lm(ln_returns~de, data=factorDataReducedDate)
summary(model_de)

#bvps **
model_bvps=lm(ln_returns~bvps, data=factorDataReducedDate)
summary(model_bvps)
plot(factorDataReducedDate$bvps, factorDataReducedDate$ln_returns)


#currentratio
model_currentratio=lm(ln_returns~currentratio, data=factorDataReducedDate)
summary(model_currentratio)

#fcfps *
model_fcfps=lm(ln_returns~fcfps, data=factorDataReducedDate)
summary(model_fcfps)

#assetsc *
model_assetsc=lm(ln_returns~assetsc, data=factorDataReducedDate)
summary(model_assetsc)

#liabilities
model_liabilities=lm(ln_returns~liabilities, data=factorDataReducedDate)
summary(model_liabilities)

#equity
model_equity=lm(ln_returns~equity, data=factorDataReducedDate)
summary(model_equity)

#ebit *
model_ebit=lm(ln_returns~ebit, data=factorDataReducedDate)
summary(model_ebit)

#revenue
model_revenue=lm(ln_returns~revenue, data=factorDataReducedDate)
summary(model_revenue)

#rnd
model_rnd=lm(ln_returns~rnd, data=factorDataReducedDate)
summary(model_rnd)



