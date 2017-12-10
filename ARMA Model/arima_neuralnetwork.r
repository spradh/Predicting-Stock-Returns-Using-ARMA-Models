######################
# part 0 - Setting Directory
######################
filename = "Pradhan_HW5.R"
filepath = file.choose()  # browse and select maya_pradhan.R in the window
dir = substr(filepath, 1, nchar(filepath)-nchar(filename))
setwd(dir)


################################################################### 
# part I - Importing Data and Calculating Log Returns
################################################################### 

#importing data
factorData<-read.csv('dataFileARQmod.csv')
header<-names(factorData)

header
#View(factorData)

#Calculating Log Returns
ln_returns<-vector()

for(i in 1:length(factorData$ticker)){
  if(identical(factorData$ticker[i+1],factorData$ticker[i])){
    ln_returns[i]=log(factorData$price[i+1]/factorData$price[i]);
  }else{
    ln_returns[i]=-9999;
  }
}



#Combining Log Returns to factor data 
factorData<-cbind(ln_returns,factorData)

header<-names(factorData)

header
#Taking out missing values/outliers
dataFile<-subset(factorData,ln_returns>-1000)



################################################################### 
# part II - Cleaning Data and Removing NAs
################################################################### 



#create list of dates
dataFile_dates<-unique(dataFile$calendardate)
dataFile_header<-names(factorData)


#for loop creating all unique data date frames
#one data file for each unique data with same format "dataFile_DATE"

#creates list of file names stored in "dataFile_uniqueDate_Dataframe_List"

dataFile_uniqueDate_dataFrame_list<-vector()

for(i in dataFile_dates){
  a<-paste("dataFile_",i,sep="");
  assign(a,subset(dataFile,calendardate=i));
  dataFile_uniqueDate_dataFrame_list<-c(dataFile_uniqueDate_dataFrame_list, a);
}


#double for loop to list headers of columns with "too many, 
#i.e. > 10%" NAs for each unique date takes uniqueDate data frames
#created above, and for each:
#1)makes an empty "NA_list_"
#2)appends the "NA_list_" file name to a list of all these file in "NA_uniqueDate_list"
#3)then loops through all headers in "dataFile_headers" and counts the number of NA's in each column
#4)If more than 10% NA's the header for that column is appended to the "NA_list_" for that date
#Note that the list of these "NA_list_" files are stored in "NA_uniqueDate_list"

NA_uniqueDate_list<-vector();
for(j in dataFile_uniqueDate_dataFrame_list){
  a<-paste("NA_list_",j,sep="");
  NA_uniqueDate_list<-c(NA_uniqueDate_list,a);
  NA_list<-vector()
  for(i in dataFile_header){
    z<-is.na(get(j)[[i]]);
    if(sum(z)/length(get(j)[,1])>.10){
      NA_list<-c(NA_list, i)
    }
  }
  assign(a,NA_list);
}


#merge all NA_uniqueDate_lists to obtain columns to be deleted

NA_uniqueMerged_list<-vector()
for(i in NA_uniqueDate_list){
  NA_uniqueMerged_list<-c(NA_uniqueMerged_list, get(i));
}
NA_uniqueMerged_list<-unique(NA_uniqueMerged_list)




#forloop creating all unique date reduced data frames with appropriate columns removed

reduced_list<-vector("list", length=length(dataFile_dates));

for(i in 1:length(dataFile_uniqueDate_dataFrame_list)){
  dataFile_reduced<-get(dataFile_uniqueDate_dataFrame_list[i])[-which(dataFile_header %in% NA_uniqueMerged_list)];
  reduced_list[[i]]<-dataFile_reduced;
}


row_bind=function(x,y){rbind(x,y)};

#stacks the rows on top of one another
reduced_file<-Reduce(row_bind,reduced_list);



#remove NA's using na.omit from reduced data files
#remove NA's
reduced_file<-na.omit(reduced_file)
#View(reduced_file)
#remove duplicate columns date, dimension colum and reporting period
reduced_file<-reduced_file[,-c(3,5,6)];
#View(reduced_file)
dataFile_dates<-unique(reduced_file$calendardate)

################################################################### 
# part III - Importing financial tickers
################################################################### 
#Importing ticker
financial_tickers<-read.csv("companylist.csv")
dim(financial_tickers)
#Filtering for midcap tickers
financial_tickers<-financial_tickers[financial_tickers$MarketCap>=300000000 & financial_tickers$MarketCap<=2000000000,]


#Seting Factors
factors<-c("bvps","de","divyield","dps","ebit","ebitda","eps","fcf","fcfps","grossmargin","intangibles","ncfi",
           "netmargin","pb","pe","pe1","revenue","netinc","rnd","tbvps")


##################################
#Creating Training and Test Sets
##################################


#Traning data frames
training_data_frame_list<-vector()
for(i in 1:4){
  a<-paste("training_data_frame_",dataFile_dates[i+15],sep="")
  data<-reduced_file[which(reduced_file$calendardate %in% dataFile_dates[i:(i+14)]),]
  #filtering all the tickers in the financial sector
  data<-data[which(data$ticker %in% financial_tickers$Symbol),]
  assign(a,data)
  training_data_frame_list<-c(training_data_frame_list, a)
}



#Test data frames
test_data_frame_list<-vector()
for(i in 1:4){
  a<-paste("test_data_frame_",dataFile_dates[i+15],sep="")
  data<-reduced_file[which(reduced_file$calendardate==dataFile_dates[i+15]),]
  #filtering all the tickers in the financial sector
  data<-data[which(data$ticker %in% financial_tickers$Symbol),]
  assign(a,data)
  test_data_frame_list<-c(test_data_frame_list, a)
}


################################################################### 
# part IV - Generating a Coefficient Matrix List
################################################################### 
coeffMatrix_list<-vector()
for(j in 1:4){
  c<-paste("coeffMatrix_",dataFile_dates[j+15])
  
  fin_data<-get(training_data_frame_list[j])
  
  tkr_dates<-fin_data[,1:3]
  fin_data<-fin_data[,-(1:3)]
  
  #cast as data frame
  fin_data<-data.frame(apply(fin_data[,factors],2,scale))
  #prepend tickers
  fin_data<-cbind(tkr_dates,fin_data)
  #removing extreme only outliers in selected variables
  fin_data <- fin_data[fin_data$bvps > -4e+05,]
  fin_data <- fin_data[fin_data$de > -110000,]
  fin_data <- fin_data[fin_data$dps < 400,]
  fin_data <- fin_data[fin_data$ebit > -2e+10,]
  fin_data <- fin_data[fin_data$pb < 1.2e+07,]
  fin_data <- fin_data[fin_data$netinc < 4e+10,]
  fin_data <- fin_data[fin_data$tbvps < 60000,]
  
  
  #Making Linear Models
  ################################################################### 
  #unique dates
  dates<-sort(unique(fin_data$calendardate))
  
  #creating the formula
  form<-as.formula(paste("ln_returns~",paste(factors, collapse= "+")))
  form
  
  model_list<-vector()
  for(i in 1:length(dates)){
    data<-subset(fin_data, calendardate==dates[i])
    a<-paste("modelDate_",dates[i],sep="")
    model<-lm(form,data=data)
    assign(a, model)
    model_list<-c(model_list, a)
    
  }
  #Creating a matrix of Coefficients for given Time Step
  ################################################################### 
  
  coeff_list<-vector()
  for(i in 1:length(model_list)){
    a<-paste("coeffDate_", dates[i],sep="")
    coeff<-coefficients(get(model_list[i]))
    assign(a,coeff)
    coeff_list<-c(coeff_list, a)
    
  }
  
  coeffMatrix<-vector()
  for(i in 1:length(model_list)){
    coeffMatrix<-cbind(coeffMatrix,get(coeff_list[i]))
  }
  
  colnames(coeffMatrix)<-dates[1:15]
  assign(c, coeffMatrix)
  coeffMatrix_list<-c(coeffMatrix_list,c)
}


###################################333
#Generating Beta's for Next Time Step
##################################3
library(forecast)
aic_Matrix<-vector()
arima_models_list<-vector()
returns_bucket_list<-vector()
for(timeStep in 1:4){
  t_bucket<-paste("buckets_time_step_",(15+timeStep),sep='')
  
  #Retrieving Training set
  training_dat<-get(training_data_frame_list[timeStep])
  
  #Training Mean
  m<-apply(training_dat[,factors],2,mean)
  #Training Sd
  s<-apply(training_dat[,factors],2,sd)
  
  #Retrieving next time step set
  next.step<-get(test_data_frame_list[timeStep])
  
  next.tkr_dates<-next.step[,1:3]
  next.step<-next.step[,-(1:3)]
  
  # Scaling next time step
  next.step<-scale(next.step[,factors],center=m,scale=s)
  next.step<-cbind(next.tkr_dates,next.step[,factors])
  
  cat("\n\nTime Step ", 15+timeStep,'\n')
  
  beta<-vector()
  aic<-vector()
  for(i in 1:21){
    model<-auto.arima(get(coeffMatrix_list[timeStep])[i,], max.p = 15,max.q = 15, seasonal = T)
    #to make sure there's no patter inside the residuals
    aic<-c(aic, model$aic) #aic should be as small as possible good number is <100
    #box and jenkins
    #hist(model$residuals)
    beta<-c(beta, predict(model,n.ahead=1)$pred[1])
  }
  aic_Matrix<-cbind(aic_Matrix,aic)
  
  num_obs=dim(next.step[,factors])[1]
  expected_returns<-as.matrix(rep(1,dim(next.step)[1])*beta[1])+as.matrix(next.step[,factors])%*%beta[2:21]
  next.step<-cbind(expected_returns,next.step)
  exp_buckets<-vector()
  next.step<-next.step[order(-expected_returns),]
  
  i=0
  buckets_return<-vector()
  for(b in seq(1,num_obs,num_obs/5)){
    i=i+1
    a<-paste('bucket_',i,sep="")
    ex<-next.step[b:(b+num_obs/5-1),]
    exp_return=mean(ex$expected_returns)
    act_return=mean(mean(ex$ln_returns))
    ret<-c(exp_return,act_return)
    buckets_return<-rbind(buckets_return,ret)
  }
  colnames(buckets_return)<-c('exp', 'act')
  rownames(buckets_return)<-1:5
  assign(t_bucket, buckets_return)
  returns_bucket_list<-c(returns_bucket_list, t_bucket)
}

#displaying the Data in a neat form
for(b in 1:4){
  bucketMatrix<-get(returns_bucket_list[b])
  dis<-vector()
  dis<-cbind(bucketMatrix[,1],rank(-bucketMatrix[,1]),bucketMatrix[,2],rank(-bucketMatrix[,2]))
  colnames(dis)<-c('expected return','expected rank','actual return','actual rank')
  rownames(dis)<-1:5
  cat("\n\nFor Date ", dataFile_dates[b+15],'\n\n')
  print(dis)
}

