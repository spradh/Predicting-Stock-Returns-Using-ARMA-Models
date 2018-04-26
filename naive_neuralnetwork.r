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

#removing outliers
reduced_file <- reduced_file[reduced_file$bvps > -4e+05,]
reduced_file <- reduced_file[reduced_file$de > -110000,]
reduced_file <- reduced_file[reduced_file$dps < 400,]
reduced_file <- reduced_file[reduced_file$ebit > -2e+10,]
reduced_file <- reduced_file[reduced_file$pb < 1.2e+07,]
reduced_file <- reduced_file[reduced_file$netinc < 4e+10,]
reduced_file <- reduced_file[reduced_file$tbvps < 60000,]

dataFile_dates<-unique(reduced_file$calendardate)

################################################################### 
# part III - Importing financial tickers
################################################################### 
#Importing ticker
financial_tickers<-read.csv("companylist.csv")
#Filtering for midcap tickers
financial_tickers<-financial_tickers[financial_tickers$MarketCap>=2000000000 & financial_tickers$MarketCap<=10000000000,]


#Seting Factors
factors<-c("bvps","de","divyield","dps","ebit","ebitda","eps","fcf","fcfps","grossmargin","intangibles","ncfi",
           "netmargin","pb","pe","pe1","revenue","netinc","rnd","tbvps")


##################################
#Creating Training and Test Sets
##################################


#Traning data frames
training_data_frame_list<-vector()
for(i in 1:4){
  a<-paste("training_data_frame_",dataFile_dates[i+14],sep="")
  data<-reduced_file[which(reduced_file$calendardate %in% dataFile_dates[(i+14)]),]
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
model_list<-vector()
library(neuralnet)
for(j in 1:4){

  
  train<-get(training_data_frame_list[j])
  
  tkr_dates<-train[,1:3]
  train<-train[,-(1:3)]
  
  #cast as data frame
  train<-data.frame(apply(train[,factors],2,scale))
  #prepend tickers
  train<-cbind(tkr_dates,train)

  #Making NN Models
  ################################################################### 

  #creating the formula
  form<-as.formula(paste("ln_returns~",paste(factors, collapse= "+")))
  form


  a<-paste("modelDate_",dataFile_dates[(14+j)],sep="")
  model<-neuralnet(form,train,hidden=14,stepmax = 1e+06)
  assign(a, model)
  model_list<-c(model_list, a)
  
}
dataFile_dates
model_list

bucket_list<-vector()
for(i in 1:4){
  a<-paste('bucket_',dataFile_dates[i],sep="")
  test<-get(test_data_frame_list[i])
  train<-get(training_data_frame_list[i])
  m<-apply(train[,factors],2,mean)
  s<-apply(train[,factors],2,sd)
  tkr_dates<-test[,1:3]
  test<-test[,-(1:3)]
  test<-scale(test[,factors],center=m,scale=s)
  test<-cbind(tkr_dates,test)
  pred<-compute(get(model_list[i]),test[,factors])
  test<-cbind(pred$net.result,test)
  colnames(test)[1]<-"pred"
  #sorting test test according to predicted returns
  test<-test[order(-test$pred),]
  num_obs=dim(test)[1]
  buckets_return<-vector()
  j=0
  for(b in seq(1,num_obs,num_obs/5)){
    j=j+1
    a<-paste('bucket_',j,sep="")
    ex<-test[b:(b+num_obs/5-1),]
    exp_return=mean(ex$pred)
    act_return=mean(mean(ex$ln_returns))
    ret<-c(exp_return,act_return)
    if(j==1){
      buckets_return<-ret
    }else{
      buckets_return<-rbind(buckets_return, ret)
    }
  }
  colnames(buckets_return)<-c('exp', 'act')
  rownames(buckets_return)<-1:5
  assign(a,buckets_return)
  bucket_list<-c(bucket_list, a)
}
#displaying the Data in a neat form
for(b in 1:4){
  bucketMatrix<-get(bucket_list[b])
  dis<-vector()
  dis<-cbind(bucketMatrix[,1],rank(-bucketMatrix[,1]),bucketMatrix[,2],rank(-bucketMatrix[,2]))
  colnames(dis)<-c('expected return','expected rank','actual return','actual rank')
  rownames(dis)<-1:5
  cat("\n\nFor Date ", dataFile_dates[b+15],'\n\n')
  print(dis)
}
