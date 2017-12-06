######################
# part 0 - Setting Directory
######################
filename = "neuralhw.R"
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
View(reduced_file)
#remove duplicate columns date, dimension colum and reporting period
reduced_file<-reduced_file[,-c(3,5,6)];
View(reduced_file)

################################################################### 
# part III - Importing financial tickers
################################################################### 
#Importing ticker
financial_mid_cap_tickers<-read.csv("companylist.csv")
financial_mid_cap_tickers<-financial_mid_cap_tickers[financial_mid_cap_tickers$MarketCap>=2000000000 & financial_mid_cap_tickers$MarketCap<=10000000000,]
#Seting Factors
factors<-c("bvps","de","divyield","dps","ebit","ebitda","eps","fcf","fcfps","grossmargin","intangibles","ncfi",
           "netmargin","pb","pe","pe1","revenue","netinc","rnd","tbvps")


##################################
#Creating Training and Test Sets
##################################


#Traning data frames
training_data_frame_list<-vector()
for(i in 1:3){
  a<-paste("training_data_frame_",dataFile_dates[i],sep="")
  data<-reduced_file[which(reduced_file$calendardate %in% dataFile_dates[i]),]
  #filtering all the tickers in the financial sector
  data<-data[which(data$ticker %in% financial_mid_cap_tickers$Symbol),]
  assign(a,data)
  training_data_frame_list<-c(training_data_frame_list, a)
}


################################################################### 
# part IV - Generating a Models
################################################################### 
library(neuralnet)
#creating the formula
form<-as.formula(paste("ln_returns~",paste(factors, collapse= "+")))
form

nn_model_list<-vector()
for(i in 1:3){
  a<-paste("Date_",i,sep="")

  data<-get(training_data_frame_list[i])

  tkr_dates<-data[,1:3]
  data<-data[,-(1:3)]
  
  data<-data.frame(apply(data[,factors],2,scale))
  
  data<-cbind(tkr_dates,data)
  #removing extreme only outliers in selected variables
  data <- data[data$bvps > -4e+05,]
  data <- data[data$de > -110000,]
  data <- data[data$dps < 400,]
  data <- data[data$ebit > -2e+10,]
  data <- data[data$pb < 1.2e+07,]
  data <- data[data$netinc < 4e+10,]
  data <- data[data$tbvps < 60000,]
  
  
  cat("Models for date: ",dataFile_dates[i],"\n")
  
  
  #Model 1
  cat("Model 1\n")
  b<-paste(a,paste("_Model_",1,sep=""))
  set.seed(1)
  nn_model<-neuralnet(form,data,hidden=c(13),stepmax=1e6)
  assign(b,nn_model)
  nn_model_list<-c(nn_model_list, b)
  
  
  #Model 2
  cat("Model 2\n")
  b<-paste(a,paste("_Model_",2,sep=""))
  set.seed(1)
  nn_model<-neuralnet(form,data[,],hidden=c(14),stepmax=1e6)
  #plot(nn_model)
  nn_pred<-compute(nn_model,data[,factors])
  nn_model_list<-c(nn_model_list, b)
  
  #Model 3
  cat("Model 3\n")
  b<-paste(a,paste("_Model_",3,sep=""))
  set.seed(1)
  nn_model<-neuralnet(form,data,hidden=c(15),stepmax=1e6)
  nn_model_list<-c(nn_model_list, b)
  
  
  
  cat("==========================\n")
}
