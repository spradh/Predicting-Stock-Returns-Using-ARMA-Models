######################
# part 0 - Setting Directory
######################
filename = "maya_pradhan.R"
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

#writing modified file onto refinedData.csv
#View(factorDataReduced)
#write.csv(factorDataReduced,file="refinedData")

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

View(reduced_file)
#remove NA's using na.omit from reduced data files
#remove NA's
reduced_file<-na.omit(reduced_file)

#remove columns date
reduced_file<-reduced_file[,-5];

#remove column
reduced_file<-reduced_file[,-3];

#store tickers
tickers_dates<-reduced_file[, 1:3];
View(tickers_dates)

#remove non-factor columns
reduced_file<-reduced_file[,-(3:1)];

#scale all non returns columns
reduced_file<-apply(reduced_file,2,scale);

#cast as data frame
reduced_file<-data.frame(reduced_file);

#prepend tickers
reduced_file<-cbind(tickers_dates, reduced_file);
View(reduced_file)
reduced_file_header<-names(reduced_file)

#removing extreme only outliers in selected variables
reduced_file <- reduced_file[reduced_file$bvps > -4e+05,]
reduced_file <- reduced_file[reduced_file$de > -110000,]
reduced_file <- reduced_file[reduced_file$dps < 400,]
reduced_file <- reduced_file[reduced_file$ebit > -2e+10,]
reduced_file <- reduced_file[reduced_file$pb < 1.2e+07,]
reduced_file <- reduced_file[reduced_file$netinc < 4e+10,]
reduced_file <- reduced_file[reduced_file$tbvps < 60000,]



################################################################### 
# part III - Importing financial tickers
################################################################### 
#Importing ticker
financial_tickers<-read.csv("companylist.csv")
#filtering all the tickers in the financial sector
financial_data<-reduced_file[reduced_file$ticker %in% financial_tickers$Symbol,]
View(financial_data)

################################################################### 
# part IV - Making Linear Models
################################################################### 

#Factors
factors<-c("bvps","de","divyield","dps","ebit","ebitda","eps","fcf","fcfps","grossmargin","intangibles","ncfi",
           "netmargin","pb","pe","pe1","revenue","netinc","rnd","tbvps")
#unique dates
dates<-sort(unique(financial_data$calendardate))

length(factors)

#creating the formula
form<-as.formula(paste("ln_returns~",paste(factors, collapse= "+")))
model_list<-vector()



for(i in 1:15){
  data=subset(financial_data, calendardate==dates[i])
  a<-paste("modelDate_",dates[i],sep="")
  model_list<-c(model_list, a)
  model<-lm(form,data=data)
  assign(a, model)
}



################################################################### 
# part V - Creating a matrix of Coefficients
################################################################### 

coeff_list<-vector()
for(i in 1:length(model_list)){
  a<-paste("coeffDate_", dates[i],spe="")
  coeff_list<-c(coeff_list, a)
  coeff<-coefficients(get(model_list[i]))
  assign(a,coeff)
}

coeffMatrix<-vector()
for(i in 1:15){
  coeffMatrix<-cbind(coeffMatrix,get(coeff_list[i]))
}
dates
colnames(coeffMatrix)<-dates[1:15]
coeffMatrix

summary_list<-vector()
for(i in 1:length(model_list)){
  a<-paste("summaryDate_", dates[i],spe="")
  summary_list<-c(summary_list, a)
  sum<-summary(get(model_list[i]))
  assign(a,sum)
}

