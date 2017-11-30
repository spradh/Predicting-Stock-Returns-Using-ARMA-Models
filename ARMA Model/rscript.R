######################
# part 0 - Setting Directory
######################
filename = "maya_pradhan1.R"
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



#remove NA's using na.omit from reduced data files
#remove NA's
reduced_file<-na.omit(reduced_file)
View(reduced_file)
#remove duplicate columns date, dimension colum and reporting period
reduced_file<-reduced_file[,-c(3,5,6)];
View(reduced_file)


##################################
#Cleaning data and scaling
##################################

reduced_file_header<-names(reduced_file)

#Traning data frames
training_data_frame_list<-vector()
for(i in 1:5){
  a<-paste("training_data_frame_",dataFile_dates[i+15],sep="")
  data<-reduced_file[which(reduced_file$calendardate %in% dataFile_dates[i:(i+14)]),]
  assign(a,data)
  training_data_frame_list<-c(training_data_frame_list, a)
}

training_data_frame_list

#Test data frames
test_data_frame_list<-vector()
for(i in 1:5){
  a<-paste("test_data_frame_",dataFile_dates[i+15],sep="")
  data<-reduced_file[which(reduced_file$calendardate==dataFile_dates[i+15]),]
  assign(a,data)
  test_data_frame_list<-c(test_data_frame_list, a)
}


################################################################### 
# part III - Importing financial tickers
################################################################### 
#Importing ticker
financial_tickers<-read.csv("companylist.csv")

#Seting Factors
factors<-c("bvps","de","divyield","dps","ebit","ebitda","eps","fcf","fcfps","grossmargin","intangibles","ncfi",
           "netmargin","pb","pe","pe1","revenue","netinc","rnd","tbvps")

coeffMatrix_list<-vector()
for(i in 1:5){
  c<-paste("coeffMatrix_",dataFile_dates[i+15])
  
  fin_data<-get(training_data_frame_list[i])
  #filtering all the tickers in the financial sector
  fin_data<-fin_data[fin_data$ticker %in% financial_tickers$Symbol,]
  
  
  tkr_dates<-fin_data[,1:3];
  fin_data<-fin_data[,-(1:3)]
  
  #cast as data frame
  fin_data<-data.frame(apply(fin_data[,factors],2,scale))
  #prepend tickers
  fin_data<-cbind(tkr_dates,fin_data[,factors])
  #removing extreme only outliers in selected variables
  fin_data <- fin_data[fin_data$bvps > -4e+05,]
  fin_data <- fin_data[fin_data$de > -110000,]
  fin_data <- fin_data[fin_data$dps < 400,]
  fin_data <- fin_data[fin_data$ebit > -2e+10,]
  fin_data <- fin_data[fin_data$pb < 1.2e+07,]
  fin_data <- fin_data[fin_data$netinc < 4e+10,]
  fin_data <- fin_data[fin_data$tbvps < 60000,]
  
  ################################################################### 
  # part IV - Making Linear Models
  ################################################################### 
  
  
  #unique dates
  dates<-sort(unique(fin_data$calendardate))
  
  #creating the formula
  form<-as.formula(paste("ln_returns~",paste(factors, collapse= "+")))
  form
  
  model_list<-vector()
  for(i in 1:15){
    data<-subset(fin_data, calendardate==dates[i])
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
  for(i in 1:length(model_list)){
    coeffMatrix<-cbind(coeffMatrix,get(coeff_list[i]))
  }
  
  
  colnames(coeffMatrix)<-dates[1:15]
  dim(coeffMatrix)
  
  
  assign(c, coeffMatrix)
  coeffMatrix_list<-c(coeffMatrix_list,c)
}




###################################333
# Scaling next time step
###################################3
training_dat<-get(training_data_frame_list[1])
dim(training_dat)


#Training Mean
m<-apply(training_dat[,factors],2,mean)
#Training Sd
s<-apply(training_dat[,factors],2,sd)

next.step<-get(test_data_frame_list[1])


next.tkr_dates<-next.step[,1:3];
next.step<-next.step[,-(1:3)]

#cast as data frame
View(next.step)
next.step<-apply(next.step[,factors],2,scale, center=m ,scale=s)
next.step<-cbind(next.tkr_dates,next.step[,factors])
View(next.step)
dim(next.step)



timeStep=1
  
cat("\n\nTime Step ", timeStep,'\n')
p=1
q=1

beta<-vector()
for(i in 1:21){
  model<-arima(get(coeffMatrix_list[1])[i,], order = c(p,0,q), method='ML')
  model
  #to make sure there's no patter inside the residuals
  cat("for beta ",i,'\n')
  print(model$aic) #aic should be as small as possible good number is <100
  #box and jenkins
  beta<-c(beta, predict(model,n.ahead=1)$pred[1])
}
  
beta
m
s


num_obs=dim(next.step[,factors])[1]

expected_returns<-as.matrix(rep(1,dim(next.step)[1])*beta[1])+as.matrix(next.step[,factors])%*%beta[2:21]

dim(expected_returns)

hist(expected_returns, breaks = 50) #histogram is much more optimistic
hist(next.step$ln_returns, breaks = 50)
mean(next.step$ln_returns)
next.step<-cbind(expected_returns,next.step)
dim(next.step)

exp_buckets<-vector()
i=0
next.step<-next.step[order(-next.step$expected_returns),]
View(next.step)

for(b in seq(1,num_obs,num_obs/5)){
  i=i+1
  a<-paste('bucket_',i,sep="")
  ex<-next.step[i:(i+num_obs/5-1),]
  cat('for bucket ',i,' expected value is ', mean(ex$expected_returns),'and  actual value is ', mean(ex$ln_returns),'\n')
  }

