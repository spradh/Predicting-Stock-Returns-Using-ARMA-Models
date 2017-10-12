#import data
dataFile<-read.csv("refinedData")


#collect headers
dataFile_header<-names(dataFile)

#create list of dates
dataFile_dates<-unique(dataFile$calendardate)



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


#merge all NA_uniqueDate_lists to obtain col;umns to be deleted

NA_uniqueMerged_list<-vector()
for(i in NA_uniqueDate_list){
  NA_uniqueMerged_list<-c(NA_uniqueMerged_list, get(i));
}
NA_uniqueMerged_list<-unique(NA_uniqueMerged_list)

reduced_list<-vector("list", length=length(dataFile_dates))


#forloop creating all unique date reduced data frames with appropriate columns removed

reduced_dataFile_uniqueDate_dataFrame_list<-vector();
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

#remove columns
reduced_file<-reduced_file[,-5];

#remove column
reduced_file<-reduced_file[,-2];

#store tickers
tickers_dates<-reduced_file[,1:3];

#remove non-factor columns
reduced_file<-reduced_file[,-(3:1)];

#scale all non returns columns
reduced_file<-apply(reduced_file,2,scale);

#cast as data frame
reduced_file<-data.frame(reduced_file);

#prepend tickers
reduced_file<-cbind(tickers_dates, reduced_file);
View(reduced_file)

write.csv(reduced_file,file="finalData.csv")
