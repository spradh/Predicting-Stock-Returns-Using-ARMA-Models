data<-read.csv("companylist.csv")
View(data)
View(data[data$MarketCap>=2000000000 & data$MarketCap<=10000000000,])
tkr<-data[,1]
tkr
