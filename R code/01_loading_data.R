#for given year, month and day it gives me the file name, maybe to modify!
file.name <- function(year,month,day){
  y<-as.character(year)
  m<-as.character(month)
  if (month<10) m<-paste('0',m, sep='')
  d<-as.character(day)
  if (day<10) d<-paste('0',d, sep='')
  date<-paste(y,m,d, sep='')
  fn<-paste('H:/dane tikowe/',y,'/GPW',date,'000tic.dat', sep='')
  fn
}

#the function which loads the data, it work correctly for 2014 now
tick.data<-function(date=NULL, yn=NULL){
  
  if (length(date)>0){
    #set dir from which to import data
    setwd(data.dir(2014))
    #file name to import
    fn<-file.name(2014, date[1],date[2])
  } 
  if (length(yn)>0){
    #set dir from which to import data
    setwd(data.dir(2014))
    #file name to import
    fn<-list.files()[yn]
  }
  data<-read.table(fn, sep=",", colClasses=c('character', 'NULL', 'NULL', 'factor', 'factor', 'character', 'numeric', rep('NULL', 4)))
  data<-split(data, data$V4)$XWAR
  data<-data[,-2]
  colnames(data)<-c('date.char',  'name', 'time.char','price')
  setwd(work.dir) 
  data
}

#the function for obtaining data of the given asset
#output: time (POSIXlt), name (factor) price (num)
read.asset<- function(asset, date=NULL, yn=NULL){
  full.data<-tick.data(date=date, yn=yn)
  data<-split(full.data, full.data$name)[[asset]]
  pm17 <- as.POSIXlt(paste(data$date.char, '16:59:59'))[1]
  pm9 <- as.POSIXlt(paste(data$date.char, '09:00:00'))[1]
  data$time<-as.POSIXlt(paste(data$date.char, data$time.char))
  data<-data.frame(time=data$time, price=data$price)
  data<-data[data$time>=pm9 & data$time<pm17,]
  arrange(data, time)
  }
#version with all transaction instead of those 9.00-16.50
read.asset.1<- function(asset, date=NULL, yn=NULL){
  full.data<-tick.data(date=date, yn=yn)
  data<-split(full.data, full.data$name)[[asset]]
  data$time<-as.POSIXlt(paste(data$date.char, data$time.char))
  data<-data.frame(time=data$time, price=data$price)
  arrange(data, time)
}