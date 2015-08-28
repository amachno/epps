#function which generates n-minute data from tick data, data is from read.asset function
#values 
#$p- price vector
#$r- return vector
#$log- log return vector
data.min<- function(data, n=10){
  #POSIXct where trade begins and ends
  day<-strftime(data[1,1],'%Y-%m-%d') 
  start<-paste(day, '09:00:00')
  start<-as.POSIXct(start)
  end<-paste(day, '16:50:00')
  end<-as.POSIXct(end)
  
  #times for processed data
  times<-seq(start, end, n*60)
  #last price before end of the interval
  price<-c(data[1,2])
  l.d<-dim(data)[1]
  l.t<-length(times)
  for (i in 2:l.t){
    index<-max((1:l.d)[data[,1]<times[i]], na.rm=T)
    price<-c(price,data[index,2])
  }
  names(price)<-strftime(times, ' %H:%M')
  return<-(price[-1]-price[-l.t])/price[-l.t]
  names(return)<-strftime(times[-l.t], ' %H:%M')
  log.return<-log(return+1)
  list(p=price, r=return, log=log.return, time.p=times, time.r=times[-l.t])
}