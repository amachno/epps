#outputs gammas and standarized returns g for data from read.asset
data.min.epps<- function(data, n=10){
      #POSIXct where trade begins and ends
      day<-strftime(data[1,1],'%Y-%m-%d') 
      start<-paste(day, '09:00:00')
      start<-as.POSIXct(start)
      end<-paste(day, '16:50:00')
      end<-as.POSIXct(end)
      
      #times for processed data
      times<-seq(start, end, n*60)
      #last price before end of the interval
      
      l.d<-dim(data)[1]
      l.t<-length(times)
      
      #indeksy dla gammy
      index<-c(1)
      for (i in 2:l.t){
            index[i]<-max((1:l.d)[data[,1]<times[i]], na.rm=T)
      }
      price<-data[index,2]
      gamma<-data[index,1]
      return<-(price[-1]-price[-l.t])/price[-l.t]
      log.return<-log(return+1)
      g<-(log.return-mean(log.return))/sd(log.return)
      data.frame(gamma=gamma[-1], norm.log.return=g)
}
#corrected correlation as in the article
cor.correct<-function(x,y, interval=1){
      day<-strftime(x[1,1],'%Y-%m-%d') 
      start<-paste(day, '09:00:00')
      start<-as.POSIXct(start)
      
      #dane minutowe bez powtorzen znormalizowane
      x.c<-data.min.epps(x,interval)
      x.c[is.na(x.c)]<-0
      y.c<-data.min.epps(y,interval)
      y.c[is.na(y.c)]<-0
      
      gamma.x<-x.c$gamma
      gamma.y<-y.c$gamma
      g.x<-x.c$norm.log.return[-dim(x.c)[1]]
      g.y<-y.c$norm.log.return[-dim(y.c)[1]]
      
      #czasy miedzy gammami
      N.x<-difftime(gamma.x,c(start, gamma.x[-length(gamma.x)]), units="secs")
      N.y<-difftime(gamma.y,c(start, gamma.y[-length(gamma.y)]), units="secs")
      N.x<-as.numeric(N.x)
      N.y<-as.numeric(N.y)
      N.a<-sqrt(mean(N.x[N.x!=0])*mean(N.y[N.y!=0]))
      
      #znormalizowane gammy
      gamma.x.n<-as.numeric(difftime(gamma.x, start), units="secs")
      gamma.y.n<-as.numeric(difftime(gamma.y, start), units="secs")
      #konce przedziałów overlapping
      min.gamma<-apply(cbind(gamma.x.n,gamma.y.n),1,min)
      max.gamma<-apply(cbind(gamma.x.n,gamma.y.n),1,max)
      #czasy overlapping
      overlap<-cbind(max.gamma[-length(max.gamma)], min.gamma[-1])
      N<-overlap[,2]-overlap[,1]
      #n<-length(N)
      
      
      #wynik
      mean((g.x*g.y*N.a)[N>0]/N[N>0])
}

#analogy do corr.interval
corr.correct.intraday<- function(data1,data2, n.l=seq(1,30,by=1)){
      #liczymy korelacje
      corr<-c()
      for (interval in n.l) {
            corr<-c(corr, cor.correct(data1,data2, interval))
      }
      output<-cbind(n.l, corr)
      colnames(output)<-c('minutes', 'correlations')
      output
}

#analogy do corr.intraday.ave
corr.correct.intraday.ave<-function(asset1, asset2, sample=c(1,2), n.l=seq(1,30,by=1)){
      sample1<-data.sample(asset=asset1, sample=sample)
      sample2<-data.sample(asset=asset2, sample=sample)
      #pierwsza kolumna to minuty, druga to pierwszy dzien
      corr<-corr.correct.intraday(sample1[[1]], sample2[[1]], n.l=n.l)
      for (i in 1:length(sample[-1])){
            part<-corr.correct.intraday(sample1[[i+1]], sample2[[i+1]], n.l=n.l)[,2]
            corr<-cbind(corr, part)
            print(c("sample", i))
      }
      output<-cbind(corr[,1], rowMeans(corr[,-1]))
      colnames(output)<-c('minutes', 'correlations')
      output
}