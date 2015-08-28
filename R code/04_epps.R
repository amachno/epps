#calculates correlation for data (data1, data2) from read.asset function, 
#for intraday returns of the lengths (i.l)
corr.intraday<- function(data1,data2, n.l=seq(1,30,by=1)){
      #liczymy korelacje
      corr<-c()
      for (interval in n.l) {
            x1<-data.min(data1, n=interval)$r
            x2<-data.min(data2, n=interval)$r
            corr<-c(corr, cor(x1,x2))
      }
      output<-cbind(n.l, corr)
      colnames(output)<-c('minutes', 'correlations')
      output
}



#combine data from more days, it produces not very clean data
data.sample<-function(asset, sample){
      dd()
      output<-list()
      for (i in 1:length(sample)){
            output[[i]]<-read.asset(asset=asset, yn=sample[i])
      }
      wd()
      output
}

#averaging correlations
corr.intraday.ave<-function(asset1, asset2, sample=c(1,2), n.l=seq(1,30,by=1)){
      sample1<-data.sample(asset=asset1, sample=sample)
      sample2<-data.sample(asset=asset2, sample=sample)
      #pierwsza kolumna to minuty, druga to pierwszy dzien
      corr<-corr.intraday(sample1[[1]], sample2[[1]], n.l=n.l)
      for (i in 1:length(sample[-1])){
            part<-corr.intraday(sample1[[i]], sample2[[i+1]], n.l=n.l)[,2]
            corr<-cbind(corr, part)
      }
      output<-cbind(corr[,1], rowMeans(corr[,-1]))
      colnames(output)<-c('minutes', 'correlations')
      output
}



