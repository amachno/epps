


#choose sample (which days to take) and intervals (which time horizons)

pic.sample<-(1:182)
pic.intervals<-seq(1,30, by=1)


dd()
pic.data.1.30<-list(
            KGHM_PKO=corr.intraday.ave("KGHM", "PKOBP",pic.sample, pic.intervals),
            KGHM_PGE=corr.intraday.ave("KGHM", "PGE", pic.sample, pic.intervals),
            KGHM_PZU=corr.intraday.ave("KGHM", "PZU", pic.sample, pic.intervals),
            PKO_PGE=corr.intraday.ave("PKOBP", "PGE", pic.sample, pic.intervals),
            PKO_PZU=corr.intraday.ave("PKOBP", "PZU", pic.sample, pic.intervals),
            PGE_PZU=corr.intraday.ave("PGE", "PZU", pic.sample, pic.intervals)
            )
wd()

dd()
pic.data.correct.1.30<-list(
      KGHM_PKO.c=corr.correct.intraday.ave("KGHM", "PKOBP",pic.sample, pic.intervals),
      KGHM_PGE=corr.correct.intraday.ave("KGHM", "PGE", pic.sample, pic.intervals),
      KGHM_PZU=corr.correct.intraday.ave("KGHM", "PZU", pic.sample, pic.intervals),
      PKO_PGE=corr.correct.intraday.ave("PKOBP", "PGE", pic.sample, pic.intervals),
      PKO_PZU=corr.correct.intraday.ave("PKOBP", "PZU", pic.sample, pic.intervals),
      PGE_PZU=corr.correct.intraday.ave("PGE", "PZU", pic.sample, pic.intervals)
)
wd()

#full
pic.sample<-(1:182)
pic.intervals<-seq(1,60, by=1/60)

dd()
pic.data.f<-list(
      KGHM_PKO=corr.intraday.ave("KGHM", "PKOBP",pic.sample, pic.intervals),
      KGHM_PGE=corr.intraday.ave("KGHM", "PGE", pic.sample, pic.intervals),
      KGHM_PZU=corr.intraday.ave("KGHM", "PZU", pic.sample, pic.intervals),
      PKO_PGE=corr.intraday.ave("PKOBP", "PGE", pic.sample, pic.intervals),
      PKO_PZU=corr.intraday.ave("PKOBP", "PZU", pic.sample, pic.intervals),
      PGE_PZU=corr.intraday.ave("PGE", "PZU", pic.sample, pic.intervals)
)
wd()

dd()
pic.data.correct.f<-list(
      KGHM_PKO.c=corr.correct.intraday.ave("KGHM", "PKOBP",pic.sample, pic.intervals),
      KGHM_PGE=corr.correct.intraday.ave("KGHM", "PGE", pic.sample, pic.intervals),
      KGHM_PZU=corr.correct.intraday.ave("KGHM", "PZU", pic.sample, pic.intervals),
      PKO_PGE=corr.correct.intraday.ave("PKOBP", "PGE", pic.sample, pic.intervals),
      PKO_PZU=corr.correct.intraday.ave("PKOBP", "PZU", pic.sample, pic.intervals),
      PGE_PZU=corr.correct.intraday.ave("PGE", "PZU", pic.sample, pic.intervals)
)
wd()

#plot
par(mfrow=c(2,3))
for (i in 1:6){
      plot(pic.data.1.30[[i]][,1], pic.data.1.30[[i]][,2], pch=19,
           ylim=c(min(pic.data.1.30[[i]][,2],pic.data.correct.1.30[[i]][,2]),
                  max(pic.data.1.30[[i]][,2],pic.data.correct.1.30[[i]][,2])),
           col=rgb(0,0,1, alpha=.5), 
           xlab="Interval length", 
           ylab="correlation",
           main=names(pic.data.1.30)[i]
           ) 
      points(pic.data.correct.1.30[[i]][,1], pic.data.correct.1.30[[i]][,2], pch=19, col=rgb(0,1,0, alpha=1))
      legend('bottomright', pch=19, 
             col=c(rgb(0,0,1, alpha=.5), rgb(0,1,0, alpha=1)), 
             legend=c('realized', 'corrected')
            )
}

#plot
setwd('./pictures')
png('full_epps.png')
par(mfrow=c(2,3))
for (i in 1:6){
      plot(pic.data.f[[i]][,1], pic.data.f[[i]][,2], pch=20, t='l',
                ylim=c(min(pic.data.f[[i]][,2],pic.data.correct.f[[i]][,2]),
                max(pic.data.f[[i]][,2],pic.data.correct.f[[i]][,2])),
           col=rgb(0,0,1, alpha=.5), 
           xlab="Interval length", 
           ylab="correlation",
           main=names(pic.data.f)[i]
      ) 
      lines(pic.data.correct.f[[i]][,1], pic.data.correct.f[[i]][,2], pch=20, col=rgb(0,1,0, alpha=.5))
      legend('bottomright', pch=20, 
             col=c(rgb(0,0,1, alpha=.5), rgb(0,1,0, alpha=.5)), 
             legend=c('realized', 'corrected')
      )
}
dev.off()
setwd('./..')

setwd('./pictures')
png('1_30_epps.png')
par(mfrow=c(2,3))
for (i in 1:6){
      plot(pic.data.1.30[[i]][,1], pic.data.1.30[[i]][,2], pch=20, t='l',
           ylim=c(min(pic.data.1.30[[i]][,2],pic.data.correct.1.30[[i]][,2]),
                  max(pic.data.1.30[[i]][,2],pic.data.correct.1.30[[i]][,2])),
           col=rgb(0,0,1, alpha=.5), 
           xlab="Interval length (min)", 
           ylab="correlation",
           main=names(pic.data.1.30)[i]
      ) 
      lines(pic.data.correct.1.30[[i]][,1], pic.data.correct.1.30[[i]][,2], pch=20, col=rgb(0,1,0, alpha=.5))
      legend('bottomright', lty=1, 
             col=c(rgb(0,0,1, alpha=.5), rgb(0,1,0, alpha=.5)), 
             legend=c('realized', 'corrected')
      )
}
dev.off()
setwd('./..')

setwd('./pictures')
png('1_30_epps_alt.png')
par(mfrow=c(3,2))
for (i in 1:6){
      plot(pic.data.1.30[[i]][,1], pic.data.1.30[[i]][,2], pch=20, t='l',
           ylim=c(min(pic.data.1.30[[i]][,2],pic.data.correct.1.30[[i]][,2]),
                  max(pic.data.1.30[[i]][,2],pic.data.correct.1.30[[i]][,2])),
           col=rgb(0,0,1, alpha=.5), 
           xlab="Interval length (min)", 
           ylab="correlation",
           main=names(pic.data.1.30)[i]
      ) 
      lines(pic.data.correct.1.30[[i]][,1], pic.data.correct.1.30[[i]][,2], pch=20, col=rgb(0,1,0, alpha=.5))
      if (i==1) {
            legend('bottomright', lty=1, 
             col=c(rgb(0,0,1, alpha=.5), rgb(0,1,0, alpha=.5)), 
             legend=c('realized', 'corrected')
            )
      }      
}
dev.off()
setwd('./..')