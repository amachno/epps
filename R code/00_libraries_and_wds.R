library(dplyr)
library(xlsx)

#those are for particular computer, to modify!

dir14<-"H:/dane tikowe/2014"
data.dir<-function(year){
  paste("H:/dane tikowe/",as.character(year), sep="")
}
work.dir<-"C:/Users/Artur/epps/R_new"
setwd(work.dir)
#ustawia workdir
wd<-function() setwd(work.dir)
#ustawia datadir
dd<-function(n=2014) setwd(data.dir(n))