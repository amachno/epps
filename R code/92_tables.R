#duration for data from read.asset
dur.f <- function(data) {
  #data length
  l.d <- dim(data)[1]
  #obcięte czasy
  temp1 <- data[2:l.d,1]
  temp0 <- data[1:(l.d - 1),1]
  
  #result
  as.numeric(difftime(temp1,temp0, units = 'secs'))
}

#creating the list of durations
duration <- list()
for (i in 1:8)
  duration[[i]] <- dur.f(data.0[[i]])
names(duration) <- names(data.0)

#descriptive statistics
desc.dur <- function(dur) {
  c('number'=length(dur), summary(dur))
}

#save
descriptive.0<-matrix(NA, 8, 7)
for (i in 1:8) descriptive.0[i,]<-desc.dur(duration[[i]])
rownames(descriptive.0)<-names(data.0)
colnames(descriptive.0)<-names(desc.dur(duration[[1]]))

#save
setwd('./tables')
write.xlsx(descriptive.0, 'descriptive_0.xlsx', sheetName='0 min')
setwd('./..')
