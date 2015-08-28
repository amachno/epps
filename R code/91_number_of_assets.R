#modify for you use
setwd(data.dir(2014))

#number of files in the directory
n.dir<-length(list.files())
cc<-rep("NULL", 11)
cc[5]<-"character"

#asset vector
ass<-c()
for (i in 1:n.dir){
  a<-read.table(list.files()[i], sep=',', colClasses = cc)
  ass<-c(ass, a[[1]])
  print(i)
}

#produce table
ass.table<-sort(table(ass), decreasing=T)

#modify for you use
setwd(work.dir) 
