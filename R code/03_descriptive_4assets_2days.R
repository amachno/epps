#example files from 2014
set.seed(2)
dd()
i.date<-sample(182, 2)
files<-list.files()[i.date]; wd()
#randomly chosen: 19.02.2014 i 07.07.2014


#wczytujemy KGHM, PKOBP, PGE i PZU z dwóch wylosowanych dni
data.0<-list()
data.0$'kghm1'<-read.asset('KGHM', yn=i.date[1])
data.0$'kghm2'<-read.asset('KGHM', yn=i.date[2])
data.0$'pko1'<-read.asset('PKOBP', yn=i.date[1])
data.0$'pko2'<-read.asset('PKOBP', yn=i.date[2])
data.0$'pge1'<-read.asset('PGE', yn=i.date[1])
data.0$'pge2'<-read.asset('PGE', yn=i.date[2])
data.0$'pzu1'<-read.asset('PZU', yn=i.date[1])
data.0$'pzu2'<-read.asset('PZU', yn=i.date[2])
# 1-, 5- i 10-min data
data.01<-list()
for (i in 1:8) data.01[[i]]<-data.min(data.0[[i]], n=1)
names(data.01)<-names(data.0)
data.05<-list()
for (i in 1:8) data.05[[i]]<-data.min(data.0[[i]], n=5)
names(data.05)<-names(data.0)
data.10<-list()
for (i in 1:8) data.10[[i]]<-data.min(data.0[[i]], n=10)
names(data.10)<-names(data.0)

