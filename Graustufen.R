# Arbeitsverzeichnis festlegen und die Daten einlesen.
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
data.train <- read.csv(file="train.csv")

data.train.low <- data.train
data.train.mid <- data.train
data.train.high <- data.train

# Die folgende for-Schleife ersetzt die Grauwerte jeweils wie in den ifelse-Funktionen angegeben.
time.start <- Sys.time()
for(i in 1:nrow(data.train)){
  for(j in 2:ncol(data.train)){
    data.train.low[i,j] <- ifelse(data.train.low[i,j]>0,255,0)
    data.train.mid[i,j] <- ifelse(data.train.mid[i,j]>50,255,0)
    data.train.high[i,j] <- ifelse(data.train.high[i,j]>100,255,0)
  }
}
time.end <- Sys.time()
time.duration <- time.end - time.start

# Speichern der bereinigten Daten in einer neuen .csv-Datei
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
write.csv(x=data.train.low,file="data.train.low.2.csv",row.names=F)
write.csv(x=data.train.mid,file="data.train.mid.2.csv",row.names=F)
write.csv(x=data.train.high,file="data.train.high.2.csv",row.names=F)