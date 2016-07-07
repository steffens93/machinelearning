# Daten einlesen.
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
data.train <- read.csv(file="train.csv")

# Berechnung der Zeilensummen
data.train.x <- matrix(0,nrow=nrow(data.train),ncol=29)
colnames(data.train.x) <- c("label",1:28)
data.train.x[,1] <- data.train[,1]
for(i in 1:nrow(data.train)){
  for(j in 1:28){
    data.train.x[i,j+1] <-  sum(data.train[i,((28*(j-1)+2):(28*j+1))])
  }
}

# Berechnung der Spaltensummen
data.train.y <- matrix(0,nrow=nrow(data.train),ncol=29)
colnames(data.train.y) <- c("label",1:28)
data.train.y[,1] <- data.train[,1]
for(i in 1:nrow(data.train)){
  for(j in 1:28){
    data.train.y[i,j+1] <-  sum(data.train[i,seq(j+1,785-(28-j),28)])
  }
}

# Maxima auf die Zeilen-/Spaltensummen bezogen
maxima <- matrix(0,nrow=nrow(data.train),ncol=3)
colnames(maxima) <- c("label","x","y")
for(i in 1:nrow(data.train)){
  maxima[i,1] <- data.train[i,1]
  maxima[i,2] <- which.max(data.train.x[i,2:29])
  maxima[i,3] <- which.max(data.train.y[i,2:29])
}

# Speichern der Daten in .csv-Dateien
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
write.csv(x=data.train.x,file="Summen_Zeilen.csv",row.names=F)
write.csv(x=data.train.y,file="Summen_Spalten.csv",row.names=F)
write.csv(x=maxima,file="maxima.csv",row.names=F)
