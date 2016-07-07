# Benötigte Pakete
library(gbm)

# Daten einlesen.
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
data.train <- read.csv(file="train.csv")
data.train[,1] <- ifelse(data.train[,1]==7,1,0)
data.train[,1] <- as.factor(data.train[,1])
set.seed(1)
training <- sample(1:nrow(data.train),size=nrow(data.train)/2)

# Boosting - ntree
boosting.duration <- rep(0,50)
boosting.prozent <- rep(0,50)
data.train.boost <- data.train
data.train.boost[,1] <- as.numeric(data.train.boost[,1])-1
for(k in 1:50){
  time.start <- Sys.time()
  set.seed(1)
  boosting.MNIST <- gbm(label~.,data=data.train.boost[training,],distribution="bernoulli",n.trees=100*k,n.cores=4)
  time.end <- Sys.time()
  boosting.duration[k] <- time.end - time.start
  boosting.pred <- predict(boosting.MNIST,type="response",newdata=data.train.boost[-training,],n.trees=100*k) # Einordnung der Testdaten anhand des Modells
  boosting.pred <- ifelse(boosting.pred>0.1,1,0) # Umwandeln der W'keiten in 0/1
  boosting.tabelle <- table(pred=boosting.pred,true=data.train.boost[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
  boosting.genauigkeit <- matrix(0,nrow=2,ncol=2)
  for(i in 1:ncol(boosting.genauigkeit)){
    spaltensumme <- sum(boosting.tabelle[,i])
    for(j in 1:nrow(boosting.genauigkeit)){
      boosting.genauigkeit[j,i] <- round(boosting.tabelle[j,i]/spaltensumme,2)
    }
  }
  boosting.prozent[k] <- sum(diag(boosting.tabelle))/sum(boosting.tabelle)
}
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten/Bilder")
pdf("boosting_ntree.pdf")
plot(boosting.prozent,main="Testgenauigkeit ~ ntree",xlab="ntree * 100",ylab="Testgenauigkeit",type="b")
dev.off()

# Boosting - interaction.depth
boosting.duration.2 <- rep(0,40)
boosting.prozent.2 <- rep(0,40)
data.train.boost.2 <- data.train
data.train.boost.2[,1] <- as.numeric(data.train.boost.2[,1])-1
for(k in 1:40){
  time.start <- Sys.time()
  set.seed(1)
  boosting.MNIST.2 <- gbm(label~.,data=data.train.boost.2[training,],distribution="bernoulli",interaction.depth=k,n.cores=4)
  time.end <- Sys.time()
  boosting.duration.2[k] <- time.end - time.start
  boosting.pred.2 <- predict(boosting.MNIST.2,type="response",newdata=data.train.boost.2[-training,],n.trees=500) # Einordnung der Testdaten anhand des Modells
  boosting.pred.2 <- ifelse(boosting.pred.2>0.1,1,0) # Umwandeln der W'keiten in 0/1
  boosting.tabelle.2 <- table(pred=boosting.pred.2,true=data.train.boost.2[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
  boosting.genauigkeit.2 <- matrix(0,nrow=2,ncol=2)
  for(i in 1:ncol(boosting.genauigkeit.2)){
    spaltensumme <- sum(boosting.tabelle.2[,i])
    for(j in 1:nrow(boosting.genauigkeit.2)){
      boosting.genauigkeit.2[j,i] <- round(boosting.tabelle.2[j,i]/spaltensumme,2)
    }
  }
  boosting.prozent.2[k] <- sum(diag(boosting.tabelle.2))/sum(boosting.tabelle.2)
}
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten/Bilder")
pdf("boosting_idepth.pdf")
plot(boosting.prozent.2,main="Testgenauigkeit ~ Interaktionstiefe",xlab="Interaktionstiefe",ylab="Testgenauigkeit",type="b")
dev.off()

# Boosting - Shrinkage Factor
schrumpffaktor <- c(0.001,0.01,0.05,0.1,0.2,0.3,0.4,0.5,1)
boosting.duration.3 <- rep(0,length(schrumpffaktor))
boosting.prozent.3 <- rep(0,length(schrumpffaktor))
data.train.boost.3 <- data.train
data.train.boost.3[,1] <- as.numeric(data.train.boost.3[,1])-1
for(k in 1:length(schrumpffaktor)){
  time.start <- Sys.time()
  set.seed(1)
  boosting.MNIST.3 <- gbm(label~.,data=data.train.boost.3[training,],distribution="bernoulli",shrinkage = schrumpffaktor[k],n.cores=4)
  time.end <- Sys.time()
  boosting.duration.3[k] <- time.end - time.start
  boosting.pred.3 <- predict(boosting.MNIST.3,type="response",newdata=data.train.boost.3[-training,],n.trees=500) # Einordnung der Testdaten anhand des Modells
  boosting.pred.3 <- ifelse(boosting.pred.3>0.1,1,0) # Umwandeln der W'keiten in 0/1
  boosting.tabelle.3 <- table(pred=boosting.pred.3,true=data.train.boost.3[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
  boosting.genauigkeit.3 <- matrix(0,nrow=2,ncol=2)
  for(i in 1:ncol(boosting.genauigkeit.3)){
    spaltensumme <- sum(boosting.tabelle.3[,i])
    for(j in 1:nrow(boosting.genauigkeit.3)){
      boosting.genauigkeit.3[j,i] <- round(boosting.tabelle.3[j,i]/spaltensumme,2)
    }
  }
  boosting.prozent.3[k] <- sum(diag(boosting.tabelle.3))/sum(boosting.tabelle.3)
}
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten/Bilder")
pdf("boosting_shrinkage.pdf")
plot(y=boosting.prozent.3,x=1:9,main="Testgenauigkeit ~ Lernrate",xlab="Lernrate",ylab="Testgenauigkeit",type="b")
dev.off()