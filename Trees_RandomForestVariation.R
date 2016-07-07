# Benötigte Pakete
library(randomForest)

# Daten einlesen.
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
data.train <- read.csv(file="train.csv")
data.train[,1] <- ifelse(data.train[,1]==7,1,0)
data.train[,1] <- as.factor(data.train[,1])
set.seed(1)
training <- sample(1:nrow(data.train),size=nrow(data.train)/2)


# Random Forests
randomForest.duration <- rep(0,20)
randomForest.prozent <- rep(0,20)
for(k in 1:20){
  time.start <- Sys.time()
  set.seed(1)
  randomForest.MNIST <- randomForest(label~.,method="class",data=data.train,subset=training,ntree=100*k)
  time.end <- Sys.time()
  randomForest.duration[k] <- time.end - time.start
  randomForest.pred <- predict(randomForest.MNIST,type="class",newdata=data.train[-training,])
  randomForest.tabelle <- table(pred=randomForest.pred,true=data.train[-training,1])
  randomForest.genauigkeit <- matrix(0,nrow=2,ncol=2)
  for(i in 1:2){
    spaltensumme <- sum(randomForest.tabelle[,i])
    for(j in 1:2){
      randomForest.genauigkeit[j,i] <- round(randomForest.tabelle[j,i]/spaltensumme,2)
    }
  }
  randomForest.prozent[k] <- sum(diag(randomForest.tabelle))/sum(randomForest.tabelle)
}
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten/Bilder")
pdf("rf_ntree.pdf")
plot(randomForest.prozent,main="Testgenauigkeit ~ ntree",xlab="ntree * 100",ylab="Testgenauigkeit",type="l")
dev.off()


randomForest.duration2 <- rep(0,20)
randomForest.prozent2 <- rep(0,20)
for(k in 1:20){
  time.start <- Sys.time()
  set.seed(1)
  randomForest.MNIST <- randomForest(label~.,method="class",data=data.train,subset=training,mtry=10*k)
  time.end <- Sys.time()
  randomForest.duration2[k] <- time.end - time.start
  randomForest.pred <- predict(randomForest.MNIST,type="class",newdata=data.train[-training,])
  randomForest.tabelle <- table(pred=randomForest.pred,true=data.train[-training,1])
  randomForest.genauigkeit <- matrix(0,nrow=2,ncol=2)
  for(i in 1:2){
    spaltensumme <- sum(randomForest.tabelle[,i])
    for(j in 1:2){
      randomForest.genauigkeit[j,i] <- round(randomForest.tabelle[j,i]/spaltensumme,2)
    }
  }
  randomForest.prozent2[k] <- sum(diag(randomForest.tabelle))/sum(randomForest.tabelle)
}
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten/Bilder")
pdf("rf_mtry.pdf")
plot(randomForest.prozent2,main="Testgenauigkeit ~ mtry",xlab="mtry * 10",ylab="Testgenauigkeit",type="l")
dev.off()