# Benötigte Pakete
library(tree) # Klassifizierungsbäume
library(randomForest) # Random Forests
library(gbm) # Boosting
library(rpart) # Recursive Partitioning

# Daten einlesen.
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
#data.train <- read.csv(file="train.csv") # Der unveränderte Trainingsdatensatz
#data.train <- read.csv(file="data.train.low.csv") # Transformierter Datensatz
#data.train <- read.csv(file="data.train.mid.csv") # Transformierter Datensatz
#data.train <- read.csv(file="data.train.high.csv") # Transformierter Datensatz
#data.train <- read.csv(file="Summen_Spalten.csv") # Transformierter Datensatz
#data.train <- read.csv(file="Summen_Zeilen.csv") # Transformierter Datensatz
data.train <- read.csv(file="maxima.csv") # Transformierter Datensatz
data.train[,1] <- ifelse(data.train[,1]==7,1,0) # Umwandlung in ein binäres Problem -> "7 oder nicht 7"
data.train[,1] <- as.factor(data.train[,1]) # Umwandlung der labels in Faktoren
set.seed(1)
training <- sample(1:nrow(data.train),size=nrow(data.train)/2) # Aufteilung des Datensatzes in 50:50 Trainings- und Testdaten

# Gewöhnlichen Klassifizierungsbaum (Classification Tree) über alle Trainingsdaten erstellen, anzeigen und beschriften
set.seed(1)
time.start <- Sys.time()
tree.MNIST <- tree(label~.,data=data.train,subset=training) # Anpassung des Entscheidungsbaums
time.end <- Sys.time()
tree.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
plot(tree.MNIST); text(tree.MNIST,pretty=1,cex=0.8) # Plotten des Entscheidungsbaums
tree.pred <- predict(tree.MNIST,type="class",newdata=data.train[-training,]) # Einordnung der Testdaten anhand des Modells
tree.tabelle <- table(pred=tree.pred,true=data.train[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
tree.genauigkeit <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(tree.genauigkeit)){
  spaltensumme <- sum(tree.tabelle[,i])
  for(j in 1:nrow(tree.genauigkeit)){
    tree.genauigkeit[j,i] <- round(tree.tabelle[j,i]/spaltensumme,2)
  }
}
tree.prozent <- sum(diag(tree.tabelle))/sum(tree.tabelle) 

# Gewöhnlichen Klassifizierungsbaum über alle Trainingsdaten erstellen, analog zu oben, diesmal mit rpart statt tree.
set.seed(1)
time.start <- Sys.time()
rpart.MNIST <- rpart(label~.,data=data.train,subset=training,method="class")
time.end <- Sys.time()
rpart.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
plot(rpart.MNIST); text(rpart.MNIST,use.n=TRUE,all=TRUE,cex=.8) # Plotten des Entscheidungsbaums
rpart.pred <- predict(rpart.MNIST,type="class",newdata=data.train[-training,]) # Einordnung der Testdaten anhand des Modells
rpart.tabelle <- table(pred=rpart.pred,true=data.train[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
rpart.genauigkeit <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(rpart.genauigkeit)){
  spaltensumme <- sum(rpart.tabelle[,i])
  for(j in 1:nrow(rpart.genauigkeit)){
    rpart.genauigkeit[j,i] <- round(rpart.tabelle[j,i]/spaltensumme,2)
  }
}
rpart.prozent <- sum(diag(rpart.tabelle))/sum(rpart.tabelle)

# Random Forests
set.seed(1)
time.start <- Sys.time()
randomForest.MNIST <- randomForest(label~.,data=data.train,subset=training)
time.end <- Sys.time()
randomForest.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
randomForest.pred <- predict(randomForest.MNIST,type="class",newdata=data.train[-training,]) # Einordnung der Testdaten anhand des Modells
randomForest.tabelle <- table(pred=randomForest.pred,true=data.train[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
randomForest.genauigkeit <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(randomForest.genauigkeit)){
  spaltensumme <- sum(randomForest.tabelle[,i])
  for(j in 1:nrow(randomForest.genauigkeit)){
    randomForest.genauigkeit[j,i] <- round(randomForest.tabelle[j,i]/spaltensumme,2)
  }
}
randomForest.prozent <- sum(diag(randomForest.tabelle))/sum(randomForest.tabelle)

# Boosting
set.seed(1)
data.train.boost <- data.train
data.train.boost[,1] <- as.numeric(data.train.boost[,1])-1
time.start <- Sys.time()
boosting.MNIST <- gbm(label~.,data=data.train.boost[training,],distribution="bernoulli",n.trees=500,interaction.depth=32,shrinkage=0.5,n.cores=4)
time.end <- Sys.time()
boosting.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
boosting.pred <- predict(boosting.MNIST,type="response",newdata=data.train.boost[-training,],n.trees=500) # Einordnung der Testdaten anhand des Modells
boosting.pred <- ifelse(boosting.pred>0.1,1,0) # Umwandeln der W'keiten in 0/1
boosting.tabelle <- table(pred=boosting.pred,true=data.train.boost[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
boosting.genauigkeit <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(boosting.genauigkeit)){
  spaltensumme <- sum(boosting.tabelle[,i])
  for(j in 1:nrow(boosting.genauigkeit)){
    boosting.genauigkeit[j,i] <- round(boosting.tabelle[j,i]/spaltensumme,2)
  }
}
boosting.prozent <- sum(diag(boosting.tabelle))/sum(boosting.tabelle)

# Bagging (Random Forests mit m=p)
set.seed(1)
time.start <- Sys.time()
bagging.MNIST <- randomForest(label~.,data=data.train,subset=training,mtry=ncol(data.train)-1)
time.end <- Sys.time()
bagging.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
bagging.pred <- predict(bagging.MNIST,type="class",newdata=data.train[-training,]) # Einordnung der Testdaten anhand des Modells
bagging.tabelle <- table(pred=bagging.pred,true=data.train[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
bagging.genauigkeit <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(bagging.genauigkeit)){
  spaltensumme <- sum(bagging.tabelle[,i])
  for(j in 1:nrow(bagging.genauigkeit)){
    bagging.genauigkeit[j,i] <- round(bagging.tabelle[j,i]/spaltensumme,2)
  }
}
bagging.prozent <- sum(diag(bagging.tabelle))/sum(bagging.tabelle)

#### Remove 0-columns
data.train.pure <- data.train[,-(which(colSums(data.train[,-1])==0)+1)]

# Gewöhnlichen Klassifizierungsbaum (Classification Tree) über alle Trainingsdaten erstellen, anzeigen und beschriften
set.seed(1)
time.start <- Sys.time()
tree.MNIST.pure <- tree(label~.,data=data.train.pure,subset=training)
time.end <- Sys.time()
tree.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
plot(tree.MNIST.pure);text(tree.MNIST.pure,pretty=1,cex=0.8) # Plotten des Entscheidungsbaums
tree.pred.pure <- predict(tree.MNIST.pure,type="class",newdata=data.train.pure[-training,]) # Einordnung der Testdaten anhand des Modells
tree.tabelle.pure <- table(pred=tree.pred.pure,true=data.train.pure[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
tree.genauigkeit.pure <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(tree.genauigkeit.pure)){
  spaltensumme <- sum(tree.tabelle.pure[,i])
  for(j in 1:nrow(tree.genauigkeit.pure)){
    tree.genauigkeit.pure[j,i] <- round(tree.tabelle.pure[j,i]/spaltensumme,2)
  }
}
tree.prozent.pure <- sum(diag(tree.tabelle.pure))/sum(tree.tabelle.pure)

# Gewöhnlichen Klassifizierungsbaum über alle Trainingsdaten erstellen, analog zu oben, diesmal mit rpart statt tree.
set.seed(1)
time.start <- Sys.time()
rpart.MNIST.pure <- rpart(label~.,data=data.train.pure,subset=training,method="class")
time.end <- Sys.time()
rpart.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
plot(rpart.MNIST.pure); text(rpart.MNIST.pure,use.n=TRUE,all=TRUE,cex=.8) # Plotten des Entscheidungsbaums
rpart.pred.pure <- predict(rpart.MNIST.pure,type="class",newdata=data.train.pure[-training,]) # Einordnung der Testdaten anhand des Modells
rpart.tabelle.pure <- table(pred=rpart.pred.pure,true=data.train.pure[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
rpart.genauigkeit.pure <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(rpart.genauigkeit.pure)){
  spaltensumme <- sum(rpart.tabelle.pure[,i])
  for(j in 1:nrow(rpart.genauigkeit.pure)){
    rpart.genauigkeit.pure[j,i] <- round(rpart.tabelle.pure[j,i]/spaltensumme,2)
  }
}
rpart.prozent.pure <- sum(diag(rpart.tabelle.pure))/sum(rpart.tabelle.pure)

# Random Forests - pure
set.seed(1)
time.start <- Sys.time()
randomForest.MNIST.pure <- randomForest(label~.,data=data.train.pure,subset=training)
time.end <- Sys.time()
randomForest.duration.pure <- time.end - time.start # Messung der Zeitdauer der Berechnung
randomForest.pred.pure <- predict(randomForest.MNIST.pure,type="class",newdata=data.train.pure[-training,]) # Einordnung der Testdaten anhand des Modells
randomForest.tabelle.pure <- table(pred=randomForest.pred.pure,true=data.train.pure[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
randomForest.genauigkeit.pure <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(randomForest.genauigkeit.pure)){
  spaltensumme <- sum(randomForest.tabelle.pure[,i])
  for(j in 1:nrow(randomForest.genauigkeit.pure)){
    randomForest.genauigkeit.pure[j,i] <- round(randomForest.tabelle.pure[j,i]/spaltensumme,2)
  }
}
randomForest.prozent.pure <- sum(diag(randomForest.tabelle.pure))/sum(randomForest.tabelle.pure)

# Boosting - pure
set.seed(1)
data.train.boost.pure <- data.train.pure
data.train.boost.pure[,1] <- as.numeric(data.train.boost.pure[,1])-1
time.start <- Sys.time()
boosting.MNIST.pure <- gbm(label~.,data=data.train.boost.pure[training,],distribution="bernoulli",n.trees=500,interaction.depth=32,shrinkage=0.5,n.cores=4)
time.end <- Sys.time()
boosting.duration.pure <- time.end - time.start # Messung der Zeitdauer der Berechnung
boosting.pred.pure <- predict(boosting.MNIST.pure,type="response",newdata=data.train.boost.pure[-training,],n.trees=500) # Einordnung der Testdaten anhand des Modells
boosting.pred.pure <- ifelse(boosting.pred.pure>0.1,1,0) # Umwandeln der W'keiten in 0/1
boosting.tabelle.pure <- table(pred=boosting.pred.pure,true=data.train.boost.pure[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
boosting.genauigkeit.pure <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(boosting.genauigkeit.pure)){
  spaltensumme <- sum(boosting.tabelle.pure[,i])
  for(j in 1:nrow(boosting.genauigkeit.pure)){
    boosting.genauigkeit.pure[j,i] <- round(boosting.tabelle.pure[j,i]/spaltensumme,2)
  }
}
boosting.prozent.pure <- sum(diag(boosting.tabelle.pure))/sum(boosting.tabelle.pure)

# Bagging - pure (Random Forests mit m=p)
set.seed(1)
time.start <- Sys.time()
bagging.MNIST <- randomForest(label~.,data=data.train.pure,subset=training,mtry=ncol(data.train.pure)-1)
time.end <- Sys.time()
bagging.duration <- time.end - time.start # Messung der Zeitdauer der Berechnung
bagging.pred <- predict(bagging.MNIST,type="class",newdata=data.train.pure[-training,]) # Einordnung der Testdaten anhand des Modells
bagging.tabelle <- table(pred=bagging.pred,true=data.train.pure[-training,1]) # Und folgende: Berechnung der Testgenauigkeit
bagging.genauigkeit <- matrix(0,nrow=2,ncol=2)
for(i in 1:ncol(bagging.genauigkeit)){
  spaltensumme <- sum(bagging.tabelle[,i])
  for(j in 1:nrow(bagging.genauigkeit)){
    bagging.genauigkeit[j,i] <- round(bagging.tabelle[j,i]/spaltensumme,2)
  }
}
bagging.prozent <- sum(diag(bagging.tabelle))/sum(bagging.tabelle)