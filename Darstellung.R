# Daten einlesen.
setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten")
data <- read.csv(file="train.csv")
data.low <- read.csv(file="data.train.low.csv")
data.mid <- read.csv(file="data.train.mid.csv")
data.high <- read.csv(file="data.train.high.csv")

rotate <- function(x) t(apply(x,2,rev)) # Funktion zum umdrehen der Matrizen zur korrekten Darstellung der Ziffern

# Die folgende for-Schleife durchläuft alle 42000 Ziffern in ihrem Originalzustand sowie den drei transformierten Zuständen und stellt diese jeweils zu viert nebeneinander dar.
for (i in 1:nrow(data)){
  setwd("D:/Dropbox/Privat/KIT/05_Master/Seminare/Daten/Bilder")
  #pdf(paste("Ziffer_",data[i,1],"_",i,".pdf",sep=""))
  par(mfrow=c(1,4))
  bild <- rotate(matrix(unlist(data[i,2:785]),nrow=28,ncol=28,byrow=T))
  bild.low <- rotate(matrix(unlist(data.low[i,2:785]),nrow=28,ncol=28,byrow=T))
  bild.mid <- rotate(matrix(unlist(data.mid[i,2:785]),nrow=28,ncol=28,byrow=T))
  bild.high <- rotate(matrix(unlist(data.high[i,2:785]),nrow=28,ncol=28,byrow=T))
  image(bild,axes=T,col = grey(seq(0, 1, length = 256)))
  image(bild.low,axes=T,col = grey(seq(0, 1, length = 256)))
  image(bild.mid,axes=T,col = grey(seq(0, 1, length = 256)))
  image(bild.high,axes=T,col = grey(seq(0, 1, length = 256)))
  mtext(data[i,1],outer=TRUE)
  #dev.off()
  Sys.sleep(0.1)
}