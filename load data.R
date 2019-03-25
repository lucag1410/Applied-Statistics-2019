library(emuR)

x11()
layout(matrix(1:4,nrow=2,byrow=T))
par(mar=c(3,2,3,1),mgp=c(2,0.5,0))
dplot(dip.fdat[,1],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "1st Formant [Hz]", col=rainbow(3),lwd=1.1,main='First formant',lty=factor(dip.spkr))

dplot(dip.fdat[,2],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "2nd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Second formant',lty=factor(dip.spkr))

dplot(dip.fdat[,3],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "3rd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Third formant',lty=factor(dip.spkr))

dplot(dip.fdat[,4],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "4th Formant [Hz]", col=rainbow(3),lwd=1.1,main='Fourth formant',lty=factor(dip.spkr))


summary(dip.fdat)
dim(dip.fdat$data)
dim(dip)
names(dip.fdat)

dim(dip.fdat$ftime)


# scelta della formante
formante = 2
tempi = as.numeric(rownames(dip.fdat$data))

n = 186

speech_recognition = dip.fdat

dip.fdat$index #indici di inizio e fine delle curve
maxlength = max(apply(dip.fdat$index,1,diff)) + 1
data.matrix = matrix(nrow=n,ncol=maxlength)
time.matrix = matrix(nrow=n,ncol=maxlength)
normalized.time.matrix = matrix(nrow=n,ncol=maxlength)

for(ii in 1:n){
  indice.ii = dip.fdat$index[ii,]
  time.std = tempi[indice.ii[1]:indice.ii[2]] - tempi[indice.ii[1]]
  time.matrix[ii,1:length(time.std)] = time.std
  normalized.time.matrix[ii,1:length(time.std)] = time.std/max(time.std)
  data.matrix[ii,1:length(time.std)] = dip.fdat$data[indice.ii[1]:indice.ii[2],formante]
}

x11()
matplot(t(time.matrix),t(data.matrix),type='l',col=factor(dip$labels),ylab='Data, frequency 2',xlab='Time',lty=1)
matplot(t(normalized.time.matrix),t(data.matrix),type='l',col=factor(dip$labels),ylab='Data, frequency 2',xlab='Normalized time',lty=1)

x11()
matplot(t(normalized.time.matrix),t(data.matrix),type='l',col=factor(dip$labels),ylab='Data, frequency 2',xlab='Normalized time',lty=1)


