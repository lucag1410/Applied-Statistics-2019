library(emuR)
x11()
layout(matrix(1:4,nrow=2,byrow=T))
par(mar=c(3,2,3,1),mgp=c(2,0.5,0))

dplot(data.Male[,2],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time (male)[%]",
      ylab = "2nd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Second formant',lty=factor(dip.spkr))

dplot(data.Female[,2],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time (female)[%]",
      ylab = "2nd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Second formant',lty=factor(dip.spkr))
