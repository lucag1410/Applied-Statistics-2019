### plot of formants in the same range of values
x11()
layout(matrix(1:4,nrow=2,byrow=T))
par(mar=c(3,2,3,1),mgp=c(2,0.5,0))
dplot(speech_data[,1],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "1st Formant [Hz]", col=rainbow(3),lwd=1.1,main='First formant',lty=factor(dip.spkr))

dplot(speech_data[,2],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "2nd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Second formant',lty=factor(dip.spkr))

dplot(speech_data[,3],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "3rd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Third formant',lty=factor(dip.spkr))

dplot(speech_data[,4],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "4th Formant [Hz]", col=rainbow(3),lwd=1.1,main='Fourth formant',lty=factor(dip.spkr))


### plot of avg formant
x11()
dplot(avg_formant_data,
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "Avg Formant [Hz]", col=rainbow(3),lwd=1.1,main='Average formant',lty=factor(dip.spkr))


### plot of mean values for all the samples (ignore the warning messages in the console)
# there is a clear distinction between the speakers
# NB the indices goes from 1 to 118 (the number of aI samples)
# I don't think that this is an issue since we are interested in the differences on the y-axis

x11()
plot(mean_samples_avg_formant[indices_aI], col = 'red', pch = 19, legend = 'aI', ylim = c(1500, max(mean_samples_avg_formant)), ylab = 'Mean sample value from T_AVG')
legend(0, max(mean_samples_avg_formant), legend=c('aI', 'aU', 'OY'), col=c("red", 'green', "blue"), lty = 1, cex = 1)
points(mean_samples_avg_formant[indices_aU], col = 'green', pch = 19, legend = 'aU')
points(mean_samples_avg_formant[indices_OY], col = 'blue', pch = 19, legend = 'OY')



### TODO: there is something wrong when plotting the data using specific indices
### I think that this is due to the fact that I still used dip$labels and not dip$"male labels", for example

