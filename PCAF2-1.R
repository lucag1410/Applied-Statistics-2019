### PCA on 2nd Formant removing measurment errors
#We substitute the zeros sampled in the second formant with the mean of that formant

library(emuR)

#_______________________________________________________________________________________________________________

speech_data = dip.fdat #We generated this variable so the original data won't be modified

#We substitute all the zeros with the mean value of the formant
for (formant in 1:4){
  speech_data$data[, formant][speech_data$data[, formant] == 0] = mean(speech_data$data[, formant])
}

num_of_samples = 186

max_F2 = c()
min_F2 = c()
first_F2 = c()
last_F2 = c()
avg_F2 = c()
for (i in 1:num_of_samples)
{
  max_F2 = c(max_F2, max(speech_data[i,2], na.rm = T))
  min_F2 = c(min_F2, min(speech_data[i,2]$data, na.rm = T))
  first_F2 = c(first_F2, speech_data[i,2]$data[1])
  last_F2 = c(last_F2, speech_data[i,2]$data[length(speech_data[i,2]$data)])
  avg_F2 = c(avg_F2, mean(speech_data[i,2]$data, na.rm = T))
}

duration_F2 = dip$end - dip$start

#We generate the data frame from the vectors

data_F2 = data.frame(max_F2, min_F2, first_F2, last_F2, avg_F2, duration_F2)

#____________________________________________________________________________________________________________

### ----- PCA -----

#Starting with a boxplot of the original data

x11()
boxplot(scale(data_F2, center = T, scale = F), col = 'gold', las = 2)

#Perform the PCA

pc.data_F2 = princomp(data_F2, scores = T)
pc.data_F2
summary(pc.data_F2)

#Compare the boxplot of the original data with the data after PCA has been performed
x11()
layout(matrix(c(1,2),2))
boxplot(scale(data_F2, center = T, scale =F), col = 'gold', las = 2)
boxplot(scale(pc.data_F2$scores, center = T, scale = F), col = 'gold', las = 2)

#Loadings

load.data_f2 = pc.data_F2$loadings
load.data_f2

#Barplot of the loadings

x11()
par(mar = c(1,4,0,2), mfrow = c(6,1))
for(i in 1:6) barplot(load.data_f2[,i], ylim = c(-1, 1))

# - non so cosa io stia per fare -

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data_F2, las=2, main='Principal components')
barplot(sapply(data_F2,sd)^2, las=2, main='Original Variables', ylab='Variances')
plot(cumsum(pc.data_F2$sd^2)/sum(pc.data_F2$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data_F2),labels=1:ncol(data_F2),las=2)