### Here we perform PCA on the normalized data values of Tx for males

library(emuR)
library(dplyr)
library(plyr)

#graphics.off()

# selection of the data
formante = 2
df_Tx = switch (formante,
  df_sample_normalized_T1,
  df_sample_normalized_T2,
  df_sample_normalized_T3,
  df_sample_normalized_T4
)
df_norm_male_Tx = df_Tx %>% filter(speaker == 'M')
df_norm_female_Tx = df_Tx %>% filter(speaker == 'F')
label.df_Tx = df_Tx[,1:3]
data.norm_male_Tx = df_norm_male_Tx[,-(1:3)]
data.norm_female_Tx = df_norm_female_Tx[,-(1:3)]
#data.norm_male_Tx = scale(data.norm_male_Tx)

# PCA
pc.norm_male_Tx = princomp(data.norm_male_Tx)
pc.norm_female_Tx = princomp(data.norm_female_Tx)
summary(pc.norm_male_Tx)
summary(pc.norm_female_Tx)

# scores
scores.norm_male_Tx = pc.norm_male_Tx$scores
scores.norm_male_Tx
scores.norm_female_Tx = pc.norm_female_Tx$scores
scores.norm_female_Tx

# loadings
load.norm_male_Tx = pc.norm_male_Tx$loadings
load.norm_male_Tx
load.norm_female_Tx = pc.norm_female_Tx$loadings
load.norm_female_Tx

# Explained variance (scree plot)
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.norm_male_Tx, las=2, main='Principal Components', ylim=c(0,6e5))
barplot(sapply(data.norm_male_Tx,sd)^2, las=2, main='Original Variables', ylim=c(0,max(sapply(data.norm_male_Tx,sd)^2)), ylab='Variances')
plot(cumsum(pc.norm_male_Tx$sde^2)/sum(pc.norm_male_Tx$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', 
     ylim=c(0,1))
abline(h=0.9, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.norm_male_Tx),labels=1:ncol(data.norm_male_Tx),las=2)


# plot the loadings
x11(width = 10)
par(mar = c(2,2,2,1), mfcol=c(3,2))
for(i in 1:3)barplot(load.norm_male_Tx[,i], ylim = c(-1, 1), main=paste('Male loadings PC ',i,sep=''))
for(i in 1:3)barplot(load.norm_female_Tx[,i], ylim = c(-1, 1), main=paste('Female loadings PC ',i,sep=''))

# biplot
x11()
biplot(pc.norm_male_Tx)
abline(h=0,v=0, col = 'grey', lty = 2)
# if you want to have an idea of the position by diphtong use the following row
#points(scores.norm_male_Tx[,1:2]*1.5, col=col.diphtongs, pch=19)

### generate palettes for diphtongs and speakers
n = length(df_norm_male_Tx[,1])
m = num_of_samples
label.df_Tx[,2] <- factor(label.df_Tx[,2], levels=c('aI', 'aU', 'OY'))
label.df_Tx[,3] <- factor(label.df_Tx[,3], levels=c('M', 'F'))
col.palette_diphtong = c('red', 'green', 'blue')
col.palette_speaker = c('deepskyblue', 'deeppink')
col.diphtongs = rep(NA, n)
col.speakers = rep(NA, n)
for(i in 1:n)
  col.diphtongs[i] = col.palette_diphtong[which(label.df_Tx[i,2] == levels(label.df_Tx[,2]))]
for (i in 1:m)
  col.speakers[i] = col.palette_speaker[which(label.df_Tx[i,3] == levels(label.df_Tx[,3]))]

# plot scores for diphtongs...
x11()
par(mar = c(2,2,2,2),mfcol=c(2,1))
plot(scores.norm_male_Tx[,1:2], col=col.diphtongs, pch=19,
     main=paste('Scores per diphtong, with male speaker and formant T', formante))
abline(h=0, v=0, lty=2, col='grey')
legend(x = 'bottomleft', legend=c('aI','aU', 'OY'), col = col.palette_diphtong, lty = 1)
plot(scores.norm_female_Tx[,1:2], col=col.diphtongs, pch=19,
     main=paste('Scores per diphtong, with female speaker and formant T', formante))
abline(h=0, v=0, lty=2, col='grey')
legend(x = 'bottomleft', legend=c('aI','aU', 'OY'), col = col.palette_diphtong, lty = 1)


# ... and for speaker
# x11()
# plot(scores.norm_male_Tx[,1:2], col=col.diphtongs, pch=19,
#      main=paste('Scores per speaker, for diphtong aIand formant T', formante))
# abline(h=0, v=0, lty=2, col='grey')
# legend(x = 'topleft', legend=c('M','F'), col = col.palette_speaker, lty = 1)

