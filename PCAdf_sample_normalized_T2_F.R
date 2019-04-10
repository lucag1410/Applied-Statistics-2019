library(emuR)
library(dplyr)
library(plyr)

#AGGIUNGI BOXPLOT DEL DATASET ORIGINARIO

#------PCA on df_sample_normalized_T2(female)

formante = 2

df_Tx= switch(formante,
    df_sample_normalized_T1,
    df_sample_normalized_T2,
    df_sample_normalized_T3,
    df_sample_normalized_T4)

df_sample_normalized_T2_F= df_Tx %>% filter(speaker == 'F')

label.df_Tx = df_Tx[,1:3]
data.df_Tx = df_Tx[,-(1:3)]

#boxplot
boxplot(scale(data.df_Tx, center = T, scale = F), col = 'gold', las = 2)

#perform the pca on the dataset
pc.df_sample_normalized_T2_F = princomp(data.df_Tx)
pc.df_sample_normalized_T2_F
summary(pc.df_sample_normalized_T2_F)

#loadings
load.df_sample_normalized_T2_F <- pc.df_sample_normalized_T2_F$loadings
load.df_sample_normalized_T2_F 

#barplot of the loadings
x11()
par(mar = c(1,4,0,2), mfrow = c(10,1))
for(i in 1:10) barplot(load.df_sample_normalized_T2_F[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

#screeplot
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.df_sample_normalized_T2_F, las=2, main='Principal Components', ylim=c(0,1e6))
abline(h=1, col='blue')
barplot(sapply(df_sample_normalized_T2_F.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,2.5e6), ylab='Variances')
plot(cumsum(pc.df_sample_normalized_T2_F$sd^2)/sum(pc.df_sample_normalized_T2_F$sd^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(df_sample_normalized_T2_F.sd),labels=1:ncol(df_sample_normalized_T2_F.sd),las=2)

#scores
scores.df_sample_normalized_T2_F <- pc.df_sample_normalized_T2_F$scores
scores.df_sample_normalized_T2_F

# generate palettes for diphtongs and speakers
n = length(df_sample_normalized_T2_F[,1])
label.df_sample_normalized_T2_F[,2] <- factor(label.df_sample_normalized_T2_F[,2], levels=c('aI', 'aU', 'OY'))
label.df_sample_normalized_T2_F[,3] <- factor(label.df_sample_normalized_T2_F[,3], levels=c('M', 'F'))
col.palette_diphtong = c('red', 'green', 'blue')
col.palette_speaker = c('deepskyblue', 'deeppink')
col.diphtongs = rep(NA, n)
col.speakers = rep(NA, n)
for(i in 1:n){
  col.diphtongs[i] = col.palette_diphtong[which(label.df_sample_normalized_T2_F[i,2] == levels(label.df_sample_normalized_T2_F[,2]))]
  col.speakers[i] = col.palette_speaker[which(label.df_sample_normalized_T2_Fdf_Tx[i,3] == levels(label.df_sample_normalized_T2_F[,3]))]
}

#plot of the scores
x11()
plot(scores.df_sample_normalized_T2_F[,1:2], col = col.diphtongs, 
     main=paste('Scores per diphtong, with female speaker and formant T', formante))
abline(h=0, v=0, lty=2, col='grey')
legend(x = 'topleft', legend=c('aI','aU', 'OY'), col = col.palette_diphtong, lty = 1)

#biplot
x11()
biplot(pc.df_sample_normalized_T2_F)