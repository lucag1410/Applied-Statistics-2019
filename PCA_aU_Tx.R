df_sample_normalized_T1 <- read.table("C:/Users/giaco/Documents/GitHub/Applied-Statistics-2019/data/df_sample_normalized_T1.txt")
df_sample_normalized_T2 <- read.table("C:/Users/giaco/Documents/GitHub/Applied-Statistics-2019/data/df_sample_normalized_T2.txt")
df_sample_normalized_T3 <- read.table("C:/Users/giaco/Documents/GitHub/Applied-Statistics-2019/data/df_sample_normalized_T3.txt")
df_sample_normalized_T4 <- read.table("C:/Users/giaco/Documents/GitHub/Applied-Statistics-2019/data/df_sample_normalized_T4.txt")

#_____________________________________________________________________________________________________________
### Here we perform PCA for every diphthong

library(emuR)
library(dplyr)
library(plyr)

# selection of the data
formante <- 2
df_Tx <- switch (formante,
                 df_sample_normalized_T1,
                 df_sample_normalized_T2,
                 df_sample_normalized_T3,
                 df_sample_normalized_T4
)
df_aU_Tx <- df_Tx %>% filter(diphtong == 'aU')
label_df_Tx <- df_Tx[,1:3]
data_aU_Tx <- df_aU_Tx[,-(1:3)]
#data.norm_aU_Tx <- scale(data.norm_aU_Tx)

# boxplot of the dataset
x11()
boxplot(data_aU_Tx, col = 'gold')

#dev.off()

# --- PCA ---
pc.data_aU_Tx <- princomp(data_aU_Tx, scores = T)
pc.data_aU_Tx
summary(pc.data_aU_Tx)

# we save scores and loadings in separeted vectors
scores.data_aU_Tx <- pc.data_aU_Tx$scores
load.data_aU_Tx <- pc.data_aU_Tx$loadings
# take a look at the values
#scores.data_aU_Tx
#load.data_aU_Tx

# scree plot
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.data_aU_Tx, las=2, main='Principal components', ylim=c(0,2e5))
barplot(sapply(data_aU_Tx,sd)^2, las=2, main='Original Variables', ylim=c(0,1.5e5), ylab='Variances')
plot(cumsum(pc.data_aU_Tx$sd^2)/sum(pc.data_aU_Tx$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data_aU_Tx),labels=1:ncol(data_aU_Tx),las=2)

#dev.off()

# barplot of the loadings
x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.data_aU_Tx[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

#dev.off()

# biplot
x11()
biplot(pc.data_aU_Tx)
abline(h=0,v=0, col = 'grey', lty = 2)

#dev.off()

### creo la paletta di colore in modo molto brutale
pal <- NULL
for (n in 1:22)
  pal <- c(pal, 'deepskyblue')
for (n in 1:22)
  pal <- c(pal, 'deeppink')

# plot scores for speakers
x11()
plot(scores.data_aU_Tx[,1:2], col = pal, pch = 19,
     main = paste('Scores per speaker, for diphthong aI and formant T', formante))
abline(h = 0, v = 0, lty = 2, col = 'grey')
legend(x = 'topleft', legend = c('M', 'F'), col = c('deepskyblue', 'deeppink'), lty = 1)

#dev.off()