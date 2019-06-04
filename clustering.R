#setwd("C:/Users/Luca/Desktop/Applied-Statistics-2019-master/Applied-Statistics-2019-master/data")
library(plyr)
library(mvtnorm)
library(rgl)
library(car)
library(MASS)

### LOAD DATASETS
df_norm = read.table('../data/df_norm.txt')
df_complete_noZeros = read.table('../data/df_complete_noZeros.txt')
df_sample_normalized_T1 = read.table('../data/df_sample_normalized_T1.txt')
df_sample_normalized_T2 = read.table('../data/df_sample_normalized_T2.txt')
df_sample_normalized_T3 = read.table('../data/df_sample_normalized_T3.txt')
df_sample_normalized_T4 = read.table('../data/df_sample_normalized_T4.txt')
df_sample = read.table('../data/df_sample.txt')
df_complete_normalized = read.table('../data/df_complete_normalized.txt')
df_sample_normalized_total = read.table('../data/df_sample_normalized_total.txt')


#write.table(df_sample_normalized_total_female, "df_sample_normalized_total_female.txt")

# diphtong_labels = df_sample_normalized_T2[,2]
# 
# data_T2 = df_sample_normalized_T2[,4:14]
# data_T2.eucl = dist(data_T2, method = 'euclidean')
# data_T2.eucl_single = hclust(data_T2.eucl, method = 'single')
# data_T2.eucl_average = hclust(data_T2.eucl, method = 'average')
# data_T2.eucl_complete = hclust(data_T2.eucl, method = 'complete')

diphtong_labels = df_sample_normalized_total_male[,2]

data_T2 = df_sample_normalized_total_male[,5:8]
data_T2.eucl = dist(data_T2, method = 'euclidean')
data_T2.eucl_single = hclust(data_T2.eucl, method = 'single')
data_T2.eucl_average = hclust(data_T2.eucl, method = 'average')
data_T2.eucl_complete = hclust(data_T2.eucl, method = 'complete')

x11()
par(mfrow=c(1,3))
plot(data_T2.eucl_single, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data_T2.eucl_single, k=3)
plot(data_T2.eucl_average, labels=F , main='euclidean-average', hang=-0.1, xlab='', cex=0.6, sub='')
rect.hclust(data_T2.eucl_average, k=3)
plot(data_T2.eucl_complete, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data_T2.eucl_complete, k=3)

# x11()
# plot(data_T2.eucl_complete, main='euclidean-complete', hang=-0.1, xlab='', labels=diphtong_labels, cex=0.6, sub='')
# rect.hclust(data_T2.eucl_complete, k=3)
df_sample_normalized_total_female = df_sample_normalized_total[df_sample_normalized_total$Speaker == 'F',]

data_T2.eucl_complete$merge

coph.data_T2_complete = cophenetic(data_T2.eucl_complete)
coph.data_T2_average = cophenetic(data_T2.eucl_average)
coph.data_T2_single = cophenetic(data_T2.eucl_single)

# x11()
# layout(rbind(c(0,1,0),c(2,3,4)))
# image(as.matrix(data_T2.eucl), main='Euclidean', asp=1 )
# image(as.matrix(coph.data_T2_single), main='Single', asp=1 )
# image(as.matrix(coph.data_T2_complete), main='Complete', asp=1 )
# image(as.matrix(coph.data_T2_average), main='Average', asp=1 )

coph_coeff_single_T2 = cor(data_T2.eucl, coph.data_T2_single)
coph_coeff_complete_T2 = cor(data_T2.eucl, coph.data_T2_complete)
coph_coeff_average_T2 = cor(data_T2.eucl, coph.data_T2_average)

c("Eucl-Single"=coph_coeff_single_T2,"Eucl-Compl."=coph_coeff_complete_T2,"Eucl-Ave."=coph_coeff_average_T2)
# average and complete are pretty similar

cluster.single_T2 = cutree(data_T2.eucl_single, k=3) # euclidean-single
cluster.complete_T2 = cutree(data_T2.eucl_complete, k=3) # euclidean-complete
cluster.average_T2 = cutree(data_T2.eucl_average, k=3) # euclidean-average

table(etichetta.vera = diphtong_labels, etichetta.cluster = cluster.single_T2)
table(etichetta.vera = diphtong_labels, etichetta.cluster = cluster.complete_T2)
table(etichetta.vera = diphtong_labels, etichetta.cluster = cluster.average_T2)
# questi risultati non vanno bene


