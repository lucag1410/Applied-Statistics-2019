#setwd("C:/Users/Luca/Desktop/Applied-Statistics-2019-master/Applied-Statistics-2019-master/data")
library(plyr)
library(dplyr)
library(mvtnorm)
library(rgl)
library(car)
library(MASS)
library(useful)
library(caret)

### LOAD DATASETS
df_norm = read.table('../data/df_norm.txt')
df_complete_noZeros = read.table('../data/df_complete_noZeros.txt')
df_sample_normalized_T1 = read.table('../data/df_sample_normalized_T1.txt')
df_sample_normalized_T2 = read.table('../data/df_sample_normalized_T2.txt')
df_sample_normalized_T3 = read.table('../data/df_sample_normalized_T3.txt')
df_sample_normalized_T4 = read.table('../data/df_sample_normalized_T4.txt')
df_sample = read.table('../data/df_sample.txt')
df_sample_normalized_percentages_as_rows = read.table('../data/df_sample_normalized_percentages_as_rows.txt')
df_sample_normalized_T1T2T3T4 = read.table('../data/df_sample_normalized_percentages_as_features.txt')

#write.table(df_sample_normalized_total_female, "df_sample_normalized_total_female.txt")

diphtong_labels = data_not_scaled_F[,2]
# 
# data_T2 = df_sample_normalized_T2[,4:14]
# data_T2.eucl = dist(data_T2, method = 'euclidean')
# data_T2.eucl_single = hclust(data_T2.eucl, method = 'single')
# data_T2.eucl_average = hclust(data_T2.eucl, method = 'average')
# data_T2.eucl_complete = hclust(data_T2.eucl, method = 'complete')

data_T1T2T3T4 = data_not_scaled_F[,4:47]
data_T1T2T3T4.eucl = dist(data_T1T2T3T4, method = 'euclidean')
data_T1T2T3T4.eucl_single = hclust(data_T1T2T3T4.eucl, method = 'single')
data_T1T2T3T4.eucl_average = hclust(data_T1T2T3T4.eucl, method = 'average')
data_T1T2T3T4.eucl_complete = hclust(data_T1T2T3T4.eucl, method = 'complete')

x11()
par(mfrow=c(1,3))
plot(data_T1T2T3T4.eucl_single, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data_T1T2T3T4.eucl_single, k=3)
plot(data_T1T2T3T4.eucl_average, labels=F , main='euclidean-average', hang=-0.1, xlab='', cex=0.6, sub='')
rect.hclust(data_T1T2T3T4.eucl_average, k=3)
plot(data_T1T2T3T4.eucl_complete, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(data_T1T2T3T4.eucl_complete, k=3)

# x11()
# plot(data_T2.eucl_complete, main='euclidean-complete', hang=-0.1, xlab='', labels=diphtong_labels, cex=0.6, sub='')
# rect.hclust(data_T2.eucl_complete, k=3)
df_sample_normalized_total_female = df_sample_normalized_total[df_sample_normalized_total$Speaker == 'F',]

data_T1T2T3T4.eucl_complete$merge

coph.data_T1T2T3T4_complete = cophenetic(data_T1T2T3T4.eucl_complete)
coph.data_T1T2T3T4_average = cophenetic(data_T1T2T3T4.eucl_average)
coph.data_T1T2T3T4_single = cophenetic(data_T1T2T3T4.eucl_single)

# x11()
# layout(rbind(c(0,1,0),c(2,3,4)))
# image(as.matrix(data_T2.eucl), main='Euclidean', asp=1 )
# image(as.matrix(coph.data_T2_single), main='Single', asp=1 )
# image(as.matrix(coph.data_T2_complete), main='Complete', asp=1 )
# image(as.matrix(coph.data_T2_average), main='Average', asp=1 )

coph_coeff_single_T1T2T3T4 = cor(data_T1T2T3T4.eucl, coph.data_T1T2T3T4_single)
coph_coeff_complete_T1T2T3T4 = cor(data_T1T2T3T4.eucl, coph.data_T1T2T3T4_complete)
coph_coeff_average_T1T2T3T4 = cor(data_T1T2T3T4.eucl, coph.data_T1T2T3T4_average)

c("Eucl-Single"=coph_coeff_single_T1T2T3T4,"Eucl-Compl."=coph_coeff_complete_T1T2T3T4,"Eucl-Ave."=coph_coeff_average_T1T2T3T4)
# average and complete are pretty similar

cluster.single_T1T2T3T4 = cutree(data_T1T2T3T4.eucl_single, k=3) # euclidean-single
cluster.complete_T1T2T3T4 = cutree(data_T1T2T3T4.eucl_complete, k=3) # euclidean-complete
cluster.average_T1T2T3T4 = cutree(data_T1T2T3T4.eucl_average, k=3) # euclidean-average

table(etichetta.vera = diphtong_labels, etichetta.cluster = cluster.single_T1T2T3T4)
table(etichetta.vera = diphtong_labels, etichetta.cluster = cluster.complete_T1T2T3T4)
table(etichetta.vera = diphtong_labels, etichetta.cluster = cluster.average_T1T2T3T4)
# questi risultati non vanno bene

#######################################################################

data_label = df_sample_normalized_T1T2T3T4[,1:3]
data_scaled = cbind(data_label, scale(df_sample_normalized_T1T2T3T4[,4:47]))
data_not_scaled = cbind(data_label, df_sample_normalized_T1T2T3T4[,4:47])

diph = 'aI'

# FEMALE #

spkr = 'F'

data_scaled_F = data_scaled %>% filter(speaker==spkr)
data_not_scaled_F = data_not_scaled %>% filter(speaker==spkr)
diphtong_labels_F = data_scaled_F[,2]


k_means_T1T2T3T4 = kmeans(data_not_scaled_F[,4:47], centers = 3)
k_means_T1 = kmeans(data_not_scaled_F[,4:14], centers = 3)
k_means_T2 = kmeans(data_not_scaled_F[,15:25], centers = 3)
k_means_T1T2 = kmeans(data_not_scaled_F[,4:25], centers = 3)
# table(k_means$cluster)
# k_means$centers
x11()
plot(x=k_means_T1T2T3T4,data = data_not_scaled_F[,4:47],
      title='KMeans clustering of Female Data'
     )

k_means_scaled = kmeans(data_scaled_F[,4:47], centers = 3)
# table(k_means_scaled$cluster)
# k_means_scaled$centers

#table(etichetta.vera_F = diphtong_labels_F, etichetta.cluster = k_means_scaled$cluster)
table(etichetta.vera_F = diphtong_labels_F, etichetta.cluster = k_means_T1T2T3T4$cluster)
table(etichetta.vera_F = diphtong_labels_F, etichetta.cluster = k_means_T1T2$cluster)
table(etichetta.vera_F = diphtong_labels_F, etichetta.cluster = k_means_T1$cluster)
table(etichetta.vera_F = diphtong_labels_F, etichetta.cluster = k_means_T2$cluster)

############################################################################
# altro modo per plottare clustering con kmeans (ci sono anche i label veri)
PCA_T1T2T3T4_F <-prcomp(data_not_scaled_F[,4:47])$x

#plot then add labels
x11()
plot(PCA_T1T2T3T4_F, main = 'KMeans clustering for Female speaker', col=k_means_T1T2T3T4$cluster)
text(x=PCA_T1T2T3T4_F[,1], y=PCA_T1T2T3T4_F[,2], cex=0.6, pos=4, labels=data_not_scaled_F$diphthong)

############################################################################

# sembra che aU possa essere identificato anche solo con la seconda formante
# (sia maschio che femmina)



# MALE #

spkr = 'M'

data_scaled_M = data_scaled %>% filter(speaker==spkr)
data_not_scaled_M = data_not_scaled %>% filter(speaker==spkr)
diphtong_labels_M = data_scaled_M[,2]

k_means_T1T2T3T4 = kmeans(data_not_scaled_M[,4:47], centers = 3)
k_means_T1 = kmeans(data_not_scaled_M[,4:14], centers = 3)
k_means_T2 = kmeans(data_not_scaled_M[,15:25], centers = 3)
k_means_T1T2 = kmeans(data_not_scaled_M[,4:25], centers = 3)
# table(k_means$cluster)
# k_means$centers
x11()
plot(x=k_means_T1T2T3T4,data = data_not_scaled_M[,4:47],
     title='KMeans clustering of Male Data'
)

k_means_scaled = kmeans(data_scaled_M[,4:47], centers = 3)
# table(k_means_scaled$cluster)
# k_means_scaled$centers

#table(etichetta.vera_M = diphtong_labels_M, etichetta.cluster = k_means_scaled$cluster)
table(etichetta.vera_M = diphtong_labels_M, etichetta.cluster = k_means_T1T2T3T4$cluster)
table(etichetta.vera_M = diphtong_labels_M, etichetta.cluster = k_means_T1T2$cluster)
table(etichetta.vera_M = diphtong_labels_M, etichetta.cluster = k_means_T1$cluster)
table(etichetta.vera_M = diphtong_labels_M, etichetta.cluster = k_means_T2$cluster)

PCA_T1T2T3T4_M <-prcomp(data_not_scaled_M[,4:47])$x

#plot then add labels
x11()
plot(PCA_T1T2T3T4_M, main = 'KMeans clustering for Male speaker', col=k_means_T1T2T3T4$cluster)
text(x=PCA_T1T2T3T4_M[,1], y=PCA_T1T2T3T4_M[,2], cex=0.6, pos=4, labels=data_not_scaled_M$diphthong)

# AI #


diph = 'aI'

data_scaled_aI = data_scaled %>% filter(diphthong==diph)
data_not_scaled_aI = data_not_scaled %>% filter(diphthong==diph)
diphtong_labels_aI = data_scaled_aI[,3]

k_means = kmeans(data_not_scaled_aI[,4:47], centers = 2)
# table(k_means$cluster)
# k_means$centers
x11()
plot.kmeans(x=k_means, data = data_not_scaled_aI[,4:47],
            title ='KMeans clustering of aI Data')

# OVERALL #

diph_labels = data_scaled[,2]
spkr_labels = data_scaled[,3]

k_means = kmeans(data_not_scaled[,4:47], centers = 2)
# table(k_means$cluster)
# k_means$centers
x11()
plot.kmeans(x=k_means, data = data_not_scaled[,4:47],
            title ='KMeans clustering of the whole dataset')
table(etichetta.vera = diph_labels, etichetta.cluster = k_means$cluster)

###########################################################################

# subset_per_diph = which(df_sample_normalized_T1T2T3T4$diphthong=='aI')
# x = seq(0,11,1)
# x11()
# plot(x, df_sample_normalized_T1T2T3T4[subset_per_diph,][,4:14], col='red')
# lines(df_sample_normalized_T1T2T3T4[subset_per_diph,][,15:25], col='green')
# lines(df_sample_normalized_T1T2T3T4[subset_per_diph,][,26:36], col='blue')
# lines(df_sample_normalized_T1T2T3T4[subset_per_diph,][,37:47], col='yellow')

###########################################################################
########################### CLASSIFICATION ################################

# Test with speech libraries for lda

library(phonTools)
## load data
data = df_sample_normalized_T1T2T3T4
## normalize diph formant frequencies
normddiphtongs = normalize(data[,4:47], data$speaker, data$diphthong)
formants = normddiphtongs[,1:44]
diphs = data$diphthong
## make a diph template based on these frequencies
template = createtemplate(formants, diphs)
## classify diphthongs
answers = ldclassify(formants[1:44], template$means, template$covariance)
## compare to known categories
table(diphs, answers)

###########################################################################

# K-Fold cross validation Females

set.seed(82)
rand = sample(93,75)
train_T1T2T3T4_F = data_not_scaled_F[rand,]
test_T1T2T3T4_F = data_not_scaled_F[-rand,]

model = train(diphthong ~ ., train_T1T2T3T4_F[,c(2,4:47)],
              method ='lda',  # nessun problema con: lda, LMT, mda, ..
              trControl = trainControl(
                method = 'cv', number = 10,
                verboseIter = TRUE
              )
)

test_T1T2T3T4_F.pred = predict(model, test_T1T2T3T4_F[,c(2,4:47)])
confusionMatrix(data = test_T1T2T3T4_F.pred, 
                reference = test_T1T2T3T4_F$diphthong)


# K-Fold cross validation Males

set.seed(82)
rand = sample(93,75)
train_T1T2T3T4_M = data_not_scaled_M[rand,]
test_T1T2T3T4_M = data_not_scaled_M[-rand,]

model = train(diphthong ~ ., train_T1T2T3T4_M[,c(2,4:47)],
              method ='lda',  # nessun problema con: lda, LMT, mda, ..
              trControl = trainControl(
                method = 'cv', number = 10,
                verboseIter = TRUE
              )
)

test_T1T2T3T4_M.pred = predict(model, test_T1T2T3T4_M[,c(2,4:47)])
confusionMatrix(data = test_T1T2T3T4_M.pred, 
                reference = test_T1T2T3T4_M$diphthong)


# K-Fold cross validation complete

set.seed(82)
rand = sample(186, 150)
train_T1T2T3T4 = data_not_scaled[rand,]
test_T1T2T3T4 = data_not_scaled[-rand,]

model = train(diphthong ~ ., train_T1T2T3T4[,2:47],
              method = 'lda',
              trControl = trainControl(
                method = 'cv', number = 10,
                verboseIter = TRUE
              )
)

test_T1T2T3T4.pred = predict(model, test_T1T2T3T4[,2:47])
confusionMatrix(data = test_T1T2T3T4.pred, 
                reference = test_T1T2T3T4$diphthong)

###########################################################################

