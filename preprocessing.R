library(emuR)

# -----------------------------------------------------------------
# variable definition
num_of_formants = 4

num_of_samples = 186
num_of_aI_samples = 118
num_of_aU_samples = 44
num_of_OY_samples = 24

diphtongs_names = unique(dip$labels)
times = as.numeric(rownames(dip.fdat$data))
speech_recognition_dataset = dip.fdat

dip_aI = dip[dip$labels == 'aI',]
dip_aU = dip[dip$labels == 'aU',]
dip_OY = dip[dip$labels == 'OY',]

# ---------------------------------------------------------------------------------------------------
# substitution of 0 values (obviously wrong) with the mean of the corresponding formant
for (formant in 1:num_of_formants){
  speech_recognition_dataset$data[,formant][speech_recognition_dataset$data[,formant]==0]   = mean(speech_recognition_dataset$data[,formant])
}

# plot of formants in the same range of values
x11()
layout(matrix(1:4,nrow=2,byrow=T))
par(mar=c(3,2,3,1),mgp=c(2,0.5,0))
dplot(speech_recognition_dataset[,1],
      dip$labels,
      normalise = F,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "1st Formant [Hz]", col=rainbow(3),lwd=1.1,main='First formant',lty=factor(dip.spkr))

dplot(speech_recognition_dataset[,2],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "2nd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Second formant',lty=factor(dip.spkr))

dplot(speech_recognition_dataset[,3],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "3rd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Third formant',lty=factor(dip.spkr))

dplot(speech_recognition_dataset[,4],
      dip$labels,
      normalise = TRUE,
      ylim = c(0,5500),
      xlab = "Normalized Time [%]",
      ylab = "4th Formant [Hz]", col=rainbow(3),lwd=1.1,main='Fourth formant',lty=factor(dip.spkr))


# some summary statistics on the dataset
summary(speech_recognition_dataset)
dim(speech_recognition_dataset$data)
dim(speech_recognition_dataset$ftime)
dim(dip)

# creation of time (normalized and standard) and data matrices for F1 and F2
speech_recognition_dataset$index #indici di inizio e fine delle curve
maxlength = max(apply(speech_recognition_dataset$index,1,diff)) + 1
data.matrix_F1 = matrix(nrow=num_of_samples,ncol=maxlength)
data.matrix_F2 = matrix(nrow=num_of_samples,ncol=maxlength)
time.matrix = matrix(nrow=num_of_samples,ncol=maxlength)
normalized.time.matrix = matrix(nrow=num_of_samples,ncol=maxlength)

for(ii in 1:num_of_samples){
  indice.ii = speech_recognition_dataset$index[ii,]
  time.std = times[indice.ii[1]:indice.ii[2]] - times[indice.ii[1]]
  time.matrix[ii,1:length(time.std)] = time.std
  normalized.time.matrix[ii,1:length(time.std)] = time.std/max(time.std)
  data.matrix_F1[ii,1:length(time.std)] = speech_recognition_dataset$data[indice.ii[1]:indice.ii[2], 1]
  data.matrix_F2[ii,1:length(time.std)] = speech_recognition_dataset$data[indice.ii[1]:indice.ii[2], 2]
}


#------------------------------------------------------------------------------------
# save diphtongs indeces

indeces_aI = c()
indeces_aU = c()
indeces_OY = c()
for (i in 1:num_of_samples){
  if (dip$labels[i] == 'aI') indeces_aI = c(indeces_aI, i)
  else if (dip$labels[i] == 'aU') indeces_aU = c(indeces_aU, i)
  else if (dip$labels[i] == 'OY') indeces_OY = c(indeces_OY, i)
}

for (i in indeces_aI){
  print(dip.fdat$index[i,][1])
  print(dip.fdat$index[i,][2])
}


#------------------------------------------------------------------------------------
# creation of vectors with relevant information for F2

#fill a vector with the max_value of each row
max_vect_F2 = c()
for (i in 1:num_of_samples) {
  max_i = max(data.matrix_F2[i,], na.rm = T)
  max_vect_F2 = c(max_vect_F2, max_i)
}

#fill a vector with the min_value of each row
min_vect_F2 = c()
for (i in 1:num_of_samples) {
  min_i = min(data.matrix_F2[i,], na.rm = T)
  min_vect_F2 = c(min_vect_F2, min_i)
}

#fill a vector with the mean_value of each row
avg_vect_F2 = c()
for (i in 1:num_of_samples) {
  avg_i = mean(data.matrix_F2[i,], na.rm = T)
  avg_vect_F2 = c(avg_vect_F2, avg_i)
}

# create vector with duration of each sample
duration_vect_F2 = dip$end - dip$start

#----------------------------------------------------------------------------------
# Basic Principal Component Analysis

df_F2 = data.frame(max_vect_F2, min_vect_F2, avg_vect_F2, duration_vect_F2)
pca = princomp(df_F2, cor = T)

summary(pca)

# testing of new plotting library
library(factoextra)

x11()
fviz_eig(pca)

fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

data.matrix
