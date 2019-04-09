#setwd("C:/Users/Luca/Desktop/Applied-Statistics-2019-master/Applied-Statistics-2019-master/data")

library(emuR)

# -----------------------------------------------------------------
# variables definition
num_of_formants = 4

num_of_samples = 186
num_of_aI_samples = 118
num_of_aU_samples = 44
num_of_OY_samples = 24
num_of_female_samples = 93
num_of_male_samples = 93

diphtongs_names = unique(dip$labels)
times = as.numeric(rownames(dip.fdat$data))

speech_data = dip.fdat
# NB speech_data is "our" dataset, I would use this instead of dip.fdat in order to
# still have the original dataset


### LOAD DATASETS
df_complete_noZeros = read.table('../data/df_complete_noZeros.txt')
df_sample_normalized_T1 = read.table('../data/df_sample_normalized_T1.txt')
df_sample_normalized_T2 = read.table('../data/df_sample_normalized_T2.txt')
df_sample_normalized_T3 = read.table('../data/df_sample_normalized_T3.txt')
df_sample_normalized_T4 = read.table('../data/df_sample_normalized_T4.txt')
df_sample = read.table('../data/df_sample.txt')



#-------------------------------------------------------------------------------------------------------------
#rows_containing_nulls contains the rows of dip.fdat$data with at least one "0" (NOT the sample in the range 1-186)
# NB there is one row containing two 0

rows_containing_nulls = c()
for (i in 1:length(dip.fdat$data[,1])){
  if(is.element(0, dip.fdat$data[i,])){
    rows_containing_nulls = c(rows_containing_nulls, i)
  }
}

# loop for showing indices and values of the formants for rows_containing_nulls
# for (i in rows_containing_nulls){
#   print(c(i, dip.fdat$data[i,]))
# }


# ---------------------------------------------------------------------------------------------------
# substitution of 0 values (they are obviously wrong) with the mean of the corresponding formant
# COMMENT THE LOOP IF YOU DO NOT WANT TO PERFORM THE SUBSTITUTION

for (formant in 1:num_of_formants){
  speech_data$data[,formant][speech_data$data[,formant]==0]   = mean(speech_data$data[,formant])
}

# creation of time (normalized and standard) and data matrices for all the formants
maxlength = max(apply(speech_data$index,1,diff)) + 1
data.matrix_F1 = matrix(nrow=num_of_samples,ncol=maxlength)
data.matrix_F2 = matrix(nrow=num_of_samples,ncol=maxlength)
data.matrix_F3 = matrix(nrow=num_of_samples,ncol=maxlength)
data.matrix_F4 = matrix(nrow=num_of_samples,ncol=maxlength)
time.matrix = matrix(nrow=num_of_samples,ncol=maxlength)
normalized.time.matrix = matrix(nrow=num_of_samples,ncol=maxlength)

for(ii in 1:num_of_samples){
  indice.ii = speech_data$index[ii,]
  time.std = times[indice.ii[1]:indice.ii[2]] - times[indice.ii[1]]
  time.matrix[ii,1:length(time.std)] = time.std
  normalized.time.matrix[ii,1:length(time.std)] = time.std/max(time.std)
  data.matrix_F1[ii,1:length(time.std)] = speech_data$data[indice.ii[1]:indice.ii[2], 1]
  data.matrix_F2[ii,1:length(time.std)] = speech_data$data[indice.ii[1]:indice.ii[2], 2]
  data.matrix_F3[ii,1:length(time.std)] = speech_data$data[indice.ii[1]:indice.ii[2], 3]
  data.matrix_F4[ii,1:length(time.std)] = speech_data$data[indice.ii[1]:indice.ii[2], 4]
  }


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



#------------------------------------------------------------------------------------
# save diphtongs indices

indices_aI = c()
indices_aU = c()
indices_OY = c()
for (i in 1:num_of_samples){
  if (dip$labels[i] == 'aI') indices_aI = c(indices_aI, i)
  else if (dip$labels[i] == 'aU') indices_aU = c(indices_aU, i)
  else if (dip$labels[i] == 'OY') indices_OY = c(indices_OY, i)
}


# CREATION OF ONE DATASET FOR EACH DIPHTONG
data.aI = speech_data[indices_aI]
data.aU = speech_data[indices_aU]
data.OY = speech_data[indices_OY]


#----------------------------------------------------------------------------------------------------
# save speakers indices

indices_female = c()
indices_male = c()
for (i in 1:num_of_samples){
  if (dip.spkr[i] == '67') indices_male = c(indices_male, i)
  else if (dip.spkr[i] == '68') indices_female = c(indices_female, i)
}

# CREATION OF ONE DATASET FOR EACH SPEAKER
data.female = speech_data[indices_female]
data.male = speech_data[indices_male]


# find indices for each diphtong and for each speaker

indices_aI_female = intersect(indices_aI, indices_female)
indices_aI_male = intersect(indices_aI, indices_male)
indices_aU_female = intersect(indices_aU, indices_female)
indices_aU_male = intersect(indices_aU, indices_male)
indices_OY_female = intersect(indices_OY, indices_female)
indices_OY_male = intersect(indices_OY, indices_male)

# CREATION OF ONE DATASET FOR EACH DIPHTONG, FOR EACH SPEAKER
data.aI_female = speech_data[indices_aI_female]
data.aI_male = speech_data[indices_aI_male]
data.aU_female = speech_data[indices_aU_female]
data.aU_male = speech_data[indices_aU_male]
data.OY_female = speech_data[indices_OY_female]
data.OY_male = speech_data[indices_OY_male]

# creation of the "average" formant T_AVG
T_AVG = c()
for (i in 1:length(speech_data$data[,1])){
  T_AVG = c(T_AVG, mean(speech_data$data[i,]))
}

# TODO: the following rows are pretty inefficient,try to change these lines
temp = speech_data # temp used to preserve our dataset
temp$data = cbind(speech_data$data, T_AVG)
avg_formant_data = temp[,5]
rm(temp) # remove temp since we don't need it anymore

#--------------------------------------------------------------------------------
# ratio_F2_F1 is a vector containing the ratio F2/F1 on all the observations (not samples)
ratio_F2_F1 = c()
for (i in 1:length(speech_data$data[,1])){
  ratio_i = speech_data$data[i,2] / speech_data$data[i,1]
  ratio_F2_F1 = c(ratio_F2_F1, ratio_i)
}

# TODO: the following rows are pretty inefficient,try to change these lines
temp = speech_data # temp used to preserve our dataset
temp$data = cbind(speech_data$data, ratio_F2_F1)
ratio_F2_F1_data = temp[,5]
rm(temp) # remove temp since we don't need it anymore


### plot ratio F2/F1 for a given case (set "chosen_index" to the indices that you want)
###DA FINIRE: C'é QUALCOSA CHE NON VA CON I VALORI MASSIMALI DELLE CURVE (E.G. MAX IN 12 AND NOT IN 4.7)
chosen_indices = indices_OY
x11()
plot(x = c(0, max(ratio_F2_F1_data$index[,2] - ratio_F2_F1_data$index[,1])),
     y = c(0, max(ratio_F2_F1_data$data)), type='n',
     xlab = 'time', ylab = 'Ratio F2/F1', 
     main = c('Ratio F2/F1 for diph. ', unique(dip.l[chosen_indices]), 'both spkrs'))
for (i in chosen_indices){
  rand_color = sample(1:length(color), 1)
  points(ratio_F2_F1_data[i,]$data, col = rand_color, pch = 19)
  lines(ratio_F2_F1_data[i,]$data, col = rand_color)
}

x11()
plot(ratio_F2_F1_data[chosen_indices[chosen_indices<=93],]$data,
     xlab = 'Index', ylab = 'Ratio F2/F1', 
     main = c('Ratio F2/F1 for diph. ', unique(dip.l[chosen_indices]), 'spkr F'),
     col = rainbow(n = 300), pch = 19)


### plot of avg formant
x11()
dplot(avg_formant_data,
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "Avg Formant [Hz]", col=rainbow(3),lwd=1.1,main='Average formant',lty=factor(dip.spkr))


#------------------------------------------------------------------------------------------
# creation of dataframe with summary statistics for the diphtongs, for females and for males
# I considered only the avg_formant_dataset but we can add columns also for each formant

df_avg_female = data.frame(
  row.names = diphtongs_names, 
  "Mean" = c(mean(avg_formant_data$data[indices_aI_female]), 
             mean(avg_formant_data$data[indices_aU_female]),
             mean(avg_formant_data$data[indices_OY_female])
             ),
  "Std" = c(sd(avg_formant_data$data[indices_aI_female]), 
            sd(avg_formant_data$data[indices_aU_female]),
            sd(avg_formant_data$data[indices_OY_female])
            ),
  "Max" = c(max(avg_formant_data$data[indices_aI_female]), 
            max(avg_formant_data$data[indices_aU_female]),
            max(avg_formant_data$data[indices_OY_female])
            ),
  "Min" = c(min(avg_formant_data$data[indices_aI_female]), 
            min(avg_formant_data$data[indices_aU_female]),
            min(avg_formant_data$data[indices_OY_female])
            )
)

df_avg_male = data.frame(
  row.names = diphtongs_names, 
  "Mean" = c(mean(avg_formant_data$data[indices_aI_male]), 
             mean(avg_formant_data$data[indices_aU_male]),
             mean(avg_formant_data$data[indices_OY_male])
  ),
  "Std" = c(sd(avg_formant_data$data[indices_aI_male]), 
            sd(avg_formant_data$data[indices_aU_male]),
            sd(avg_formant_data$data[indices_OY_male])
  ),
  "Max" = c(max(avg_formant_data$data[indices_aI_male]), 
            max(avg_formant_data$data[indices_aU_male]),
            max(avg_formant_data$data[indices_OY_male])
  ),
  "Min" = c(min(avg_formant_data$data[indices_aI_male]), 
            min(avg_formant_data$data[indices_aU_male]),
            min(avg_formant_data$data[indices_OY_male])
  )
)

# the matrices are overall very similar, except for the Std of aU, which is way larger for the males



#--------------------------------------------------------------------------------
# creation of vector with mean value of the avg_formant for each sample
# basically this vector contains the mean value of each line in the avg_formant graph
# use the indices in order to have more specific information (e.g. mean_samples_avg_formant[indices_aI])

mean_samples_avg_formant = c()

for (i in 1:186){
  mean_samples_avg_formant = c(mean_samples_avg_formant, mean(avg_formant_data[i]$data))
}

### plot of mean values for all the samples (ignore the warning messages in the console)
# there is a clear distinction between the speakers
# NB the indices goes from 1 to 118 (the number of aI samples)
# I don't think that this is an issue since we are interested in the differences on the y-axis

x11()
plot(mean_samples_avg_formant[indices_aI], col = 'red', pch = 19, ylim = c(1500, max(mean_samples_avg_formant)), ylab = 'Mean sample value from T_AVG')
legend(0, max(mean_samples_avg_formant), legend=c('aI', 'aU', 'OY'), col=c("red", 'green', "blue"), lty = 1, cex = 1)
points(mean_samples_avg_formant[indices_aU], col = 'green', pch = 19)
points(mean_samples_avg_formant[indices_OY], col = 'blue', pch = 19)

### TODO: there is something wrong when plotting the data using specific indices
### I think that this is due to the fact that I still used dip$labels and not dip$"male labels", for example


#----------------------------------------------------------------------------------
### creation of complete matrix with all the information from dip.fdat and dip

library(dplyr) # for operations on dataframes
library(plyr)  # for vectors handling
# see this link for explanations on the package dplyr: https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/

num_observations = length(dip.fdat[,1]$data)

#create diphtong labels vector with 5462 values
diph_vect = c()
for (i in 1:num_of_samples){
  label_i = dip$labels[i]
  diph_vect = c(diph_vect, rep(label_i, length(dip.fdat[i,]$data)/4))
}

#create speaker vector with 5462 values ('F' for female, 'M' for Male)
spkr_vect = c()
for (i in 1:num_of_samples){
  spkr_i = ifelse(dip.spkr[i] == '67', 'M', 'F')
  spkr_vect = c(spkr_vect, rep(spkr_i, length(dip.fdat[i,]$data)/4))
}

#create samples vector with 5462 values
sample_vect = c()
for (i in 1:num_of_samples){
  sample_vect = c(sample_vect, rep(i, length(dip.fdat[i,]$data)/4))
}
sample_vect = type.convert(sample_vect)

#creation of the dataframe, we can add more columns with df_complete['newcolumn_name'] = new_column
df_complete = data.frame(
  row.names = c(1:num_observations),
  'sample' = sample_vect,
  'observation' = c(1:num_observations),
  'time' = times,
  'diphtong' = diph_vect,
  'speaker' = spkr_vect,
  'T1' = dip.fdat[,1]$data,
  'T2' = dip.fdat[,2]$data,
  'T3' = dip.fdat[,3]$data,
  'T4' = dip.fdat[,4]$data
)

### creation of temporary sub-dataframes in order to create the dataframe without zeros

df_aI_female = df_complete %>% filter(diphtong == 'aI', speaker == 'F')
df_aU_female = df_complete %>% filter(diphtong == 'aU', speaker == 'F')
df_OY_female = df_complete %>% filter(diphtong == 'OY', speaker == 'F')
df_aI_male = df_complete %>% filter(diphtong == 'aI', speaker == 'M')
df_aU_male = df_complete %>% filter(diphtong == 'aU', speaker == 'M')
df_OY_male = df_complete %>% filter(diphtong == 'OY', speaker == 'M')

# for each formant find mean for each pair speaker, dipthong and replace the values in the correct sub-df

for (i in 1:num_of_formants){
  # compute the means..
  mean_aI_female = mean((df_complete %>% filter(speaker == 'F', diphtong == 'aI') %>% select(i+5))[,1])
  mean_aU_female = mean((df_complete %>% filter(speaker == 'F', diphtong == 'aU') %>% select(i+5))[,1])
  mean_OY_female = mean((df_complete %>% filter(speaker == 'F', diphtong == 'OY') %>% select(i+5))[,1])
  mean_aI_male = mean((df_complete %>% filter(speaker == 'M', diphtong == 'aI') %>% select(i+5))[,1])
  mean_aU_male = mean((df_complete %>% filter(speaker == 'M', diphtong == 'aU') %>% select(i+5))[,1])
  mean_OY_male = mean((df_complete %>% filter(speaker == 'M', diphtong == 'OY') %>% select(i+5))[,1])
  
  # .. and replace the zeros with the correct mean
  df_aI_female[,i+5] = mapvalues(df_aI_female[,i+5], from= 0, to= mean_aI_female)
  df_aU_female[,i+5] = mapvalues(df_aU_female[,i+5], from= 0, to= mean_aU_female)
  df_OY_female[,i+5] = mapvalues(df_OY_female[,i+5], from= 0, to= mean_OY_female)
  df_aI_male[,i+5] = mapvalues(df_aI_male[,i+5], from= 0, to= mean_aI_male)
  df_aU_male[,i+5] = mapvalues(df_aU_male[,i+5], from= 0, to= mean_aU_male)
  df_OY_male[,i+5] = mapvalues(df_OY_male[,i+5], from= 0, to= mean_OY_male)
}

# creation of the dataframe without null values

df_complete_noZeros = rbind(df_aI_female, df_aI_male, df_OY_female, df_OY_male, df_aU_female, df_aU_male)
df_complete_noZeros = df_complete_noZeros[order(df_complete_noZeros$observation),]
rownames(df_complete_noZeros) = seq(1,num_observations)

df_complete_noZeros['T2/T1'] = df_complete_noZeros$T2 / df_complete_noZeros$T1
df_complete_noZeros['T_AVG'] = rowMeans(df_complete_noZeros %>% select(T1, T2, T3, T4))

# dataframes for each pair diphtong-speaker without zeros

df_aI_female_noZeros = filter(df_complete_noZeros, diphtong == 'aI', speaker == 'F')
df_aU_female_noZeros = filter(df_complete_noZeros, diphtong == 'aU', speaker == 'F')
df_OY_female_noZeros = filter(df_complete_noZeros, diphtong == 'OY', speaker == 'F')
df_aI_male_noZeros = filter(df_complete_noZeros, diphtong == 'aI', speaker == 'M')
df_aU_male_noZeros = filter(df_complete_noZeros, diphtong == 'aU', speaker == 'M')
df_OY_male_noZeros = filter(df_complete_noZeros, diphtong == 'OY', speaker == 'M')

# plot formant values and means

x11()
plot(df_complete_noZeros$T1, xlab = 'Observation', ylab = 'Formant[Hz]', ylim =  c(0, max(df_complete[,6:9])), col = 'red')
points(df_complete_noZeros$T2, col = 'blue')
points(df_complete_noZeros$T3, col = 'green')
points(df_complete_noZeros$T4, col = 'orange')
abline(h = c(mean(df_complete_noZeros$T1),mean(df_complete_noZeros$T2),mean(df_complete_noZeros$T3),mean(df_complete_noZeros$T4)), col = c('red','blue','green','orange'), lwd = 2)
abline(v = which.max(df_complete_noZeros[,'speaker'] == 'F'))
legend(x = 0, y = 5300, legend = c("T1","T2","T3","T4"), col = c('red','blue','green','orange'), lty = 1)
text(1500,5300, 'Males')
text(3500,5300, 'Females')


### CREATION OF THE SAMPLES SUMMARY DATASET

df_sample = data.frame(
  row.names = c(1:num_of_samples),
  'sample' = c(1:num_of_samples),
  'diphtong' = dip.l,
  'speaker' = ifelse(dip.spkr == '67', 'M', 'F')
)

# functions for retrieving the first and the last observations given a sample
get_first_observation <- function(dataset, smpl){
  result = dataset %>% filter(sample == smpl)
  result = result[1,]
  return(result)
}

get_last_observation <- function(dataset, smpl){
  result = dataset %>% filter(sample == smpl)
  result = tail(result, n=1)
  return(result)
}

sample_duration = c()
for (i in 1:num_of_samples){
  sample_duration = c(sample_duration, 
                      get_last_observation(df_complete_noZeros, i)$time -
                        get_first_observation(df_complete_noZeros, i)$time)
}
df_sample['duration'] = sample_duration

T1_min_per_sample = c()
T2_min_per_sample = c()
T1_max_per_sample = c()
T2_max_per_sample = c()
T1_first_per_sample = c()
T2_first_per_sample = c()
T1_last_per_sample = c()
T2_last_per_sample = c()
for (i in 1:num_of_samples){
  T1_min_per_sample = c(T1_min_per_sample, 
                      min(df_complete_noZeros[df_complete_noZeros$sample == i,]$T1))
  T2_min_per_sample = c(T2_min_per_sample, 
                        min(df_complete_noZeros[df_complete_noZeros$sample == i,]$T2))
  T1_max_per_sample = c(T1_max_per_sample, 
                        max(df_complete_noZeros[df_complete_noZeros$sample == i,]$T1))
  T2_max_per_sample = c(T2_max_per_sample, 
                        max(df_complete_noZeros[df_complete_noZeros$sample == i,]$T2))
  T1_first_per_sample = c(T1_first_per_sample,
                          get_first_observation(df_complete_noZeros, i)$T1)
  T2_first_per_sample = c(T2_first_per_sample,
                          get_first_observation(df_complete_noZeros, i)$T2)
  T1_last_per_sample = c(T1_last_per_sample,
                          get_first_observation(df_complete_noZeros, i)$T1)
  T2_last_per_sample = c(T2_last_per_sample,
                          get_first_observation(df_complete_noZeros, i)$T2)
}
df_sample['T1_min'] = T1_min_per_sample
df_sample['T2_min'] = T2_min_per_sample
df_sample['T1_max'] = T1_max_per_sample
df_sample['T2_max'] = T2_max_per_sample
df_sample['T1_first'] = T1_first_per_sample
df_sample['T2_first'] = T2_first_per_sample
df_sample['T1_last'] = T1_last_per_sample
df_sample['T2_last'] = T2_last_per_sample


### PCA
samples.sd <- scale(df_complete_noZeros[,-(1:5)])
samples.sd <- data.frame(samples.sd)
samples.label = df_complete_noZeros[,(1:5)]

head(samples.sd)

pc.samples.sd <- princomp(samples.sd, scores=T)
pc.samples.sd
summary(pc.samples.sd)

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.samples.sd, las=2, main='Principal Components', ylim=c(0,7))
abline(h=1, col='blue')
barplot(sapply(samples.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,7), ylab='Variances')
plot(cumsum(pc.samples.sd$sde^2)/sum(pc.samples.sd$sde^2), type='b', axes=F, xlab='Number of components', ylab='Contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(samples.sd),labels=1:ncol(samples.sd),las=2)


load.samples.sd <- pc.samples.sd$loadings
load.samples.sd

x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.samples.sd[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))


scores.samples.sd <- pc.samples.sd$scores
scores.samples.sd

x11()
plot(scores.samples.sd[,1:2], main = 'scores 1:2')
abline(h=0, v=0, lty=2, col='grey')

x11()
biplot(pc.samples.sd)

# We order the labels according to time order
samples.label[,4] <- factor(samples.label[,4], levels=c('aI', 'aU', 'OY'))
col.ramp <- rainbow(3)
col.lab1 <- rep(NA, length(df_complete_noZeros[,1]))
for(i in 1:length(df_complete_noZeros[,1]))
  col.lab1[i] = col.ramp[which(samples.label[i,4] == levels(samples.label[,4]))]

x11(width = 14)
par(mfrow=c(1,2),mar=rep(8,4))
plot(scores.samples.sd[,1:2], col=col.lab1, pch=19, xlim=c(-16,3), ylim=c(-3,3.2))
abline(h=-3, v=-16, col=1)
points(scores.samples.sd[,1], rep(-3, length(df_complete_noZeros[,1])), col=col.lab1, pch=19)
points(rep(-16, length(df_complete_noZeros[,1])),scores.samples.sd[,2], col=col.lab1, pch=19)
abline(h=0, v=0, lty=2, col='grey')
plot(1:3, rep(0, 3), pch=15, col=rainbow(3), axes=F, xlab='',ylab='')
axis(1, 1:3, levels(samples.label[,2]), las=2)
abline(v=1:3, col='grey', lty=2)
box()

###  CREATION OF NORMALIZED DATASET FOR EACH FORMANT
num_of_interpolations = 60

df_complete_normalized = data.frame(row.names = c(1:(num_of_interpolations*num_of_samples)))

T1_approx = c()
T2_approx = c()
T3_approx = c()
T4_approx = c()
for (i in 1:num_of_samples){
  i_T1_approx = approx(df_complete_noZeros[df_complete_noZeros$sample == i,]$T1, n= 60)[2]
  i_T2_approx = approx(df_complete_noZeros[df_complete_noZeros$sample == i,]$T2, n= 60)[2]
  i_T3_approx = approx(df_complete_noZeros[df_complete_noZeros$sample == i,]$T3, n= 60)[2]
  i_T4_approx = approx(df_complete_noZeros[df_complete_noZeros$sample == i,]$T4, n= 60)[2]
  for (j in i_T1_approx){
    T1_approx = c(T1_approx,j)
  }
  for (j in i_T2_approx){
    T2_approx = c(T2_approx,j)
  }
  for (j in i_T3_approx){
    T3_approx = c(T3_approx,j)
  }
  for (j in i_T4_approx){
    T4_approx = c(T4_approx,j)
  }
}

df_complete_normalized['T1'] = T1_approx
df_complete_normalized['T2'] = T2_approx
df_complete_normalized['T3'] = T3_approx
df_complete_normalized['T4'] = T4_approx

df_sample_normalized_T1 = df_sample[,1:3]
df_sample_normalized_T2 = df_sample[,1:3]
df_sample_normalized_T3 = df_sample[,1:3]
df_sample_normalized_T4 = df_sample[,1:3]

for (i in c(1:186)){
  df_sample_normalized_T1[i,4:63] = df_complete_normalized[((i-1)*60+1):(i*60),1]
  df_sample_normalized_T2[i,4:63] = df_complete_normalized[((i-1)*60+1):(i*60),2]
  df_sample_normalized_T3[i,4:63] = df_complete_normalized[((i-1)*60+1):(i*60),3]
  df_sample_normalized_T4[i,4:63] = df_complete_normalized[((i-1)*60+1):(i*60),4]
}

df_sample_normalized_T1 = df_sample_normalized_T1[,c(1:3, 4,10,16,22,28,34,40,46,52,58,63)]
df_sample_normalized_T2 = df_sample_normalized_T2[,c(1:3, 4,10,16,22,28,34,40,46,52,58,63)]
df_sample_normalized_T3 = df_sample_normalized_T3[,c(1:3, 4,10,16,22,28,34,40,46,52,58,63)]
df_sample_normalized_T4 = df_sample_normalized_T4[,c(1:3, 4,10,16,22,28,34,40,46,52,58,63)]

colnames(df_sample_normalized_T1) = c('sample','diphtong','speaker','T1_0%','T1_10%','T1_20%','T1_30%','T1_40%',
                                               'T1_50%','T1_60%','T1_70%','T1_80%','T1_90%', 'T1_100%')
colnames(df_sample_normalized_T2) = c('sample','diphtong','speaker','T2_0%','T2_10%','T2_20%','T2_30%','T2_40%',
                                      'T2_50%','T2_60%','T2_70%','T2_80%','T2_90%', 'T2_100%')
colnames(df_sample_normalized_T3) = c('sample','diphtong','speaker','T3_0%','T3_10%','T3_20%','T3_30%','T3_40%',
                                      'T3_50%','T3_60%','T3_70%','T3_80%','T3_90%', 'T3_100%')
colnames(df_sample_normalized_T4) = c('sample','diphtong','speaker','T4_0%','T4_10%','T4_20%','T4_30%','T4_40%',
                                      'T4_50%','T4_60%','T4_70%','T4_80%','T4_90%', 'T4_100%')
