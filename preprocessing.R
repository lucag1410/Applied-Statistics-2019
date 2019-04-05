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

# creation of colors vector
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] 
#pie(rep(1,200), col=sample(color, 200))


speech_data = dip.fdat
# NB speech_data is "our" dataset, I would use this instead of dip.fdat in order to
# still have the original dataset


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

# some summary statistics on the dataset
summary(speech_data)
dim(speech_data$data)
dim(speech_data$ftime)
dim(dip)

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

# comparison between T1 and T2 for all the diphtongs for both female and male speakers
x11()
layout(matrix(1:6,nrow=3,byrow=T))
plot(data.OY_female$data, xlim = c(100, 800), ylim = c(550,2800), main='OY_female')
plot(data.OY_male$data, xlim = c(100, 800),ylim = c(550,2800), main='OY_male')
plot(data.aU_female$data, xlim = c(100, 900),ylim = c(550,2800), main='aU_female')
plot(data.aU_male$data, xlim = c(100, 900),ylim = c(550,2800), main='aU_male')
plot(data.aI_female$data, xlim = c(100, 1000),ylim = c(550,2800), main='aI_female')
plot(data.aI_male$data, xlim = c(100, 1000),ylim = c(550,2800), main='aI_male')
graphics.off()


formante = 4
min_f = min(data.OY_female$data[,formante])
max_f = max(data.OY_female$data[,formante])
min_m = min(data.OY_male$data[,formante])
max_m = max(data.OY_male$data[,formante])

min_ov = min(min_f,min_m)
max_ov = max(max_f, max_m)

x11()
boxplot(data.OY_female$data[,formante], col = 'gold', ylim = c(min_ov,max_ov), main='OY_female')
x11()
boxplot(data.OY_male$data[,formante], col = 'gold', ylim = c(min_ov,max_ov), main='OY_male')


x11()
plot(T_AVG[indices_OY_male], T_AVG[indices_OY_female], xlab = 'OY_male', ylab = 'OY_female',
        main = 'Comparison of T_AVG between OY_female and OY_male', pch=19)
x11()
plot(T_AVG[indices_aU_male], T_AVG[indices_aU_female], xlab = 'aU_male', ylab = 'aU_female',
     main = 'Comparison of T_AVG between aU_female and aU_male', pch=19)
x11()
plot(T_AVG[indices_aI_male], T_AVG[indices_aI_female], xlab = 'aI_male', ylab = 'aI_female',
     main = 'Comparison of T_AVG between aI_female and aI_male', pch=19)


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
  if (dip.spkr[i] == '67') indices_female = c(indices_female, i)
  else if (dip.spkr[i] == '68') indices_male = c(indices_male, i)
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
# see this link for explanations on the package: https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/

num_observations = length(dip.fdat[,1]$data)
#use times for retrieving time indices

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
df_complete['T_AVG'] = rowMeans(df_complete %>% select(T1, T2, T3, T4))

# df_complete %>% filter(sample == 1, time < 1300 | time > 1320)
# dim(df_complete %>% filter(speaker == 'F'))
# dim(df_complete %>% filter(speaker == 'M'))

x11()
plot(df_complete$T1, xlab = 'Observaton', ylab = 'Formant[Hz]', ylim =  c(0, max(df_complete[,6:9])), col = 'red')
points(df_complete$T2, col = 'blue')
points(df_complete$T3, col = 'green')
points(df_complete$T4, col = 'orange')
abline(h = c(mean(df_complete$T1),mean(df_complete$T2),mean(df_complete$T3),mean(df_complete$T4)), col = c('red','blue','green','orange'), lwd = 2)
abline(v = which.max(df_complete[,'speaker'] == 'M'))
legend(x = 0, y = 5500, legend = c("T1","T2","T3","T4"), col = c('red','blue','green','orange'), lty = 1)

