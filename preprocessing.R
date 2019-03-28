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

#------------------------------------------------------------------------------
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
plot(mean_samples_avg_formant[indices_aI], col = 'red', pch = 19, legend = 'aI', ylim = c(1500, max(mean_samples_avg_formant)), ylab = 'Mean sample value from T_AVG')
legend(0, max(mean_samples_avg_formant), legend=c('aI', 'aU', 'OY'), col=c("red", 'green', "blue"), lty = 1, cex = 1)
points(mean_samples_avg_formant[indices_aU], col = 'green', pch = 19, legend = 'aU')
points(mean_samples_avg_formant[indices_OY], col = 'blue', pch = 19, legend = 'OY')


### TODO: there is something wrong when plotting the data using specific indices
### I think that this is due to the fact that I still used dip$labels and not dip$"male labels", for example



### FROM NOW ON THERE ARE NO RELEVANT INFORMATION, I WAS JUST TRYING TO DO SOME ANALYSIS ON THE SECOND FORMANT
# #------------------------------------------------------------------------------------
# # creation of vectors with relevant information for F2
# 
# #fill a vector with the max_value of each row
# max_vect_F2 = c()
# for (i in 1:num_of_samples) {
#   max_i = max(data.matrix_F2[i,], na.rm = T)
#   max_vect_F2 = c(max_vect_F2, max_i)
# }
# 
# #fill a vector with the min_value of each row
# min_vect_F2 = c()
# for (i in 1:num_of_samples) {
#   min_i = min(data.matrix_F2[i,], na.rm = T)
#   min_vect_F2 = c(min_vect_F2, min_i)
# }
# 
# #fill a vector with the mean_value of each row
# avg_vect_F2 = c()
# for (i in 1:num_of_samples) {
#   avg_i = mean(data.matrix_F2[i,], na.rm = T)
#   avg_vect_F2 = c(avg_vect_F2, avg_i)
# }
# 
# # create vector with duration of each sample
# duration_vect_F2 = dip$end - dip$start
# 
# #----------------------------------------------------------------------------------
# # Basic Principal Component Analysis
# 
# df_F2 = data.frame(max_vect_F2, min_vect_F2, avg_vect_F2, duration_vect_F2)
# pca = princomp(df_F2, cor = T)
# 
# summary(pca)
# 
# # testing of new plotting library
# library(factoextra)
# 
# x11()
# fviz_eig(pca)
# 
# fviz_pca_var(pca,
#              col.var = "contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )