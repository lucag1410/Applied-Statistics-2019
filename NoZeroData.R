library(emuR)

layout(matrix(1:4,nrow=2,byrow=T))
par(mar=c(3,2,3,1),mgp=c(2,0.5,0))
dplot(dip.fdat[,1],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "1st Formant [Hz]", col=rainbow(3),lwd=1.1,main='First formant',lty=factor(dip.spkr))

dplot(dip.fdat[,2],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "2nd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Second formant',lty=factor(dip.spkr))

dplot(dip.fdat[,3],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "3rd Formant [Hz]", col=rainbow(3),lwd=1.1,main='Third formant',lty=factor(dip.spkr))

dplot(dip.fdat[,4],
      dip$labels,
      normalise = TRUE,
      xlab = "Normalized Time [%]",
      ylab = "4th Formant [Hz]", col=rainbow(3),lwd=1.1,main='Fourth formant',lty=factor(dip.spkr))


summary(dip.fdat)
dim(dip.fdat$data)
dim(dip)
names(dip.fdat)

dim(dip.fdat$ftime)


# scelta della formante
formante = 4
tempi = as.numeric(rownames(dip.fdat$data))
n = 186
dip.fdat$index #indici inizio e fine delle curve
maxlength = max(apply(dip.fdat$index,1,diff)) + 1
data.matrix = matrix(nrow=n,ncol=maxlength)
time.matrix = matrix(nrow=n,ncol=maxlength)
normalized.time.matrix = matrix(nrow=n,ncol=maxlength)

for(ii in 1:n){
  indice.ii = dip.fdat$index[ii,]
  time.std = tempi[indice.ii[1]:indice.ii[2]] - tempi[indice.ii[1]]
  time.matrix[ii,1:length(time.std)] = time.std
  normalized.time.matrix[ii,1:length(time.std)] = time.std/max(time.std)
  data.matrix[ii,1:length(time.std)] = dip.fdat$data[indice.ii[1]:indice.ii[2],formante]
}

matplot(t(time.matrix),t(data.matrix),type='l',col=factor(dip$labels),ylab='Data, frequency 2',xlab='Time',lty=1)
matplot(t(normalized.time.matrix),t(data.matrix),type='l',col=factor(dip$labels),ylab='Data, frequency 2',xlab='Normalized time',lty=1)


dimnames(dip.fdat)

colMeans(dip.fdat)







##########################################################################################################
##########################################################################################################


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




#cose inutili
#X = data.aI_female$data[,3]
#Y = data.aI_female$data[,4]
#new_aI = data.aI_female
#lin = lm( data.aI_female$data[,4] ~ data.aI_female$data[,0])
#summary(lin)
#fine cose inutili


#esempi di indicatori della posizione degli zeri nelle formanti 

nulls_t1 = c();
for (i in 1:length(data.aI_female$data[,1])){
  if (is.element(0, data.aI_female$data[i,1])){
    nulls_t1 = c(nulls_t1, i)
  }
}


nulls_t2 = c();
for (i in 1:length(data.aI_female$data[,2])){
  if (is.element(0, data.aI_female$data[i,2])){
    nulls_t2 = c(nulls_t2, i)
  }
}


nulls_t3 = c();
for (i in 1:length(data.aI_female$data[,3])){
  if (is.element(0, data.aI_female$data[i,3])){
    nulls_t3 = c(nulls_t3, i)
  }
}

nulls_t4 = c();
for (i in 1:length(data.aI_female$data[,4])){
  if (is.element(0, data.aI_female$data[i,4])){
    nulls_t4 = c(nulls_t4, i)
  }
}


#ciclo inutile
#for (formant in 1:num_of_formants){
#  new_aI$data[,formant][new_aI$data[,formant]==0]   = mean(new_aI$data[,formant])
#}

#costruisco i dataset ausiliari da usare per non modificare gli originali

inter_female_aI = data.aI_female
inter_female_aU = data.aU_female
inter_female_OY = data.OY_female
inter_male_aI = data.aI_male
inter_male_aU = data.aU_male
inter_male_Oy = data.OY_male

#interpolazione utilizzando la media della formante per ogni sesso e per ogni dittongo

for (formant in 1:num_of_formants){
  
  inter_female_aI$data[,formant][inter_female_aI$data[,formant]==0]   = mean(inter_female_aI$data[,formant])
  inter_female_aU$data[,formant][inter_female_aU$data[,formant]==0]   = mean(inter_female_aU$data[,formant])
  inter_female_OY$data[,formant][inter_female_OY$data[,formant]==0]   = mean(inter_female_OY$data[,formant])
  
  inter_male_aI$data[,formant][inter_male_aI$data[,formant]==0]   = mean(inter_male_aI$data[,formant])
  inter_male_aU$data[,formant][inter_male_aU$data[,formant]==0]   = mean(inter_male_aU$data[,formant])
  inter_male_Oy$data[,formant][inter_male_Oy$data[,formant]==0]   = mean(inter_male_Oy$data[,formant])
}
