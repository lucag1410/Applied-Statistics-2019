library(emuR)

#graphics.off()

relevant_percentages = c(0.0, 0.25, 0.5, 0.75, 1.0)

temp_m = dip.spkr == '67'
m.l = dip.l[temp_m]
m.fdat = dip.fdat[temp_m,]

temp_f = dip.spkr == '68'
f.l = dip.l[temp_f]
f.fdat = dip.fdat[temp_f,]

temp_aI = dip.l == 'aI'
aI.spkr = dip.spkr[temp_aI]
aI.fdat = dip.fdat[temp_aI,]

temp_aU = dip.l == 'aU'
aU.spkr = dip.spkr[temp_aU]
aU.fdat = dip.fdat[temp_aU,]

temp_OY = dip.l == 'OY'
OY.spkr = dip.spkr[temp_OY]
OY.fdat = dip.fdat[temp_OY,]


### whole dataset
x11()
par(mar = c(4.1, 4.1, 4.1, 2.1), mfrow = c(3,2))
for (perc in relevant_percentages){
  dip.fdat.i = dcut(dip.fdat, perc, prop = T)
  eplot(dip.fdat.i[,1:2], dip.l, centroid = T, xlab ='F1 (Hz)', ylab='F2 (Hz)',
        ylim = c(0,3000), col = c('red','green','blue'),
        main=paste('Diphtongs ellipses at ', perc*100, ' %'))
}

### only males
x11()
par(mar = c(4.1, 4.1, 4.1, 2.1), mfrow = c(3,2))
for (perc in relevant_percentages){
  m.fdat.i = dcut(m.fdat, perc, prop = T)
  eplot(m.fdat.i[,1:2], m.l, centroid = T, xlab ='F1 (Hz)', ylab='F2 (Hz)',
        ylim = c(300,2500), col = c('red','green','blue'),
        main=paste('Diphtongs ellipses for males at ', perc*100, ' %'))
}

### only females
x11()
par(mar = c(4.1, 4.1, 4.1, 2.1), mfrow = c(3,2))
for (perc in relevant_percentages){
  f.fdat.i = dcut(f.fdat, perc, prop = T)
  eplot(f.fdat.i[,1:2], f.l, centroid = T, xlab ='F1 (Hz)', ylab='F2 (Hz)',
        ylim = c(0,3200), col = c('red','green','blue'),
        main=paste('Diphtongs ellipses for females at ', perc*100, ' %'))
}

### only aI
x11()
par(mar = c(4.1, 4.1, 4.1, 2.1), mfrow = c(3,2))
for (perc in relevant_percentages){
  aI.fdat.i = dcut(aI.fdat, perc, prop = T)
  eplot(aI.fdat.i[,1:2], ifelse(aI.spkr == '67', 'M','F'), centroid = T, xlab ='F1 (Hz)', ylab='F2 (Hz)',
        main=paste('Diphtongs ellipses for aI at ', perc*100, ' %'))
}

### only aU
x11()
par(mar = c(4.1, 4.1, 4.1, 2.1), mfrow = c(3,2))
for (perc in relevant_percentages){
  aU.fdat.i = dcut(aU.fdat, perc, prop = T)
  eplot(aU.fdat.i[,1:2], ifelse(aU.spkr == '67', 'M','F'), centroid = T, xlab ='F1 (Hz)', ylab='F2 (Hz)',
        main=paste('Diphtongs ellipses for aU at ', perc*100, ' %'))
}

### only OY
x11()
par(mar = c(4.1, 4.1, 4.1, 2.1), mfrow = c(3,2))
for (perc in relevant_percentages){
  OY.fdat.i = dcut(OY.fdat, perc, prop = T)
  eplot(OY.fdat.i[,1:2], ifelse(OY.spkr == '67', 'M','F'), centroid = T, xlab ='F1 (Hz)', ylab='F2 (Hz)',
        main=paste('Diphtongs ellipses for OY at ', perc*100, ' %'))
}

### T1, T2 for males
x11()
dplot(m.fdat[,2], m.l, col = c('red','green','blue'), normalise = T,
      main=paste('Overall diphtongs values for male'))