library(emuR)

#summary normalization

df_complete_noZeros <- read.table('df_complete_noZeros.txt')
df_complete_normalized <- read.table('df_complete_normalized.txt')
df_sample <- read.table('df_sample.txt')
df_sample_normalized_T1 <- read.table('df_sample_normalized_T1.txt')
df_sample_normalized_T2 <- read.table('df_sample_normalized_T2.txt')
df_sample_normalized_T3 <- read.table('df_sample_normalized_T3.txt')
df_sample_normalized_T4 <- read.table('df_sample_normalized_T4.txt')

num_of_samples = 186

indices_faI = c()
indices_faU = c()
indices_fOY = c()
indices_maI = c()
indices_maU = c()
indices_mOY = c()
for (i in 1:num_of_samples){
  if ( df_sample_normalized_T1$speaker[i] == 'M')
    
    if (df_sample_normalized_T1$diphtong[i] == 'aI') indices_maI = c(indices_maI, i)
    else if (df_sample_normalized_T1$diphtong[i] == 'aU') indices_maU = c(indices_maU, i)
    else if (df_sample_normalized_T1$diphtong[i] == 'OY') indices_mOY = c(indices_mOY, i)

}

for ( i in 1:num_of_samples){
  
  if(df_sample_normalized_T1$speaker[i] == 'F')

    if (df_sample_normalized_T1$diphtong[i] == 'aI') indices_faI = c(indices_faI, i)
    else if (df_sample_normalized_T1$diphtong[i] == 'aU') indices_faU = c(indices_faU, i)
    else if (df_sample_normalized_T1$diphtong[i] == 'OY') indices_fOY = c(indices_fOY, i)
    
}


df_normalized_faI_T1 = df_sample_normalized_T1[indices_faI,]
df_normalized_faI_T2 = df_sample_normalized_T2[indices_faI,]
df_normalized_faU_T1 = df_sample_normalized_T1[indices_faU,]
df_normalized_faU_T2 = df_sample_normalized_T2[indices_faU,]
df_normalized_fOY_T1 = df_sample_normalized_T1[indices_fOY,]
df_normalized_fOY_T2 = df_sample_normalized_T2[indices_fOY,]

df_normalized_maI_T1 = df_sample_normalized_T1[indices_maI,]
df_normalized_maI_T2 = df_sample_normalized_T2[indices_maI,]
df_normalized_maU_T1 = df_sample_normalized_T1[indices_maU,]
df_normalized_maU_T2 = df_sample_normalized_T2[indices_maU,]
df_normalized_mOY_T1 = df_sample_normalized_T1[indices_mOY,]
df_normalized_mOY_T2 = df_sample_normalized_T2[indices_mOY,]


s <- shapiro.test(df_normalized_maI_T2$T2_50.)
s$

df_normalized_faI_T1 = df_sample_normalized_T1[indices_faI, 4:14]
df_normalized_faI_T2 = df_sample_normalized_T2[indices_faI, 4:14]
df_normalized_faI_T3 = df_sample_normalized_T3[indices_faI, 4:14]
df_normalized_faI_T4 = df_sample_normalized_T4[indices_faI, 4:14]
df_normalized_faU_T1 = df_sample_normalized_T1[indices_faU, 4:14]
df_normalized_faU_T2 = df_sample_normalized_T2[indices_faU, 4:14]
df_normalized_faU_T3 = df_sample_normalized_T3[indices_faU, 4:14]
df_normalized_faU_T4 = df_sample_normalized_T4[indices_faU, 4:14]
df_normalized_fOY_T1 = df_sample_normalized_T1[indices_fOY, 4:14]
df_normalized_fOY_T2 = df_sample_normalized_T2[indices_fOY, 4:14]
df_normalized_fOY_T3 = df_sample_normalized_T3[indices_fOY, 4:14]
df_normalized_fOY_T4 = df_sample_normalized_T4[indices_fOY, 4:14]

df_normalized_maI_T1 = df_sample_normalized_T1[indices_maI, 4:14]
df_normalized_maI_T2 = df_sample_normalized_T2[indices_maI, 4:14]
df_normalized_maI_T3 = df_sample_normalized_T3[indices_maI, 4:14]
df_normalized_maI_T4 = df_sample_normalized_T4[indices_maI, 4:14]
df_normalized_maU_T1 = df_sample_normalized_T1[indices_maU, 4:14]
df_normalized_maU_T2 = df_sample_normalized_T2[indices_maU, 4:14]
df_normalized_maU_T3 = df_sample_normalized_T3[indices_maU, 4:14]
df_normalized_maU_T4 = df_sample_normalized_T4[indices_maU, 4:14]
df_normalized_mOY_T1 = df_sample_normalized_T1[indices_mOY, 4:14]
df_normalized_mOY_T2 = df_sample_normalized_T2[indices_mOY, 4:14]
df_normalized_mOY_T3 = df_sample_normalized_T3[indices_mOY, 4:14]
df_normalized_mOY_T4 = df_sample_normalized_T4[indices_mOY, 4:14]


mcshapiro.test(df_normalized_faI_T1)
mcshapiro.test(df_normalized_faI_T2)
mcshapiro.test(df_normalized_faU_T1)
mcshapiro.test(df_normalized_faU_T2)
mcshapiro.test(df_normalized_fOY_T1)
mcshapiro.test(df_normalized_fOY_T2)

mcshapiro.test(df_normalized_maI_T1)
mcshapiro.test(df_normalized_maI_T2)
mcshapiro.test(df_normalized_maU_T1)
mcshapiro.test(df_normalized_maU_T2)
mcshapiro.test(df_normalized_mOY_T1)
mcshapiro.test(df_normalized_mOY_T2)

processing <- function(data, dist){
  mean_data <- colMeans(data)
  cov_data <- cov(data)
  d_mal <- matrix(mahalanobis(data, mean_data, cov_data))
  data_new <- data[which(d_mal < dist),]
  m1 <- mcshapiro.test(data)
  m2 <- mcshapiro.test(data_new)
  dim1 <- dim(data)[1]
  dim2 <- dim(data_new)[1]
  
  results <- list( data_new = data_new, mac1 = m1, mac_new = m2, dim1 = dim1, dim_new = dim2)
  
  return(results)
  
}


pmai_T1 <- processing(df_normalized_maI_T1, 14)
pmai_T2 <- processing(df_normalized_maI_T2, 21)
pmai_T3 <- processing(df_normalized_maI_T3, 10)
pmai_T4 <- processing(df_normalized_maI_T4, 7.2)
pmau_T1 <- processing(df_normalized_maU_T1, 17.4)
pmau_T2 <- processing(df_normalized_maU_T2, 17.4)
pmau_T3 <- processing(df_normalized_maU_T3, 10.4)
pmau_T4 <- processing(df_normalized_maU_T4, 18)
pmoy_T1 <- processing(df_normalized_mOY_T1, 14)#no need
pmoy_T2 <- processing(df_normalized_mOY_T2, 14)#no need
pmoy_T3 <- processing(df_normalized_mOY_T3, 14)#no need
pmoy_T4 <- processing(df_normalized_mOY_T4, 14)#no need

pfai_T1 <- processing(df_normalized_faI_T1, 20.5)
pfai_T2 <- processing(df_normalized_faI_T2, 17)
pfai_T3 <- processing(df_normalized_faI_T3, 14)
pfai_T4 <- processing(df_normalized_faI_T4, 16)
pfau_T1 <- processing(df_normalized_faU_T1, 17.8)
pfau_T2 <- processing(df_normalized_faU_T2, 17.4)#no need
pfau_T3 <- processing(df_normalized_faU_T3, 17)
pfau_T4 <- processing(df_normalized_faU_T4, 17)
pfoy_T1 <- processing(df_normalized_fOY_T1, 14)#no need
pfoy_T2 <- processing(df_normalized_fOY_T2, 14)#no need
pfoy_T3 <- processing(df_normalized_fOY_T3, 14)#no need
pfoy_T4 <- processing(df_normalized_fOY_T4, 14)#no need

M_T1 <- rbind(pmai_T1$data_new, pmau_T1$data_new, pmoy_T1$data_new)
M_T2 <- rbind(pmai_T2$data_new, pmau_T2$data_new, pmoy_T2$data_new)
M_T3 <- rbind(pmai_T3$data_new, pmau_T3$data_new, pmoy_T3$data_new)
M_T4 <- rbind(pmai_T4$data_new, pmau_T4$data_new, pmoy_T4$data_new)

F_T1 <- rbind(pfai_T1$data_new, pfau_T1$data_new, pfoy_T1$data_new)
F_T2 <- rbind(pfai_T2$data_new, pfau_T2$data_new, pfoy_T2$data_new)
F_T3 <- rbind(pfai_T3$data_new, pfau_T3$data_new, pfoy_T3$data_new)
F_T4 <- rbind(pfai_T4$data_new, pfau_T4$data_new, pfoy_T4$data_new)

dip_m_T1 <- factor(c(rep("aI", pmai_T1$dim_new), rep("aU", pmau_T1$dim_new), rep("OY", pmoy_T1$dim_new)))
dip_m_T4 <- factor(c(rep("aI", pmai_T4$dim_new), rep("aU", pmau_T4$dim_new), rep("OY", pmoy_T4$dim_new)))

maI.mean_T1 <- colMeans(df_normalized_maI_T1)
maI.cov_T1 <- cov(df_normalized_maI_T1)
d_maI_T1 <- matrix(mahalanobis(df_normalized_maI_T1, maI.mean_T1, maI.cov_T1))
df_normalized_maI_T1_1 <- df_normalized_maI_T1[which(d_maI_T1 < 14),]
mcshapiro.test((df_normalized_maI_T1))
mcshapiro.test(df_normalized_maI_T1_1)

dim(df_normalized_maI_T1_1)

fOY.mean <- colMeans(df_normalized_fOY_T2)
fOY.cov <- cov(df_normalized_fOY_T2)
d_fOY_T2 <- matrix(mahalanobis(df_normalized_fOY_T2, fOY.mean, fOY.cov))
df_normalized_fOY_T2.2 <- df_normalized_fOY_T2[which(d_fOY_T2 < 7.5),]
mcshapiro.test(df_normalized_fOY_T2.2)



#ANOVA E MANOVA per lo studio della significatività delle formanti per determinare sesso/dittongo (vogliamo che resti solo la seconda)
#Per ogni sample per ogni sesso calcoliamo la media (prima e seconda formante), costruiamo un vettore media (basandoci sulla PCA per la significatività) e costruiamo un test per:
#1. Costruire un ellissoide di confidenza 95% per eventuali nuove medie
#2. Studiare le differenze tra le medie di maschi e femmine
#Come ricaviamo la significatività delle prime due formanti rispetto alle ultime due?

#si prende una formante, si studia con un test (MANOVA) la differenza tra le medie dei tre dittonghi in ognuno degli 11 campionamenti (la funzione calcolerà le medie per ogni sample) e dal vettore pval
#avremo sia i passi campionari più significativi sia l'effettiva consistenza della formante. Dovremmo così riuscire ad eliminare non solo le formanti significative ma pure i passi campionari inutili


library(devtools)

install_github("alessiapini/fdatest")

library(fdatest)

##Functional ANOVA

##Faccio prima a creare di nuovo le due matrici per maschi e femmine
df_sample_normalized_T1_M = df_sample_normalized_T1[1:93, 4:14]
df_sample_normalized_T1_F = df_sample_normalized_T1[94:186, 4:14]

df_sample_normalized_T2_M = df_sample_normalized_T2[1:93, 4:14]
df_sample_normalized_T2_F = df_sample_normalized_T2[94:186, 4:14]

df_sample_normalized_T3_M = df_sample_normalized_T3[1:93, 4:14]
df_sample_normalized_T3_F = df_sample_normalized_T3[94:186, 4:14]

df_sample_normalized_T4_M = df_sample_normalized_T4[1:93, 4:14]
df_sample_normalized_T4_F = df_sample_normalized_T4[94:186, 4:14]

dipthong_m <- factor(df_sample_normalized_T1[1:93,2])
dipthong_f <- factor(df_sample_normalized_T1[94:186,2])

dim(as.matrix(df_sample_normalized_T1_M))[2]

##verifico dopo le ipotesi perché è un lavoraccio


##modello MANOVA (che in realtà sono 11 ANOVA)
ITP.result_MF1 <- ITPaovbspline( as.matrix(df_sample_normalized_T1_M) ~ dipthong_m, B=1000, nknots=11, order=3)
summary(ITP.result_MF1)

ITP.result_MF1$pval.matrix.F

ITP.result_MF4 <- ITPaovbspline( as.matrix(df_sample_normalized_T4_M) ~ dipthong_m, B=1000, nknots=11, order=3)
summary(ITP.result_MF4)

ITP.result_MF4$pval.F

ITP.result_MF_T1 <- ITPaovbspline( as.matrix(M_T1) ~ dip_m_T1, B=1000, nknots=10, order=3)
summary(ITP.result_MF_T1)

ITP.result_MF_T1$pval.F

ITP.result_MF_T4 <- ITPaovbspline( as.matrix(M_T4) ~ dip_m_T4, B=1000, nknots=10, order=3)
summary(ITP.result_MF_T4)

ITP.result_MF_T4$pval.F


##extra

pp <- processing(df_sample_normalized_T1[1:93,4:14], 7.5)

##bisogna capire perché la 4 formante da pvalue a favore di H1. 
##Bisogna chiedere quali sono i pvalue da considerare
##Bisogna chiedere come confermare le ipotesi
##Prova a costruire solo con due dittonghi
##Forse, sebbene i dati siano schiacciati in F3 e F4, le medie dei tre dittonghi sono troppo diverse tra loro per poter avere risultati significativi (grafico a bande distinte)
##ITP2bspline 

##test funzionale sulle medie dei dittonghi a coppie


test1 <- ITP2bspline(pmai_T2$data_new, pmau_T2$data_new, mu = 0,
            order = 2, nknots = 10, B = 10000, paired = FALSE)
test1$adjusted.pval

test11 <- ITP2bspline(pmai_T2$data_new, pmoy_T2$data_new, mu = 0,
                      order = 2, nknots = 10, B = 10000, paired = FALSE)
test11$adjusted.pval

test2 <- ITP2bspline(pmai_T4$data_new, pmau_T4$data_new, mu = 0,
                     order = 2, nknots = 10, B = 10000, paired = FALSE)
test2$adjusted.pval

test22 <- ITP2bspline(pmai_T4$data_new, pmau_T4$data_new, mu = 0,
                      order = 2, nknots = 10, B = 10000, paired = FALSE)
test22$adjusted.pval
##se ti avanza tempo prova qualche test tra i sessi

##fai una writetable e carica le tabelle normalizzate


##provo box-cox sui dati campionati

df_normalized_maI_T1

Box_Cox <- function (data, nrows){

  lambda_bc = powerTransform(data)
  BC_mat = matrix(, nrow = nrows, ncol = 11)
  pval = c()
  
  for (i in 1:11){
    
    BC_mat[,i] <- bcPower(data[,i], lambda_bc$lambda[i])
    
    pp <- shapiro.test(BC_mat[,i])
    
    pval[i] <- pp$p.value
    
  }
  
  results <- list (lambda = lambda_bc, new_data = BC_mat, shapiro_pval = pval)
  
  return(results)
  
}


mai_T1 <- Box_Cox(df_normalized_maI_T1, 59)
mai_T2 <- Box_Cox(df_normalized_maI_T2, 59)
mai_T3 <- Box_Cox(df_normalized_maI_T3, 59)
mai_T4 <- Box_Cox(df_normalized_maI_T4, 59)

fai_T1 <- Box_Cox(df_normalized_faI_T1, 59)
fai_T2 <- Box_Cox(df_normalized_faI_T2, 59)##da rifare
fai_T3 <- Box_Cox(df_normalized_faI_T3, 59)##da rifare
fai_T4 <- Box_Cox(df_normalized_faI_T4, 59)

mau_T1 <- Box_Cox(df_normalized_maU_T1, 22)
mau_T2 <- Box_Cox(df_normalized_maU_T2, 22)
mau_T3 <- Box_Cox(df_normalized_maU_T3, 22)##da rifare
mau_T4 <- Box_Cox(df_normalized_maU_T4, 22)##da rifare

fau_T1 <- Box_Cox(df_normalized_faU_T1, 22)
fau_T2 <- Box_Cox(df_normalized_faU_T2, 22)
fau_T3 <- Box_Cox(df_normalized_faU_T3, 22)##da rifare
fau_T4 <- Box_Cox(df_normalized_faU_T4, 22)##da rifare

##fai_T2
l_fai_T2 = c()
BC_fai_T2 = matrix( , 59, 11)
pval = c()

for (i in 1:11){
  
  l <- powerTransform(df_normalized_faI_T2[,i])
  l_fai_T2[i] <- l$lambda
  
  BC_fai_T2[ ,i] <- bcPower(df_normalized_faI_T2[,i], l_fai_T2[i])
  
  pp <- shapiro.test(BC_fai_T2[,i])
  pval[i] <- pp$p.value
  
}

fai_T2 <- list(lambda = l_fai_T2, new_data = BC_fai_T2, shapiro_pval = pval)

##fai_T3
l_fai_T3 = c()
BC_fai_T3 = matrix( , 59, 11)
pval = c()

for (i in 1:11){
  if ( i == 7){
    l_fai_T3[i] = l_fai_T3[i-1]
    BC_fai_T3[ ,i] <- bcPower(df_normalized_faI_T3[,i], l_fai_T3[i])
    
    pp <- shapiro.test(BC_fai_T3[,i])
    pval[i] <- pp$p.value
    
  }
  else{
  
    l <- powerTransform(df_normalized_faI_T3[,i])
    l_fai_T3[i] <- l$lambda
    
    BC_fai_T3[ ,i] <- bcPower(df_normalized_faI_T3[,i], l_fai_T3[i])
    
    pp <- shapiro.test(BC_fai_T3[,i])
    pval[i] <- pp$p.value
  }
}

fai_T3 <- list(lambda = l_fai_T3, new_data = BC_fai_T3, shapiro_pval = pval)

##mau_T3

l_mau_T3 = c()
BC_mau_T3 = matrix( , 22, 11)
pval = c()

for (i in 1:11){
  
  if ( i == 2 || i == 4){
    l_mau_T3[i] = l_mau_T3[i-1]
    
  }
  
  else{
    l <- powerTransform(df_normalized_maU_T3[,i])
    l_mau_T3[i] <- l$lambda
    }
    
  BC_mau_T3[ ,i] <- bcPower(df_normalized_maU_T3[,i], l_mau_T3[i])
  
  pp <- shapiro.test(BC_mau_T3[,i])
  pval[i] <- pp$p.value
  
}

mau_T3 <- list(lambda = l_mau_T3, new_data = BC_mau_T3, shapiro_pval = pval)

##mau_T4
l_mau_T4 = c()
BC_mau_T4 = matrix( , 22, 11)
pval = c()

for (i in 1:11){
  
  l <- powerTransform(df_normalized_maU_T4[,i])
  l_mau_T4[i] <- l$lambda
  
  BC_mau_T4[ ,i] <- bcPower(df_normalized_maU_T4[,i], l_mau_T4[i])
  
  pp <- shapiro.test(BC_mau_T4[,i])
  pval[i] <- pp$p.value
  
}

mau_T4 <- list(lambda = l_mau_T4, new_data = BC_mau_T4, shapiro_pval = pval)

##fau_T3 1,5,7


l_fau_T3 = c()
BC_fau_T3 = matrix( , 22, 11)
pval = c()

for (i in 1:11){
  
  if ( i == 5 || i == 7){
    l_fau_T3[i] = l_fau_T3[i-1]
    
  }
  
  else if (i ==1 ){
    l <- powerTransform(df_normalized_faU_T3[,2])
    l_fau_T3 <- l$lambda
  }
  
  else{
    l <- powerTransform(df_normalized_faU_T3[,i])
    l_fau_T3[i] <- l$lambda
  }
  
  BC_fau_T3[ ,i] <- bcPower(df_normalized_faU_T3[,i], l_fau_T3[i])
  
  pp <- shapiro.test(BC_fau_T3[,i])
  pval[i] <- pp$p.value
  
}

fau_T3 <- list(lambda = l_fau_T3, new_data = BC_fau_T3, shapiro_pval = pval)

##fau_T4 1,2,8

l_fau_T4 = c()
BC_fau_T4 = matrix( , 22, 11)
pval = c()

for (i in 1:11){
  
  if (i == 1){
    l <- powerTransform(df_normalized_faU_T4[,3])
    l_fau_T4[i] <- l$lambda
  }
  
  else if (i == 2){
    l <- powerTransform(df_normalized_faU_T4[,3])
    l_fau_T4[i] <- l$lambda
    
  }
  
  else if ( i == 8){
    l_fau_T4[i] = l_fau_T4[i-1]
    
  }
  
  else{
    l <- powerTransform(df_normalized_faU_T4[,i])
    l_fau_T4[i] <- l$lambda
  }
  
  BC_fau_T4[ ,i] <- bcPower(df_normalized_faU_T4[,i], l_fau_T4[i])
  
  pp <- shapiro.test(BC_fau_T4[,i])
  pval[i] <- pp$p.value
  
}

fau_T4 <- list(lambda = l_fau_T4, new_data = BC_fau_T4, shapiro_pval = pval)

##

mm_T1 <- df_sample_normalized_T1[1:93, 4:14]
mm_T2 <- df_sample_normalized_T2[1:93, 4:14]
mm_T3 <- df_sample_normalized_T3[1:93, 4:14]
mm_T4 <- df_sample_normalized_T4[1:93, 4:14]

Box_Cox(mm_T1, 93)
Box_Cox(mm_T2, 93)
Box_Cox(mm_T3, 93)
Box_Cox(mm_T4, 93)

ff_T1 <- df_sample_normalized_T1[94:186, 4:14]
ff_T2 <- df_sample_normalized_T2[94:186, 4:14]
ff_T3 <- df_sample_normalized_T3[94:186, 4:14]
ff_T4 <- df_sample_normalized_T4[94:186, 4:14]

Box_Cox(ff_T1, 93)
Box_Cox(ff_T2, 93)
Box_Cox(ff_T3, 93)
Box_Cox(ff_T4, 93)

test <- rbind(fau_T3$new_data, mau_T3$new_data)

pval = c()
for (i in 1:11){
  pp <- shapiro.test(test[,i])
  pval[i] <- pp$p.value
}

##writetable
write.table(mai_T1$new_data, file = "df_gaussian_normalized_maI_T1.txt")
write.table(mai_T2$new_data, file = "df_gaussian_normalized_maI_T2.txt")
write.table(mai_T3$new_data, file = "df_gaussian_normalized_maI_T3.txt")
write.table(mai_T4$new_data, file = "df_gaussian_normalized_maI_T4.txt")

write.table(fai_T1$new_data, file = "df_gaussian_normalized_faI_T1.txt")
write.table(fai_T2$new_data, file = "df_gaussian_normalized_faI_T2.txt")
write.table(fai_T3$new_data, file = "df_gaussian_normalized_faI_T3.txt")
write.table(fai_T4$new_data, file = "df_gaussian_normalized_faI_T4.txt")

write.table(mau_T1$new_data, file = "df_gaussian_normalized_maU_T1.txt")
write.table(mau_T2$new_data, file = "df_gaussian_normalized_maU_T2.txt")
write.table(mau_T3$new_data, file = "df_gaussian_normalized_maU_T3.txt")
write.table(mau_T4$new_data, file = "df_gaussian_normalized_maU_T4.txt")

write.table(fau_T1$new_data, file = "df_gaussian_normalized_faU_T1.txt")
write.table(fau_T2$new_data, file = "df_gaussian_normalized_faU_T2.txt")
write.table(fau_T3$new_data, file = "df_gaussian_normalized_faU_T3.txt")
write.table(fau_T4$new_data, file = "df_gaussian_normalized_faU_T4.txt")


write.table(df_normalized_fOY_T1, file = "df_sample_normalized_fOY_T1.txt")
write.table(df_normalized_fOY_T2, file = "df_sample_normalized_fOY_T2.txt")
write.table(df_normalized_fOY_T3, file = "df_sample_normalized_fOY_T3.txt")
write.table(df_normalized_fOY_T4, file = "df_sample_normalized_fOY_T4.txt")

write.table(df_normalized_mOY_T1, file = "df_sample_normalized_mOY_T1.txt")
write.table(df_normalized_mOY_T2, file = "df_sample_normalized_mOY_T2.txt")
write.table(df_normalized_mOY_T3, file = "df_sample_normalized_mOY_T3.txt")
write.table(df_normalized_mOY_T4, file = "df_sample_normalized_mOY_T4.txt")

