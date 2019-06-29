setwd("C:/Users/Cecilia Frapiccini/Desktop/APPLIED STATISTICS/PROJECT")
load("C:/Users/Cecilia Frapiccini/Desktop/APPLIED STATISTICS/PROJECT/mcshapiro.test.RData")

library(MASS)

attach(df_sample_normalized_T1T2T3T4)
diphtongs_names <- factor(diphtong, labels = c('aI', 'aU', 'OY'))

g= 3

i1 <- which(diphtongs_names=='aI')
i2 <- which(diphtongs_names=='aU')
i3 <- which(diphtongs_names=='OY')

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)

n <- n1 + n2 + n3

df_sample_normalized_T1T2T3T4_2 <- df_sample_normalized_T1T2T3T4[,4:47]
mcshapiro.test(df_sample_normalized_T1T2T3T4_2)

s1 <- cov(df_sample_normalized_T1T2T3T4_2[i1,])
s2 <- cov(df_sample_normalized_T1T2T3T4_2[i2,])
s3 <- cov(df_sample_normalized_T1T2T3T4_2[i3,])
Sp <-((n1-1)*s1+(n2-1)*s2+(n3-1)*s3)/(n-g)

#lda
T1T2T3T4.lda <- lda(df_sample_normalized_T1T2T3T4_2, diphtongs_names)
T1T2T3T4.lda

T1T2T3T4.Lda <- predict(T1T2T3T4.lda, df_sample_normalized_T1T2T3T4_2)

#compute the APER
T1T2T3T4.Lda$class

#creation of the confusion matrix 
table(class.true = diphtongs_names, class.assigned = T1T2T3T4.Lda$class)

errors.lda <- (T1T2T3T4.Lda$class != diphtongs_names)
errors.lda

APER.lda <- sum(errors.lda)/length(diphtongs_names)
APER.lda

#compute the AER
T1T2T3T4_CV.lda <- lda(df_sample_normalized_T1T2T3T4_2, diphtongs_names, CV = T)
T1T2T3T4_CV.lda$class

#creation of the confusion matrix
table(class.true =diphtongs_names, class.assigned = T1T2T3T4_CV.lda$class)
errorsCV.lda <- (diphtongs_names != T1T2T3T4_CV.lda$class)

AERCV.lda <- sum(errorsCV.lda)/length(diphtongs_names)
AERCV.lda


#QDA on T1 and T2
#we scale the values of the groups, otherwise the values are too low to perform QDA
#diphtongs_names<-scale(diphtongs_names)
T1T2T3T4.qda <- qda(df_sample_normalized_T1T2T3T4_2[,23:44], diphtongs_names)
T1T2T3T4.qda

T1T2T3T4.Qda <- predict(T1T2T3T4.qda, df_sample_normalized_T1T2T3T4_2[,23:44])
T1T2T3T4.Qda

#compute the APER
T1T2T3T4.Qda$class

#creation of the confusion matrix
table(class.true = diphtongs_names, class.assigned = T1T2T3T4.Qda$class)

errors.qda <- (diphtongs_names != T1T2T3T4.Qda$class)
APER.qda <- sum(errors.qda)/length(diphtongs_names)
APER.qda

#compute the AER - NON VA BENE
T1T2T3T4_CV.qda <- qda(df_sample_normalized_T1T2T3T4_2, diphtongs_names = T)
T1T2T3T4_CV.qda

#creation of the confusion matrix
table(class.true = diphtongs_names, class.assigned = T1T2T3T4_CV.qda$class)
errorsCV.qda <- (diphtongs_names != T1T2T3T4_CV.qda$class)

AER.qda <- sum(errorsCV.qda)/length(diphtongs_names)
AER.qda

