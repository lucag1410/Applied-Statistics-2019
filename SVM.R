rm(list = ls())
library(dplyr)
all_dat <- read.table("C:/Users/giaco/Documents/Università/AppliedStatistics/Project/df_sample_normalized_T1T2T3T4.txt")
header <- all_dat[,(1:3)]
dat_m <- all_dat[(1:93),]
dat_f <- all_dat[(94:186),]

# GENERATE THE TEST SET

aI_m <- all_dat %>% filter(diphthong == "aI" & speaker == "M")
aU_m <- all_dat %>% filter(diphthong == "aU" & speaker == "M")
OY_m <- all_dat %>% filter(diphthong == "OY" & speaker == "M")

test_aI <- sample_n(aI_m, 10)
test_aU <- sample_n(aU_m, 4)
test_OY <- sample_n(OY_m, 2)

#test_aI <- test_aI[order(test_aI$sample),]
#test_aU <- test_aU[order(test_aU$sample),]
#test_OY <- test_OY[order(test_OY$sample),]

train_aI <- aI_m %>% filter(!(sample %in% test_aI$sample))
train_aU <- aU_m %>% filter(!(sample %in% test_aU$sample))
train_OY <- OY_m %>% filter(!(sample %in% test_OY$sample))

library(e1071)

# FIRST SVM -------------------------------

aI_vs_aU_m <- dat_m %>% filter(sample %in% train_aI$sample | sample %in% train_aU$sample)
aI_vs_aU_m <- aI_vs_aU_m[,-3]
aI_vs_aU_m <- aI_vs_aU_m[,-1]
#lbl1 <- c()
#for(i in (1:length(aI_vs_aU_m[,1]))) {
#  if(aI_vs_aU_m[i,1] == "aI") {
#    lbl1 <- c(lbl1, 1)
#  } else {
#    lbl1 <- c(lbl1, -1)
#  }
#}
#aI_vs_aU_m <- aI_vs_aU_m[,-1]
#aI_vs_aU_m <- data.frame(lbl1, aI_vs_aU_m)

attach(aI_vs_aU_m)
tune1 <- tune(svm, diphthong~., data = aI_vs_aU_m, kernel = "linear",
              ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tune1)
best1 <- tune1$best.model
summary(best1)

# SECOND SVM ------------------------------

aI_vs_OY_m <- dat_m %>% filter(sample %in% train_aI$sample | sample %in% train_OY$sample)
aI_vs_OY_m <- aI_vs_OY_m[,-3]
aI_vs_OY_m <- aI_vs_OY_m[,-1]
#lbl2 <- c()
#for(i in (1:length(aI_vs_OY_m[,1]))) {
#  if(aI_vs_OY_m[i,1] == "aI") {
#    lbl2 <- c(lbl2, 1)
#  } else {
#    lbl2 <- c(lbl2, -1)
#  }
#}
#aI_vs_OY_m <- aI_vs_OY_m[,-1]
#aI_vs_OY_m <- data.frame(lbl2, aI_vs_OY_m)

attach(aI_vs_OY_m)
tune2 <- tune(svm, diphthong~., data = aI_vs_OY_m, kernel = "linear",
              ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tune2)
best2 <- tune2$best.model
summary(best2)

# THIRD SVM -------------------------------

aU_vs_OY_m <- dat_m %>% filter(sample %in% train_aU$sample | sample %in% train_OY$sample)
aU_vs_OY_m <- aU_vs_OY_m[,-3]
aU_vs_OY_m <- aU_vs_OY_m[,-1]
#lbl3 <- c()
#for(i in (1:length(aU_vs_OY_m[,1]))) {
#  if(aU_vs_OY_m[i,1] == "aU") {
#    lbl3 <- c(lbl3, 1)
#  } else {
#    lbl3 <- c(lbl3, -1)
#  }
#}
#aU_vs_OY_m <- aU_vs_OY_m[,-1]
#aU_vs_OY_m <- data.frame(lbl3, aU_vs_OY_m)

attach(aU_vs_OY_m)
tune3 <- tune(svm, diphthong~., data = aU_vs_OY_m, kernel = "linear",
              ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
summary(tune3)
best3 <- tune3$best.model
summary(best3)

# PREDICTION ON TEST SET aI

res_aI <- c()
for(i in 1:length(test_aI[,1])) {
  count_aI <- 0
  count_aU <- 0
  count_OY <- 0
  pred1 <- predict(best1, test_aI[i, -(1:3)])
  if(pred1 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred1 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred2 <- predict(best2, test_aI[i, -(1:3)])
  if(pred2 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred2 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred3 <- predict(best3, test_aI[i, -(1:3)])
  if(pred3 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred3 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  if(count_aI > count_aU & count_aI > count_OY) {
    res_aI <- c(res_aI, "aI")
  } else if(count_aU > count_aI & count_aU > count_OY) {
    res_aI <- c(res_aI, "aU")
  } else if(count_OY > count_aI & count_OY > count_aU) {
    res_aI <- c(res_aI, "OY")
  } else {
    res_aI <- c(res_aI, NA)
  }
}

# PREDICTION ON TEST SET aU

res_aU <- c()
for(i in 1:length(test_aU[,1])) {
  count_aI <- 0
  count_aU <- 0
  count_OY <- 0
  
  pred1 <- predict(best1, test_aU[i, -(1:3)])
  if(pred1 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred1 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred2 <- predict(best2, test_aU[i, -(1:3)])
  if(pred2 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred2 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred3 <- predict(best3, test_aU[i, -(1:3)])
  if(pred3 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred3 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  if(count_aI > count_aU & count_aI > count_OY) {
    res_aU <- c(res_aU, "aI")
  } else if(count_aU > count_aI & count_aU > count_OY) {
    res_aU <- c(res_aU, "aU")
  } else if(count_OY > count_aI & count_OY > count_aU) {
    res_aU <- c(res_aU, "OY")
  } else {
    res_aU <- c(res_aU, NA)
  }
}

# PREDICTION ON TEST SET OY

res_OY <- c()
for(i in 1:length(test_OY[,1])) {
  count_aI <- 0
  count_aU <- 0
  count_OY <- 0
  
  pred1 <- predict(best1, test_OY[i, -(1:3)])
  if(pred1 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred1 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred2 <- predict(best2, test_OY[i, -(1:3)])
  if(pred2 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred2 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred3 <- predict(best3, test_OY[i, -(1:3)])
  if(pred3 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred3 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  if(count_aI > count_aU & count_aI > count_OY) {
    res_OY <- c(res_OY, "aI")
  } else if(count_aU > count_aI & count_aU > count_OY) {
    res_OY <- c(res_OY, "aU")
  } else if(count_OY > count_aI & count_OY > count_aU) {
    res_OY <- c(res_OY, "OY")
  } else {
    res_OY <- c(res_OY, NA)
  }
}