rm(list = ls())
library(dplyr)
library(e1071)
all_dat <- read.table("C:/Users/giaco/Documents/Università/AppliedStatistics/Project/df_sample_normalized_T1T2T3T4.txt")
header <- all_dat[,(1:3)]

### SVM with CV
right <- 0
wrong <- 0
n <- 0
miss <- c()

for (i in 1:nrow(all_dat)) {
  
  test <- all_dat[i,]
  training <- all_dat[-i,]
  
  # FIRST SVM -------------------------------
  
  aI_vs_aU <- training %>% filter(diphtong == "aI" | diphtong == "aU")
  aI_vs_aU <- aI_vs_aU[,-3]
  aI_vs_aU <- aI_vs_aU[,-1]
  
  attach(aI_vs_aU)
  tune1 <- tune(svm, diphtong~., data = aI_vs_aU, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
  summary(tune1)
  best1 <- tune1$best.model
  summary(best1)
  detach(aI_vs_aU)
  
  # SECOND SVM ------------------------------
  
  aI_vs_OY <- training %>% filter(diphtong == "aI" | diphtong == "OY")
  aI_vs_OY <- aI_vs_OY[,-3]
  aI_vs_OY <- aI_vs_OY[,-1]
  
  attach(aI_vs_OY)
  tune2 <- tune(svm, diphtong~., data = aI_vs_OY, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
  summary(tune2)
  best2 <- tune2$best.model
  summary(best2)
  detach(aI_vs_OY)
  
  # THIRD SVM -------------------------------
  
  aU_vs_OY <- training %>% filter(diphtong == "aU" | diphtong == "OY")
  aU_vs_OY <- aU_vs_OY[,-3]
  aU_vs_OY <- aU_vs_OY[,-1]
  
  attach(aU_vs_OY)
  tune3 <- tune(svm, diphtong~., data = aU_vs_OY, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
  summary(tune3)
  best3 <- tune3$best.model
  summary(best3)
  detach(aU_vs_OY)
  
  # COUNT MISSCLASSIFICATIONS ---------------
  
  count_aI <- 0
  count_aU <- 0
  count_OY <- 0
  pred1 <- predict(best1, test[,-(1:3)])
  if(pred1 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred1 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred2 <- predict(best2, test[,-(1:3)])
  if(pred2 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred2 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  pred3 <- predict(best3, test[,-(1:3)])
  if(pred3 == "aI") {
    count_aI <- count_aI + 1
  } else if(pred3 == "aU") {
    count_aU <- count_aU + 1
  } else {
    count_OY <- count_OY + 1
  }
  
  if (count_aI > count_aU && count_aI > count_OY && test[,2] == "aI") {
    right <- right + 1
    n <- n + 1
  } else if (count_aU > count_aI && count_aU > count_OY && test[,2] == "aU") {
    right <- right + 1
    n <- n + 1
  } else if (count_OY > count_aI && count_OY > count_aU && test[,2] == "OY") {
    right <- right + 1
    n <- n + 1
  } else {
    wrong <- wrong + 1
    n <- n + 1
    miss <- c(miss, test[,2])
  }
}

# SHOW THE NUMBER OF CORRECT CLASSIFICATIONS
right

# SHOW THE NUMBER OF WRONG CLASSIFICATIONS
wrong

# SHOW THE NUMBER OF CORRECT CLASSIFICATIONS W.R.T. THE TOTAL NUMBER OF CLASSIFICATIONS
right/n

# THE ELEMENTS MISSCLASSIFIED ARE
miss