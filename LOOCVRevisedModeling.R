library(tidyverse)
library(FNN)
library(car)
library(tree)
library(randomForest)
library(gbm)
library(psych)
library(MASS)
library(glmnet)
library(pls)

#setwd("C:/Users/Nishant/Desktop/STA567")
load("stat_learning_proj.Rdata")
FinalModData<- read.csv("StrucModelDat.csv")

### Add back Player Names ###
rowmatch <- match(FinalModData$player_id, players_training$player_id) 
FinalModData$Name <- players_training[rowmatch, "names"]
rownames(FinalModData) <- FinalModData$Name
FinalModData$Name <-NULL
FinalModData$player_id <-NULL

### SUBSET THE DATAFRAME TO ONLY INCLUDE THE "TOP" PLAYERS ###
Scale.Data <- data.frame(scale(FinalModData[,c(11:17)]))
Scale.Data$Sum <- rowSums(Scale.Data)
Scale.Data<- Scale.Data[order(-Scale.Data$Sum),]
Scale.Data$Rank <- 1:length(Weekly.Predictions$Sum)
Scale.Data <- Scale.Data[Scale.Data$Sum>0,]
FinalModData<- FinalModData[rownames(FinalModData) %in% rownames(Scale.Data),]
FinalModData <- data.frame(Position=FinalModData[,2], scale(FinalModData[,3:ncol(FinalModData)]))

### Now the dataset only includes 163 players ###

### CREATE DATASETs TO USE FOR MODELING ###
point.data <- FinalModData[,c(1:17,26)]
names(point.data) <- gsub("[[:digit:]]", "", names(point.data) )
colnames(point.data)[length(colnames(point.data))] <- "PTS16"
names(point.data)[names(point.data) == 'XP'] <- 'XP3'

rebounds.data <- FinalModData[,c(1:17,29)]
names(rebounds.data) <- gsub("[[:digit:]]", "", names(rebounds.data) )
colnames(rebounds.data)[length(colnames(rebounds.data))] <- "TRB16"
names(rebounds.data)[names(rebounds.data) == 'XP'] <- 'XP3'

assist.data <- FinalModData[,c(1:17,27)]
names(assist.data) <- gsub("[[:digit:]]", "", names(assist.data) )
colnames(assist.data)[length(colnames(assist.data))] <- "AST16"
names(assist.data)[names(assist.data) == 'XP'] <- 'XP3'

steals.data <- FinalModData[,c(1:17,30)]
names(steals.data) <- gsub("[[:digit:]]", "", names(steals.data) )
colnames(steals.data)[length(colnames(steals.data))] <- "STL16"
names(steals.data)[names(steals.data) == 'XP'] <- 'XP3'

blocks.data <- FinalModData[,c(1:17,28)]
names(blocks.data) <- gsub("[[:digit:]]", "", names(blocks.data) )
colnames(blocks.data)[length(colnames(blocks.data))] <- "BLK16"
names(blocks.data)[names(blocks.data) == 'XP'] <- 'XP3'

threept.data <- FinalModData[,c(1:17,31)]
names(threept.data) <- gsub("[[:digit:]]", "", names(threept.data) )
colnames(threept.data)[length(colnames(threept.data))] <- "X3P16"
names(threept.data)[names(threept.data) == 'XP'] <- 'XP3'

FT.data <- FinalModData[,c(1:17,32)]
names(FT.data) <- gsub("[[:digit:]]", "", names(FT.data) )
colnames(FT.data)[length(colnames(FT.data))] <- "Perc16"
names(FT.data)[names(FT.data) == 'XP'] <- 'XP3'

######################################################
####Create one test and train index for all the data 
set.seed(013117)
test_index <- sample(1:163,75)


#####################Model for Rebounds  ######################################
PREDs <- matrix(NA, nrow=nrow(rebounds.data), ncol=8)
MSEs <- matrix(NA, nrow=nrow(rebounds.data), ncol=8)
for (i in 1:nrow(rebounds.data)) {
  train_data <- rebounds.data[-i,]
  test_data <- rebounds.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(TRB16  ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)

  #Bagging
  bag_mod <- randomForest(TRB16  ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
  
  #Random Forests
  rf_mod <- randomForest(TRB16  ~., data=train_data, mtry=7)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(TRB16 ~ . , data=train_data,
                   n.trees=4000 , distribution = "gaussian",
                   interaction.depth = 5) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=5)
  
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$TRB16 , k = 10, algorithm = "brute")
  
  
  #Single Tree-Based Regression
  simple_mod <- lm(TRB16  ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(TRB16  ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=10)
  
  ##Partial Least Squares Regression
  plsr.fit <- plsr(TRB16  ~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=6)
  
  
  MSEs[i,1]<- mean((test_data$TRB16  - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$TRB16 -bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$TRB16 - rf_pred)^2))
  MSEs[i,4]<- mean((test_data$TRB16 - boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$TRB16 -knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$TRB16 -simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$TRB16 -pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$TRB16 -plsr_preds)^2)
  
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
 
}

RebAvgMSE<-colMeans(MSEs)

View(MSEs)
write.csv(MSEs, "ReboundsLOOCV.csv")
write.csv(PREDs, "ReboundsPreds.csv")
#####################Model for Steals ######################################
PREDs <- matrix(NA, nrow=nrow(rebounds.data), ncol=8)
MSEs <- matrix(NA, nrow=nrow(steals.data), ncol=8)
for (i in 1:nrow(steals.data)) {
  train_data <- steals.data[-i,]
  test_data <- steals.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(STL16 ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)

  #Bagging
  bag_mod <- randomForest(STL16 ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
 
  #Random Forests
  rf_mod <- randomForest(STL16 ~., data=train_data, mtry=16)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(STL16~ . , data=train_data,
                   n.trees=5000, distribution = "gaussian",
                   interaction.depth = 1) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=1)
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$STL16, k = 2, algorithm = "brute")
  
  
  #Single Tree-Based Regression
  simple_mod <- lm(STL16 ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(STL16 ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=14)

  ##Partial Least Squares Regression
  plsr.fit <- plsr(STL16 ~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=3)
  
  
  MSEs[i,1]<- mean((test_data$STL16 - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$STL16-bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$STL16- rf_pred)^2))
  MSEs[i,4]<- mean((test_data$STL16- boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$STL16-knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$STL16-simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$STL16-pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$STL16-plsr_preds)^2)
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
  
  
}

StealsAvgMSE<-colMeans(MSEs)
write.csv(MSEs, "StealsLOOCV.csv")
write.csv(PREDs, "StealsPreds.csv")
#####################Model for X3 Points  ######################################

MSEs <- matrix(NA, nrow=nrow(threept.data), ncol=8)
for (i in 1:nrow(threept.data)) {
  train_data <- threept.data[-i,]
  test_data <- threept.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(X3P16 ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)
  
  #Bagging
  bag_mod <- randomForest(X3P16 ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
  
  #Random Forests
  rf_mod <- randomForest(X3P16 ~., data=train_data, mtry=12)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(X3P16 ~ . , data=train_data,
                   n.trees=8000, distribution = "gaussian",
                   interaction.depth = 3) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=3)
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$X3P16, k = 4, algorithm = "brute")
  
  
  #Single Tree-Based Regression
  simple_mod <- lm(X3P16 ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(X3P16 ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=11)
  
  ##Partial Least Squares Regression
  plsr.fit <- plsr(X3P16 ~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=4)
  
  
  MSEs[i,1]<- mean((test_data$X3P16 - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$X3P16-bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$X3P16- rf_pred)^2))
  MSEs[i,4]<- mean((test_data$X3P16- boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$X3P16-knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$X3P16-simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$X3P16-pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$X3P16-plsr_preds)^2)
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
  
}


X3PAvgMSE<-colMeans(MSEs)
write.csv(MSEs, "ThreepointsLOOCV.csv")
write.csv(PREDs, "ThreepointsPreds.csv")


#####################Model for FT  Points  ######################################

MSEs <- matrix(NA, nrow=nrow(FT.data), ncol=8)
for (i in 1:nrow(FT.data)) {
  train_data <- FT.data[-i,]
  test_data <- FT.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(Perc16 ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)
 
  #Bagging
  bag_mod <- randomForest(Perc16 ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
  
  #Random Forests
  rf_mod <- randomForest(Perc16  ~., data=train_data, mtry=10)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(Perc16  ~ . , data=train_data,
                   n.trees=1000, distribution = "gaussian",
                   interaction.depth =2) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=2)
  
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$Perc16 , k = 12, algorithm = "brute")
  
  
  #Single Tree-Based Regression
  simple_mod <- lm(Perc16  ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(Perc16  ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=10)
  
  ##Partial Least Squares Regression
  plsr.fit <- plsr(Perc16 ~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=1)
  
  
  MSEs[i,1]<- mean((test_data$Perc16  - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$Perc16 -bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$Perc16 - rf_pred)^2))
  MSEs[i,4]<- mean((test_data$Perc16 - boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$Perc16 -knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$Perc16 -simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$Perc16 -pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$Perc16 -plsr_preds)^2)
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
}

FreeThrowsPAvgMSE<-colMeans(MSEs)
write.csv(MSEs, "FreeThrowsLOOCV.csv")

write.csv(PREDs, "FreeThrowsPreds.csv")

#####################Model for Points  ######################################

MSEs <- matrix(NA, nrow=nrow(point.data), ncol=8)
for (i in 1:nrow(point.data)) {
  train_data <- point.data[-i,]
  test_data <- point.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(PTS16 ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)
  
  #Bagging
  bag_mod <- randomForest(PTS16  ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
  
  #Random Forests
  rf_mod <- randomForest(PTS16   ~., data=train_data, mtry=7)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(PTS16   ~ . , data=train_data,
                   n.trees=6000, distribution = "gaussian",
                   interaction.depth = 1) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=1)
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$PTS16  , k = 3, algorithm = "brute")
  
  #Single Tree-Based Regression
  simple_mod <- lm(PTS16  ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(PTS16   ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=13)
  
  ##Partial Least Squares Regression
  plsr.fit <- plsr(PTS16  ~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=5)
  
  
  MSEs[i,1]<- mean((test_data$PTS16   - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$PTS16  -bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$PTS16 - rf_pred)^2))
  MSEs[i,4]<- mean((test_data$PTS16  - boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$PTS16  -knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$PTS16  -simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$PTS16  -pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$PTS16  -plsr_preds)^2)
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
  
}

PointsAvgMSE<-colMeans(MSEs)
write.csv(MSEs, "PointsLOOCV.csv")

write.csv(PREDs, "PointsPreds.csv")

#####################Model for Assists ######################################

MSEs <- matrix(NA, nrow=nrow(assist.data), ncol=8)
for (i in 1:nrow(assist.data)) {
  train_data <- assist.data[-i,]
  test_data <- assist.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(AST16 ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)
  
  #Bagging
  bag_mod <- randomForest(AST16  ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
  
  #Random Forests
  rf_mod <- randomForest(AST16   ~., data=train_data, mtry=2)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(AST16   ~ . , data=train_data,
                   n.trees=5000, distribution = "gaussian",
                   interaction.depth = 3) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=3)
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$AST16  , k = 2, algorithm = "brute")
  
  
  #Single Tree-Based Regression
  simple_mod <- lm(AST16  ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(AST16   ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=4)
  
  ##Partial Least Squares Regression
  plsr.fit <- plsr(AST16  ~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=2)
  
  
  MSEs[i,1]<- mean((test_data$AST16   - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$AST16  -bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$AST16 - rf_pred)^2))
  MSEs[i,4]<- mean((test_data$AST16 - boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$AST16  -knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$AST16  -simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$AST16  -pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$AST16 -plsr_preds)^2)
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
  
}


AssistsAvgMSE<-colMeans(MSEs)
write.csv(MSEs, "AssistsLOOCV.csv")

write.csv(PREDs, "AssistsPreds.csv")


#####################Model for Blocks ######################################

MSEs <- matrix(NA, nrow=nrow(blocks.data), ncol=8)
for (i in 1:nrow(assist.data)) {
  train_data <- blocks.data[-i,]
  test_data <- blocks.data[i,]
  
  #Single Tree-Based Regression
  tree_mod <- tree(BLK16 ~ . , data=train_data)
  tree_pred <- predict(tree_mod, test_data)
  
  #Bagging
  bag_mod <- randomForest(BLK16 ~ . , data=train_data, mtry=17)
  bag_pred <- predict(bag_mod, test_data)
  
  
  #Random Forests
  rf_mod <- randomForest(BLK16 ~., data=train_data, mtry=7)
  rf_pred <- predict(rf_mod, test_data)
  
  #Boosting
  boost_mod <- gbm(BLK16 ~ . , data=train_data,
                   n.trees=2000, distribution = "gaussian",
                   interaction.depth = 5) 
  boost_mod_pred <- predict( boost_mod, test_data, n.tree=5)
  
  #K Nearest Neighbours
  knnTest <- knn.reg(train = train_data[,2:ncol(train_data)], test = test_data[,2:ncol(test_data)],
                     y = train_data$BLK16   , k = 6, algorithm = "brute")
  
  
  #Single Tree-Based Regression
  simple_mod <- lm( BLK16 ~ .  , data=train_data)
  simple_preds <- predict(simple_mod, test_data)
  
  #Principal Components Regression
  pcr.fit <- pcr(BLK16 ~ ., data=train_data, #
                 validation="CV", scale=TRUE) 
  pcr_preds <- predict(pcr.fit, test_data,ncomp=9)
  
  ##Partial Least Squares Regression
  plsr.fit <- plsr(BLK16~ ., data=train_data,validation="CV", scale=TRUE)
  plsr_preds <- predict(plsr.fit, test_data,ncomp=3)
  
  
  MSEs[i,1]<- mean((test_data$BLK16 - tree_pred)^2)
  MSEs[i,2]<- mean((test_data$BLK16 -bag_pred)^2)
  MSEs[i,3]<- mean(((test_data$BLK16 - rf_pred)^2))
  MSEs[i,4]<- mean((test_data$BLK16 - boost_mod_pred)^2)
  MSEs[i,5]<- mean((test_data$BLK16 -knnTest$pred)^2)
  MSEs[i,6]<- mean((test_data$BLK16  -simple_preds)^2) 
  MSEs[i,7]<- mean((test_data$BLK16 -pcr_preds)^2)
  MSEs[i,8]<- mean((test_data$BLK16 -plsr_preds)^2)
  
  ##Predictions
  PREDs[i,1]<-  tree_pred
  PREDs[i,2]<- bag_pred
  PREDs[i,3]<- rf_pred
  PREDs[i,4]<-  boost_mod_pred
  PREDs[i,5]<-knnTest$pred
  PREDs[i,6]<- simple_preds
  PREDs[i,7]<- pcr_preds
  PREDs[i,8]<- plsr_preds
  
}

BlocksAvgMSE<-colMeans(MSEs)
write.csv(MSEs, "BlocksLOOCV.csv")

write.csv(PREDs, "BlocksPreds.csv")

AVGMSEs<-rbind(RebAvgMSE,StealsAvgMSE,X3PAvgMSE,FreeThrowsPAvgMSE,PointsAvgMSE,AssistsAvgMSE,BlocksAvgMSE)


write.csv(AVGMSEs, "AVGMSes.csv")



##### Predictions for the 2017 season ####

data2016 <- data2016[,c(1,18:ncol(data2016))]
names(data2016) <- gsub("[[:digit:]]", "", names(data2016) )
names(data2016)[names(data2016) == 'XP'] <- 'XP3'
#names(data2016)[names(data2016) == 'sdXP'] <- 'sdXP3'


#####################Model for Rebounds  ######################################
PREDs <- matrix(NA, nrow=nrow(data2016), ncol=7)
train_data <- rebounds.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(TRB16  ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(TRB16  ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(TRB16  ~., data=train_data, mtry=7)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,c(3:ncol(train_data)-1)], test = test_data[,c(2:ncol(test_data))],
                   y = train_data$TRB16 , k = 10, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm(TRB16  ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(TRB16  ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=10)
##Partial Least Squares Regression
plsr.fit <- plsr(TRB16  ~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=6)

PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds

Rebounds.Preds <- as.data.frame(PREDs)
names(Rebounds.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")


#####################Model for Steals ######################################
PREDs <- matrix(NA, nrow=nrow(data2016), ncol=7)
train_data <- steals.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(STL16 ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(STL16 ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(STL16 ~., data=train_data, mtry=16)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,3:ncol(train_data)-1], test = test_data[,2:ncol(test_data)],
                   y = train_data$STL16, k = 2, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm(STL16 ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(STL16 ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=14)

##Partial Least Squares Regression
plsr.fit <- plsr(STL16 ~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=3)
##Predictions
PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds

Steals.Preds <- as.data.frame(PREDs)
names(Steals.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")


#####################Model for X3 Points  ######################################
PREDs <- matrix(NA, nrow=nrow(data2016), ncol=7)

train_data <- threept.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(X3P16 ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(X3P16 ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(X3P16 ~., data=train_data, mtry=12)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,3:ncol(train_data)-1], test = test_data[,2:ncol(test_data)],
                   y = train_data$X3P16, k = 4, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm(X3P16 ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(X3P16 ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=11)
##Partial Least Squares Regression
plsr.fit <- plsr(X3P16 ~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=4)

##Predictions
PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds

ThreePT.Preds <- as.data.frame(PREDs)
names(ThreePT.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")


#####################Model for FT  Points  ######################################

PREDs <- matrix(NA, nrow=nrow(data2016), ncol=7)

train_data <- FT.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(Perc16 ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(Perc16 ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(Perc16  ~., data=train_data, mtry=10)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,3:ncol(train_data)-1], test = test_data[,2:ncol(test_data)],
                   y = train_data$Perc16 , k = 12, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm(Perc16  ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(Perc16  ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=10)
##Partial Least Squares Regression
plsr.fit <- plsr(Perc16 ~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=1)

##Predictions
PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds

FT.Preds <- as.data.frame(PREDs)
names(FT.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")

#####################Model for Points  ######################################

PREDs<- matrix(NA, nrow=nrow(data2016), ncol=7)
train_data <- point.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(PTS16 ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(PTS16  ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(PTS16   ~., data=train_data, mtry=7)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,3:ncol(train_data)-1], test = test_data[,2:ncol(test_data)],
                   y = train_data$PTS16  , k = 3, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm(PTS16  ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(PTS16   ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=13)

##Partial Least Squares Regression
plsr.fit <- plsr(PTS16  ~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=5)


##Predictions
PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds

Points.Preds <- as.data.frame(PREDs)
names(Points.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")

#####################Model for Assists ######################################

PREDs <- matrix(NA, nrow=nrow(data2016), ncol=7)
train_data <- assist.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(AST16 ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(AST16  ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(AST16   ~., data=train_data, mtry=2)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,3:ncol(train_data)-1], test = test_data[,2:ncol(test_data)],
                   y = train_data$AST16  , k = 2, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm(AST16  ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(AST16   ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=4)
##Partial Least Squares Regression
plsr.fit <- plsr(AST16  ~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=2)

##Predictions
PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds


Assists.Preds <- as.data.frame(PREDs)
names(Assists.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")
#####################Model for Blocks ######################################

PREDs <- matrix(NA, nrow=nrow(data2016), ncol=7)
train_data <- blocks.data
test_data <- data2016

#Single Tree-Based Regression
tree_mod <- tree(BLK16 ~ . , data=train_data)
tree_pred <- predict(tree_mod, test_data)
#Bagging
bag_mod <- randomForest(BLK16 ~ . , data=train_data, mtry=17)
bag_pred <- predict(bag_mod, test_data)
#Random Forests
rf_mod <- randomForest(BLK16 ~., data=train_data, mtry=7)
rf_pred <- predict(rf_mod, test_data)
#K Nearest Neighbours
knnTest <- knn.reg(train = train_data[,3:ncol(train_data)-1], test = test_data[,2:ncol(test_data)],
                   y = train_data$BLK16   , k = 6, algorithm = "brute")
#Single Tree-Based Regression
simple_mod <- lm( BLK16 ~ .  , data=train_data)
simple_preds <- predict(simple_mod, test_data)
#Principal Components Regression
pcr.fit <- pcr(BLK16 ~ ., data=train_data, #
               validation="CV", scale=TRUE) 
pcr_preds <- predict(pcr.fit, test_data,ncomp=9)
##Partial Least Squares Regression
plsr.fit <- plsr(BLK16~ ., data=train_data,validation="CV", scale=TRUE)
plsr_preds <- predict(plsr.fit, test_data,ncomp=3)

##Predictions
PREDs[,1]<-  tree_pred
PREDs[,2]<- bag_pred
PREDs[,3]<- rf_pred
PREDs[,4]<-knnTest$pred
PREDs[,5]<- simple_preds
PREDs[,6]<- pcr_preds
PREDs[,7]<- plsr_preds

Blocks.Preds <- as.data.frame(PREDs)
names(Blocks.Preds) <- c("Tree","Bagging","R Forest", "KNN","MLR","PCR","PLSR")





