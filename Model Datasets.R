library(tidyverse)
library(FNN)
library(car)
library(tree)
library(randomForest)
library(gbm)
setwd("C:/Users/Nishant/Desktop/STA567")
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
Scale.Data$Rank <- 1:length(Scale.Data$Sum)
Scale.Data <- Scale.Data[Scale.Data$Sum>0,]
FinalModData<- FinalModData[rownames(FinalModData) %in% rownames(Scale.Data),]
FinalModData <- data.frame(Position=FinalModData[,2], scale(FinalModData[,3:ncol(FinalModData)]))

### Now the dataset only includes 163 players ###
add_cv_cohorts <- function(dat,cv_K){
  if(nrow(dat) %% cv_K == 0){ # if perfectly divisible
    dat$cv_cohort <- sample(rep(1:cv_K, each=(nrow(dat)%/%cv_K)))
  } else { # if not perfectly divisible
    dat$cv_cohort <- sample(c(rep(1:(nrow(dat) %% cv_K), each=(nrow(dat)%/%cv_K + 1)),
                              rep((nrow(dat) %% cv_K + 1):cv_K,each=(nrow(dat)%/%cv_K)) ) )
  }
  return(dat)
}

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


### Import all the Model Predictions ###
points.preds <- read.csv("PointsPreds.csv")
rownames(points.preds) <- rownames(point.data)
points.preds$X <- NULL
rebounds.preds <- read.csv("ReboundsPreds.csv")
rownames(rebounds.preds) <- rownames(point.data)
rebounds.preds$X <- NULL
assists.preds <- read.csv("AssistsPreds.csv")
rownames(assists.preds) <- rownames(point.data)
assists.preds$X <- NULL
steals.preds <- read.csv("StealsPreds.csv")
rownames(steals.preds) <- rownames(point.data)
steals.preds$X <- NULL
blocks.preds <- read.csv("BlocksPreds.csv")
rownames(blocks.preds) <- rownames(point.data)
blocks.preds$X <- NULL
FT.preds <- read.csv("FreeThrowsPreds.csv")
rownames(FT.preds) <- rownames(point.data)
FT.preds$X <- NULL
threepts.preds <- read.csv("ThreepointsPreds.csv")
rownames(threepts.preds) <- rownames(point.data)
threepts.preds$X <- NULL

### Import the Model MSEs ###
AvgMSEs <- read.csv("AVGMSes.csv")
rownames(AvgMSEs) <- c("Rebounds","Steals","ThreePT","FT","Points","Assists","Blocks")
AvgMSEs$X <- NULL

### Drop Boosting since it is very poor --- DROP TREE FOR ASSISTS AND POINTS ###
AvgMSEs$Boosting <- NULL
points.preds$Boosting <- NULL
points.preds$Tree<-NULL
rebounds.preds$Boosting <- NULL
assists.preds$Boosting <- NULL
assists.preds$Tree <-NULL
steals.preds$Boosting<-NULL
blocks.preds$Boosting<-NULL
FT.preds$Boosting <- NULL
FT.preds$Tree <- NULL
threepts.preds$Boosting <- NULL
### Create weights for each predictions ###
Weights <- as.matrix((1/AvgMSEs)/rowSums(1/AvgMSEs))
points.preds <- t(as.matrix(points.preds))
rebounds.preds <-t(as.matrix(rebounds.preds))
threepts.preds <-t(as.matrix(threepts.preds))
steals.preds <-t(as.matrix(steals.preds))
assists.preds <-t(as.matrix(assists.preds))
blocks.preds <-t(as.matrix(blocks.preds))
FT.preds <-t(as.matrix(FT.preds))

### Drop tree model for the Points, Free Throws and Assists ###
Weight.PA <- AvgMSEs
Weight.PA$Tree <- NULL
Weight.PA <- as.matrix((1/Weight.PA)/rowSums(1/Weight.PA))


Weighted.LOOCV.Preds <- data.frame(PointsPred=t(Weight.PA["Points",1:6]%*%points.preds),
                                   ReboundsPred=t(Weights["Rebounds",1:7]%*%rebounds.preds),
                                   StealsPred=t(Weights["Steals",1:7]%*%steals.preds),
                                   AssistsPred=t(Weight.PA["Assists",1:6]%*%assists.preds),
                                   BlocksPred=t(Weights["Blocks",1:7]%*%blocks.preds),
                                   FTPred = t(Weight.PA["FT",1:6]%*%FT.preds),
                                   ThreePTPred = t(Weights["ThreePT",1:7]%*%threepts.preds) )

Weighted.LOOCV.MSEs <- data.frame(PTS= mean(((FinalModData$PTS16-Weighted.LOOCV.Preds$PointsPred)^2)),
                                  TRB=mean(((FinalModData$TRB16-Weighted.LOOCV.Preds$ReboundsPred)^2)),
                                  STL=mean(((FinalModData$STL16-Weighted.LOOCV.Preds$StealsPred)^2)),
                                  AST=mean(((FinalModData$AST16-Weighted.LOOCV.Preds$AssistsPred)^2)),
                                  BLK=mean(((FinalModData$BLK16-Weighted.LOOCV.Preds$BlocksPred)^2)),
                                  FT=mean(((FinalModData$Perc16-Weighted.LOOCV.Preds$FTPred)^2)),
                                  X3P=mean(((FinalModData$X3P16-Weighted.LOOCV.Preds$ThreePTPred)^2)))
Weighted.LOOCV.MSEs <- data.frame(Weighted.MSE=t(Weighted.LOOCV.MSEs))
rownames(Weighted.LOOCV.MSEs)<-c("Points","Rebounds","Steals","Assists","Blocks","FT","ThreePT")

### Add to the Average MSEs ###
rowmatch <- match(rownames(AvgMSEs),rownames(Weighted.LOOCV.MSEs))
AvgMSEs$Weighted.MSE <- Weighted.LOOCV.MSEs[rowmatch,"Weighted.MSE"]
