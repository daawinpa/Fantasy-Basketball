########-------------------------------------------------------------###################
rownames(Points.Preds)<-rownames(data2016)
rownames(Rebounds.Preds)<-rownames(data2016)
rownames(Assists.Preds)<-rownames(data2016)
rownames(Blocks.Preds)<-rownames(data2016)
rownames(Steals.Preds)<-rownames(data2016)
rownames(FT.Preds)<-rownames(data2016)
rownames(ThreePT.Preds)<-rownames(data2016)

### Create Weighted Predictions ###
AvgMSEs <- read.csv("AVGMSes.csv")
rownames(AvgMSEs) <- c("Rebounds","Steals","ThreePT","FT","Points","Assists","Blocks")
AvgMSEs$X <- NULL

### Drop Boosting since it is very poor --- DROP TREE FOR ASSISTS AND POINTS ###
AvgMSEs$Boosting <- NULL
Points.Preds$Tree<-NULL
Assists.Preds$Tree <-NULL
FT.Preds$Tree <- NULL
### Create weights for each predictions ###
Weights <- as.matrix((1/AvgMSEs)/rowSums(1/AvgMSEs))
points.preds <- t(as.matrix(Points.Preds))
rebounds.preds <-t(as.matrix(Rebounds.Preds))
threepts.preds <-t(as.matrix(ThreePT.Preds))
steals.preds <-t(as.matrix(Steals.Preds))
assists.preds <-t(as.matrix(Assists.Preds))
blocks.preds <-t(as.matrix(Blocks.Preds))
FT.preds <-t(as.matrix(FT.Preds))

### Drop tree model for the Points, Free Throws and Assists ###
Weight.PA <- AvgMSEs
Weight.PA$Tree <- NULL
Weight.PA <- as.matrix((1/Weight.PA)/rowSums(1/Weight.PA))


Weekly.Predictions<- data.frame(PointsPred=t(Weight.PA["Points",1:6]%*%points.preds),
                                   ReboundsPred=t(Weights["Rebounds",1:7]%*%rebounds.preds),
                                   StealsPred=t(Weights["Steals",1:7]%*%steals.preds),
                                   AssistsPred=t(Weight.PA["Assists",1:6]%*%assists.preds),
                                   BlocksPred=t(Weights["Blocks",1:7]%*%blocks.preds),
                                   FTPred = t(Weight.PA["FT",1:6]%*%FT.preds),
                                   ThreePTPred = t(Weights["ThreePT",1:7]%*%threepts.preds) )




### Create Weekly Predicition Data ###
Weekly.Predictions$Sum <- rowSums(Weekly.Predictions)
Weekly.Predictions<- Weekly.Predictions[order(-Weekly.Predictions$Sum),]
Weekly.Predictions$Rank <- 1:length(Weekly.Predictions$Sum)

write.csv(Weekly.Predictions, "Weekly Predictions.csv")
####---------------------------------------------------------------------------------------------#


Weekly.Predictions <-read.csv("Weekly Predictions.csv")
rownames(Weekly.Predictions) <- Weekly.Predictions$X
Weekly.Predictions$X <-NULL

Team.Names<-c("") 
Team.1 <-c("Stephen Curry")
Team.2<- c("James Harden")
Team.3<- c("Kevin Durant")
Drafted<-c(Team.1,Team.2,Team.3) ### <--- still have to put these in manually. 

draft.player<- function(Drafted, Team.Names) {
default<- data.frame(PointPred=1,ASTPred=1,BLKPred=1, TRBPred=1, STLPred=1, X3Pred=1, PercPred=1, Sum=0, Rank=1, Names="Start")
Team <- Weekly.Predictions[rownames(Weekly.Predictions) %in% Team.Names ,] 
Start <- if (length(Team.Names)<=2) default else Team
Team.Sums <- colSums(Weekly.Predictions[rownames(Weekly.Predictions) %in% Team.Names ,])/length(Team.Names)
Team.1.Sums <- colSums(Weekly.Predictions[rownames(Weekly.Predictions) %in% Team.1 ,])/length(Team.1)
Team.2.Sums <- colSums(Weekly.Predictions[rownames(Weekly.Predictions) %in% Team.2 ,])/length(Team.2)
Team.3.Sums <- colSums(Weekly.Predictions[rownames(Weekly.Predictions) %in% Team.3 ,])/length(Team.3)
Game.Results <- data.frame(Opponent=c("Team.1","Team.2","Team.3"),
                           Won= c(sum((Team.Sums[1:7]>Team.1.Sums[1:7])),
                                  sum(Team.Sums[1:7]>Team.2.Sums[1:7]),
                                  sum(Team.Sums[1:7]>Team.2.Sums[1:7])) )

Remaining.Players <- Weekly.Predictions[!rownames(Weekly.Predictions) %in% Drafted & ! rownames(Weekly.Predictions) %in% Team.Names ,]
Potential.Add <- data.frame(Name=rownames(Remaining.Players), Added=NA)
for(i in 1:nrow(Remaining.Players)){
P.Team.Names <- c(Team.Names, as.character(Potential.Add$Name[i]))
P.Team <- Weekly.Predictions[rownames(Weekly.Predictions) %in% P.Team.Names[i] ,] 
P.Team.Sums<- colSums(Weekly.Predictions[rownames(Weekly.Predictions) %in% P.Team.Names ,])/length(P.Team.Names)
Potential.Add$Added[i] <- sum(c(sum((P.Team.Sums[1:7]>Team.1.Sums[1:7])),
                                sum(P.Team.Sums[1:7]>Team.2.Sums[1:7]),
                                sum(P.Team.Sums[1:7]>Team.2.Sums[1:7]))) -
                                  sum(c(sum((Team.Sums[1:7]>Team.1.Sums[1:7])),
                                  sum(Team.Sums[1:7]>Team.2.Sums[1:7]),
                                  sum(Team.Sums[1:7]>Team.2.Sums[1:7])) ) 
}
selection <-Potential.Add[which.max(Potential.Add$Added),]
    #min.sum <- names(which.min(colSums(Start[,1:8])))
    #players<- Remaining.Players[order(-Remaining.Players[,min.sum]),][1:20,]
    #top.players <- players[players[,min.sum] >= max(players[,min.sum])-(1/3),]
    #selection <- if(length(top.players$Rank)>1) top.players[which.min(top.players$Rank),] else top.players
selection <- as.character(selection$Name)
Names <- c(Team.Names, selection)
assign('Team.Names', Names, envir=.GlobalEnv)
Team <- Weekly.Predictions[rownames(Weekly.Predictions) %in% Names ,] 
assign('Team', Team,envir=.GlobalEnv)
return(selection)
}

### Tells you the player to select 
draft.player(Drafted,Team.Names)

