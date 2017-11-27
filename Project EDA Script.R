library(ggplot2)
library(data.table)
library(lubridate)
library(stringi)
library(Hmisc)
library(dplyr)
library(scales)
setwd("C:/Users/Nishant/Desktop/STA567")
load("stat_learning_proj.Rdata")
tail(players_training)
tail(games_training)
tail(yearly_training)
teamswitch <- yearly_training[yearly_training$tm=="TOT",]

###Player ids
rowmatch <- match(yearly_training$player,players_training$names)
yearly_training$playerid <- players_training[rowmatch,"player_id"]

###Get Position
rowmatch <- match(games_training$player_id,yearly_training$playerid)
games_training$position <- yearly_training[rowmatch,"pos"]

### Minutes Played Correction
games_training$MP <- as.numeric(as.period(ms(games_training$MP), unit = "sec")/60)
games_training$MP <- ifelse(is.na(games_training$MP),0,games_training$MP)



### Converts points to a numeric variable we can work with
test <- games_training
test$PTS <- as.numeric(test$PTS)
test$PTS <- ifelse(is.na(test$PTS),0,test$PTS)
test$Week <- strftime(test$Date, format="%W")
### Corrects for the end of year weeks that overlap, they are now all part of Week 52
test$Week <- ifelse(ymd(test$Date)<="2015-01-04" & ymd(test$Date)>="2014-12-29","52", test$Week)
test$Week <- ifelse(ymd(test$Date)<="2016-01-03" & ymd(test$Date)>="2015-12-28","52", test$Week)


### Take each player, and average their statistics by week. Plot 2 seasons worth of weekly
### averages for each player. Put in average/median/etc 

### Total number of games aggregated by week
games<-aggregate(PTS~player_id+year+Week, data=test, FUN = length)
View(games)

### Total Points aggregated by Week
points<-aggregate(PTS~player_id+year+Week, data=test, FUN = sum)
View(points)



points.games <- merge(points,games, by=c("player_id","year","Week"))
colnames(points.games)<- c("player_id","year","Week","PTS","Games")
points.games$pergame <- points.games$PTS/points.games$Games


weekly.avgpoints <- aggregate(PTS~player_id+year, data=points.games, FUN=mean)
weekly.medpoints <- aggregate(PTS~player_id+year, data=points.games, FUN=median)
View(weekly.avgpoints)

weekly.sdavgpoints <- aggregate(PTS~player_id+year, data=points.games, FUN=sd)
View(weekly.sdavgpoints)

##########################################################################
weekly.sdavgrebounds <- aggregate(TRB~player_id+year, data=rebounds, FUN=sd)
##########################################################################

weekly.sdavgsteals <- aggregate(STL~player_id+year, data=steals, FUN=sd)
View(weekly.sdavgsteals)
######################################################################
weekly.sdavgassists<- aggregate(AST~player_id+year, data=assists, FUN=sd)
View(weekly.sdavgassists)

####################################################################
weekly.sdavgblocks<- aggregate(BLK~player_id+year, data=blocks, FUN=sd)
View(weekly.sdavgblocks)

####################################################################
weekly.sdavgthreepoints<- aggregate(X3P~player_id+year, data=threepts, FUN=sd)
View(weekly.sdavgthreepoints)

####################################################################
weekly.sdavgFT <- aggregate(Perc ~ player_id+year, data=FT, FUN=sd)
####################################################################
MP <- aggregate(MP ~ player_id+year+Week, data=test, FUN=sum)
weekly.medMP <- aggregate(MP ~ player_id+year, data=MP, FUN=median)
weekly.sdavgMP <- aggregate(MP ~ player_id+year, data=MP, FUN=sd)



points.plots <- merge(weekly.avgpoints,weekly.sdavgpoints, by=c("player_id","Year","Games"))
points.plots <- merge(points.plots, weekly.pointsper, by=c("player_id","Year","Games"))
View(points.plots)
colnames(points.plots) <- c("player_id","Year","Games", "Points", "SD", "PerGame")
points.plots$SD <- ifelse(is.na(points.plots$SD),0,points.plots$SD)

### Add position
rowmatch <- match(points.plots$player_id,yearly_training$playerid)
points.plots$Position <- yearly_training[rowmatch,"pos"]

### Collapse to just 5 positions
points.plots$Position <- ifelse(points.plots$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                substr(points.plots$Position,1,2), points.plots$Position)


### Facet plot by Games that Week ###
pp <- ggplot(points.plots, aes(x=Points, y=seq_along(points.plots$Points))) + geom_point(shape=1)

pp<-ggplot(points.plots, aes(x=Points)) + geom_histogram()

pp + facet_grid(.~ Games)

###Radar Chart for all the statistics###
weekly.avgpoints <- aggregate(PTS~player_id+year, data=points, FUN=mean)
View(weekly.avgpoints)

rowmatch <- match(weekly.avgpoints$player_id,yearly_training$playerid)
weekly.avgpoints$Position <- yearly_training[rowmatch,"pos"]

### Collapse to just 5 positions
weekly.avgpoints$Position <- ifelse(weekly.avgpoints$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                substr(weekly.avgpoints$Position,1,2), weekly.avgpoints$Position)

points.bypos <- aggregate(PTS ~ Position, data=weekly.avgpoints, FUN=mean)
points.bypos

test$TRB <- as.numeric(test$TRB)
test$TRB <- ifelse(is.na(test$TRB),0,test$TRB)
rebounds<-aggregate(TRB ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgrebounds <- aggregate(TRB~player_id+year, data=rebounds, FUN=mean)
weekly.medrebounds <- aggregate(TRB~player_id+year, data=rebounds, FUN=median)


rowmatch <- match(weekly.avgrebounds$player_id,yearly_training$playerid)
rowmatch <- match(weekly.medrebounds$player_id,yearly_training$playerid)
weekly.avgrebounds$Position <- yearly_training[rowmatch,"pos"]
weekly.avgrebounds$Position <- ifelse(weekly.avgrebounds$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                    substr(weekly.avgrebounds$Position,1,2), weekly.avgrebounds$Position)
weekly.medrebounds$Position <- yearly_training[rowmatch,"pos"]
weekly.medrebounds$Position <- ifelse(weekly.medrebounds$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                      substr(weekly.medrebounds$Position,1,2), weekly.medrebounds$Position)

rebounds.bypos <- aggregate(TRB ~ Position, data=weekly.avgrebounds, FUN=mean)
rebounds.bypos





test$STL <- as.numeric(test$STL)
test$STL <- ifelse(is.na(test$STL),0,test$STL)
steals<-aggregate(STL ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgsteals <- aggregate(STL~player_id+year, data=steals, FUN=mean)
weekly.medsteals <- aggregate(STL~player_id+year, data=steals, FUN=median)

rowmatch <- match(weekly.avgsteals$player_id,yearly_training$playerid)
weekly.avgsteals$Position <- yearly_training[rowmatch,"pos"]
weekly.avgsteals$Position <- ifelse(weekly.avgsteals$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                      substr(weekly.avgsteals$Position,1,2), weekly.avgsteals$Position)
rowmatch <- match(weekly.medsteals$player_id,yearly_training$playerid)
weekly.medsteals$Position <- yearly_training[rowmatch,"pos"]
weekly.medsteals$Position <- ifelse(weekly.medsteals$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                    substr(weekly.medsteals$Position,1,2), weekly.medsteals$Position)

steals.bypos <- aggregate(STL ~ Position, data=weekly.avgsteals, FUN=mean)
steals.bypos



test$AST  <- as.numeric(test$AST )
test$AST  <- ifelse(is.na(test$AST ),0,test$AST )
assists<-aggregate(AST ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgassists <- aggregate(AST~player_id+year, data=assists, FUN=mean)
weekly.medassists <- aggregate(AST~player_id+year, data=assists, FUN=median)

rowmatch <- match(weekly.avgassists$player_id,yearly_training$playerid)
weekly.avgassists$Position <- yearly_training[rowmatch,"pos"]
weekly.avgassists$Position <- ifelse(weekly.avgassists$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                    substr(weekly.avgassists$Position,1,2), weekly.avgassists$Position)
rowmatch <- match(weekly.medassists$player_id,yearly_training$playerid)
weekly.medassists$Position <- yearly_training[rowmatch,"pos"]
weekly.medassists$Position <- ifelse(weekly.medassists$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                     substr(weekly.medassists$Position,1,2), weekly.medassists$Position)

assists.bypos <- aggregate(AST ~ Position, data=weekly.avgassists, FUN=mean)
assists.bypos


test$BLK  <- as.numeric(test$BLK )
test$BLK  <- ifelse(is.na(test$BLK ),0,test$BLK )
blocks<-aggregate(BLK ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgblocks <- aggregate(BLK~player_id+year, data=blocks, FUN=mean)
weekly.medblocks <- aggregate(BLK~player_id+year, data=blocks, FUN=median)

rowmatch <- match(weekly.avgblocks$player_id,yearly_training$playerid)
weekly.avgblocks$Position <- yearly_training[rowmatch,"pos"]
weekly.avgblocks$Position <- ifelse(weekly.avgblocks$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                     substr(weekly.avgblocks$Position,1,2), weekly.avgblocks$Position)
rowmatch <- match(weekly.medblocks$player_id,yearly_training$playerid)
weekly.medblocks$Position <- yearly_training[rowmatch,"pos"]
weekly.medblocks$Position <- ifelse(weekly.medblocks$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                    substr(weekly.medblocks$Position,1,2), weekly.medblocks$Position)

blocks.bypos <- aggregate(BLK ~ Position, data=weekly.avgblocks, FUN=mean)
blocks.bypos





test$FT  <- as.numeric(test$FT )
test$FT <- ifelse(is.na(test$FT),0,test$FT )
FT<-aggregate(FT ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgFT <- aggregate(FT~player_id+year, data=FT, FUN=mean)
weekly.medFT <- aggregate(FT~player_id+year, data=FT, FUN=median)

rowmatch <- match(weekly.avgFT$player_id,yearly_training$playerid)
weekly.avgFT$Position <- yearly_training[rowmatch,"pos"]
weekly.avgFT$Position <- ifelse(weekly.avgFT$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                    substr(weekly.avgFT$Position,1,2), weekly.avgFT$Position)
rowmatch <- match(weekly.medFT$player_id,yearly_training$playerid)
weekly.medFT$Position <- yearly_training[rowmatch,"pos"]
weekly.medFT$Position <- ifelse(weekly.medFT$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                substr(weekly.medFT$Position,1,2), weekly.medFT$Position)

FT.bypos <- aggregate(FT~ Position, data=weekly.avgFT, FUN=mean)
FT.bypos

test$FTA  <- as.numeric(test$FTA )
test$FTA <- ifelse(is.na(test$FTA),0,test$FTA )
FTA<-aggregate(FTA ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgFTA <- aggregate(FTA~player_id+year, data=FTA, FUN=mean)
weekly.medFTA <- aggregate(FTA~player_id+year, data=FTA, FUN=median)

rowmatch <- match(weekly.avgFTA$player_id,yearly_training$playerid)
weekly.avgFTA$Position <- yearly_training[rowmatch,"pos"]
weekly.avgFTA$Position <- ifelse(weekly.avgFTA$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                substr(weekly.avgFTA$Position,1,2), weekly.avgFTA$Position)
rowmatch <- match(weekly.medFTA$player_id,yearly_training$playerid)
weekly.medFTA$Position <- yearly_training[rowmatch,"pos"]
weekly.medFTA$Position <- ifelse(weekly.medFTA$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                 substr(weekly.medFTA$Position,1,2), weekly.medFTA$Position)

FTA.bypos <- aggregate(FTA~ Position, data=weekly.avgFTA, FUN=mean)
FTA.bypos

rowmatch <- match(interaction(FT$player_id,FT$Week, FT$year), interaction(FTA$player_id, FTA$Week, FTA$year))
FT$FTA <- FTA[rowmatch,"FTA"]
FT$Perc <- FT$FT/FT$FTA
FT$Perc <- ifelse(is.na(FT$Perc),0,FT$Perc)

weekly.medFT <- aggregate(Perc ~ player_id+year, data=FT, FUN=median)

test$X3P  <- as.numeric(test$X3P )
test$X3P <- ifelse(is.na(test$X3P),0,test$X3P )
threepts <-aggregate(X3P ~player_id+year+Week, data=test, FUN = sum,na.action =  na.exclude)

weekly.avgthreepts <- aggregate(X3P~player_id+year, data=threepts, FUN=mean)
weekly.medthreepts <- aggregate(X3P~player_id+year, data=threepts, FUN=median)

rowmatch <- match(weekly.avgthreepts$player_id,yearly_training$playerid)
weekly.avgthreepts$Position <- yearly_training[rowmatch,"pos"]
weekly.avgthreepts$Position <- ifelse(weekly.avgthreepts$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                 substr(weekly.avgthreepts$Position,1,2), weekly.avgthreepts$Position)
rowmatch <- match(weekly.medthreepts$player_id,yearly_training$playerid)
weekly.medthreepts$Position <- yearly_training[rowmatch,"pos"]
weekly.medthreepts$Position <- ifelse(weekly.medthreepts$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                      substr(weekly.medthreepts$Position,1,2), weekly.medthreepts$Position)

threepts.bypos <- aggregate(X3P~ Position, data=weekly.avgthreepts, FUN=mean)
threepts.bypos


ByPosition <- Reduce(function(x, y) merge(x, y, all=TRUE),  list(points.bypos, rebounds.bypos, blocks.bypos, steals.bypos, 
                                                                 assists.bypos,FT.bypos, FTA.bypos,threepts.bypos))
ByPosition$FTPCT <- ByPosition$FT/ByPosition$FTA
ByPosition$FT <- NULL
ByPosition$FTA <- NULL

ByPosition$PTS <- rescale(ByPosition$PTS, c(1,2))
ByPosition$TRB <- rescale(ByPosition$TRB, c(1,2))
ByPosition$BLK <- rescale(ByPosition$BLK, c(1,2))
ByPosition$STL <- rescale(ByPosition$STL, c(1,2))
ByPosition$AST <- rescale(ByPosition$AST, c(1,2))
ByPosition$X3P <- rescale(ByPosition$X3P, c(1,2))
ByPosition$FTPCT <- rescale(ByPosition$FTPCT, c(1,2))
ByPosition %>%
  mutate_each(funs(rescale,5), -Position)  -> ByPosition

library(fmsb)
radarchart


library(radarchart)
ByPosition <-as.data.frame(t(ByPosition))
ByPosition <-ByPosition[-1,]
colnames(ByPosition) <- c("C","PF","PG","SF","SG")
ByPosition <- data.frame(lapply(ByPosition, function(x) as.numeric(as.character(x))))
scores <- list(
  "C" = ByPosition$C,
  "PF"= ByPosition$PF,
  "PG"= ByPosition$PG,
  "SF"= ByPosition$SF,
  "SG"= ByPosition$SG
)

labs <- c("Points","Rebounds","Blocks","Steals","Assists","3PT","FT%")


chartJSRadar(scores=scores, labs=labs)


### Times Ranked in top 10 of Points
library(dplyr)
points %>% group_by(Week,year) %>% 
  mutate(rank = dense_rank(-PTS)) -> rank.points

top10points <- rank.points[rank.points$rank<=10,]

top10rankpoints <-aggregate(rank ~ player_id, data=top10points, FUN=length)

rowmatch <- match(top10rankpoints$player_id,yearly_training$playerid)
top10rankpoints$Position <- yearly_training[rowmatch,"pos"]
names <- yearly_training[rowmatch,"player"]
rownames(top10rankpoints) <- names
top10rankpoints$Position <- ifelse(top10rankpoints$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
       substr(top10rankpoints$Position,1,2), top10rankpoints$Position)

###Appeared atleast 5 weeks in the top 10
top10rankpoints <- top10rankpoints[top10rankpoints$rank>=10,]

x <- top10rankpoints[order(-top10rankpoints$rank),] # sort by mpg
x$Position <- factor(x$Position) # it must be a factor
x$color[x$Position=="PG"] <- "red"
x$color[x$Position=="SG"] <- "blue"
x$color[x$Position=="SF"] <- "green"
x$color[x$Position=="PF"] <- "orange"
x$color[x$Position=="C"] <- "purple"
dotchart(x$rank,labels=row.names(x),pch=19,cex=0.65, groups= x$Position,
         main="Appearances in Top 10 Points Scored Per Week\n 2015 and 2016 Seasons (Min 10 times)",
         xlab="# of Times", gcolor="black",color=x$color)



### Times Ranked in top 10 of Rebounds
library(dplyr)
rebounds %>% group_by(Week,year) %>% 
  mutate(rank = dense_rank(-TRB)) -> rank.rebounds

top10rebounds <- rank.rebounds[rank.rebounds$rank<=10,]

top10rankrebounds <-aggregate(rank ~ player_id, data=top10rebounds, FUN=length)

rowmatch <- match(top10rankrebounds$player_id,yearly_training$playerid)
top10rankrebounds$Position <- yearly_training[rowmatch,"pos"]
names <- yearly_training[rowmatch,"player"]
rownames(top10rankrebounds) <- names
top10rankrebounds$Position <- ifelse(top10rankrebounds$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                   substr(top10rankrebounds$Position,1,2), top10rankrebounds$Position)

###Appeared atleast 5 weeks in the top 10
top10rankrebounds <- top10rankrebounds[top10rankrebounds$rank>=10,]

x <- top10rankrebounds[order(-top10rankrebounds$rank),] # sort by mpg
x$Position <- factor(x$Position) # it must be a factor
x$color[x$Position=="PG"] <- "red"
x$color[x$Position=="SG"] <- "blue"
x$color[x$Position=="SF"] <- "green"
x$color[x$Position=="PF"] <- "orange"
x$color[x$Position=="C"] <- "purple"
dotchart(x$rank,labels=row.names(x),pch=19,cex=0.65, groups= x$Position,
         main="Appearances in Top 10 Rebounds Per Week\n 2015 and 2016 Seasons (Min 10 times)",
         xlab="# of Times", gcolor="black",color=x$color)




### Times Ranked in top 10 of Assists
library(dplyr)
assists %>% group_by(Week,year) %>% 
  mutate(rank = dense_rank(-AST)) -> rank.assists

top10assists <- rank.assists[rank.assists$rank<=10,]

top10rankassists <-aggregate(rank ~ player_id, data=top10assists, FUN=length)

rowmatch <- match(top10rankassists$player_id,yearly_training$playerid)
top10rankassists$Position <- yearly_training[rowmatch,"pos"]
names <- yearly_training[rowmatch,"player"]
rownames(top10rankassists) <- names
top10rankassists$Position <- ifelse(top10rankassists$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                     substr(top10rankassists$Position,1,2), top10rankassists$Position)

###Appeared atleast 5 weeks in the top 10
top10rankassists <- top10rankassists[top10rankassists$rank>=10,]

x <- top10rankassists[order(-top10rankassists$rank),] # sort by mpg
x$Position <- factor(x$Position) # it must be a factor
x$color[x$Position=="PG"] <- "red"
x$color[x$Position=="SG"] <- "blue"
x$color[x$Position=="SF"] <- "green"
x$color[x$Position=="PF"] <- "orange"
x$color[x$Position=="C"] <- "purple"
dotchart(x$rank,labels=row.names(x),pch=19,cex=0.65, groups= x$Position,
         main="Appearances in Top 10 Assists Per Week\n 2015 and 2016 Seasons (Min 10 times)",
         xlab="# of Times", gcolor="black",color=x$color)


### Times Ranked in top 10 of Steals
library(dplyr)
steals %>% group_by(Week,year) %>% 
  mutate(rank = dense_rank(-STL)) -> rank.steals

top10steals <- rank.steals[rank.steals$rank<=10,]

top10ranksteals <-aggregate(rank ~ player_id, data=top10steals, FUN=length)

rowmatch <- match(top10ranksteals$player_id,yearly_training$playerid)
top10ranksteals$Position <- yearly_training[rowmatch,"pos"]
names <- yearly_training[rowmatch,"player"]
rownames(top10ranksteals) <- names
top10ranksteals$Position <- ifelse(top10ranksteals$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                     substr(top10ranksteals$Position,1,2), top10ranksteals$Position)

###Appeared atleast 5 weeks in the top 10
top10ranksteals <- top10ranksteals[top10ranksteals$rank>=40,]

x <- top10ranksteals[order(-top10ranksteals$rank),] # sort by mpg
x$Position <- factor(x$Position) # it must be a factor
x$color[x$Position=="PG"] <- "red"
x$color[x$Position=="SG"] <- "blue"
x$color[x$Position=="SF"] <- "green"
x$color[x$Position=="PF"] <- "orange"
x$color[x$Position=="C"] <- "purple"
dotchart(x$rank,labels=row.names(x),pch=19,cex=0.65, groups= x$Position,
         main="Appearances in Top 10 Steals Per Week\n 2015 and 2016 Seasons (Min 40 times)",
         xlab="# of Times", gcolor="black",color=x$color)


### Times Ranked in top 10 of Blocks
library(dplyr)
blocks %>% group_by(Week,year) %>% 
  mutate(rank = dense_rank(-BLK)) -> rank.blocks

top10blocks <- rank.blocks[rank.blocks$rank<=10,]

top10rankblocks <-aggregate(rank ~ player_id, data=top10blocks, FUN=length)

rowmatch <- match(top10rankblocks$player_id,yearly_training$playerid)
top10rankblocks$Position <- yearly_training[rowmatch,"pos"]
names <- yearly_training[rowmatch,"player"]
rownames(top10rankblocks) <- names
top10rankblocks$Position <- ifelse(top10rankblocks$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                   substr(top10rankblocks$Position,1,2), top10rankblocks$Position)

###Appeared atleast 5 weeks in the top 10
top10rankblocks <- top10rankblocks[top10rankblocks$rank>=40,]

x <- top10rankblocks[order(-top10rankblocks$rank),] # sort by mpg
x$Position <- factor(x$Position) # it must be a factor
x$color[x$Position=="PG"] <- "red"
x$color[x$Position=="SG"] <- "blue"
x$color[x$Position=="SF"] <- "green"
x$color[x$Position=="PF"] <- "orange"
x$color[x$Position=="C"] <- "purple"
dotchart(x$rank,labels=row.names(x),pch=19,cex=0.65, groups= x$Position,
         main="Appearances in Top 10 Blocks Per Week\n 2015 and 2016 Seasons (Min 40 times)",
         xlab="# of Times", gcolor="black",color=x$color)


## Top 10 3pters
threepts %>% group_by(Week,year) %>% 
  mutate(rank = dense_rank(-X3P)) -> rank.threepts

top10threepts <- rank.threepts[rank.threepts$rank<=10,]

top10rankthreepts <-aggregate(rank ~ player_id, data=top10threepts, FUN=length)

rowmatch <- match(top10rankthreepts$player_id,yearly_training$playerid)
top10rankthreepts$Position <- yearly_training[rowmatch,"pos"]
names <- yearly_training[rowmatch,"player"]
rownames(top10rankthreepts) <- names
top10rankthreepts$Position <- ifelse(top10rankthreepts$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                   substr(top10rankthreepts$Position,1,2), top10rankthreepts$Position)

###Appeared atleast 5 weeks in the top 10
top10rankthreepts <- top10rankthreepts[top10rankthreepts$rank>=30,]

x <- top10rankthreepts[order(-top10rankthreepts$rank),] # sort by mpg
x$Position <- factor(x$Position) # it must be a factor
x$color[x$Position=="PG"] <- "red"
x$color[x$Position=="SG"] <- "blue"
x$color[x$Position=="SF"] <- "green"
x$color[x$Position=="PF"] <- "orange"
x$color[x$Position=="C"] <- "purple"
dotchart(x$rank,labels=row.names(x),pch=19,cex=0.65, groups= x$Position,
         main="Appearances in Top 10 3PT Made Per Week\n 2015 and 2016 Seasons (Min 30 times)",
         xlab="# of Times", gcolor="black",color=x$color)



corrtest <- cbind(points.plots$Points,points.plots$SD)

library(corrplot)
corrplot(cor(corrtest), method="square")

cor(corrtest)

sds <- list(weekly.sdavgpoints, weekly.sdavgblocks,weekly.sdavgrebounds, weekly.sdavgassists, 
            weekly.sdavgthreepoints, weekly.sdavgFT, weekly.sdavgsteals, weekly.sdavgMP)                                                               
avgs <- list(weekly.avgpoints, weekly.avgassists, weekly.avgblocks, weekly.avgrebounds,
             weekly.avgsteals, weekly.avgthreepts, weekly.avgFT, weekly.avgMP)
meds <- list(weekly.medpoints, weekly.medassists, weekly.medblocks, weekly.medrebounds,
             weekly.medsteals, weekly.medthreepts, weekly.medFT, weekly.medMP)


std.devs <- Reduce(function(x, y) merge(x, y, all=TRUE), sds)
rowmatch <- match(std.devs$player_id,yearly_training$playerid)
std.devs$Position <- yearly_training[rowmatch,"pos"]
std.devs$Position <- ifelse(std.devs$Position %in% c("PF-SF","SF-PF","PG-SG","SG-SF","SG-PG"),  
                                     substr(std.devs$Position,1,2), std.devs$Position)
colnames(std.devs) <- c("player_id","year", "sdPTS", "sdBLKS", "sdTRB", "sdAST", "sdX3P", "sdFT", "sdSTLS", "sdMP", "Position")

                                                                 
averages <- Reduce(function(x, y) merge(x, y, all=TRUE), avgs)
medians <- Reduce(function(x, y) merge(x, y, all=TRUE), meds)


#### Data to be used for Modeling ####
model.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(std.devs,averages))
model.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(std.devs,medians))
model.data$Position <- as.factor(model.data$Position)

write.csv(model.data, file="567 Model Data.csv")


### Model to Predict Points ### 
train_data <- model.data[model.data$year=="2015",]
row.names(train_data) <- train_data$player_id
train_data$player_id <- NULL

test_data <- model.data[model.data$year=="2016",]
row.names(test_data) <- test_data$player_id
test_data$player_id <- NULL

library(randomForest)
library(tree)
mod1 <- tree(PTS~.-year, data=train_data )
plot(mod1)

str(train_data)
