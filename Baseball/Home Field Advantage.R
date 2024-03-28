library(httr)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
library(expss)
library(e1071)
library(caret)
library(XML)

#unzip('./Data/2024/gl2022.zip', exdir = "./Data/2024")
#unzip('./Data/2024/gl2023.zip', exdir = "./Data/2024")

##### Using last 2 years of games to calculate Home Field Advantage based on Home Win% compared to overall Win% ######
## Read in data
# 2022 Game Log
Games_2022<- read.table("./Data/2024/gl2022.txt", header=F, sep=",")
glnames<-c("Date", "Away", "Home", "Away_Score", "Home_Score", "DayorNight","Park_ID",
           "Attendence")
head(Games_2022)
# Get only the columns we need
Games2022.dat<-Games_2022[,c(1,4,7,10,11,13,17,18)]
# Set column names
names(Games2022.dat)<-glnames
head(Games2022.dat)

# 2023 Game Log
Games_2023<- read.table("./Data/2024/gl2023.txt", header=F, sep=",")
glnames<-c("Date", "Away", "Home", "Away_Score", "Home_Score", "DayorNight","Park_ID",
           "Attendence")
head(Games_2023)
# Get only the columns we need
Games2023.dat<-Games_2023[,c(1,4,7,10,11,13,17,18)]
# Set column names
names(Games2023.dat)<-glnames
head(Games2023.dat)

# Join to have 2 full years of data (Post-Covid)
Games.full<-rbind(Games2023.dat, Games2022.dat)

# Calculate if home team won or lost
Games.full$HomeWin<-ifelse(Games.full$Home_Score>Games.full$Away_Score,1,0)
Games.full$AwayWin<-ifelse(Games.full$Home_Score<Games.full$Away_Score,1,0)
head(Games.full)

## Name Parks ###
sort(unique(Games.full$Park_ID))
sort(unique(Games.full$Home))
# Set team corresponding to park to be ParkID
Games.full$Park_ID<-strtrim(Games.full$Park_ID,3)
#Ensure
xtabs(~Park_ID + Home, data=Games.full)
# Make sure names are equal
Games.full$Park_ID<-case_when(
  Games.full$Park_ID=='PHO' ~ 'ARI',
  Games.full$Park_ID=='ARL' ~ 'TEX',
  Games.full$Park_ID=='CHI' &  Games.full$Home == 'CHA' ~ 'CHA',
  Games.full$Park_ID=='CHI' &  Games.full$Home == 'CHN' ~ 'CHN',
  Games.full$Park_ID=='NYC' &  Games.full$Home == 'NYA' ~ 'NYA',
  Games.full$Park_ID=='NYC' &  Games.full$Home == 'NYN' ~ 'NYN',
  # Special Dyersville
  Games.full$Park_ID=='DYE' ~ 'C',
  T~Games.full$Park_ID
)
#Ensure
xtabs(~Park_ID + Home, data=Games.full)

#Ensure
xtabs(~Park_ID + Home, data=Games.full)

# Make Home team name and Park name same
Games.full$Park_ID<-Games.full$Home
xtabs(~Park_ID + Home, data=Games.full)

## Calculate Home Wins ##
Teams<-unique(Games.full$Home)
# Initiate data frame
records<-data.frame(Team=paste0(Teams),
                    HomeWins=rep(0),
                    HomeLosses=rep(0),
                    HomeWinningPct=rep(0),
                    AwayWins=rep(0),
                    AwayLosses=rep(0),
                    AwayWinningPct=rep(0),
                    TotalWinningPct=rep(0))
for(i in 1:length(Teams)){
  #i<-1
records[i,2]<-sum(Games.full$Home==Teams[i] & Games.full$HomeWin==1)
records[i,3]<-sum(Games.full$Home==Teams[i] & Games.full$AwayWin==1)
records[i,4]<-records[i,2]/sum(records[i,2],records[i,3])
records[i,5]<-sum(Games.full$Away==Teams[i] & Games.full$AwayWin==1)
records[i,6]<-sum(Games.full$Away==Teams[i] & Games.full$HomeWin==1)
records[i,7]<-records[i,5]/sum(records[i,5],records[i,6])
records[i,8]<-sum(records[i,2],records[i,5])/sum(records[i,2], records[i,3], records[i,5], records[i,6])
}
records

# Calculate Record Difference
records$HomeSplit<-records$HomeWinningPct-records$TotalWinningPct
records$RelativeDiff<-(records$HomeWinningPct-records$TotalWinningPct)/records$TotalWinningPct
##* Note - Relative diff will be used when calculating win probability for modeling**

#### Investigate ####
histogram(records$HomeSplit)
histogram(records$RelativeDiff) 
qqnorm(records$RelativeDiff) 
qqnorm(records$HomeSplit)

mean(records$HomeSplit) #0.027 increase in WP when at home
mean(records$RelativeDiff)*100 #5.52% greater chance to win

#Best Teams
records[which(records$HomeSplit==max(records$HomeSplit)),] #Colorado = +0.90 WP
records[which(records$RelativeDiff==max(records$RelativeDiff)),] #Colorado = +22.8% Chance to win **Colorado is the Outlier**

#Worst
records[which(records$HomeSplit==min(records$HomeSplit)),] #HOUSTON = -0.025 WP
records[which(records$RelativeDiff==min(records$RelativeDiff)),] #WASHINGTON = -4.88% Chance to win

##P Print file
write.csv(records,'./Data/2024/Home Field Advantage - Raw.csv', row.names=F)
