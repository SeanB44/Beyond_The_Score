library(httr)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
library(expss)
library(e1071)
library(caret)
library(XML)
library(xml2)
library(rvest)
library(readxl)

### Read in Data ###
## 2023 EOY ELO ratings
elo.data <- read.csv("./Data/2024/mlb-elo-latest.csv")%>%
  filter(season==2023)%>%
  mutate(playoff = ifelse(is.na(playoff) | playoff == "", "Reg",  "Post"))

elo.data<-elo.data%>%
  filter(gm_no1==160)%>% #Identify the team's elo at game 150 (last 12 games excluded due to teams not playing to ability (resting players, "giving up" etc.))
  arrange(team1)%>%
  distinct()%>%
  select(season, team1, elo1_post)

head(elo.data)

## 2024 PECOTA Projections
pecota.data<-read_xlsx("./Data/2024/PECOTA_Proj.xlsx", sheet=1)
head(pecota.data)

## 2024 FanGraph's Team WAR Projections
fg.war.data<-read_xlsx("./Data/2024/FG_TeamWAR_Proj.xlsx", sheet=1)
head(fg.war.data)

## Davenport Projections
dav.data<-read_xlsx("./Data/2024/Davenport_Proj.xlsx", sheet=1)
head(dav.data)

## Home Field Advantage
park.data<-read.csv("./Data/2024/Home Field Advantage - Raw.csv",check.names = F)
head(park.data)

## Make names all match
teamNames<-sort(unique(pecota.data$Team))
teamNames

# Match Elo names
elo.data$team1[which(!elo.data$team1%in%teamNames)]

elo.data$team1<-case_when(
  elo.data$team1=="FLA"~"MIA",
  elo.data$team1=="ANA"~"LAA",
  elo.data$team1=="WSN"~"WAS",
  elo.data$team1=="TBD"~"TBR",
  TRUE~elo.data$team1
)
elo.data$team1[which(!elo.data$team1%in%teamNames)]

# Match fg.war names
fg.war.data$Team[which(!fg.war.data$Team%in%teamNames)]

# Match fg.war names
park.data$Team[which(!park.data$Team%in%teamNames)]

park.data$Team<-case_when(
  park.data$Team=="CHN"~"CHC",
  park.data$Team=="LAN"~"LAD",
  park.data$Team=="SDN"~"SDP",
  park.data$Team=="SLN"~"STL",
  park.data$Team=="KCA"~"KCR",
  park.data$Team=="NYA"~"NYY",
  park.data$Team=="TBA"~"TBR",
  park.data$Team=="CHA"~"CHW",
  park.data$Team=="NYN"~"NYM",
  park.data$Team=="SFN"~"SFG",
  park.data$Team=="ANA"~"LAA",
  TRUE~park.data$Team
)
park.data$Team[which(!park.data$Team%in%teamNames)]

# Match davenport names
dav.data$Team[which(!dav.data$Team%in%teamNames)]

dav.data$Team<-case_when(
  dav.data$Team=="TBD"~"TBR",
  TRUE~dav.data$Team
)
dav.data$Team[which(!dav.data$Team%in%teamNames)]

#### Join Data Frames ####
elo.data$season<-NULL
names(elo.data)[1]<-"Team"

full.raw.dat<-left_join(elo.data, fg.war.data,by="Team")%>%
  left_join(., pecota.data, by="Team")%>%
  left_join(., dav.data, by="Team")%>%
  left_join(., park.data, by="Team")

# Select only needed variables
raw.dat<-full.raw.dat%>%
  mutate(Dav_Projected_WP = Wins/(Wins+Losses))%>%
  select(Team,elo1_post,WAR, Sim_WP,Dav_Projected_WP, RelativeDiff )%>%
  rename("Home_Field_Advantage"="RelativeDiff")

head(raw.dat)

## Calculate z-scores
scaled.dat<-raw.dat
scaled.dat[,-c(1,ncol(scaled.dat))]<-apply(raw.dat[,2:(ncol(raw.dat)-1)],2,function(x) scale(x))
colMeans(scaled.dat[,-1]) #Should be near 0

## Average to get final rating
names(scaled.dat)
ratings.dat<-data.frame(Team = scaled.dat$Team, 
                        Rating = rowMeans(scaled.dat[,-c(1, ncol(scaled.dat))]))

#Scale to Elo Range
mean(elo.data$elo1_post) #1505
sd(elo.data$elo1_post)

ratings.dat$Elo_Rating<-(ratings.dat$Rating*sd(raw.dat$elo1_post)+1505)
mean(raw.dat$elo1_post)
mean(ratings.dat$Elo_Rating)
sd(raw.dat$elo1_post)
sd(ratings.dat$Elo_Rating)

ratings.dat[order(ratings.dat$Elo_Rating,decreasing = T),]
elo.data[order(elo.data$elo1_post,decreasing = T),]

# Join ratings with park data
ratings.data<-left_join(ratings.dat, scaled.dat[,c(1,ncol(scaled.dat))], by="Team")
## Final rating data
ratings.data

#### Forecast 2024 ####
## Load in 2024 Schedule
library(baseballr)
schedule.2024<-baseballr::mlb_schedule(2024)
names(schedule.2024)

schedule.2024<-schedule.2024%>%
  select(date, teams_home_team_name, teams_away_team_name,game_type)
unique(schedule.2024$teams_home_team_name)

# Align names
schedule.2024$home_team<-case_when(
  schedule.2024$teams_home_team_name=="San Diego Padres"~"SDP",
  schedule.2024$teams_home_team_name=="Boston Red Sox"~"BOS",
  schedule.2024$teams_home_team_name=="Texas Rangers"~"TEX",
  schedule.2024$teams_home_team_name=="Chicago Cubs"~"CHC",
  schedule.2024$teams_home_team_name=="Cleveland Guardians"~"CLE",
  schedule.2024$teams_home_team_name=="Detroit Tigers"~"DET",
  schedule.2024$teams_home_team_name=="Kansas City Royals"~"KCR",
  schedule.2024$teams_home_team_name=="Los Angeles Angels"~"LAA",
  schedule.2024$teams_home_team_name=="Minnesota Twins"~"MIN",
  schedule.2024$teams_home_team_name=="New York Yankees"~"NYY",
  schedule.2024$teams_home_team_name=="Oakland Athletics"~"OAK",
  schedule.2024$teams_home_team_name=="Seattle Mariners"~"SEA",
  schedule.2024$teams_home_team_name=="Tampa Bay Rays"~"TBR",
  schedule.2024$teams_home_team_name=="Toronto Blue Jays"~"TOR",
  schedule.2024$teams_home_team_name=="Arizona Diamondbacks"~"ARI",
  schedule.2024$teams_home_team_name=="Atlanta Braves"~"ATL",
  schedule.2024$teams_home_team_name=="Baltimore Orioles"~"BAL",
  schedule.2024$teams_home_team_name=="Chicago White Sox"~"CHW",
  schedule.2024$teams_home_team_name=="Cincinnati Reds"~"CIN",
  schedule.2024$teams_home_team_name=="Colorado Rockies"~"COL",
  schedule.2024$teams_home_team_name=="Houston Astros"~"HOU",
  schedule.2024$teams_home_team_name=="Los Angeles Dodgers"~"LAD",
  schedule.2024$teams_home_team_name=="Miami Marlins"~"MIA",
  schedule.2024$teams_home_team_name=="Milwaukee Brewers"~"MIL",
  schedule.2024$teams_home_team_name=="New York Mets"~"NYM",
  schedule.2024$teams_home_team_name=="Philadelphia Phillies"~"PHI",
  schedule.2024$teams_home_team_name=="Pittsburgh Pirates"~"PIT",
  schedule.2024$teams_home_team_name=="San Francisco Giants"~"SFG",
  schedule.2024$teams_home_team_name=="St. Louis Cardinals"~"STL",
  schedule.2024$teams_home_team_name=="Washington Nationals"~"WAS",
  TRUE~schedule.2024$teams_home_team_name
)

schedule.2024$away_team<-case_when(
  schedule.2024$teams_away_team_name=="San Diego Padres"~"SDP",
  schedule.2024$teams_away_team_name=="Boston Red Sox"~"BOS",
  schedule.2024$teams_away_team_name=="Texas Rangers"~"TEX",
  schedule.2024$teams_away_team_name=="Chicago Cubs"~"CHC",
  schedule.2024$teams_away_team_name=="Cleveland Guardians"~"CLE",
  schedule.2024$teams_away_team_name=="Detroit Tigers"~"DET",
  schedule.2024$teams_away_team_name=="Kansas City Royals"~"KCR",
  schedule.2024$teams_away_team_name=="Los Angeles Angels"~"LAA",
  schedule.2024$teams_away_team_name=="Minnesota Twins"~"MIN",
  schedule.2024$teams_away_team_name=="New York Yankees"~"NYY",
  schedule.2024$teams_away_team_name=="Oakland Athletics"~"OAK",
  schedule.2024$teams_away_team_name=="Seattle Mariners"~"SEA",
  schedule.2024$teams_away_team_name=="Tampa Bay Rays"~"TBR",
  schedule.2024$teams_away_team_name=="Toronto Blue Jays"~"TOR",
  schedule.2024$teams_away_team_name=="Arizona Diamondbacks"~"ARI",
  schedule.2024$teams_away_team_name=="Atlanta Braves"~"ATL",
  schedule.2024$teams_away_team_name=="Baltimore Orioles"~"BAL",
  schedule.2024$teams_away_team_name=="Chicago White Sox"~"CHW",
  schedule.2024$teams_away_team_name=="Cincinnati Reds"~"CIN",
  schedule.2024$teams_away_team_name=="Colorado Rockies"~"COL",
  schedule.2024$teams_away_team_name=="Houston Astros"~"HOU",
  schedule.2024$teams_away_team_name=="Los Angeles Dodgers"~"LAD",
  schedule.2024$teams_away_team_name=="Miami Marlins"~"MIA",
  schedule.2024$teams_away_team_name=="Milwaukee Brewers"~"MIL",
  schedule.2024$teams_away_team_name=="New York Mets"~"NYM",
  schedule.2024$teams_away_team_name=="Philadelphia Phillies"~"PHI",
  schedule.2024$teams_away_team_name=="Pittsburgh Pirates"~"PIT",
  schedule.2024$teams_away_team_name=="San Francisco Giants"~"SFG",
  schedule.2024$teams_away_team_name=="St. Louis Cardinals"~"STL",
  schedule.2024$teams_away_team_name=="Washington Nationals"~"WAS",
  TRUE~schedule.2024$teams_away_team_name
)

unique(schedule.2024$home_team)[!unique(schedule.2024$home_team) %in% ratings.dat$Team]
unique(schedule.2024$away_team)[!unique(schedule.2024$away_team) %in% ratings.dat$Team]

schedule.2024<-schedule.2024%>%
  filter(game_type=="R")%>% #Get just regular season games
  select(date, home_team, away_team)

schedule.2024$Result<-NA
# QC schedule shows correct number of games for each team
apply(schedule.2024[,-1],2,table, useNA = "always")

schedule.2024$home_rating<-ratings.data$Elo_Rating[match(schedule.2024$home_team, ratings.data$Team)]
schedule.2024$away_rating<-ratings.data$Elo_Rating[match(schedule.2024$away_team, ratings.data$Team)]

schedule.2024$home_field_adv<-ratings.data$Home_Field_Adv[match(schedule.2024$home_team, ratings.data$Team)]
head(schedule.2024)
schedule.2024<-schedule.2024[,c(1:3,5:7,4)]

## Monte Carlo Simulation
# 1) Simulate the season 10,000 times
# 2) Use the results to calculate the average number of wins for each team

## Function to simulate game
# Ha = Home win probability
# g = game number

sim_game<-function(Ha, g){
  if(runif(1)<Ha){
    return(c(schedule.2024$home_team[g],
             schedule.2024$Result[g]<-1))
    schedule.2024$Winner[g]<-schedule.2024$home_team[g]
    schedule.2024$Loser[g]<-schedule.2024$away_team[g]
  }else{
    return(c(schedule.2024$away_team[g],
             schedule.2024$Result[g]<-0))
    schedule.2024$Winner[g]<-schedule.2024$away_team[g]
    schedule.2024$Loser[g]<-schedule.2024$home_team[g]
  }
}

schedule.2024$Winner<-""
schedule.2024$Loser<-""

season.list<-list()

# Initiate data frame to save simulated games
simulated.results<-data.frame("Team" = unique(schedule.2024$home_team))

## Export Elo Ratings
library(openxlsx)
wb<-createWorkbook()
addWorksheet(wb, "Elo Ratings")
addWorksheet(wb, "Schedule")
writeData(wb, "Schedule", schedule.2024)
elo.dat.final<-schedule.2024%>%
  select(home_team, home_rating)%>%
  group_by(home_team)%>%
  arrange(desc(home_rating))%>%
  rename("Team" = "home_team", "Elo Rating" = "home_rating")%>%
  distinct()

writeData(wb, "Elo Ratings", elo.dat.final)
# Save Workbook
saveWorkbook(wb, paste0("./Output/2024 ","Team Elo Ratings - ",Sys.Date(), ".xlsx"), overwrite=TRUE)

# Set number of simulations
num.sims<-10000

# Ha = Home win prob
# Aa = Away win prob
# g = game
for(j in 1:num.sims){
  
  schedule.2024.temp<-schedule.2024
  
  for(g in 1:nrow(schedule.2024.temp)){
    #g<-1
    (Ha<-1/(1+10^(((schedule.2024.temp$away_rating[g]-schedule.2024.temp$home_rating[g]))/400))+(.5*schedule.2024.temp$home_field_adv[g])) #Calc HOme win prob from Elo and add Home field advantage
    (Aa<-1/(1+10^((schedule.2024.temp$home_rating[g]-schedule.2024.temp$away_rating[g])/400)))-(.5*schedule.2024.temp$home_field_adv[g]) #Calc away win prb from Elo and subtract home field advantage
    
    #Sim game and save result
    schedule.2024.temp$Result[g]<-as.numeric(sim_game(Ha, g)[2])
    schedule.2024.temp$Winner[g]<-sim_game(Ha, g)[1]
    schedule.2024.temp$Loser[g]<-ifelse(schedule.2024.temp$Result[g]==1, schedule.2024.temp$away_team[g], schedule.2024.temp$home_team[g])
    
    #Adjust Elo for home team
    schedule.2024.temp$home_rating[which(schedule.2024.temp$home_team==schedule.2024.temp$home_team[g])]<-schedule.2024.temp$home_rating[g]+15*(ifelse(schedule.2024.temp$Result[g]==1, 1, 0)-Ha)
    schedule.2024.temp$away_rating[which(schedule.2024.temp$away_team==schedule.2024.temp$home_team[g])]<-schedule.2024.temp$home_rating[g]+15*(ifelse(schedule.2024.temp$Result[g]==1, 1, 0)-Ha)
    
    #Adjust Elo for Away team
    schedule.2024.temp$away_rating[which(schedule.2024.temp$away_team==schedule.2024.temp$away_team[g])]<-schedule.2024.temp$away_rating[g]+15*(ifelse(schedule.2024.temp$Result[g]==1, 0, 1)-Aa)
    schedule.2024.temp$home_rating[which(schedule.2024.temp$home_team==schedule.2024.temp$away_team[g])]<-schedule.2024.temp$away_rating[g]+15*(ifelse(schedule.2024.temp$Result[g]==1, 0, 1)-Aa)
    
  }
  
  sim.dat<-data.frame(aggregate(Result~Winner, data=schedule.2024.temp, FUN=length))%>%rename("Team" = "Winner")
  names(sim.dat)[2]<-paste0("Wins_",j)
  simulated.results<-left_join(simulated.results, sim.dat, by = "Team")
}

## Calculate final Win Totals by Averaging together simulated seasons
num.sims<-ncol(simulated.results)-1
num.sims #Stopped it early as it was taking VERY long to run - 8,555 simulations - Will use 5,000 for final results
num.sims<-5000
simulated.results$Projected_Wins<-rowMeans(simulated.results[,2:num.sims+1])
simulated.results[order(simulated.results$Projected_Wins, decreasing=T),c(1, ncol(simulated.results))]

## Calculate projected Losses
simulated.results$Projected_Losses<-162-simulated.results$Projected_Wins

## Calculate Win Percentage
simulated.results$Projected_WP<-simulated.results$Projected_Wins/162
simulated.results$Projected_WP<-round(simulated.results$Projected_WP,3)
simulated.results[order(simulated.results$Projected_WP, decreasing=T),c(1, ncol(simulated.results))]
simulations.dat<-simulated.results
## Export to Excel
library(openxlsx)
#write.xlsx(simulations.dat, "./Output/Raw Simulations - 2024 (Pre-season).xlsx", rowNames = F)
## Export to Excel
Preseason.projections.out<-simulated.results%>%
  select(Team, Projected_Wins, Projected_Losses, Projected_WP)
(Preseason.projections.out<-Preseason.projections.out%>%
  arrange(desc(Projected_Wins)))

Preseason.projections.out$Projected_Wins<-round(Preseason.projections.out$Projected_Wins,1)
Preseason.projections.out$Projected_Losses<-round(Preseason.projections.out$Projected_Losses,1)

#write.xlsx(Preseason.projections.out, paste0("./Output/MLB 2024 Season Projections - ", Sys.Date(), ".xlsx"), rowNames = F)

## Post-hoc checks of convergence
library(openxlsx)
raw.sims.dat<-read.xlsx("./Output/Raw Simulations - 2024 (Pre-season).xlsx", sheet = 2, colNames = T) #get cumulative average page
raw.sims.dat<-data.frame(raw.sims.dat, check.names = F)
str(raw.sims.dat)
# Variance in win totals
raw.sims.dat$SD<-apply(raw.sims.dat[,2:ncol(raw.sims.dat)],1,sd)

sim.variance.dat<-raw.sims.dat%>%
  select("Team", "SD")%>%
  arrange(desc(SD))
sim.variance.dat

plot(sim.variance.dat[,2], type="l", col=c("blue", "red"), xlab="Team", ylab="Standard Deviation in Wins", main="Standar Deviation in Wins by Team")

# Export to Excel
write.xlsx(sim.variance.dat, paste0("./Output/MLB 2024 Season Projections - Variance in Wins - ", Sys.Date(), ".xlsx"), rowNames = F)
