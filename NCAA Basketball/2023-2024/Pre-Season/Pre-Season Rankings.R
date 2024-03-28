### R Version 4.3.1 ###
source("./Functions.R")

### Read in Projection Data ###
# Bart Torvik Projections, KenPom Projections, Nolan ELO Projections
KP_proj2024.dat<-read.xlsx("./Data/2024 Preseason Projections.xlsx", sheet="KenPom")
TOR_proj2024.dat<-read.xlsx("./Data/2024 Preseason Projections.xlsx", sheet="Torvik")

# Power Rankings from teamrankings.com
url <- paste0("https://www.teamrankings.com/ncaa-basketball/ranking/predictive-by-other")
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table") %>% .[[1]]
table_df <- table_html %>% html_table()
head(table_df)
table_df$Team<-trimws(gsub("\\(0-0)", "", table_df$Team))

pred_PwrRk2024.dat<-table_df%>%
  select(Team, Rank, Rating)

# Last Years NET Rankings
EoS2023_NET.dat<-ncaa_mbb_NET_rankings()%>%
  select(rank, school)%>%
  mutate(Team=school)%>%
  mutate(rank2023=rank)%>%
  select(-c(school, rank))

## Align Team Names
#KP and TOR
KP_proj2024.dat$Team[which(!KP_proj2024.dat$Team %in% TOR_proj2024.dat$Team)]<-c("North Carolina St.",
                                                                                 "College of Charleston",
                                                                                 "Louisiana Lafayette",
                                                                                 "Detroit",
                                                                                 "Fort Wayne",
                                                                                 "LIU Brooklyn",
                                                                                 "St. Francis PA")
## Prior Years Net Rankings for Regression to Mean Calculation
#2023
url <- paste0("https://stats.ncaa.org/selection_rankings/nitty_gritties/31492")
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table")# %>% .[[1]]
table_df_2023 <- data.frame(table_html %>% html_table())
table_df_2023<-table_df_2023%>%
  select(Team, NET)%>%
  rename(NET_23 = NET)

#2022
url <- paste0("https://stats.ncaa.org/selection_rankings/nitty_gritties/25584")
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table")# %>% .[[1]]
table_df_2022 <- data.frame(table_html %>% html_table())
table_df_2022<-table_df_2022%>%
  select(Team, NET)%>%
  rename(NET_22 = NET)


#Join
table_df_all<-left_join(table_df_2023, table_df_2022, by="Team")
table_df_all<-na.omit(table_df_all)

#Determine Regression to mean
(coef<-(100*(1-cor(table_df_all$NET_23, table_df_all$NET_22))))
#Regress NET
EoS2023_NET.dat$Proj_NET<-(EoS2023_NET.dat$rank2023+coef*((mean(EoS2023_NET.dat$rank2023)-EoS2023_NET.dat$rank2023)/mean(EoS2023_NET.dat$rank2023)))

## Fix Names - NET and KP/TOR
EoS2023_NET.dat<-EoS2023_NET.dat[order(EoS2023_NET.dat$Team),]
replace_idx<-which(EoS2023_NET.dat$Team %in% sort(EoS2023_NET.dat$Team[which(!EoS2023_NET.dat$Team %in% KP_proj2024.dat$Team)]))
EoS2023_NET.dat$Team[replace_idx]<-c("Texas A&M Corpus Chris",
                                     "Alcorn St.",
                                     "Appalachian St.",
                                     "Arkansas Pine Bluff",
                                     "Army",
                                     "Bethune Cookman",
                                     "Boston University",
                                     "Cal Baptist",
                                     "Central Arkansas",
                                     "Central Connecticut",
                                     "Central Michigan",
                                     "Charleston Southern",
                                     "College of Charleston",
                                     "Cal St. Bakersfield",
                                     "Cal St. Northridge",
                                     "Detroit",
                                     "Eastern Illinois",
                                     "Eastern Kentucky",
                                     "Eastern Michigan",
                                     "Eastern Washington",
                                     "East Tennessee St.",
                                     "Fairleigh Dickinson",
                                     "Florida Gulf Coast",
                                     "Florida Atlantic",
                                     "Georgia Southern",
                                     "Gardner Webb",
                                     "Grambling St.",
                                     "Hartford",
                                     "UMKC",
                                     "Lamar",
                                     "LIU Brooklyn",
                                     "Loyola Marymount",
                                     "Louisiana Lafayette",
                                     "Loyola MD",
                                     "McNeese St.",
                                     "Miami FL",
                                     "Miami OH",
                                     "Middle Tennessee",
                                     "Mississippi Valley St.",
                                     "North Carolina A&T",
                                     "North Carolina Central",
                                     "North Carolina St.",
                                     "Nicholls St.",
                                     "Northern Illinois",
                                     "North Alabama",
                                     "Northern Arizona",
                                     "Northern Colorado",
                                     "Northern Kentucky",
                                     "Mississippi",
                                     "Nebraska Omaha",
                                     "Prairie View A&M",
                                     "Fort Wayne",
                                     "Queens",
                                     "St. Francis PA",
                                     "Saint Mary's",
                                     "Sam Houston St.",
                                     "Seattle",
                                     "Stephen F. Austin",
                                     "SIU Edwardsville",
                                     "South Florida",
                                     "Southeast Missouri St.",
                                     "Southeastern Louisiana",
                                     "USC",
                                     "Southern Illinois",
                                     "Southern Indiana",
                                     "Southern Miss",
                                     "Southern",
                                     "St. Francis Brooklyn",
                                     "St. John's",
                                     "St. Thomas",
                                     "Texas A&M Commerce",
                                     "Albany",
                                     "Connecticut",
                                     "Illinois Chicago",
                                     "Incarnate Word",
                                     "Louisiana Monroe",
                                     "Maryland Eastern Shore",
                                     "UNC Wilmington",
                                     "Northern Iowa",
                                     "Tennessee Martin",
                                     "UT Rio Grande Valley",
                                     "Western Carolina",
                                     "Western Illinois",
                                     "Western Kentucky",
                                     "Western Michigan")

## Fix Names - Pred PwrRk
pred_PwrRk2024.dat<-pred_PwrRk2024.dat[order(pred_PwrRk2024.dat$Team),]

pred_PwrRk2024.dat$Team<-gsub(" St", " St.", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("E ", "Eastern ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("TN ", "Tennessee ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("N ", "Northern ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("Fla ", "Florida ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("TX", "Texas", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("Geo ", "George ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("NC-", "North Carolina ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("NC ", "North Carolina ", pred_PwrRk2024.dat$Team) 
pred_PwrRk2024.dat$Team<-gsub("W ", "West ", pred_PwrRk2024.dat$Team)
pred_PwrRk2024.dat$Team<-gsub("S ", "South ", pred_PwrRk2024.dat$Team)
pred_PwrRk2024.dat$Team<-gsub("St.ate", "State", pred_PwrRk2024.dat$Team)
pred_PwrRk2024.dat$Team<-gsub("Miss ", "Mississippi ", pred_PwrRk2024.dat$Team)
pred_PwrRk2024.dat$Team<-gsub("State", "St.", pred_PwrRk2024.dat$Team)

pred_PwrRk2024.dat$Team[which(!pred_PwrRk2024.dat$Team %in% KP_proj2024.dat$Team)]

replace_idx<-which(pred_PwrRk2024.dat$Team %in% sort(pred_PwrRk2024.dat$Team[which(!pred_PwrRk2024.dat$Team %in% KP_proj2024.dat$Team)]))

pred_PwrRk2024.dat$Team[replace_idx]<-c("Abilene Christian","Alabama A&M", "Appalachian St.", "Little Rock", "Arkansas Pine Bluff", "Bethune Cookman",
    "Boston College", "Boston University", "Bowling Green", "Cal St. Northridge", "Central Arkansas", "Central Connecticut",
    "Central Michigan", "Charleston Southern", "The Citadel","Coastal Carolina", "College of Charleston","Cal St. Bakersfield",
    "Cal St. Fullerton", "East Carolina", "East Tennessee St.", "Eastern Washington","Fairleigh Dickinson", "Florida Gulf Coast",
    "FIU", "Georgia Southern","Georgia Tech", "Gardner Webb","George Washington","Grand Canyon","Houston Christian",
    "Illinois Chicago","Incarnate Word","Fort Wayne","James Madison", "Jacksonville St.", "UMKC","Louisiana Tech","Long Beach St.",
    "LIU Brooklyn","Louisiana Lafayette", "Loyola Chicago", "Loyola MD","Loyola Marymount","UMBC","Maryland Eastern Shore","UMass Lowell",
    "Miami FL","Miami OH","Middle Tennessee","Mississippi Valley St.", "Mount St. Mary's", "North Alabama","North Carolina","North Dakota St.",
    "North Florida","New Hampshire", "New Mexico St.","UNC Asheville", "UNC Greensboro", "UNC Wilmington","Nebraska Omaha",
    "Nicholls St.","Northeastern","Northwestern St.", "Prairie View A&M", "Robert Morris", "South Carolina St.","Southern Illinois",
    "Southern Indiana", "SMU","Southern Miss","Southern Utah", "Sacramento St.", "Sacred Heart", "Sam Houston St.", "USC Upstate",
    "Southeastern Louisiana","Southeast Missouri St.","SIU Edwardsville", "St. Bonaventure", "St. Francis PA", "St. John's", "Saint Joseph's",
    "Saint Mary's", "Saint Peter's", "Stephen F. Austin", "UT Arlington", "UT Rio Grande Valley", "Texas A&M Corpus Chris", "Texas A&M Commerce",
    "TCU", "UTEP", "Massachusetts", "Penn", "UC Santa Barbara", "UC San Diego", "Louisiana Monroe", "Virginia Tech","Western Carolina", 
    "Western Illinois", "Western Kentucky","Western Michigan", "Washington St.", "Green Bay", "Milwaukee", "William & Mary", "Youngstown St."
    )
pred_PwrRk2024.dat$Team[which(!pred_PwrRk2024.dat$Team %in% KP_proj2024.dat$Team)]

## Arrange Dataframes for Join
head(EoS2023_NET.dat)
EoS2023_NET.dat<-EoS2023_NET.dat[,c(3,1)]

head(KP_proj2024.dat)
KP_proj2024.dat<-KP_proj2024.dat[,c(2,1, 3:ncol(KP_proj2024.dat))]%>%
  rename(KenPom_proj = Rk)

head(TOR_proj2024.dat)
TOR_proj2024.dat<-TOR_proj2024.dat[,c(2,1, 3:ncol(TOR_proj2024.dat))]%>%
  rename(Torvik_proj = Rk)

head(pred_PwrRk2024.dat)
pred_PwrRk2024.dat<-pred_PwrRk2024.dat%>%
  rename(PwrRk_proj = Rank)%>%
  rename(PwrRating = Rating)


## Dealing with new or former DI schools
#Add LY LeMoyne
EoS2023_NET.dat<-rbind(EoS2023_NET.dat, data.frame(Team = c("Le Moyne"), Proj_NET=max(EoS2023_NET.dat$Proj_NET)+1))

## Join
full.proj.dat<-left_join(EoS2023_NET.dat, KP_proj2024.dat, by="Team")%>%
  left_join(TOR_proj2024.dat)%>%
  left_join(pred_PwrRk2024.dat)%>%
  select(Team, Proj_NET, KenPom_proj, Torvik_proj, PwrRk_proj)

## Create Average Ranking
str(full.proj.dat)
full.proj.dat$Torvik_proj<-as.numeric(full.proj.dat$Torvik_proj)
full.proj.dat$Aggregate_proj<-round(rowMeans(full.proj.dat[,-1]),2)

# Remove schools no longer in DI
full.proj.dat<-na.omit(full.proj.dat)%>%
  arrange(Aggregate_proj)

## Scale Then convert to ELO
scale_elo<-function(x){
  1500+(mean(x)-x)*(sd(x)/sd(x))
}

full.proj.dat$scaled_rating<-round(scale_elo(full.proj.dat$Aggregate_proj), 0)
#QC
sd(full.proj.dat$scaled_rating)
sd(full.proj.dat$Aggregate_proj)

## Home court Adjustment
names(hoopR::espn_mbb_scoreboard(2023))

Games_2023<-hoopR::load_mbb_schedule(2023)%>%
  filter(neutral_site == FALSE, conference_competition==TRUE)%>%
  select(home_location, away_location, home_winner)
Games_2022<-hoopR::load_mbb_schedule(2022)%>%
  filter(neutral_site == FALSE, conference_competition==TRUE)%>%
  select(home_location, away_location, home_winner)

Games_22_23<-rbind(Games_2023, Games_2022)
(home_adj<-(sum(Games_22_23$home_winner==TRUE)/nrow(Games_22_23))-0.5) #+9.1% for Home Team

full.proj.dat$home_adjustment<-c(home_adj, rep("",nrow(full.proj.dat)-1))

## Probability of Home Team Winning formula
# Pr(A) = (1/(10*(-EloDiff/400)+1))+1

temp.wb<-loadWorkbook('../Tools/Early Season NCAA MBB Prediction Tool.xlsx')
writeData(temp.wb, 'Data',full.proj.dat)

## Export
saveWorkbook(temp.wb, paste0('../Tools/Early Season NCAA MBB Prediction Tool - ',Sys.Date(),'.xlsx'), overwrite = T)
