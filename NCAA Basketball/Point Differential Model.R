source('./Functions.R')

####### PREDICTING POINT DIFFERENTIAL #####
### Load Data ###
raw.dat<-read.xlsx('../Data/NCAA BB Data.xlsx', sheet = "Data", check.names = FALSE, sep.names="_")
raw.diff.dat<-read.xlsx('../Data/NCAA BB Data.xlsx', sheet = "Data Diff", check.names = FALSE, sep.names="_")

## Data Cleaning
# Initial cleaning of unneeded variables
names(raw.dat)
raw.dat<-raw.dat[-c(1, 3, 5, 7:22, 149)]%>%
  mutate(Result = ifelse(Final_H >Final_A, 1, 0))%>%
  na.omit()

names(raw.diff.dat)
raw.diff.dat<-raw.diff.dat[, -c(1,3,5,7:18)]%>%
  mutate(Winner = ifelse(Final_H >Final_A, 1, 0))%>%
  rename(Result = Winner)%>% #1 = Home Win, 0 = Away Win
  na.omit()

#### Looking a Difference Data - predicting difference ######
names(raw.diff.dat)
diff.dat<-raw.diff.dat%>% #Don't need total right now
  mutate(Difference = Final_H-Final_A)%>%
  dplyr::select(-c('Final_H', 'Final_A', 'Total'))
names(diff.dat)

# Arrange future DV's for future
diff.dat<-diff.dat[, c(1:(ncol(diff.dat)-3), ncol(diff.dat), ncol(diff.dat)-1)]
all(diff.dat$difference == raw.dat$Diff) #QC

# QC Dataframes match
all(raw.dat$Result==diff.dat$Result)
all(raw.dat$Diff==diff.dat$Difference)

# Remove missing data from some rankings
removals<-which(raw.dat$H_BPI==0 | raw.dat$H_SOR==0 | raw.dat$A_BPI==0 | raw.dat$A_SOR==0)

raw.dat<-raw.dat[-removals,]
diff.dat<-diff.dat[-removals,]

# QC Row Removal
all(raw.dat$Diff==diff.dat$Difference)

# Give game index for tracking and reference
diff.dat$Game_ID<-1:nrow(diff.dat)
raw.dat$Game_ID<-1:nrow(raw.dat)

diff.dat<-diff.dat[, c(ncol(diff.dat), 1:(ncol(diff.dat)-1))]
raw.dat<-raw.dat[, c(ncol(raw.dat), 1:(ncol(raw.dat)-1))]

# Make 'Neutral' binary 1 - 0
diff.dat<-diff.dat%>%
  mutate(Neutral = ifelse(Neutral =="Y", 1, 0))
table(diff.dat$Neutral, useNA = "always")

raw.dat<-raw.dat%>%
  mutate(Neutral = ifelse(Neutral =="Y", 1, 0))
table(raw.dat$Neutral, useNA = "always")

# Remove Neutral due to poor distribution
raw.dat$Neutral<-diff.dat$Neutral<-NULL

################ ONLY USING DIFFERENCE DATA FROM HERE OUT ######################

## Removal - round 1
# Points_Tm, Points_Opp WAB, Totals_STL, Totals_TOV, Totals_AST, Totals_3PA, Totals_FTA,
# Totals_BLK, Totals_PF, ORB_PG, TRB_PG, Totals_FGA (used to calculate metrics)
# RPI (Repeat of RPI Rank)
diff.dat<-diff.dat%>%
  dplyr::select(-c("Points_Tm", "Points_Opp", "WAB", "Totals_STL", "Totals_TOV", "Totals_AST", "Totals_3PA","Totals_FGA",
            "Totals_FTA", "Totals_BLK", "Totals_PF", "Totals_ORB", "Totals_TRB", "RPI", "Totals_3P","Totals_FT"))
names(diff.dat)

## Removing interactins - may look into for next year
diff.dat[,47:51]<-NULL

## Multicollinearity and Correlations ##
# Check for Multicollinearity
mc_check <- lm(Difference ~ ., data = diff.dat[, -c(1:3, ncol(diff.dat))])
vif(mc_check)
# Calculate correlation matrix
cor_matrix <- cor(diff.dat[,-c(1:3, ncol(diff.dat))])
# Check for correlation coefficients
high_correlation_pairs <- which(cor_matrix > 0.8 & cor_matrix < 1, arr.ind = TRUE)
# Check eigenvalues
eigenvalues <- eigen(cor_matrix)$values

sort(vif(mc_check), decreasing = T)
print(high_correlation_pairs)
print(eigenvalues)


sort(vif(mc_check), decreasing = T)
sort(high_correlation_pairs[,1])
table(high_correlation_pairs[,1])
print(eigenvalues)

## Check if RPI Rank or RPI Rating is better
cor_matrix[which(row.names(cor_matrix)=="RPI_Rank"|row.names(cor_matrix)=="RPI_Rating"),]
rpi_rating<-lm(Difference ~ RPI_Rating, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(rpi_rating)
rpi_rank<-lm(Difference ~ RPI_Rank, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(rpi_rank) #Keep RPI_Rating
# Remove RPI_Rank


## Check if Elo (raw ELO) or ELO (ELO Rank) is better
# *NOTE: 'Elo' has one of the higher number of High Corr pairs
cor_matrix[which(row.names(cor_matrix)=="ELO"|row.names(cor_matrix)=="Elo"),]
Elo_rating<-lm(Difference ~ Elo, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(Elo_rating) #Keep Elo
ELO_rank<-lm(Difference ~ ELO, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(ELO_rank)
# Remove ELO

## Check which Efficiency metrics are better
cor_matrix[which(row.names(cor_matrix)=="AdjO"|row.names(cor_matrix)=="AdjOE"),]
AdjO<-lm(Difference ~ AdjO, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(AdjO)
AdjOE<-lm(Difference ~ AdjOE, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(AdjOE) 

cor_matrix[which(row.names(cor_matrix)=="AdjD"|row.names(cor_matrix)=="AdjDE"),]
AdjD<-lm(Difference ~ AdjD, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(AdjD)
AdjDE<-lm(Difference ~ AdjDE, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(AdjDE) 
## AdjO and AdjD are slightly better

## Second Round of Removals
diff.dat<-diff.dat%>%
  dplyr::select(-c("ELO", "RPI_Rank", "AdjOE", "AdjDE"))
names(diff.dat)

# Repeat Check for Multicollinearity
mc_check <- lm(Difference ~ ., data = diff.dat[, -c(1:3, ncol(diff.dat))])
vif(mc_check)
# Calculate correlation matrix
cor_matrix <- cor(diff.dat[, -c(1:3, ncol(diff.dat))])
# Check for correlation coefficients
high_correlation_pairs <- which(cor_matrix > 0.8 & cor_matrix < 1, arr.ind = TRUE)
# Check eigenvalues
eigenvalues <- eigen(cor_matrix)$values

sort(vif(mc_check), decreasing = T)
print(high_correlation_pairs)
print(eigenvalues)
print(cor_matrix)

## Check variables with very low correlation with Difference
sort(abs(cor_matrix["Difference",]), decreasing = T)
Luck_check <- lm(Difference ~ Luck, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(Luck_check)
Tempo_check <- lm(Difference ~ AdjT, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(Tempo_check)
RPI_Delta_check <- lm(Difference ~ RPI_Delta, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(RPI_Delta_check)
PF_PG_check <- lm(Difference ~ PF_PG, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(PF_PG_check)
ORB_PG_check <- lm(Difference ~ ORB_PG, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(ORB_PG_check) #*Significant
# Remove AdjT, RPI_Delta, Luck, PF_PG, for very low correlation to Difference (<10%)

## Third Round of Removals
diff.dat<-diff.dat%>%
  dplyr::select(-c("AdjT", "RPI_Delta", "Luck", "PF_PG"))
names(diff.dat)


## AdjEM and POM are essentially same thing 

temp.AjdEM<-lm(Difference ~ AdjEM, data = diff.dat[, -c(1:3, ncol(diff.dat))])
temp.POM<-lm(Difference ~ POM, data = diff.dat[, -c(1:3, ncol(diff.dat))])
summary(temp.AjdEM)
summary(temp.POM)

diff.dat$POM<-NULL

## Look at PPG and PPG_Diff
PPG.dat<-diff.dat%>%
  dplyr::select("Difference", "PPG", "PPG_Diff")

PPG.lm<-lm(Difference ~ PPG + PPG_Diff, data = PPG.dat)
summary(PPG.lm)
PPG.lm<-lm(Difference ~ PPG, data = PPG.dat)
summary(PPG.lm)
PPG_Diff.lm<-lm(Difference ~ PPG_Diff, data = PPG.dat)
summary(PPG_Diff.lm)
PPGxPPG_Diff.lm<-lm(Difference ~ PPG * PPG_Diff, data = PPG.dat)
summary(PPGxPPG_Diff.lm)

crf <- calc.relimp(Difference~.,PPG.dat, type = c("lmg"), rela = TRUE )
sort(crf$lmg, decreasing = T)

## Drop PPG
diff.dat$PPG<-NULL

## Investigate SOS variables
sort(cor_matrix["Strength_of_Schedule_AdjEM",])
sort(cor_matrix["Strength_of_Schedule_OppO",])

sos.dat<-diff.dat%>%
  dplyr::select("Difference", "Strength_of_Schedule_AdjEM", "Strength_of_Schedule_OppO",
         "Strength_of_Schedule_OppD","Overall_SOS")
SOS.lm<-lm(Difference ~ Strength_of_Schedule_AdjEM + Strength_of_Schedule_OppO + Strength_of_Schedule_OppD + Overall_SOS, data = sos.dat)
summary(SOS.lm)
SOSadjem.lm<-lm(Difference ~ Strength_of_Schedule_AdjEM, data = sos.dat)
summary(SOSadjem.lm)

Oppo.lm<-lm(Difference ~ Strength_of_Schedule_OppO, data = sos.dat)
summary(Oppo.lm)

OppD.lm<-lm(Difference ~ Strength_of_Schedule_OppD, data = sos.dat)
summary(OppD.lm)

Osos.lm<-lm(Difference ~ Overall_SOS, data = sos.dat)
summary(Osos.lm)

crf <- calc.relimp(Difference~.,sos.dat, type = c("lmg"), rela = TRUE )
sort(crf$lmg, decreasing = T)

cor_matrix["Difference",]
# Only keep overall SOS
diff.dat$Strength_of_Schedule_AdjEM<-diff.dat$Strength_of_Schedule_OppO<-diff.dat$Strength_of_Schedule_OppD<-NULL

#Look at the Adjusted effeciency metrics
AdjEs.lm<-lm(Difference ~ AdjEM, data = diff.dat)
summary(AdjEs.lm)

crf<-calc.relimp(Difference~AdjEM + AdjO + AdjD, data = diff.dat, type = c("lmg"), rela = TRUE )
sort(crf$lmg, decreasing = T)

AdjEs.lm<-lm(Difference ~ AdjEM+AdjO+AdjD, data = diff.dat)
summary(AdjEs.lm)

# Keep all 

# ## Lots of STL and TOV variables
# stl.tov.lm<-lm(Difference~STL_PG + TOV_PG + STL_TOV_Ratio, diff.dat[, -c(1:3, ncol(diff.dat))])
# summary(stl.tov.lm)
# 
# stl.tov.step.1<-step(stl.tov.lm, direction = "backward", k=2)
# summary(stl.tov.step.1)
# 
# stl.tov.step.2<-step(stl.tov.lm, direction = "forward", k=2)
# summary(stl.tov.step.2)
# 
# crf<-calc.relimp(Difference~STL_PG + TOV_PG + STL_TOV_Ratio + H_TOVxA_STL + A_TOVxH_STL, data = diff.dat, type = c("lmg"), rela = TRUE )
# sort(crf$lmg, decreasing = T)
# 
# vif(stl.tov.step.1)
# 
# stl.tov.full.BSS <-regsubsets(Difference~STL_PG + TOV_PG + STL_TOV_Ratio + H_TOVxA_STL + A_TOVxH_STL, diff.dat[, 4:(ncol(diff.dat)-1)], nvmax=5)#, force.in = c("Neutral","AdjEM"))
# (reg.summary.stl.tov.full<-summary(stl.tov.full.BSS ))
# 
# which.min(reg.summary.stl.tov.full$cp) #Best-4
# (mod1.best.cp<-names(which(reg.summary.stl.tov.full$which[4,]==TRUE)))
# min(reg.summary.stl.tov.full$cp) #4
# plot(reg.summary.stl.tov.full$cp, xlab = "Number of Variables",
#      ylab = "Mallow's cp", type = "l")
# 
# 
# which.min(reg.summary.stl.tov.full$bic) #Best-2
# min(reg.summary.stl.tov.full$bic) #-44
# (mod1.best.bic<-names(which(reg.summary.stl.tov.full$which[2,]==TRUE)))
# plot(reg.summary.stl.tov.full$bic, xlab = "Number of Variables",
#      ylab = "Bayesian Information Criterion (BIC)", type = "l")
# 
# which.max(reg.summary.stl.tov.full$adjr2) #Best-20
# (mod1.best.adjr2<-names(which(reg.summary.stl.tov.full$which[4,]==TRUE)))
# max(reg.summary.stl.tov.full$adjr2) 
# reg.summary.stl.tov.full$adjr2[4]
# plot(reg.summary.stl.tov.full$adjr2, xlab = "Number of Variables",
#      ylab = "Adjusted RSq", type = "l")
# 
# ## Examine what variables are best ##
# mod1.best.cp
# mod1.best.bic
# mod1.best.adjr2
# 
# diff.dat$A_TOVxH_STL<-NULL

## Final Variable Set to begin regression modeling
names(diff.dat)
model.dat<-diff.dat


##### Best Subsets Regression fo Find best group of predictors####
### MODEL 1: Best Subsets - identify best combination of predictors ####
regfit.full.BSS <-regsubsets(Difference~., model.dat[, 4:(ncol(model.dat)-1)], nvmax=24)#, force.out = c("H_TOVxA_STL"))
(reg.summary.BSS<-summary(regfit.full.BSS))
names(reg.summary.BSS)

which.min(reg.summary.BSS$cp) #Best-5
(mod1.best.cp<-names(which(reg.summary.BSS$which[5,]==TRUE)))
min(reg.summary.BSS$cp) #-1.2
plot(reg.summary.BSS$cp, xlab = "Number of Variables",
     ylab = "Mallow's cp", type = "l")

which.min(reg.summary.BSS$bic) #Best-2
min(reg.summary.BSS$bic) #-365
(mod1.best.bic<-names(which(reg.summary.BSS$which[2,]==TRUE))) # AdjEM, BPI
plot(reg.summary.BSS$bic, xlab = "Number of Variables",
     ylab = "Bayesian Information Criterion (BIC)", type = "l")

which.max(reg.summary.BSS$adjr2) #Best-17
(mod1.best.adjr2<-names(which(reg.summary.BSS$which[17,]==TRUE)))
max(reg.summary.BSS$adjr2) #0.368
reg.summary.BSS$adjr2[17]
plot(reg.summary.BSS$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

## Examine what variables are best ##
mod1.best.cp #Try
mod1.best.bic
mod1.best.adjr2 #Try

## Best 6 and best 16 are good
#17
names(which(reg.summary.BSS$which[17,]==TRUE))
bss.names.17<-c(names(which(reg.summary.BSS$which[17,]==TRUE)))[-1]
bss.names.17<-gsub("`", "", bss.names.17)
bss.names.17<-c(bss.names.17, "Difference")

which(names(model.dat) %in% bss.names.17)
names(model.dat)[which(names(model.dat) %in% bss.names.17)]

step.lm<-lm(Difference~.,data=model.dat[, bss.names.17])
summary(step.lm) #Adj R2 = 0.370

forward.lm<-step(step.lm, direction = "forward", k=2)
summary(forward.lm) #Adj R2 = 0.370

backward.lm<-step(step.lm, direction = "backward", k=2)
summary(backward.lm) #Adj R2 = 0.3652
names(backward.lm$coefficients)

var.names.17<-names(model.dat)[which(names(model.dat) %in% bss.names.17)]
var.names.17.subset<-c(gsub("`", "", names(backward.lm$coefficients)[-1]), "Difference")

#5
names(which(reg.summary.BSS$which[5,]==TRUE))
bss.names.5<-c(names(which(reg.summary.BSS$which[5,]==TRUE)))[-1]
bss.names.5<-gsub("`", "", bss.names.5)
bss.names.5<-c(bss.names.5, "Difference")

which(names(model.dat) %in% bss.names.5)
names(model.dat)[which(names(model.dat) %in% bss.names.5)]

step.lm<-lm(Difference~.,data=model.dat[, bss.names.5])
summary(step.lm) #Adj R2 = 0.366

forward.lm<-step(step.lm, direction = "forward", k=2)
summary(forward.lm) #Adj R2 = 0.366

backward.lm<-step(step.lm, direction = "backward", k=2)
summary(backward.lm) #Adj R2 = 0.366
names(step.lm$coefficients)

var.names.5<-names(model.dat)[which(names(model.dat) %in% bss.names.5)]


# Variable imporatnce
#17
new.mod.dat.17<-model.dat%>%
  dplyr::select("Game_ID", "Home", "Away", all_of(var.names.17), "Result")

crf.17 <- calc.relimp(Difference~.,new.mod.dat.17[, 4:(ncol(new.mod.dat.17)-1)], type = c("lmg"), rela = TRUE )
sort(crf.17$lmg, decreasing = T)

new.mod.17<-lm(Difference~., new.mod.dat.17[, 4:(ncol(new.mod.dat.17)-1)])
summary(new.mod.17)
sort(vif(new.mod.17), decreasing = T)

#17 - subset
new.mod.dat.17v2<-model.dat%>%
  dplyr::select("Game_ID", "Home", "Away", all_of(var.names.17.subset), "Result")

crf.17v2 <- calc.relimp(Difference~.,new.mod.dat.17v2[, 4:(ncol(new.mod.dat.17v2)-1)], type = c("lmg"), rela = TRUE )
sort(crf.17v2$lmg, decreasing = T)

new.mod.17v2<-lm(Difference~., new.mod.dat.17v2[, 4:(ncol(new.mod.dat.17v2)-1)])
summary(new.mod.17v2)
sort(vif(new.mod.17v2), decreasing = T)


#6
new.mod.dat.5<-model.dat%>%
  dplyr::select("Game_ID", "Home", "Away", all_of(var.names.5), "Result")

crf.5 <- calc.relimp(Difference~.,new.mod.dat.5[, 4:(ncol(new.mod.dat.5)-1)], type = c("lmg"), rela = TRUE )
sort(crf.5$lmg, decreasing = T)

new.mod.5<-lm(Difference~., new.mod.dat.5[, 4:(ncol(new.mod.dat.5)-1)])
summary(new.mod.5)
sort(vif(new.mod.5), decreasing = T)

### Ridge Regression
library(glmnet)

model1.dat<-new.mod.dat.17
# model2.dat<-new.mod.dat.5
# model3.dat<-new.mod.dat.17v2

##
model.data<-model1.dat
# model.data<-model3.dat
# names(model.data)

x<-model.matrix(Difference~.,model.data[, 4:(ncol(model.data)-1)])
y<-model.data$Difference

# Get indices for train and test sets
set.seed(8)
subset<-sort(sample(1:nrow(model.data),nrow(model.data)*.6, replace = F ))
remain<-which(!(1:nrow(model.data))%in%subset)
train <- subset
test <- remain
y.test<-y[test]

cv.out<-cv.glmnet(x[train,],y[train], alpha=0, nfolds=100)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam 
# 0.852

ridge.mod<-glmnet(x,y,alpha=0,lambda = bestlam)
ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2) 
#Model 1: MSE=113.9
#Model 2: MSE=115.4
#Model 3: MSE=115.6
out<-glmnet(x,y, alpha = 0)
(coefs<-predict(out,type="coefficients",s=bestlam)[1:19,]) #Note, ridge regression does not perform variable selection


# Test Accuraccy in wins
test.set<-model.data[test,]
test.set$predicted<-ridge.pred
test.set$Predicted_home_win<-ifelse(test.set$predicted>-2, 1, 0)
confusionMatrix(as.factor(test.set$Result), as.factor(test.set$Predicted_home_win)) #72.5%
# Model 1: 71.9% (74.3% with Home Adv = 2 pts)
# Model 2: 71.5% (74.0% with Home Adv = 4.75 pts)
# Model 3: 71.1% (73.7% with Home Adv = 2.5 pts)

## Add  home advantage
home_adv<- 2


### MODEL 1 IS BEST - Same accurracy, near identical MSE and simpler !!!!
#

## Export Coefficients
coefs<-data.frame(t(coefs), check.names = F)
coefs<-cbind(coefs, home_adv)
write.xlsx(coefs, paste0("./Point Differential Coefficients - ",Sys.Date(), ".xlsx"))

## Add schedule
names(hoopR::load_mbb_schedule())
schedule<-hoopR::load_mbb_schedule()%>%
  select("date","game_id","neutral_site", "home_location", "away_location")%>%
  mutate(neutral_site = ifelse(neutral_site == TRUE, 1, 0))

schedule<-schedule[order(schedule$date),]

addWorksheet(wb, sheetName = "Schedule")
writeData(wb, "Schedule", schedule)


saveWorkbook(wb, "./2024 NCAA MBB Prediction Tool.xlsx", overwrite = T)


