source('./Functions.R')
source('./Point Differential Model.R')
# 
# ####### PREDICTING POINT DIFFERENTIAL #####
# ### Load Data ###
# raw.dat<-read.xlsx('../Data/NCAA BB Data.xlsx', sheet = "Data", check.names = FALSE, sep.names="_")
# raw.diff.dat<-read.xlsx('../Data/NCAA BB Data.xlsx', sheet = "Data Diff", check.names = FALSE, sep.names="_")
# 
# ## Data Cleaning
# # Initial cleaning of unneeded variables
# names(raw.dat)
# raw.dat<-raw.dat[-c(1, 3, 5, 7:22, 149)]%>%
#   mutate(Result = ifelse(Final_H >Final_A, 1, 0))%>%
#   na.omit()
# 
# names(raw.diff.dat)
# raw.diff.dat<-raw.diff.dat[, -c(1,3,5,7:18)]%>%
#   mutate(Winner = ifelse(Final_H >Final_A, 1, 0))%>%
#   rename(Result = Winner)%>% #1 = Home Win, 0 = Away Win
#   na.omit()
# 
# #### Looking at Result Win/Loss ######
# names(raw.diff.dat)
# diff.dat<-raw.diff.dat%>% #Don't need total right now
#   dplyr::select(-c('Final_H', 'Final_A', 'Total'))
# names(diff.dat)
# 
# # QC Dataframes match
# all(raw.dat$Result==diff.dat$Result)
# 
# # Remove missing data from some rankings
# removals<-which(raw.dat$H_BPI==0 | raw.dat$H_SOR==0 | raw.dat$A_BPI==0 | raw.dat$A_SOR==0)
# 
# raw.dat<-raw.dat[-removals,]
# diff.dat<-diff.dat[-removals,]
# 
# # QC Row Removal
# all(raw.dat$Result==diff.dat$Result)
# 
# # Give game index for tracking and reference
# diff.dat$Game_ID<-1:nrow(diff.dat)
# raw.dat$Game_ID<-1:nrow(raw.dat)
# 
# diff.dat<-diff.dat[, c(ncol(diff.dat), 1:(ncol(diff.dat)-1))]
# raw.dat<-raw.dat[, c(ncol(raw.dat), 1:(ncol(raw.dat)-1))]
# 
# # Make 'Neutral' binary 1 - 0
# diff.dat<-diff.dat%>%
#   mutate(Neutral = ifelse(Neutral =="Y", 1, 0))
# table(diff.dat$Neutral, useNA = "always")
# 
# raw.dat<-raw.dat%>%
#   mutate(Neutral = ifelse(Neutral =="Y", 1, 0))
# table(raw.dat$Neutral, useNA = "always")
# 
# # Remove Neutral due to poor distribution
# raw.dat$Neutral<-diff.dat$Neutral<-NULL
# 
# ################ ONLY USING DIFFERENCE DATA FROM HERE OUT ######################
# 
# ## Removal - round 1
# # Points_Tm, Points_Opp WAB, Totals_STL, Totals_TOV, Totals_AST, Totals_3PA, Totals_FTA,
# # Totals_BLK, Totals_PF, ORB_PG, TRB_PG, Totals_FGA (used to calculate metrics)
# # RPI (Repeat of RPI Rank)
# diff.dat<-diff.dat%>%
#   dplyr::select(-c("Points_Tm", "Points_Opp", "WAB", "Totals_STL", "Totals_TOV", "Totals_AST", "Totals_3PA","Totals_FGA",
#                    "Totals_FTA", "Totals_BLK", "Totals_PF", "Totals_ORB", "Totals_TRB", "RPI", "Totals_3P","Totals_FT"))
# names(diff.dat)
# 
# ## Removal Round 2
# # Remove Interaction Home x Away variables as they cannot be used in Power Ranking
# # "H_AdjOxA_AdjD"              
# # "A_AdjOxH_AdjD"              
# # "H_TOVxA_STL"               
# # "A_TOVxH_STL" 
# # "H_PFxA_FT%"                
# # "A_PFxH_FT%"   
# diff.dat<-diff.dat%>%
#   dplyr::select(-c( "H_AdjOxA_AdjD","A_AdjOxH_AdjD","H_TOVxA_STL","A_TOVxH_STL", "H_PFxA_FT%", "A_PFxH_FT%" ))   
# names(diff.dat)
# 
# ## Multicollinearity and Correlations ##
# # Check for Multicollinearity
# mc_check <- glm(Result ~ ., family="binomial", data = diff.dat[, -c(1:3)])
# vif(mc_check)
# # Calculate correlation matrix
# cor_matrix <- cor(diff.dat[,-c(1:3)])
# # Check for correlation coefficients
# high_correlation_pairs <- which(cor_matrix > 0.8 & cor_matrix < 1, arr.ind = TRUE)
# # Check eigenvalues
# eigenvalues <- eigen(cor_matrix)$values
# 
# sort(vif(mc_check), decreasing = T)
# print(high_correlation_pairs)
# print(eigenvalues)
# 
# 
# ## Check if RPI Rank or RPI Rating is better
# cor_matrix[which(row.names(cor_matrix)=="RPI_Rank"|row.names(cor_matrix)=="RPI_Rating"),]
# rpi_rating<-glm(Result ~ RPI_Rating,family="binomial",  data = diff.dat[, -c(1:3)])
# summary(rpi_rating)
# rpi_rank<-glm(Result ~ RPI_Rank,family="binomial", data = diff.dat[, -c(1:3)])
# summary(rpi_rank) #Keep RPI_Rank
# # Remove RPI_Rating
# 
# 
# ## Check if Elo (raw ELO) or ELO (ELO Rank) is better
# # *NOTE: 'Elo' has one of the higher number of High Corr pairs
# cor_matrix[which(row.names(cor_matrix)=="ELO"|row.names(cor_matrix)=="Elo"),]
# Elo_rating<-glm(Result ~ Elo, family="binomial", data = diff.dat[, -c(1:3)])
# summary(Elo_rating) #Keep Elo
# ELO_rank<-glm(Result ~ ELO, family="binomial",data = diff.dat[, -c(1:3)])
# summary(ELO_rank)
# # Remove ELO
# 
# ## Check which Efficiency metrics are better
# cor_matrix[which(row.names(cor_matrix)=="AdjO"|row.names(cor_matrix)=="AdjOE"),]
# AdjO<-glm(Result ~ AdjO,family="binomial", data = diff.dat[, -c(1:3)])
# summary(AdjO)
# AdjOE<-glm(Result ~ AdjOE,family="binomial", data = diff.dat[, -c(1:3)])
# summary(AdjOE) 
# 
# cor_matrix[which(row.names(cor_matrix)=="AdjD"|row.names(cor_matrix)=="AdjDE"),]
# AdjD<-glm(Result ~ AdjD,family="binomial", data = diff.dat[, -c(1:3)])
# summary(AdjD)
# AdjDE<-glm(Result ~ AdjDE, family="binomial",data = diff.dat[, -c(1:3)])
# summary(AdjDE) 
# ## AdjO and AdjD are slightly better
# 
# ## Second Round of Removals
# diff.dat<-diff.dat%>%
#   dplyr::select(-c("ELO", "RPI_Rating", "AdjOE", "AdjDE"))
# names(diff.dat)
# 
# # Repeat Check for Multicollinearity
# mc_check <- glm(Result ~ .,family='binomial', data = diff.dat[, -c(1:3)])
# vif(mc_check)
# 
# # Calculate correlation matrix
# cor_matrix <- cor(diff.dat[, -c(1:3)])
# # Check for correlation coefficients
# high_correlation_pairs <- which(cor_matrix > 0.8 & cor_matrix < 1, arr.ind = TRUE)
# # Check eigenvalues
# eigenvalues <- eigen(cor_matrix)$values
# 
# sort(vif(mc_check), decreasing = T)
# print(high_correlation_pairs)
# print(eigenvalues)
# print(cor_matrix)
# 
# ## Check variables with very low correlation with Difference
# sort(abs(cor_matrix["Result",]), decreasing = T)
# Luck_check <- glm(Result ~ Luck, family='binomial', data = diff.dat[, -c(1:3)])
# summary(Luck_check)
# Tempo_check <- glm(Result ~ AdjT, family='binomial', data = diff.dat[, -c(1:3)])
# summary(Tempo_check)
# RPI_Delta_check <- glm(Result ~ RPI_Delta, family='binomial', data = diff.dat[, -c(1:3)])
# summary(RPI_Delta_check)
# ORB_PG_check <- glm(Result ~ ORB_PG, family='binomial', data = diff.dat[, -c(1:3)])
# summary(ORB_PG_check)
# FTA_PG_check <- glm(Result ~ FTA_PG, family='binomial', data = diff.dat[, -c(1:3)])
# summary(ORB_PG_check)
# ThreePA_PG_check <- glm(Result ~ `3PA_PG`, family='binomial', data = diff.dat[, -c(1:3)])
# summary(ThreePA_PG_check) #*Significant
# 
# # Remove AdjT, RPI_Delta, Luck, ORB, and FTA_PG for very low correlation to Result and insignificance
# ## Third Round of Removals
# diff.dat<-diff.dat%>%
#   dplyr::select(-c("AdjT", "RPI_Delta", "Luck", "ORB_PG", "FTA_PG"))
# names(diff.dat)
# 
# ## AdjEM and POM are same thing 
# 
# temp.AjdEM<-glm(Result ~ AdjEM, family = "binomial",data = diff.dat[, -c(1:3)])
# temp.POM<-glm(Result ~ POM, family = "binomial",data = diff.dat[, -c(1:3)])
# summary(temp.AjdEM)
# summary(temp.POM)
# diff.dat$POM<-NULL
# 
# ## Look at PPG and PPG_Diff
# PPG.dat<-diff.dat%>%
#   dplyr::select("Result", "PPG", "PPG_Diff")
# 
# PPG.lm<-glm(Result ~ PPG + PPG_Diff, family = "binomial", data = PPG.dat)
# summary(PPG.lm)
# PPG.lm<-glm(Result ~ PPG,family = "binomial", data = PPG.dat)
# summary(PPG.lm)
# PPG_Diff.lm<-glm(Result ~ PPG_Diff,family = "binomial", data = PPG.dat)
# summary(PPG_Diff.lm)
# 
# crf <- calc.relimp(Result~.,PPG.dat, type = c("lmg"), rela = TRUE )
# sort(crf$lmg, decreasing = T)
# 
# ## Drop PPG
# diff.dat$PPG<-NULL
# 
# ## Investigate SOS variables
# sort(cor_matrix["Strength_of_Schedule_AdjEM",])
# sort(cor_matrix["Strength_of_Schedule_OppO",])
# 
# sos.dat<-diff.dat%>%
#   dplyr::select("Result", "Strength_of_Schedule_AdjEM", "Strength_of_Schedule_OppO",
#                 "Strength_of_Schedule_OppD","Overall_SOS")
# SOS.lm<-glm(Result ~ Strength_of_Schedule_AdjEM + Strength_of_Schedule_OppO + Strength_of_Schedule_OppD + Overall_SOS, family = "binomial", data = sos.dat)
# summary(SOS.lm)
# 
# # Only keep overall SOS
# diff.dat$Strength_of_Schedule_AdjEM<-diff.dat$Strength_of_Schedule_OppO<-diff.dat$Strength_of_Schedule_OppD<-NULL
# 
# ## Lots of STL and TOV variables
# stl.tov.lm<-glm(Result~STL_PG + TOV_PG + STL_TOV_Ratio, family = "binomial",diff.dat[, -c(1:3)])
# summary(stl.tov.lm) #*All sig
# 
# ## Final Variable Set to begin regression modeling
# names(diff.dat)
# model.dat<-diff.dat
# 
# ##### Best Subsets Regression fo Find best group of predictors####
# ### MODEL 1: Best Subsets - identify best combination of predictors ####
# #install.packages("bestglm")
# library(bestglm)
# #?bestglm()
# mod<-glm(Result~., family = "binomial",model.dat[, -c(1:3)])
# step1.mod<-step(mod, direction = "forward", k=2)
# summary(step1.mod)
# 
# step2.mod<-step(mod, direction = "backward", k=2)
# summary(step2.mod)
# 
# model.dat<-model.dat%>%
#   dplyr::select("Game_ID", "AdjO", "AdjD", "BARTHAG", "TOV_PG", "STL_TOV_Ratio", "STL_PG", "RPI_Rank", "SOR", "Result")
# 
# ### Relative Importance

mod.dat<-model.data%>%
  dplyr::select(-Difference)

for (i in 4:(ncol(mod.dat)-1)){
mod.dat[,i] <-calculate_z_score(mod.dat[,i])
}
mod.dat

crf <- calc.relimp(Result~.,mod.dat[, 4:(ncol(mod.dat))], type = c("lmg"), rela = TRUE )
sort(crf$lmg, decreasing = T)

RI<-data.frame(t(sort(crf$lmg, decreasing = T)), check.names = F)
RI
write.xlsx(RI, paste0("./Relative Importance - ",Sys.Date(), ".xlsx"))

