library(httr)
#library(ElemStatLearn)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
library(expss)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)
# if(!requireNamespace('pacman', quietly = TRUE)){
#   install.packages('pacman')
# }
# pacman::p_load_current_gh("billpetti/baseballr")
library(baseballr)

elo.data <- read.csv("./Data/mlb-elo-latest.csv")%>%
  filter(season>1975)%>%
  mutate(playoff = ifelse(is.na(playoff) | playoff == "", "Reg",  "Post"))
#filter out duplicate dates
elo.data<-elo.data[!duplicated(elo.data$date),]

head(elo.data)



#################### Get Game Results ######################
date<-today()
  dat<-rbind(mlb_game_pks(date-1),
             mlb_game_pks(date-2),
             mlb_game_pks(date-3),
             mlb_game_pks(date-4),
             mlb_game_pks(date-5),
             mlb_game_pks(date-6),
             mlb_game_pks(date-7),
             mlb_game_pks(date-8),
             mlb_game_pks(date-9),
             mlb_game_pks(date-10),
             mlb_game_pks(date-11),
             mlb_game_pks(date-12),
             mlb_game_pks(date-13),
             mlb_game_pks(date-14),
             mlb_game_pks(date-15),
             mlb_game_pks(date-16),
             mlb_game_pks(date-17),
             mlb_game_pks(date-18),
             mlb_game_pks(date-19),
             mlb_game_pks(date-20),
             mlb_game_pks(date-21),
             mlb_game_pks(date-22),
             mlb_game_pks(date-23),
             mlb_game_pks(date-24),
             mlb_game_pks(date-25),
             mlb_game_pks(date-26),
             mlb_game_pks(date-27),
             mlb_game_pks(date-28),
             mlb_game_pks(date-29),
             mlb_game_pks(date-30),
             mlb_game_pks(date-31),
             mlb_game_pks(date-32),
             mlb_game_pks(date-33),
             mlb_game_pks(date-34),
             mlb_game_pks(date-35),fill= TRUE)