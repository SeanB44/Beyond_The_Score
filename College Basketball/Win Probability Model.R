source('./Functions.R')

####### CALCULATING WIN PROBABILITY #########
### Load Data ###
raw.dat<- read.xlsx("../Data/Win_Prob_Data.xlsx")
# get rid of missing data/games not logged
raw.dat<-na.omit(raw.dat) 
# check structure & names
str(raw.dat)
names(raw.dat)
# Make Neutral Binary 1-0
raw.dat$Neutral<-ifelse(raw.dat$Neutral=="Y",1,0)

### Create Win Probability Model ###
mod.dat<-raw.dat%>%
  dplyr::select("Game", "Neutral", "Home_PR", "Away_PR", "PR_Diff", "Result")
table(mod.dat$Result)
mod.dat$Result<-factor(mod.dat$Result, levels=c(0,1), labels=c("Loss", "Win"))
table(mod.dat$Result)

subset<-1:(nrow(mod.dat)*.667)
train.dat<-mod.dat[subset,]
test.dat<-mod.dat[-subset,]

# Model 1
full.mod<-glm(Result~Neutral+Home_PR+Away_PR, data=train.dat[,-1], family="binomial")
summary(full.mod)
full.mod$fitted.values
pred<-predict(full.mod, newdata=test.dat[,-c(1,ncol(test.dat)-1,ncol(test.dat))], type="response")
predicted_result<-ifelse(pred>.5,"Win","Loss")
confusionMatrix(as.factor(predicted_result), test.dat$Result) #68.3% accuracy

# Model 2
diff.mod<-glm(Result~Neutral+PR_Diff, data=train.dat[,-1], family="binomial")
summary(diff.mod)
pred<-predict(diff.mod, newdata=test.dat[,c(2,ncol(test.dat)-1)], type="response")
predicted_result<-ifelse(pred>.5,"Win","Loss")
confusionMatrix(as.factor(predicted_result), test.dat$Result) #68.6% accuracy


# Model 3
home.diff.mod<-glm(Result~Neutral+Home_PR+PR_Diff, data=train.dat[,-1], family="binomial")
summary(home.diff.mod)
pred<-predict(home.diff.mod, newdata=test.dat[,c(2,3,ncol(test.dat)-1)], type="response")
predicted_results<-ifelse(pred>.5,"Win","Loss")
confusionMatrix(as.factor(predicted_results), test.dat$Result) #68.3% accuracy

# Model 1 - No Neutral
full.modv2<-glm(Result~Home_PR+Away_PR, data=train.dat[,-1], family="binomial")
summary(full.modv2)
pred<-predict(full.modv2, newdata=test.dat[,-c(1:2,ncol(test.dat)-1,ncol(test.dat))], type="response")
predicted_result<-ifelse(pred>.5,"Win","Loss")
confusionMatrix(as.factor(predicted_result), test.dat$Result) #66.2% accuracy

# Model 2 - No Neutral
diff.modv2<-glm(Result~PR_Diff, data=train.dat[,-1], family="binomial")
summary(diff.modv2)
pred<-predict(diff.modv2, newdata=data.frame(PR_Diff = test.dat[,c(ncol(test.dat)-1)]), type="response")
predicted_result<-ifelse(pred>.5,"Win","Loss")
confusionMatrix(as.factor(predicted_result), test.dat$Result) #66.2% accuracy

### Model 2 is best
# Model 2
diff.mod<-glm(Result~Neutral+PR_Diff, data=train.dat[,-1], family="binomial")
summary(diff.mod)
pred<-predict(diff.mod, newdata=test.dat[,c(2,ncol(test.dat)-1)], type="response")
predicted_result<-ifelse(pred>.5,"Win","Loss")
confusionMatrix(as.factor(predicted_result), test.dat$Result) #68.6% accuracy

coefs<-diff.mod$coefficients
test.calcs<-exp(train.dat$Neutral*coefs[2] + train.dat$PR_Diff*coefs[3] + coefs[1])/(1+exp(train.dat$Neutral*coefs[2] + train.dat$PR_Diff*coefs[3] + coefs[1]))
test.calcs==diff.mod$fitted.values

# Export coefficients
write.xlsx(data.frame(coefs),"./win_prob_coefs.xlsx", sheetName="Coefs", rowNames=TRUE)
