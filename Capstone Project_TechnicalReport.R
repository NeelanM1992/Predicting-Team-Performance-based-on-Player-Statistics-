install.packages("dplyr", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("Boruta", dependencies = TRUE)


setwd("C:/Users/neela/Desktop")

library(dplyr)
library(readxl)
library(tidyr)
library(caret)
library(ggplot2)
library(Boruta)

#Read in Capstone Project_Dataset.xlsx excel file for both commands below 
BasketballReference<-read_excel(file.choose())
WinRatio<-read_excel(file.choose(),sheet = 2)

########################################################################################################################################################################

BasketballReference$TeamMP_Total<-(48*82)
BasketballReference<-BasketballReference%>%
  mutate(Percent_MinPlayed=(MP_Total/TeamMP_Total))%>%
  select(1,2,3,4,5,6,7,108,109,8:107)

########################################################################################################################################################################

BasketballReference_Per36Min<-BasketballReference%>%
  select(1:5,8,9,33:49)

BasketballReference_Advanced<-BasketballReference%>%
  select(1:5,8,9,87,99:106,109)

BasketballReference_Advanced2<-BasketballReference%>%
  select(1:5,8,9,88:98)

BasketballReference_Per36Min[is.na(BasketballReference_Per36Min)] <- 0
BasketballReference_Advanced[is.na(BasketballReference_Advanced)] <- 0
BasketballReference_Advanced2[is.na(BasketballReference_Advanced2)] <- 0


########################################################################################################################################################################


v<-BasketballReference_Advanced$Percent_MinPlayed
BasketballReference_Per36Min_Edit1<-BasketballReference_Per36Min%>%
  select(1,5)
BasketballReference_Per36Min_Edit2<-BasketballReference_Per36Min%>%
  select(8:24)  
BasketballReference_Per36Min_Edit3<-BasketballReference_Per36Min_Edit2 * v
  
BasketballReference_Per36Min<-cbind(BasketballReference_Per36Min_Edit1,BasketballReference_Per36Min_Edit3)%>%
  group_by(Season,Tm)%>%
  summarise_each(funs(sum))

########################################################################################################################################################################


BasketballReference_Advanced_Edit1<-BasketballReference_Advanced%>%
  select(1:7)
BasketballReference_Advanced_Edit2<-BasketballReference_Advanced%>%
  select(8:17)
BasketballReference_Advanced_Edit3<-BasketballReference_Advanced_Edit2 * v

BasketballReference_Advanced<-cbind(BasketballReference_Advanced_Edit1,BasketballReference_Advanced_Edit3)%>%
  select(1,5,8:17)%>%
  group_by(Season,Tm)%>%
  summarise_each(funs(sum))

########################################################################################################################################################################


BasketballReference_Advanced2<-BasketballReference_Advanced2%>%
  select(1,5,8:18)%>%
  group_by(Season,Tm)%>%
  summarise_each(funs(mean))

########################################################################################################################################################################


TeamStats<-merge(BasketballReference_Per36Min,BasketballReference_Advanced,by=c("Season","Tm"))
TeamStats<-merge(TeamStats,BasketballReference_Advanced2,by=c("Season","Tm"))
TeamStats<-merge(TeamStats,WinRatio,by=c("Season","Tm"))

TeamStats<-TeamStats%>%
  select(1:40,"WinRatio"=45)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

#Remove Win Share variables as these variables cannot be used for model 
TeamStats_Correlation<-TeamStats%>%
  filter(Season %in% c("2009-2010","2010-2011","2011-2012","2012-2013","2013-2014","2014-2015","2015-2016"))%>%
  select(3:20,25:41)

#Assess for Multicollinearity 

correlationMatrix<-cor(TeamStats_Correlation)
highlycorrelated<-findCorrelation(correlationMatrix,cutoff = 0.75,verbose = F)
highlycorrelated<-sort(highlycorrelated)
names(TeamStats_Correlation)[highlycorrelated]

TeamStats_Model<-TeamStats_Correlation%>%
  select(2:5,8,9,11:21,24:27,29:35)

#Cross Validation 
train_control<-trainControl(method = "cv",number = 10,savePredictions = TRUE)
model<-train(WinRatio~.,data = TeamStats_Model,trControl=train_control,method="lm")
check<-model$pred

head(check)
print(model)
summary(model)

modelvalues<-data.frame(obs = check$obs, pred=check$pred)
defaultSummary(modelvalues)

#Feature Selection using Caret
set.seed(123)
control<-rfeControl(functions=rfFuncs,method="cv",number=10)
rfe.train<-rfe(TeamStats_Model[,1:27],TeamStats_Model[,28],sizes = c(1,27),rfeControl = control)
rfe.train

plot(rfe.train,type=c("g","o"),cex=1.0,col=1:11)
predictors(rfe.train)

#Feature selection using Boruta
set.seed(123)
boruta.train<-Boruta(WinRatio~.,data = TeamStats_Model,doTrace=2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

getSelectedAttributes(final.boruta, withTentative = F)

boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

#Based on Feature Selection from Caret
model1<-train(WinRatio~BPM_Advanced,data=TeamStats_Model,trControl=train_control,method="lm")


#Based on Feature Selection from Boruta (Top 5)
model2<-train(WinRatio~BPM_Advanced + DBPM_Advanced + OBPM_Advanced + PER_Advanced + PTS_Per36Mins,data=TeamStats_Model,trControl=train_control,method="lm")


#Model1 is the simpler model and has good results
summary(model1)
summary(model2)

#Validate Model using Model 1 above  
Season_2016<-TeamStats%>%
  filter(Season == "2016-2017")

predictedVal<-predict(model1,Season_2016)

modelvalues<-data.frame(Actual_WinRatio = Season_2016$WinRatio, Predicted_WinRatio=predictedVal)

modelvalues<-modelvalues%>%
  select(2)

Season_2016<-cbind(Season_2016,modelvalues)%>%
  arrange(desc(WinRatio))

Rank=c(1:30)
Rank<-data.frame(Rank)

Season_2016<-cbind(Season_2016,Rank)
plot(Season_2016$Rank,Season_2016$WinRatio,main = "Season 2016-2017", ylab = "Win Ratio (Wins/82)", xlab = "Rank",col="red",pch=16,ylim=c(0, 0.9))
text(Season_2016$Rank,Season_2016$WinRatio,labels = Season_2016$Tm, col="red",cex=2/3,pos = 1)

Predicted_WinRatio<-Season_2016$Predicted_WinRatio
points(Predicted_WinRatio,pch=16,col="green")
legend(1,0.4,c("Green Dots: Predicted Win Ratio","Red Dots: Actual Win Ratio"),pt.cex = 1,cex=0.75)

modelvalues<-data.frame(obs = Season_2016$WinRatio, pred=Season_2016$Predicted_WinRatio)
defaultSummary(modelvalues)

########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

#Exploratory Analysis Using Subset of Data (Season Data from 2009-2010)

Season_2009to2010<-TeamStats%>%
  filter(Season %in% c("2009-2010"))

Season_2009to2010_Correlation<-TeamStats%>%
  filter(Season %in% c("2009-2010"))%>%
  select(3:20,25:41)

correlationMatrix_2009to2010<-cor(Season_2009to2010_Correlation)


#Assessing Team PER Vs Win Ratio 
plot(Season_2009to2010$WinRatio,Season_2009to2010$PER_Advanced,main = "Correlation between Team PER and Win Ratio (Season 2009-2010)", xlab = "Win Ratio (Wins/82)", ylab = "PER")
abline(lm(Season_2009to2010$PER_Advanced~Season_2009to2010$WinRatio),col=2) 
text(Season_2009to2010$WinRatio,Season_2009to2010$PER_Advanced,labels = Season_2009to2010$Tm, col="red",cex=2/3,pos = 1)
text(x=0.2,y=80,label="R^2=0.719")
text(x=0.21,y=78,label="r=0.84")

model_PER<-lm(WinRatio~PER_Advanced,data=Season_2009to2010)
summary(model_PER)

#Assessing Team Plus/Minus Vs Win Ratio  
plot(Season_2009to2010$WinRatio,Season_2009to2010$`Plus/Minus_Per36Min`,main = "Correlation between Team Plus/Minus and Win Ratio (Season 2009-2010)", xlab = "Win Ratio (Wins/82)", ylab = "Plus/Minus")
abline(lm(Season_2009to2010$`Plus/Minus_Per36Min`~Season_2009to2010$WinRatio),col=2) 
text(Season_2009to2010$WinRatio,Season_2009to2010$`Plus/Minus_Per36Min`,labels = Season_2009to2010$Tm, col="red",cex=2/3,pos = 1)
text(x=0.2,y=10,label="R^2=0.8921")
text(x=0.21,y=5.5,label="r=0.944")

model_PlusMinus<-lm(WinRatio~`Plus/Minus_Per36Min`,data=Season_2009to2010)
summary(model_PlusMinus)


#Assessing Team BPM Vs Win Ratio 
plot(Season_2009to2010$WinRatio,Season_2009to2010$BPM_Advanced,main = "Correlation between Team BPM and Win Ratio (Season 2009-2010)", xlab = "Win Ratio (Wins/82)", ylab = "BPM")
abline(lm(Season_2009to2010$BPM_Advanced~Season_2009to2010$WinRatio),col=2) 
text(Season_2009to2010$WinRatio,Season_2009to2010$BPM_Advanced,labels = Season_2009to2010$Tm, col="red",cex=2/3,pos = 1)
text(x=0.2,y=5,label="R^2=0.949")
text(x=0.21,y=3.5,label="r=0.97")

model_BPM<-lm(WinRatio~BPM_Advanced,data=Season_2009to2010)
summary(model_BPM)

#Assessing Team VORP Vs Win Ratio 
plot(Season_2009to2010$WinRatio,Season_2009to2010$VORP_Advanced,main = "Correlation between Team VORP and Win Ratio (Season 2009-2010)", xlab = "Win Ratio (Wins/82)", ylab = "BPM")
abline(lm(Season_2009to2010$VORP_Advanced~Season_2009to2010$WinRatio),col=2) 
text(Season_2009to2010$WinRatio,Season_2009to2010$VORP_Advanced,labels = Season_2009to2010$Tm, col="red",cex=2/3,pos = 1)
text(x=0.2,y=5,label="R^2=0.9046")
text(x=0.21,y=3.5,label="r=0.95")

model_VORP<-lm(WinRatio~VORP_Advanced,data=Season_2009to2010)
summary(model_VORP)


########################################################################################################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

#Working Files

split<-createDataPartition(y = TeamStats_Model$WinRatio, p = 0.6, list = FALSE)
dev<-TeamStats_Model[split,]
val<-TeamStats_Model[-split,]

predictedVal<-predict(model,val)
modelvalues<-data.frame(obs = val$WinRatio, pred=predictedVal)
defaultSummary(modelvalues)
