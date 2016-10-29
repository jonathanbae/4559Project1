#read in table
date <- read.csv("~/School/Fall 2016/DS 4559/Project 1/4559Project1/Speed Dating Data.csv",na.strings=c(""," ","NA"))
date <- read.csv("")
library(mlbench)
library(caret)
library(C50)
library(partykit)
library(gmodels)
#1
#-------------------
# Data cleaning
#-------------------
#number of corr
plot(table(date$int_corr))
#dec_o versus attr_o
plot(date$dec_o, date$attr_o)
#Using a linear model we see that there is a correlation between the decision of the partner and the rating that was given
lm <- glm(dec_o ~ attr_o, data=date, family = binomial)
summary(lm)
#Remove rows where columns 16:23 are null
#Should have 8305 rows
for(k in 1:2){
  for(i in 1:nrow(date)){
    for(j in 16:23){
      if(is.na(date[i,j]) && j == 23){
        date <- date[-i,]
     } 
      if(!is.na(date[i,j])) break
    }
  }
}
#Make mn_sat, tuition, income as numeric
date$mn_sat <- as.numeric(as.character(date$mn_sat))
date$tuition <- as.numeric(as.character(date$tuition))
date$income <- as.numeric(as.character(date$income))
#Decision as factor
date$field_cd <- factor(date$field_cd)
date$career_c <- factor(date$career_c)
date$dec_o <- as.factor(date$dec_o)


#Male and female data frames
male <- date[which(date$gender==1),]
female <- date[which(date$gender==0),]

#-------------------
# Model Creation for Career
#-------------------
#make a career data frame - where not undergra, field_cd, career_c is not na
#attempted to create model with schools but unable to get a tree with enough predictions
career <- male[which(!is.na(male$undergra)),]
career <- career[which(!is.na(career$age_o)),]
set.seed(12345)
career_rand <- career[order(runif(2407)), ]
#Split the data frames into 75%/25%
career_train <- career_rand[1:1805, ]
career_test  <- career_rand[1806:2407, ]
#Create a C5.0 model
career_model <- C5.0(career_train[,c("field_cd", "career_c", "age_o")],career_train$dec_o)
plot(career_model)

career_pred <- predict(career_model, career_test)
#Cross tabulation of predicted versus actual classes
CrossTable(career_test$dec_o, career_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
