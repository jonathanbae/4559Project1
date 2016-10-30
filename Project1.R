#read in table
date <- read.csv("~/School/Fall 2016/DS 4559/Project 1/4559Project1/Speed Dating Data.csv",na.strings=c(""," ","NA"))
date <- read.csv("")
library(mlbench)
library(caret)
library(C50)
library(partykit)
library(gmodels)

#-------------------
# Data cleaning
#-------------------
#Male and female data frames
male <- date[which(date$gender==1),]
female <- date[which(date$gender==0),]

#number of corr
plot(table(male$int_corr)) 
plot(table(female$int_corr))

#Make mn_sat, tuition, income as numeric
male$mn_sat <- as.numeric(as.character(male$mn_sat))
male$tuition <- as.numeric(as.character(male$tuition))
male$income <- as.numeric(as.character(male$income))
#Decision as factor
male$field_cd <- factor(male$field_cd)
male$career_c <- factor(male$career_c)
male$dec_o <- as.factor(male$dec_o)

#Remove rows where columns 99:104 are null (attr, sinc, intel, fun, amb)
#Should have 4099 rows
for(x in 1:3){
  for(i in 1:nrow(male)){
    for(j in 99:103){
      if(is.na(male[i,j]) && j == 103){
        male <- male[-i,]
     } 
      if(!is.na(male[i,j])) break
    }
  }
}
#Get rows of speedmaler by iid
attrMiss <- which(is.na(male$attr))
sincMiss <- which(is.na(male$sinc))
intelMiss <- which(is.na(male$intel))  
funMiss <- which(is.na(male$fun))
ambMiss <- which(is.na(male$amb))

avera <- function(missing, num){
  #Average attractive from other dates
  for(i in missing){
    #Get id of the first missing attractive
    getiid <- male[i, 1]
    #Store rows of values for the id
    store <- which(getiid==male[,1])
    numRate = 0
    tot = 0
    #Get average value from the other people
    for(j in store){
      if(!is.na(male[j,num]) ){
        numRate = numRate + 1
        tot = tot + male[j,num]
      }
    }
    #store the average of the scores
    male[i,num] <<- tot/numRate
  }
}
avera(attrMiss, 99)
avera(sincMiss, 100)
avera(intelMiss, 101)
avera(funMiss, 102)
avera(ambMiss, 103)
#Empty values for iid 465 for fun and intel; 187 for amb
which(is.na(male$attr))
which(is.na(male$sinc))
intelMiss <- which(is.na(male$intel))  
male[3376,1]
funMiss <- which(is.na(male$fun))
ambMiss <- which(is.na(male$amb))
male[1228,1]
#For arbitrary values, insert 5 into fun, intel and ambition
for( i in intelMiss ){
  male[i, 101] <- 5
  male[i, 102] <- 5
}
for( i in ambMiss ){
  male[i, 103] <- 5
}

#-------------------
# Model Creation for Career
#-------------------
#make a career data frame - where field_cd, career_c is not na
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
