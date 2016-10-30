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

#Get rows of speeddater by iid for ones missing attribute ratings
attrMiss <- which(is.na(male$attr_o))
sincMiss <- which(is.na(male$sinc_o))
intelMiss <- which(is.na(male$intel_o))  
funMiss <- which(is.na(male$fun_o))
ambMiss <- which(is.na(male$amb_o))

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
avera(attrMiss, 25)
avera(sincMiss, 26)
avera(intelMiss, 27)
avera(funMiss, 28)
avera(ambMiss, 29)
#Empty values for iid 465 for fun and intel; 187 for amb
which(is.na(male$attr_o))
which(is.na(male$sinc_o))
which(is.na(male$intel_o))
which(is.na(male$fun_o))
which(is.na(male$amb_o))
#For arbitrary values, mean imputation
meanVal <- function(miss, column){
  total = 0
  num = 0
  for(i in 1:nrow(male)){
    if(!is.na(male[i,column])){
      total = total + male[i, column]
      num = num + 1
    }
  }
  for(i in miss){
    male[i, column] <<- total/num
  }
}

#View every attribute spread
hist(male$attr)
hist(male$sinc)
hist(male$intel)
hist(male$fun)
hist(male$amb)

#View age; remove rows with no age
hist(male$age)
which(is.na(male$age))
male <- male[-which(is.na(male$age)),]

#Field_cd
which(is.na(male$field_cd))
male[which(is.na(male$field_cd)), c("iid", "field","field_cd" )]
#The individual with missing code has Operations research which is field_cd = 5.
which(male$field == "Operations Research")
male[1807, c("field_cd")]
male[which(is.na(male$field_cd)), c("field_cd")] <- 5
plot(table(male$field_cd))
#Interesting to note that most people are in the business/econ/finance field; Engineering is a far second
#and then sciences third
plot(table(male[which(male$field_cd != 8), c("field_cd")]))

#Race; no native americans
hist(male$race)
table(male$race)
which(is.na(male$race))

#income - too many NAs maybe come back
which(is.na(male$income))

#goal; most people say it was for fun night out, second to meet new people
which(is.na(male$goal))
table(male$goal)
#look at age bins; as age gets higher more people want serious relationship
table(male[which(male$age < 23), c("goal")])
plot(table(male[which(male$age > 23 & male$age < 25), c("goal")]))
plot(table(male[which(male$age > 25 & male$age < 27), c("goal")]))
plot(table(male[which(male$age > 27 & male$age < 29), c("goal")]))
plot(table(male[which(male$age > 29 & male$age < 31), c("goal")]))
plot(table(male[which(male$age > 31 & male$age < 45), c("goal")]))

#date; remove row with this preference
which(is.na(male$date))
male[which(is.na(male$date)), c("iid")]
male <- male[-which(is.na(male$date)),]
hist(male$date)

#goout
which(is.na(male$go_out))
hist(male$go_out)

#career_c
which(is.na(male$career_c))
male[which(is.na(male$career_c)), c("iid", "career","career_c" )]
#The individual with missing code has tech professional which is field_cd = 5.
male[which(is.na(male$career_c)), c("career_c")] <- 5
plot(table(male$career_c))
#Fields similar to career 

#exhappy
which(is.na(male$exphappy))
hist(male$exphappy)
table(male$exphappy)
#There are some people who are expecting to be a 1 happiness; why even bother going

#met; assume if no mark just say they have not met
which(is.na(male$met))
male[which(is.na(male$met)), c("met")] <- 2

#See if any of the activities are na;
#sports	tvsports	exercise	dining	museums	art	hiking	gaming
#clubbing	reading	tv	theater	movies	concerts	music	shopping	yoga
sapply(male, function(x) sum(is.na(x)))


#-------------------
# Model Creation for Male
#-------------------
#Attributes: age, field_cd, race, (income), goal, date, go_out, career_c, activities, exhappy, attr, sinc
#intel, fun, amb, shar, met, 
