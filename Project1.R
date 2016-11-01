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
male$race <- as.factor(male$race)

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

#View every attribute spread; The spread of numbers is fairly high (not uniform). May be due to the idea that 
#the people rating are concious of their ratings and don't wish to be too mean.
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
#Sentiment Analysis
#-------------------
library(twitteR)
library(base64enc)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(dplyr)
library(RTextTools)
library(plyr)
library(tm)
library(wordcloud)
key='5VmEHYyBmVtY6nXjbnK9mu5uR'
secret='8VcjEVcit7AGUBfh3B3Z13OiXHVwtuvAFWMaIgVWFBTnMUsuo6'
setwd("~/School/Fall 2016/DS 4559/text_mining_and_web_scraping/")
access_token='788199063812399104-8L8mRuzn1Mn8Ld9WMVlCuPbJD0Mk4pH'
access_token_secret='0fZm0jViHzVnn5UI5T5w3RGgbhIjmTgZ6qiAPy1jf7dpM'

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="~/School/Fall 2016/DS 4559/text_mining_and_web_scraping/cacert.pem",
              method="curl")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret, access_token, access_token_secret)
save(authenticate, file="twitter authentication.Rdata")
# Speed Dating: word clouds created for speed dating, love, romance
#Geocode for Charlottesville: 38.0293,78.4767
#Geocode for Manhattan 40.7831, 73.9712
some_tweets = searchTwitter("romance", n=2000, lang="en",since ='2000-01-01',
                            until ='2016-10-31',  retryOnRateLimit=120, geocode='38.0293,78.4767,500mi')
# get the text
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

col=brewer.pal(6,"Dark2")
wordcloud(some_txt, min.freq=20, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=50, random.order=F,colors=col)

positives= readLines("~/School/Fall 2016/DS 4559/R/positive_words.txt")
negatives= readLines("~/School/Fall 2016/DS 4559/R/negative_words.txt")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

Score <- score.sentiment(some_txt,positives,negatives,.progress='none')
head(Score)
hist(Score$score,xlab="Sentiment Score ",main="Sentiment of sample tweets that have Trump in them ",
     border="black",col="skyblue")
sum(Score$score)

classify_emotion <- function(textColumns,algorithm="bayes",prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("~/School/Fall 2016/DS 4559/R/emotions.csv.gz",header=FALSE)
  counts <- list(anger=length(which(lexicon[,2]=="anger")),disgust=length(which(lexicon[,2]=="disgust")),fear=length(which(lexicon[,2]=="fear")),joy=length(which(lexicon[,2]=="joy")),sadness=length(which(lexicon[,2]=="sadness")),surprise=length(which(lexicon[,2]=="surprise")),total=nrow(lexicon))
  documents <- c()
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(anger=0,disgust=0,fear=0,joy=0,sadness=0,surprise=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[,2]==key),]
        index <- pmatch(word,emotions[,1],nomatch=0)
        if (index > 0) {
          entry <- emotions[index,]
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          score <- 1.0
          if (algorithm=="bayes") score <- abs(log(score*prior/count))
          if (verbose) {
            print(paste("WORD:",word,"CAT:",category,"SCORE:",score))
          }
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents,c(scores$anger,scores$disgust,scores$fear,scores$joy,scores$sadness,scores$surprise,best_fit))
  }
  colnames(documents) <- c("ANGER","DISGUST","FEAR","JOY","SADNESS","SURPRISE","BEST_FIT")
  return(documents)
}

classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
  matrix <- create_matrix(textColumns,...)
  lexicon <- read.csv("~/School/Fall 2016/DS 4559/R/subjectivity.csv.gz",header=FALSE)
  counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
  documents <- c()
  for (i in 1:nrow(matrix)) {
    if (verbose) print(paste("DOCUMENT",i))
    scores <- list(positive=0,negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc,lowfreq=1)
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        if (verbose) {
          print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
        }
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
    if (verbose) {
      print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  colnames(documents) <- c("POS","NEG","POS/NEG","BEST_FIT")
  return(documents)
}

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
library(ggplot2)
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity))

text.corpus <- Corpus(VectorSource(some_txt))
dtm <- DocumentTermMatrix(text.corpus)

#-------------------
# Model Creation for Male
#-------------------
#Attributes: age, field_cd, race, (income), goal, date, go_out, career_c, activities, exhappy, attr, sinc
#intel, fun, amb, shar, met, 

# get rid wave 5 and 12 because of their special features
male <- male[which(male$wave != 5 & male$wave != 12),]

# get a identifier for wave with maganize
male$magazine <- 0
for(i in nrow(male)){
  male[i,"wave"] 
  if(male[i,"wave"] >= 18){
    i
    male[i, "magazine"] = 1
  }
}
# still need mn_sat, tuition,i ncome,
m <- subset(male, select=c(age, round, race,
                           goal, date, go_out, exphappy, met,
                           sports, tvsports, exercise, dining, museums, art,
                           hiking, gaming, clubbing, reading, tv, theater,
                           movies, concerts, music, shopping, yoga, magazine, attr_o,
                           sinc_o, intel_o, fun_o,amb_o, field_cd, career_c, 
                           dec_o))

m <- m[order(runif(nrow(m))),]
m_train <- m[1:round(nrow(m)*3/4),]
m_test <- m[round(nrow(m)*3/4):nrow(m),]

#tree with all vars
m_tree <- C5.0(m_train[-ncol(m_train)], m_train$dec_o)
m_pred <- predict(m_tree, m_test[-ncol(m_train)])
summary(m_tree)

CrossTable(m_test$dec_o, m_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

m_ctree <- ctree(dec_o ~ ., data = m_train)
m_cpred <- predict(m_ctree, m_test)

m_ctree
CrossTable(m_test$dec_o, m_cpred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

#simplification
correlationMatrix <- cor(m_train[,c(-32,-33, -34)])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
m_train <- m_train[,-highlyCorrelated]
m_test <- m_test[,-highlyCorrelated]

# these simplication does not work well
m_var <- randomForest(dec_o ~ ., m_train)
varImpPlot(m_var)
varImp(m_var)

set.seed(7)
m_train_rfe <- m_train[1:1000,] 
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(m_train_rfe[,-ncol(m_train_rfe)], m_train_rfe[,ncol(m_train_rfe)], sizes=c(5:12), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))

#subsetting
m_train2 <- subset(m_train, select = c(attr_o, sinc_o, intel_o, fun_o,amb_o,field_cd, career_c, round, age, race, dec_o  ))
m_test2 <- subset(m_test, select = c(attr_o, sinc_o, intel_o, fun_o,amb_o,field_cd, career_c, round,age, race,dec_o ))
m_tree2 <- C5.0(m_train2[-ncol(m_train2)], m_train2$dec_o)
m_pred2 <- predict(m_tree2, m_test2[-ncol(m_train2)])

summary(m_tree2)
CrossTable(m_test2$dec_o, m_pred2,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

