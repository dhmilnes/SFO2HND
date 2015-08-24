### Ponpare Coupon Purchase Prediction ###
### Author: Doug Milnes ###
#read in all the input data
library(tidyr)
library(dplyr)
library(ggplot2)
library(glmnet)
library(MASS)
library(Matrix)
library(reshape2)
source("prepsubfn.R")
source("MAP10.R")
cpltr <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=T)
cplte <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=T)
cpdtr <- read.csv("../Data/coupon_detail_train.csv", stringsAsFactors=T)
user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)
capsule <- read.csv("../Data/genrelookup.csv")
load("../Data/cpag.Rdata")
load("../Data/optcosinepurchweights")
cpag$distgroup <- as.factor(cpag$distgroup)

### train to train plus validate
### thought process:  all users are valid in all cases so just need to hold out couponsfor second to last week.  
### random sample users to make set smaller.  
cpltr$DISPFROM <- as.Date(as.character(cpltr$DISPFROM))
cplte$DISPFROM <- as.Date(as.character(cplte$DISPFROM))

cplest <- cpltr[cpltr$DISPFROM >= '2012-06-17',]
cpl_tr <- cpltr[cpltr$DISPFROM < '2012-06-17',]

### Identify purchases for estimate
cps_est <- cpdtr$COUPON_ID_hash %in% cplest$COUPON_ID_hash
estcps<- cpdtr[cps_est,c("COUPON_ID_hash","USER_ID_hash")]
estcps <- unique(estcps)
estcps$purch <- 1
estpurch <- estcps %>% group_by(USER_ID_hash) %>% 
          summarize(ACTUAL_COUPON=paste(COUPON_ID_hash,collapse=" "))

#####  Similarity 

train <- merge(cpdtr,cpl_tr)
train <- merge(train,cpag)

###### TO ADD ITEM COUNT:  ADD

cols <- c("COUPON_ID_hash","USER_ID_hash",
          "GENRE_NAME","CAPSULE_TEXT","DISCOUNT_PRICE","PRICE_RATE",
          "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
          "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
          "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name","distgroup")

### ADD IN ITEM COUNT AS A VOTE??
train <- train[,cols]


##combine the test set with the train
cplest$USER_ID_hash <- "estimateduser"
cplte$USER_ID_hash <- "dummyuser"
cplest <- merge(cplest,cpag)
cplte <- merge(cplte,cpag)
cpchar <- cplte[,cols]
cpest <- cplest[,cols]
train <- rbind(train,cpchar,cpest)

#NA imputation
train[is.na(train)] <- 1

#feature engineering
train$DISCOUNT_PRICE <- 1/log10(train$DISCOUNT_PRICE)
train$PRICE_RATE <- log10(train$PRICE_RATE*100+1)

#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + GENRE_NAME + DISCOUNT_PRICE +
                                             PRICE_RATE + USABLE_DATE_FRI + USABLE_DATE_SAT + USABLE_DATE_SUN +
                                             USABLE_DATE_HOLIDAY + USABLE_DATE_BEFORE_HOLIDAY + 
                                             ken_name + small_area_name + CAPSULE_TEXT + GENRE_NAME:ken_name,
                                           train[,-c(1,2)], 
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)))], contrasts, contrasts=FALSE)))


                         
#separate the test from train
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
est <- train[train$USER_ID_hash=="estimateduser",]
est <- est[,-2]
train <- train[!(train$USER_ID_hash %in% c("dummyuser","estimateduser")),]

#data frame of user characteristics

uchar <- train[,-1] %>% group_by(USER_ID_hash) %>% summarise_each(funs(mean))

uchar$DISCOUNT_PRICE <- 1
uchar$PRICE_RATE <- 1

#### MATRIX W WEIGHTS

save(est,uchar,estcps, file="../Data/cosine.Rdata")
require(Matrix)
weights <- c(3,1,0,0,1,4,0,2) #(3,1,0,0,3,4,3) c(3,1,0,0,3,4,0)
#Genre 13, Discount_Price, Price_rate, Usable_Dates, ken_name, small_area_name
reps <- c(13,1,1,5,47,55,25,611)

length(weights)==length(reps)

W<- Diagonal(x=rep(weights, reps))
score_te <-  as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))*.95
score_est <- as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(est[,2:ncol(est)]))*.95

#### Create Estimate Scores
raw_score_est <- as.data.frame(as.matrix(score_est))
colnames(raw_score_est)<- est$COUPON_ID_hash
raw_score_est$USER_ID_hash <- uchar$USER_ID_hash
raw_score_est <- gather(raw_score_est,COUPON_ID_hash,cosine,-USER_ID_hash)
topscores_est <- raw_score_est %>% 
                  group_by(USER_ID_hash) %>%
                  arrange(desc(cosine)) %>%
                  slice(1:30)

ts_est <- merge(topscores_est,estcps, all.x=T)
ts_est$purch[is.na(ts_est$purch)]<-0
ts_est <- ts_est %>% group_by(USER_ID_hash) %>% arrange(desc(cosine)) %>%slice(1:10)
MAP10.fn(ts_est,"USER_ID_hash","COUPON_ID_hash","cosine","purch")

### convert raw scores
raw_score_te <- as.data.frame(as.matrix(score_te))
colnames(raw_score_te)<- test$COUPON_ID_hash
raw_score_te$USER_ID_hash <- uchar$USER_ID_hash
raw_score_te <- gather(raw_score_te,COUPON_ID_hash,cosine,-USER_ID_hash)
topscores_te <- raw_score_te %>% 
  group_by(USER_ID_hash) %>%
  arrange(desc(cosine)) %>%
  slice(1:20)

save(topscores_est, topscores_te, file="../Data/cosinepurchscores.Rdata")

sub <- prepsub(as.data.frame(topscores_te),"USER_ID_hash","COUPON_ID_hash","cosine", user)
load("../Data/NaiveCoupon.Rdata")
load("../Data/cosinevisittecores.Rdata")
load("../Data/known.Rdata")
visitsub <- prepsub(as.data.frame(topscores_te_visit),"USER_ID_hash","COUPON_ID_hash","cosine", user)
known <- which(sub$USER_ID_hash %in% subknown$USER_ID_hash)
sub <- sub[-known,]
sub <- rbind(sub,subknown)
replaced<- sub$USER_ID_hash[which(is.na(sub[,2]))]
visitsub <- visitsub[visitsub$USER_ID_hash%in%replaced,]
sub <- sub[!is.na(sub[,2]),]
sub <- rbind(sub,visitsub)
load("../Data/NaiveCoupon.Rdata")
sub[is.na(sub$PURCHASED_COUPONS),"PURCHASED_COUPONS"]<- coupons_te
write.csv(sub, file="../Data/cosineensemble.csv", row.names=F)



