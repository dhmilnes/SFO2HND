### Ponpare Coupon Purchase Prediction ###
### Author: Doug Milnes ###
#read in all the input data
library(dplyr)
library(ggplot2)
library(glmnet)
library(MASS)
library(tidyr)
#setwd("Code")
source("prepsubfn.R")
source("MAP10.R")

cpltr <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=T)
cpdtr <- read.csv("../Data/coupon_detail_train.csv", stringsAsFactors=T)
cplte <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=T)
cpvtr <- read.csv("../Data/user_coupon_impressions_train.csv", stringsAsFactors=T)
load("../Data/cpag.Rdata")
cpag$distgroup <- as.factor(cpag$distgroup)

### train to train plus validate
### thought process:  all users are valid in all cases so just need to hold out couponsfor second to last week.  
### random sample users to make set smaller.  
cpltr$DISPFROM <- as.Date(as.character(cpltr$DISPFROM))
cplte$DISPFROM <- as.Date(as.character(cplte$DISPFROM))

cplest <- cpltr[cpltr$DISPFROM >= '2012-06-17',]
cpl_tr <- cpltr[cpltr$DISPFROM < '2012-06-17',]

cps_est <- cpdtr$COUPON_ID_hash %in% cplest$COUPON_ID_hash
estcps<- cpdtr[cps_est,c("COUPON_ID_hash","USER_ID_hash")]
estcps <- unique(estcps)
estcps$purch <- 1
rm(cpdtr)

#####  Similarity 
train <- merge(cpvtr,cpl_tr)
train <- merge(train,cpag)
rm(cpvtr,cpl_tr, cpltr)

purchweight <- 1
userweights <- train %>% mutate(weighted = impressions+purchweight*purchases)
userweights <- userweights[,c("USER_ID_hash","COUPON_ID_hash","weighted")]

cols <- c("COUPON_ID_hash","USER_ID_hash",
          "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
          #"USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
          #"USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
          #"USABLE_DATE_BEFORE_HOLIDAY",
          #"ken_name",
          "small_area_name","distgroup")

train <- train[,cols]
#combine the test set with the train
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
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)))], contrasts, contrasts=FALSE)))
                                           
#separate the test from train
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
est <- train[train$USER_ID_hash=="estimateduser",]
est <- est[,-2]
train <- train[!(train$USER_ID_hash %in% c("dummyuser","estimateduser")),]

rm(cpag,cpchar,cpest)


#data frame of user characteristics
uchar <- merge(train,userweights)
rm(userweights, train)

us <- uchar[,3:82]*uchar$weighted
us$USER_ID_hash <- uchar$USER_ID_hash
uchar <- us
rm(us)
uchar <- uchar %>% group_by(USER_ID_hash) %>% summarise_each(funs(mean))

uchar$DISCOUNT_PRICE <- 1
uchar$PRICE_RATE <- 1

require(Matrix)

#### GENRE PRICE, DISCOUNT, 

weights <- c(3,1,3,4,3)  #c(3,1,0,0,3,4,3)
reps <- c(13,1,1,55,10) #c(13,1,1,9,47,55,10)
W<- Diagonal(x=rep(weights, reps))

score_te <-  as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))
score_est <- as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(est[,2:ncol(est)]))



raw_score_est <- as.data.frame(as.matrix(score_est))
rm(score_est)
colnames(raw_score_est)<- est$COUPON_ID_hash
raw_score_est$USER_ID_hash <- uchar$USER_ID_hash
raw_score_est <- gather(raw_score_est,COUPON_ID_hash,cosine,-USER_ID_hash)
topscores_est_visit <- raw_score_est %>% 
  group_by(USER_ID_hash) %>%
  arrange(desc(cosine)) %>%
  slice(1:30)

ts_est <- merge(topscores_est_visit,estcps, all.x=T)
ts_est$purch[is.na(ts_est$purch)]<-0
ts_est <- ts_est %>% group_by(USER_ID_hash) %>% arrange(desc(cosine)) %>%slice(1:10)
MAP10.fn(ts_est,"USER_ID_hash","COUPON_ID_hash","cosine","purch")


####
raw_score_te <- as.data.frame(as.matrix(score_te))
colnames(raw_score_te)<- test$COUPON_ID_hash
raw_score_te$USER_ID_hash <- uchar$USER_ID_hash
raw_score_te <- gather(raw_score_te,COUPON_ID_hash,cosine,-USER_ID_hash)
topscores_te_visit <- raw_score_te %>% 
  group_by(USER_ID_hash) %>%
  arrange(desc(cosine)) %>%
  slice(1:20)

user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)
save(topscores_est_visit, file="../Data/cosinevisitestcores.Rdata")
save(topscores_te_visit, file="../Data/cosinevisittecores.Rdata")


sub <- prepsub(as.data.frame(topscores_te_visit),"USER_ID_hash","COUPON_ID_hash","cosine", user)
load("../Data/NaiveCoupon.Rdata")
sub[,2]<- as.character(sub[,2])
sub[is.na(sub$PURCHASED_COUPONS),"PURCHASED_COUPONS"]<- coupons_te
write.csv(sub, file="../Data/submitvcosine.csv", row.names=F)
