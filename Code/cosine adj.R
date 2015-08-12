### Ponpare Coupon Purchase Prediction ###
### Author: Doug Milnes ###
#read in all the input data
library(plyr)
library(ggplot2)
library(glmnet)
library(MASS)
library(reshape2)
source("Auc_Accuracy.R")
source("prepsubfn.R")

cpltr <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=T)
cplte <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=T)
cpdtr <- read.csv("../Data/coupon_detail_train.csv", stringsAsFactors=T)
user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)
capsule <- read.csv("../Data/genrelookup.csv")
load("../Data/cpag.Rdata")
cpag$distgroup <- as.factor(cpag$distgroup)

### train to train plus validate
### thought process:  all users are valid in all cases so just need to hold out couponsfor second to last week.  
### random sample users to make set smaller.  
cpltr$DISPFROM <- as.Date(as.character(cpltr$DISPFROM))
cplte$DISPFROM <- as.Date(as.character(cplte$DISPFROM))

cplest <- cpltr[cpltr$DISPFROM >= '2012-06-17',]
cpl_tr <- cpltr[cpltr$DISPFROM < '2012-06-17',]

set.seed(1)
cpsamp <- sample(nrow(cplest),100)
set.seed(2)
usamp <- sample(nrow(user),400)


#####  Similarity 

train <- merge(cpdtr,cpl_tr)
train <- merge(train,cpag)
cols <- c("COUPON_ID_hash","USER_ID_hash",
          "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
          "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
          "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
          "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name","distgroup")
### HOW TO ADD IN ITEM COUNT?
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
train$PRICE_RATE <- (train$PRICE_RATE*train$PRICE_RATE)/(100*100)

#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)))], contrasts, contrasts=FALSE)))
                                           
#separate the test from train
test <- train[train$USER_ID_hash=="dummyuser",]
test <- test[,-2]
est <- train[train$USER_ID_hash=="estimateduser",]
est <- est[,-2]
train <- train[!(train$USER_ID_hash %in% c("dummyuser","estimateduser")),]

#data frame of user characteristics
uchar <- aggregate(.~USER_ID_hash, data=train[,-1],FUN=mean)
uchar$DISCOUNT_PRICE <- 1
uchar$PRICE_RATE <- 1

require(Matrix)
W <- as.matrix(Diagonal(x=c(rep(3,13), rep(1,1), rep(0.2,1), rep(0,9), rep(1,47), rep(1,55), rep(1,10))))
score_te <-  as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(test[,2:ncol(test)]))
score_est <- as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(est[,2:ncol(est)]))

rownames(score_te)<- uchar$USER_ID_hash
colnames(score_te)<- test$COUPON_ID_hash

topscores_te <- do.call(rbind,
  lapply(1:nrow(uchar), FUN=function(user){
    top20 <- sort(score_te[user,],decreasing=T)[1:20]
    cps <- melt(top20)
    cps$USER_ID_hash <- uchar$USER_ID_hash[user]
    names(cps)[1] <- "score"
    cps$COUPON_ID_hash <- rownames(cps)
    cps
    }
  )
)

rownames(score_est)<- uchar$USER_ID_hash
colnames(score_est)<- est$COUPON_ID_hash

topscores_est <- do.call(rbind,
                        lapply(1:nrow(uchar), FUN=function(user){
                          top20 <- sort(score_est[user,],decreasing=T)[1:20]
                          cps <- melt(top20)
                          cps$USER_ID_hash <- uchar$USER_ID_hash[user]
                          names(cps)[1] <- "score"
                          cps$COUPON_ID_hash <- rownames(cps)
                          cps
                        }
                        )
)


save(topscores_est, topscores_te, file="../Data/cosinepurchscores.Rdata")

sub <- prepsub(topscores_te,"USER_ID_hash","COUPON_ID_hash","score", user)
load("../Data/NaiveCoupon.Rdata")
sub[,2]<- as.character(sub[,2])
sub[is.na(sub$PURCHASED_COUPONS),"PURCHASED_COUPONS"]<- coupons_te
write.csv(sub, file="../Data/submitwcosine.csv", row.names=F)










### prep estimate
ts_est <- merge(topscores_est,cp_pop_est[,-1])
cps_est <- cpdtr$COUPON_ID_hash %in% cplest$COUPON_ID_hash
estcps<- cpdtr[cps_est,c("COUPON_ID_hash","USER_ID_hash")]
estcps <- unique(estcps)
estcps$flag <- 1
ts_est <- merge(ts_est,estcps, all.x=T)
names(ts_est)[3:5] <- c("cosine","pop","y") 
ts_est$y[is.na(ts_est$y)]<-0


###### Make Model one more split - estimate into train and estimate
set.seed(1)
ens_tr_rows <- sample(nrow(ts_est), .8*nrow(ts_est))
                                        
ens_tr_df <- ts_est[ens_tr_rows,] 
ens_est_df <- ts_est[-ens_tr_rows,]

### prep test
ts_te <- merge(usertopscores_te,cp_pop_test[,-1])
names(ts_te)[2] <- c("cosine")
names(ts_te)[4] <- c("pop")
mmte <- model.matrix(~cosine+pop, data=ts_te)

mmtr <- model.matrix(y~pop+cosine, data=ens_tr_df)
mmest <- model.matrix(y~pop+cosine, data=ens_est_df)

cv.ens <- cv.glmnet(y=ens_tr_df$y, x=mmtr, family="binomial", alpha=1)
lambda.min <- cv.ens$lambda.min 
ens <- glmnet(x= mmtr, y=ens_tr_df$y, family="binomial", alpha=1, intercept=T)

py_est <- predict(ens,newx=mmest, type="response", s=lambda.min)
py_te <-predict(ens,newx=mmte, type="response", s=lambda.min)

auc.fn(py_est,ens_est_df$y)
#plotauc.fn(py_te,ens_te_df$y)

ts_te$py <- py_te
names(ts_te)[5] <- "py"  

rm(mmest,mmte,mmtr,score_est,score_te,estcps,est,ens_tr_df,ens_est_df,cpltr,cplte,cplest, py_est,cpag)

sub <- prepsub(ts_te,"USER_ID_hash","COUPON_ID_hash","py", user)
load("../Data/NaiveCoupon.Rdata")
sub[,2]<- as.character(sub[,2])
sub[is.na(sub$PURCHASED_COUPONS),"PURCHASED_COUPONS"]<- coupons_te
write.csv(sub, file="../Data/submitwcosinepop.csv", row.names=F)


