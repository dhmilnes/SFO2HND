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

#### load model results
load("../Data/cosinepurchscores.Rdata")
cp_pop_est<- read.csv("../Data/expectedpopest.csv")
cp_pop_test <- read.csv("../Data/expectedpoptest.csv")

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
ts_te <- merge(topscores_te,cp_pop_test[,-1])
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

rm(mmest,mmte,mmtr,estcps,ens_tr_df,ens_est_df,cpltr,cplte,cplest, py_est,cpag)

sub <- prepsub(ts_te,"USER_ID_hash","COUPON_ID_hash","py", user)
load("../Data/NaiveCoupon.Rdata")
sub[,2]<- as.character(sub[,2])
sub[is.na(sub$PURCHASED_COUPONS),"PURCHASED_COUPONS"]<- coupons_te
write.csv(sub, file="../Data/submitwcosinepop.csv", row.names=F)

