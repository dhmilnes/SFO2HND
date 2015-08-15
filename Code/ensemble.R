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
source("precision.R")
source("scalingdffn.R")
cpltr <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=T)
cplte <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=T)
cpdtr <- read.csv("../Data/coupon_detail_train.csv", stringsAsFactors=T)
user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)

### train to train plus validate
### thought process:  all users are valid in all cases so just need to hold out couponsfor second to last week.  
### random sample users to make set smaller.  
cpltr$DISPFROM <- as.Date(as.character(cpltr$DISPFROM))
cplte$DISPFROM <- as.Date(as.character(cplte$DISPFROM))

cplest <- cpltr[cpltr$DISPFROM >= '2012-06-17',]
cpl_tr <- cpltr[cpltr$DISPFROM < '2012-06-17',]

#### load model results
load("../Data/cosinepurchscores.Rdata")
load("../Data/cosinevisitestcores.Rdata")
load("../Data/cosinevisittecores.Rdata")
cp_pop_est<- read.csv("../Data/expectedpopest.csv")
cp_pop_test <- read.csv("../Data/expectedpoptest.csv")

names(topscores_est_visit)[1]<- "visitsc"
names(topscores_te_visit)[1]<- "visitsc"

### prep estimate
ts_est <- merge(topscores_est,topscores_est_visit,all.x=T)
#reverse <- merge(topscores_est,topscores_est_visit, all.y=T)
#reverse <- reverse[reverse$score,
ts_est <- merge(ts_est,cp_pop_est[,-1])
cps_est <- cpdtr$COUPON_ID_hash %in% cplest$COUPON_ID_hash
estcps<- cpdtr[cps_est,c("COUPON_ID_hash","USER_ID_hash")]
estcps <- unique(estcps)
estcps$purch <- 1
ts_est <- merge(ts_est,estcps, all.x=T)
ts_est$purch[is.na(ts_est$purch)]<-0
ts_est$visitsc[is.na(ts_est$visitsc)]<-0
names(ts_est)[names(ts_est)=="prfest"]<- "pop"
rm(topscores_est, topscores_est_visit)

### use scaling function to scale each users scores want 1/0 scaling not zero center scaling for regression (I think)
ts_est <- scale.cols.10.fn(ts_est,"USER_ID_hash",c("score","visitsc","pop"))
ts_est$visitsc[is.na(ts_est$visitsc)]<-0



###### Make Model one more split - estimate into train and estimate
set.seed(1)
ens_tr_rows <- sample(nrow(ts_est), .8*nrow(ts_est))
                                        
ens_tr_df <- ts_est[ens_tr_rows,] 
ens_est_df <- ts_est[-ens_tr_rows,]

### prep test
ts_te <- merge(topscores_te,topscores_te_visit,all.x=T)
ts_te <- merge(ts_te,cp_pop_test[,-1])
ts_te$visitsc[is.na(ts_te$visitsc)]<-0
names(ts_te)[names(ts_te)=="prfte"]<- "pop"
ts_te <- scale.cols.10.fn(ts_te,"USER_ID_hash",c("score","visitsc","pop"))
ts_te$visitsc[is.na(ts_te$visitsc)]<-0
head(ts_te)


mmte <- model.matrix(~score+pop+visitsc, data=ts_te)
mmtr <- model.matrix(purch~pop+score+visitsc, data=ens_tr_df)
mmest <- model.matrix(purch~pop+score+visitsc, data=ens_est_df)

cv.ens <- cv.glmnet(y=ens_tr_df$purch, x=mmtr, family="binomial", alpha=1)
lambda.min <- cv.ens$lambda.min 
ens <- glmnet(x= mmtr, y=ens_tr_df$purch, family="binomial", alpha=1, intercept=T)
plot(ens)
py_tr <- predict(ens,newx=mmtr, type="response", s=lambda.min)
py_est <- predict(ens,newx=mmest, type="response", s=lambda.min)
py_te <-predict(ens,newx=mmte, type="response", s=lambda.min)

auc.fn(py_est,ens_est_df$purch)
plotprec.fn(py_est,ens_est_df$purch)
#plotauc.fn(py_est,ens_est_df$purch)

#### Explore ensemble Models
ens_est_df$prob <- py_est
ens_est_df <- ens_est_df[with(ens_est_df, order(USER_ID_hash,-prob)),]
ens_est_df[ens_est_df$USER_ID_hash=="0000b53e182165208887ba65c079fc21",]

ens_tr_df$prob <- py_tr
ens_tr_df <- ens_tr_df[with(ens_tr_df, order(USER_ID_hash,-prob)),]
ens_tr_df[ens_tr_df$USER_ID_hash=="32f93b1872c1fe4e7e590d0305a3f02d",]

ts_te$py <- py_te
ts_te <- ts_te[with(ts_te, order(USER_ID_hash,-py)),]

head(ts_te,20)
rm(mmest,mmte,mmtr,estcps,ens_tr_df,ens_est_df,cpltr,cplte,cplest)
sub <- prepsub(ts_te,"USER_ID_hash","COUPON_ID_hash","py", user)
load("../Data/NaiveCoupon.Rdata")
sub[,2]<- as.character(sub[,2])
sub[is.na(sub$PURCHASED_COUPONS),"PURCHASED_COUPONS"]<- coupons_te
write.csv(sub, file="../Data/submitensemble.csv", row.names=F)
