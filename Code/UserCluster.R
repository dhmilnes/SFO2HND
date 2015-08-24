library(tidyr)
library(dplyr)
library(Matrix)
cpltr <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=T)
cpdtr <- read.csv("../Data/coupon_detail_train.csv", stringsAsFactors=T)
user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)
load("../Data/cpag.Rdata")
load("../Data/optcosinepurchweights")
cpag$distgroup <- as.factor(cpag$distgroup)

### train to train plus validate
### thought process:  all users are valid in all cases so just need to hold out couponsfor second to last week.  
### random sample users to make set smaller.  
cpltr$DISPFROM <- as.Date(as.character(cpltr$DISPFROM))

cpl_tr <- cpltr[cpltr$DISPFROM < '2012-06-17',]

train <- merge(cpdtr,cpl_tr)
train <- merge(train,cpag)

###### TO ADD ITEM COUNT:  ADD

cols <- c("COUPON_ID_hash","USER_ID_hash",
          "GENRE_NAME","DISCOUNT_PRICE","PRICE_RATE",
          "USABLE_DATE_MON","USABLE_DATE_TUE","USABLE_DATE_WED","USABLE_DATE_THU",
          "USABLE_DATE_FRI","USABLE_DATE_SAT","USABLE_DATE_SUN","USABLE_DATE_HOLIDAY",
          "USABLE_DATE_BEFORE_HOLIDAY","ken_name","small_area_name","distgroup")

### ADD IN ITEM COUNT AS A VOTE??
train <- train[,cols]

#NA imputation
train[is.na(train)] <- 1

#feature engineering
train$DISCOUNT_PRICE <- 1/log10(train$DISCOUNT_PRICE)
train$PRICE_RATE <- log10(train$PRICE_RATE*100+1)

#convert the factors to columns of 0's and 1's
train <- cbind(train[,c(1,2)],model.matrix(~ -1 + .,train[,-c(1,2)],
                                           contrasts.arg=lapply(train[,names(which(sapply(train[,-c(1,2)], is.factor)))], contrasts, contrasts=FALSE)))

uchar <- train[,-1] %>% group_by(USER_ID_hash) %>% summarise_each(funs(mean))

uchar$DISCOUNT_PRICE <- 1
uchar$PRICE_RATE <- 1

#### MATRIX W WEIGHTS
rm(cpdtr,cpl_tr,user, train, cpag)
weights <- optweight[[1]]  #(3,1,0,0,3,4,3) 
reps <- c(13,1,1,9,47,55,10)
W<- Diagonal(x=rep(weights, reps))
user.mat <- as.matrix(uchar[,2:ncol(uchar)])
users <-  user.mat %*% W%*% t(user.mat)

save(users, file="../Data/factorcosineusers.Rdata")

