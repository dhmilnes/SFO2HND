### attempt to optimize
setwd("Code")
load("../Data/cosine.Rdata")
source("MAP10.R")
library(dplyr)
library(Matrix)
library(tidyr)

weights <- c(3,1,.02,0,3,4,3)
reps <- c(13,10,1,1,47,55,130)

Map10score <- function(wvector){
    W<- Diagonal(x=rep(wvector, reps))
    score_est <- as.matrix(uchar[,2:ncol(uchar)]) %*% W %*% t(as.matrix(est[,2:ncol(est)]))*.95
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
    1/MAP10.fn(ts_est,"USER_ID_hash","COUPON_ID_hash","cosine","purch")
    }

Map10score(weights)
parm <- c(1,1,1,1,1,1,1)
optweight <- optim(parm,Map10score)
save(optweight,file="../Data/optcosinepurchweights")
###