setwd("Code")
library(dplyr)

prepsub <- function(df, user_col_name, coupon_col_name, score_col_name, user_df){
  df$users <- df[,user_col_name]
  df$cp <- df[,coupon_col_name]
  df$eval <- df[,score_col_name]
  submission <- df %>% group_by(users) %>% arrange(users,desc(eval)) %>% 
    summarize(leng = min(length(cp),10), PURCHASED_COUPONS=paste(cp[1:leng],collapse=" "))
  names(submission)[1]<- "USER_ID_hash"
  s <- merge(user_df,submission, all.x=T)
  s[,c("USER_ID_hash","PURCHASED_COUPONS")]
}


visit <- read.csv("../Data/coupon_visit_train.csv")
cplte <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=T)
user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)

visit2 <- visit[visit$VIEW_COUPON_ID_hash %in% cplte$COUPON_ID_hash,]
visit2 <- visit2[as.character(visit2$PURCHASEID_hash)!="",]
unique(as.character(visit2$PURCHASEID_hash))
