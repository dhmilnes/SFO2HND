#### convert data frame with scores into a submission
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


#dplyr
  
#  df <- t
#  user_col_name <- "USER_ID_hash"
#  coupon_col_name <- "COUPON_ID_hash"
#  score_col_name <- "score"
#  u <- 1
# user_df<- user
