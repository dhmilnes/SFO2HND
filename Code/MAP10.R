#  Doug Milnes
#
# Script to calculate map10 on a holdout set.  Format is 1 row per 
#MAP@10 = 1/|U| sigma1-U = 1/min(m,10)Sigma k=1-min(n,10) P(k) (true positives)/(number of true plus false) 
#
#where |U| is the number of users, P(k) is the precision at cutoff k, n is the number of predicted coupons, 
#and m is the number of purchased coupons for the given user. If m = 0, the precision is defined to be 0.

library(dplyr)

### user precision
U10.fn <- function(recos, purchases){
  purchases <- purchases[purchases!=""]
  m <- length(purchases)
  if(m == 0 ) {0} else {
        n <- min(length(recos),10)
        tp <- 0
        fp <- 0
        sigmak<- 0 
        for (k in 1:n) {
          if (recos[k] %in% purchases){
            tp<- tp+1
          } else {fp<-fp+1}
          
          sigmak<- sigmak+tp/(fp+tp)
          #print(c(sigmak,tp,fp))  ## function checker
        }
        sigmak/min(m,10)
  }
}
 
MAP10.fn <- function(df,user_col_name,coupon_col_name,score_col_name, actual_col_name){
          names(df)[names(df) == user_col_name] <- "user_id"
          df$user_id<- as.character(df$user_id)
          
          names(df)[names(df) == coupon_col_name] <- "cp_id"
          df$cp_id<- as.character(df$cp_id)
          
          names(df)[names(df) == score_col_name] <- "score"
          names(df)[names(df) == actual_col_name] <- "actual"

          df <- df[order(df$user_id,-df$score),]
          df$actual_cps <- ifelse(df$actual==1,df$cp_id,"")
          
          #### dplyr
          U10scores <- df %>%
                            group_by(user_id) %>%
                            summarize(U10sc = U10.fn(cp_id,actual_cps))
          MAP10score<- mean(U10scores$U10sc)
          MAP10score
}

