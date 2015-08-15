#
#MAP@10 = 1/|U| sigma1-U = 1/min(m,10)Sigma k=1-min(n,10) P(k) (true positives)/(number of true plus false) 
#
#where |U| is the number of users, P(k) is the precision at cutoff k, n is the number of predicted coupons, 
#and m is the number of purchased coupons for the given user. If m = 0, the precision is defined to be 0.


### user precision
library(dplyr)

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
          head(df)
          df <- df[order(df$user_id,-df$score),]
          df$actual_cps <- ifelse(df$actual==1,df$cp_id,"")
          
          
          #### fancy dplyr
          U10scores <- df %>%
                            group_by(user_id) %>%
                            summarize(U10sc = U10.fn(cp_id,actual_cps))
          MAP10score<- mean(U10scores$U10sc)
          MAP10score
}

MAP10.fn(ts_est,"USER_ID_hash","COUPON_ID_hash","score","purch")

# toy <- ts_est[ts_est$USER_ID_hash=="036dd5a7dff393b474199c45b6bc32bc",]
# save(toy, file="../Data/toy.Rdata")
# load("../Data/toy.Rdata")
#MAP10 <- function(df,user_col,score_col,coupon_col,actual_col)
# purchases <- toy[toy$purch==1,"COUPON_ID_hash"]
# recos <-   toy[order(toy$score,decreasing=T),"COUPON_ID_hash"]
# recos <- recos[c(4,1:3,5:10)]
# 
# recos
# purchases <- ""

# df <- ts_est
# user_col_name <- "USER_ID_hash"
# coupon_col_name <- "COUPON_ID_hash"
# score_col_name <- "score"
# actual_col_name <- "purch"
#  user_id<-"036dd5a7dff393b474199c45b6bc32bc"
