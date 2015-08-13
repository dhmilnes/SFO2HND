#### convert data frame with scores into a submission




prepsub <- function(df, user_col_name, coupon_col_name, score_col_name, user_df){
  user <- which(names(df) == user_col_name)
  cp <- which(names(df) == coupon_col_name)
  score <- which(names(df)== score_col_name)
  df[,cp]<- as.character(df[,cp])
  df[,user]<- as.character(df[,user])
  uchar <- unique(df[,user])
  s<- split(df, df[,user])
  s2 <- do.call(rbind,
                lapply(1:length(s),FUN=function(u){
                        dfu <- s[[u]]
                        dfu <- dfu[order(dfu[,score], decreasing=T),]
                        c<- paste(dfu[,cp][1:10],collapse=" ")
                        c(USER_ID_hash=dfu[1,user],PURCHASED_COUPONS=c)
                      }))
  s2<- data.frame(s2)
  s2 <- merge(user_df,s2, all.x=T)[,c("USER_ID_hash","PURCHASED_COUPONS")]
}


#  df <- ts_te
#  user_col_name <- "USER_ID_hash"
#  coupon_col_name <- "COUPON_ID_hash"
#  score_col_name <- "score"
# u <- 1

