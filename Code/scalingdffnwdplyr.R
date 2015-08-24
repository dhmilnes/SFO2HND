library(dplyr)

### scale columns of a data frame between 1 and 0
scale.cols.10.fn <- function(df,split_col,vector_scale_col_names){
  df$splitcol <- df[,split_col]
  df <- df %>% 
    group_by(splitcol) %>%
    mutate_each_(funs(percent_rank), vector_scale_col_names)
  df
}

### scale columns of a data frame to center 0 and standard dev
scale.cols.sd.fn <- function(df,split_col,vector_scale_col_names){
    cols <- which(names(df) %in% vector_scale_col_names)
    df$splitcol <- df[,split_col]
    df <- df %>% 
      group_by(splitcol) %>%
      mutate_each_(funs(scale), vector_scale_col_names)
    df
  }

###
#df <- ts_est
#split_col<- "USER_ID_hash"
#vector_scale_col_names <- c("score","visitsc","prfest")
# scale.cols.sd.fn(ts_est,"USER_ID_hash",c("score","visitsc","prfest"))