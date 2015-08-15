
### scale columns of a data frame between 1 and 0
scale.cols.10.fn <- function(df,split_col,vector_scale_col_names){
  cols <- which(names(df) %in% vector_scale_col_names)
  dfsplit <- split(df,as.character(df[,split_col]))
  dfsplit <- lapply(dfsplit, FUN=function(spl){
    sspl <- spl[,cols]  
    maxs <- sapply(sspl,max)
    mins <- sapply(sspl,min)
    p <- scale(sspl,center=mins, scale=maxs-mins)
    spl[,cols]<- p
    spl
  })
  df <- do.call(rbind,dfsplit)
  df
}

### scale columns of a data frame to center 0 and standard dev
scale.cols.sd.fn <- function(df,split_col,vector_scale_col_names){
  cols <- which(names(df) %in% vector_scale_col_names)
  dfsplit <- split(df,as.character(df[,split_col]))
  dfsplit <- lapply(dfsplit, FUN=function(spl){
    sspl <- spl[,cols]  
    p <- scale(sspl)
    spl[,cols]<- p
    spl
  })
  df <- do.call(rbind,dfsplit)
  df
}