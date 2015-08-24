#setwd("Code")
library(tidyr)
library(dplyr)
library(Matrix)
library(igraph)

#### Jaccard Formula
#####  http://bit.ly/1U2BGH2
jaccard_dist <- function(m) {
  ## common values:
  A = tcrossprod(m)
  ## indexes for non-zero common values
  im = which(A > 0, arr.ind=TRUE)
  ## counts for each row
  b = rowSums(m)
  
  ## only non-zero values of common
  Aim = A[im]
  
  ## Jacard formula: #common / (#i + #j - #common)
  J = sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = 1-Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
  
  return( J )
}

cpltr <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=T)
cpdtr <- read.csv("../Data/coupon_detail_train.csv", stringsAsFactors=T)

### train to train plus validate
cpltr$DISPFROM <- as.Date(as.character(cpltr$DISPFROM))
cpl_tr <- cpltr[cpltr$DISPFROM < '2012-06-17',]

### only evaluating similarity for the 'train' 'train' set excluding the validation hold-out (I call estimate)
cpdtr <- cpdtr[cpdtr$COUPON_ID_hash %in% cpl_tr$COUPON_ID_hash,c("USER_ID_hash","COUPON_ID_hash")]
cpdtr <- unique(cpdtr)
cpdtr$uid <- as.numeric(cpdtr$USER_ID_hash)
cpdtr$cid <- as.numeric(cpdtr$COUPON_ID_hash)
cpdtr <- unique(cpdtr[,c("USER_ID_hash","uid")])
rm(cpl_tr,cpltr)

smat <- sparseMatrix(i=cpdtr$uid,j=cpdtr$cid,x=1)
jsmat <- jaccard_dist(smat)
rm(smat)

save(jsmat, file="../Data/jaccard.Rdata")
load("../Data/jaccard.Rdata")

jdf <- as.data.frame(summary(jsmat))
rm(jsmat)

jdf<-jdf[jdf$x!=0,]

#### only need triangular matrix for distances
jdf<-jdf[with(jdf, j>=i),]

#### can choose nearest neighbors or cut by proximity range
###
nn <- 3
jdf<-jdf %>% group_by(i) %>% arrange(x) %>% slice(1:nn)
save(jdf,file="procjdf.Rdata")

gjdf <- graph.data.frame(jdf, directed = F)
E(gjdf)$weights<- jdf$x
fgc <- cluster_fast_greedy(gjdf)
sizes(fgc)
members <- membership(fgc)
uidmember <- data.frame(uid = names(members),group= as.numeric(members))
usergroups <- merge(cpdtr,uidmember)
save(usergroups, file="usergroups.Rdata")
