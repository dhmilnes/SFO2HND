source("prepsubfn.R")
cpvtr <- read.csv("../Data/user_coupon_impressions_train.csv", stringsAsFactors=T)
cplte <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=T)
user <- read.csv("../Data/user_list.csv", stringsAsFactors=T)
test <- merge(cpvtr,cplte)

test<- test[,1:2]
test$score <- 1

sub <- prepsub(t,"USER_ID_hash","COUPON_ID_hash","score",user)
head(sub)
subknown <- sub[!is.na(sub$PURCHASED_COUPONS),]

write.csv(sub, file="../Data/invisitpurch.csv", row.names=F)
head(sub)
save(subknown,file="../Data/known.Rdata")
