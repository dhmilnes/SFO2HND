###  SCRIPT TO JOIN DESCRIPTIONS TO COUPON MASTER
###
#setwd("Code")
CLtrain <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=F)
capsule <- read.csv("../Data/capsule.csv")
cap <- table(CLtrain$CAPSULE_TEXT)
cap[grep("^WEB",names(cap))]<- 43
cap <- sort(cap, decreasing=T)
capsule <- cbind(capsule, names(cap), cap)
caps <- capsule[,c(2,3)]
names(caps)<- c("Eng","CAPSULE_TEXT")
CLtrain <- merge(CLtrain, caps, by="CAPSULE_TEXT")
rownames(capsule) <- capsule$English.Translation
names(capsule$cap) <- capsule$English.Translation
save(CLtrain,file="../Data/CLtrain.Rdata")

