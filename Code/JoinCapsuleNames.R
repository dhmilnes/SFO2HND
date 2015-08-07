###  SCRIPT TO JOIN DESCRIPTIONS TO COUPON MASTER
###
#setwd("Code")
CLtrain <- read.csv("../Data/coupon_list_train.csv", stringsAsFactors=F)
CLtest <- read.csv("../Data/coupon_list_test.csv", stringsAsFactors=F)
capsule <- read.csv("../Data/capsule.csv")
cap <- table(CLtrain$CAPSULE_TEXT)
cap[grep("^WEB",names(cap))]<- 43
cap <- sort(cap, decreasing=T)
capsule <- cbind(capsule, names(cap), cap)
caps <- capsule[,c(2,3)]
names(caps)<- c("Eng","CAPSULE_TEXT")
CLtrain <- merge(CLtrain, caps, by="CAPSULE_TEXT")
CLtest <- merge(CLtest, caps, by="CAPSULE_TEXT")
save(CLtrain,CLtest,file="../Data/CLwithEng.Rdata")

masterg <- rbind(CLtrain,CLtest)
str(masterg)
masterg <- masterg[,c("COUPON_ID_hash","Eng")]

masterg

write.csv(masterg,"../Data/genrelookup.csv")
