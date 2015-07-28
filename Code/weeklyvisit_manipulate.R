###
###WV = weekly visits

#setwd("Code")
WVtrain <- read.csv("../Data/weekly_visit_train.csv", stringsAsFactors=F)
WVtrain$week <- as.Date(WVtrain$week)
WVtrain$week_p1 <- WVtrain$week+7

WVwithPrior <- merge(WVtrain,WVtrain, by.x=c("week_p1","user_hash"),by.y=c("week","user_hash"), all.x=T)
WVwithPrior <- WVwithPrior[,-8]
rm(WVtrain)

names(WVwithPrior) = c("week", "user_hash","week_p","visit_p", "purch_p","visit_c","purch_c")

WVwithPrior$visit_c[is.na(WVwithPrior$visit_c)]<-0
WVwithPrior$purch_c[is.na(WVwithPrior$purch_c)]<-0

save(WVwithPrior,file="../Data/visitspriorweek.Rdata")


