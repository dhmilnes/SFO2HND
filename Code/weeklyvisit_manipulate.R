###
###WV = weekly visits
library(plyr)
#setwd("Code")
WVtrain <- read.csv("../Data/weekly_visit_train.csv", stringsAsFactors=F)
WVtrain$week <- as.Date(WVtrain$week)
WVtrain$week_p1 <- WVtrain$week+7


### Join current to prior - All entries have a prior visit
WVwithPrior <- merge(WVtrain,WVtrain, by.x=c("week_p1","user_hash"),by.y=c("week","user_hash"), all.x=T)
WVwithPrior <- WVwithPrior[,-8]
names(WVwithPrior) = c("week", "user_hash","week_p","visit_p", "purch_p","visit_c","purch_c")
WVwithPrior$visit_c[is.na(WVwithPrior$visit_c)]<-0
WVwithPrior$purch_c[is.na(WVwithPrior$purch_c)]<-0

### Join prior to current - All entries have a current visit
WVwithCurrent <- merge(WVtrain,WVtrain, by.x=c("week","user_hash"),by.y=c("week_p1","user_hash"), all.x=T)
WVwithCurrent <- WVwithCurrent[,-5]
names(WVwithCurrent) = c("week", "user_hash","visit_c","purch_c", "week_p","visit_p","purch_p")
WVwithCurrent <- WVwithCurrent[,c("week", "user_hash","week_p","visit_p", "purch_p","visit_c","purch_c")]
#head(WVwithCurrent[WVwithCurrent$week == '2011-07-09',],10)

### Append non-overlap portion of WVwithCurrent
App <- WVwithCurrent[is.na(WVwithCurrent$visit_p),]
App$visit_p[is.na(App$visit_p)]<-0
App$purch_p[is.na(App$purch_p)]<-0
App$week_p[is.na(App$week_p)]<- App$week[is.na(App$week_p)]-7
unioned <- rbind(WVwithPrior,App)
unioned <- unioned[order(unioned$week),]

rm(WVtrain, WVwithCurrent, WVwithPrior, App)

### some additional features
WV <- ddply(unioned, .(user_hash), mutate,
               firstweek = min(week),
               weeks = (week-firstweek)/7)
WV$weeks <- as.numeric(WV$weeks)

WV$ret_flag <- WV$visit_c!=0 
WV$ret_purch <- WV$purch_c!=0
WV$flag_first_week <- WV$week==WV$firstweek

save(WV,file="../Data/visitspriorweek.Rdata")

rm(unioned)
