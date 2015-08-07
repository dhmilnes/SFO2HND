###
### PARSE WEEKLY VISIT DATA
###

#setwd("Code")
library(plyr)
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
WV$flag_first_week <- as.numeric(WV$week==WV$firstweek)

save(WV,file="../Data/visitspriorweek.Rdata")

rm(unioned)

####
#### Create Test and Train Week Data
####

finalweek <- as.Date('2012-06-30')

#these cohorts behave differently than all other cohorts - believe represents a prior group of users
cohorts <- c(as.Date('2011-07-02'),as.Date('2011-07-09'),as.Date('2011-07-16'))


processweekset <- function(week_list,user_list,WV){
  ## make entries
  df <- merge(week_list,user_list)
  names(df)[1]<- "week"
  df <- merge(df, WV, by= c("week","user_hash"), all.x=T)
  
  ### manage NAs
  nas <- is.na(df$visit_p)
  df$visit_p[nas]<-0
  df$visit_c[nas]<-0
  df$purch_p[nas]<-0
  df$purch_c[nas]<-0
  df$ret_flag[nas]<-0
  df$ret_purch[nas]<-0
  df <- df[,-9]
  names(df)[3]<-"firstweek"
  df$weeks[nas] <- as.numeric((df$week[nas]-df$firstweek[nas])/7)
  df <- df[df$weeks>0,]
  df$cohort <- df$firstweek %in% cohorts
  df
}

any(is.na(WV$weeks))

weeks <- unique(WV$week)
weeks <- weeks[weeks!=finalweek]
users <- unique(WV[,c("user_hash","firstweek")])

set.seed(5)
samp <- sample(length(weeks),20)
weeks.train <- weeks[samp[1:17]]
weeks.test <- weeks[samp[18:20]]

submit <- processweekset(finalweek,users,WV)
train <- processweekset(weeks.train,users,WV)
test <- processweekset(weeks.test,users,WV)

save(WV,submit,train,test, file="../Data/ProcessedVisits.Rdata")
