library(reshape2)
#setwd("Code")
library(vegan)

cpatr <- read.csv("../Data/coupon_area_train.csv", stringsAsFactors=T)
cpate <- read.csv("../Data/coupon_area_test.csv", stringsAsFactors=T)
cpatr$flag <- 1
cpate$flag <- 1
cpa <- rbind(cpatr,cpate)
d <- dcast(cpa,COUPON_ID_hash~SMALL_AREA_NAME, sum, fill=0)
du <- unique(d[,-1])
ddu <- vegdist(du,method ="jaccard")
rm(cpa, cpate,cpatr)
hddu <- hclust(ddu,method="ward.D")
#plot(hddu)
#rect.hclust(hddu, k=20)  ##visually selected k, no real science
cuts <- cutree(hddu, k=20)
duc <- cbind(du,cuts)
f <- merge(d,duc)

cpag <- f[,c("COUPON_ID_hash","cuts")]
names(cpag)[2]<- "distgroup"
save(cpag,file="../Data/cpag.Rdata")