## Functions to give fit measures
## Doug's Recall Precision Curve
library(ROCR)
plotprec.fn <- function(prediction, actuals){
  pred <- prediction(prediction, actuals)
  perf <- performance(pred, measure = "prec")
  plot(perf)
}