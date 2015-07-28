## Functions to give fit measures
## Doug's Aucwrapper
library(ROCR)
auc.fn <- function(prediction, actuals){
  pred <- prediction(prediction, actuals)
  perf <- performance(pred, measure = "auc")
  as.numeric(perf@y.values)
}

## Addition to Doug's Aucwrapper to also plot ROC curve
plotauc.fn <- function(prediction, actuals){ ## prediction as called with predict(... "residuals")
  pred <- prediction(prediction, actuals)
  perf <- performance(pred, measure = "auc")
  auc <- as.numeric(perf@y.values)
  ## alternative:   auc <- attributes(performance(pred,'auc'))$y.values[[1]]
  perf.tpr.fpr <- performance(pred, measure = "tpr","fpr")
  
  plot(perf.tpr.fpr, main="ROC plot", cex=0.8, cex.axis=0.8, 
       cex.main=0.8, cex.lab = 0.8, cex.sub = 0.8, colorize = T)
  abline(a=0, b=1)
  text(0.3,0.6, cex = 0.8, paste("AUC = ",format(auc, digits=3, scientific=FALSE)))
}

## PDSWR Listing 9.1
## A function to calculate log likelihood (for calculating deviance).

loglikelihood <- function(y, py) {
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1, 1-1e-12, py))
  sum(y * log(pysmooth) + (1-y)*log(1 - pysmooth))
}

## A function to calculate and return various measures on the model: 
## normalized deviance, prediction accuracy,
## and f1, which is the product of precision and recall.

accuracyMeasures <- function(pred, truth, name="model") {
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)  
  ## Normalize the deviance by the number of data points so that we can
  ## compare the deviance across training and test sets.  
  ctable <- table(truth=truth,
                  ## Convert the class probability estimator into a classifier by
                  ## labeling documents that score greater than 0.5 as spam.
                  pred=(pred>0.5))
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  specificity <- ctable[1,1]/sum(ctable[1,])
  f1 <- precision*recall
  data.frame(model=name, accuracy=accuracy, specificity = specificity, sens_recall = recall, f1=f1, dev.norm)
}
