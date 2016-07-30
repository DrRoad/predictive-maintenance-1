installAndLoadPackages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

setupDataFrame <- function(dataframe,sampleRate = 48000,windowsSize = 4096, timeFrameMs = 600000 ){
  #generate header
  names(dataframe) <- paste(rep("Hz",ncol(dataframe)),round(seq(1,ncol(dataframe),1)*sampleRate/windowsSize,0),sep="")
  
  #generate time
  timeMilliSecondVector <- round(seq(1,nrow(dataframe)) * timeFrameMs/nrow(dataframe),0)
  
  #label vector
  labelList <- rep(0,nrow(dataframe))
  
  sampleFull <<- cbind(timeMs = timeMilliSecondVector,label = labelList, dataframe)
  
}

filterSampleByTimeByHz <- function(dataframe, msFrom, msTo,hzFrom = 12, hzTo = 24000){
  hzLower<-  grep(paste("^Hz",hzFrom,"$",sep=""), colnames(dataframe))
  hzUpper<-  grep(paste("^Hz",hzTo,"$",sep=""), colnames(dataframe))
  sampleFiltered <<- dataframe %>%
                      filter(timeMs >= msFrom & timeMs <= msTo) %>% 
                      select(timeMs,hzLower:hzUpper)
}

labelRow <- function(dataframe, timeMsMin, timeMsMax, labelValue){
  lista <- dataframe$timeMs[dataframe$timeMs >= timeMsMin & dataframe$timeMs <= timeMsMax]
  dataframe$label[dataframe$timeMs %in% lista] <- labelValue
  sampleLabeled <<- dataframe
}

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num / denom)
}

evaluateModel <- function(prediction,label,print ="no"){
  confTable  <- table(Predicted = prediction ,Actual = label)
  sensitivityMeasure <<- percent(sensitivity(confTable, positive = "1"))
  specificity <- percent(specificity(confTable, negative = "0"))
  precision <- ifelse(is.finite(posPredValue(confTable, positive = "1") == TRUE),percent(posPredValue(confTable, positive = "1")),0) 
  recall <- percent(sensitivity(confTable, positive = "1"))
  
  pred <- prediction(predictions = as.numeric(prediction), labels = as.numeric(label))
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")

  
  perf.auc <- performance(pred, measure = "auc")
  auc <<- round(unlist(perf.auc@y.values),4)
  accuracy <<- percent((sum(prediction== label))/length(label)) 
  
  results <- cbind(
    Measure = c("Accuracy","Sensitivity", "Specificity", "Precision", "Recall", "AUC"),
    Value = c(accuracy,sensitivityMeasure, specificity, precision, recall, auc)
  )
  
  if (print == "print") {
    plot(perf, main = "ROC curve", col = "blue", lwd = 3)
    abline(a = 0, b = 1, lwd = 2, lty = 2)
    print(results)
    print(confTable)
  }

}

compareSvmParamsAccuracy <- function(costs) {
  result <- data.frame(matrix(NA, nrow = 4, ncol = costs))
  kernels <- c("polydot", "vanilladot", "tanhdot", "rbfdot")
  names(result) <- paste("cost=",c(1:costs),sep="")
  rownames(result) <- kernels
  
  for (kernel in kernels) {
    for (cost in 1:costs) {
      SVM_model  <-
        ksvm(label ~ .,
             data = sampleLabeled.training ,
             kernel = kernel,
             C = cost)
      
      svmPrediction  <-
        predict(SVM_model , sampleLabeled.test , type = "response")
      
      #evaluate model
      evaluateModel(svmPrediction, sampleLabeled.testLabels)
      result[[kernel, cost]] <- accuracy
    }
  }
  print(result)
}

compareSvmParamsSensitivity <- function(costs) {
  result <- data.frame(matrix(NA, nrow = 4, ncol = costs))
  kernels <- c("polydot", "vanilladot", "tanhdot", "rbfdot")
  names(result) <- paste("cost=",c(1:costs),sep="")
  rownames(result) <- kernels
  
  for (kernel in kernels) {
    for (cost in 1:costs) {
      SVM_model  <-
        ksvm(label ~ .,
             data = sampleLabeled.training ,
             kernel = kernel,
             C = cost)
      
      svmPrediction  <-
        predict(SVM_model , sampleLabeled.test , type = "response")
      
      #evaluate model
      evaluateModel(svmPrediction, sampleLabeled.testLabels)
      result[[kernel, cost]] <- sensitivityMeasure
    }
  }
  print(result)
}

compareSvmParamsAuc <- function(costs) {
  result <- data.frame(matrix(NA, nrow = 4, ncol = costs))
  kernels <- c("polydot", "vanilladot", "tanhdot", "rbfdot")
  names(result) <- paste("cost=",c(1:costs),sep="")
  rownames(result) <- kernels
  
  for (kernel in kernels) {
    for (cost in 1:costs) {
      SVM_model  <-
        ksvm(label ~ .,
             data = sampleLabeled.training ,
             kernel = kernel,
             C = cost)
      
      svmPrediction  <-
        predict(SVM_model , sampleLabeled.test , type = "response")
      
      #evaluate model
      evaluateModel(svmPrediction, sampleLabeled.testLabels)
      result[[kernel, cost]] <- auc
    }
  }
  print(result)
}


