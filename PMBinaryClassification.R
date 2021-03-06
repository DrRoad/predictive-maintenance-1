###################################### Load UDFs & packages ######################################

# load UDFs
source("functionsPlot.r")
source("functionsUtility.r")

packages <- c("ggplot2","dplyr","scales","grid","class","gmodels","caret","ROCR","kernlab","e1071","corrplot")
installAndLoadPackages(packages)

# turn off scientific notations
options("scipen"=10)

###################################### Load & Transform Dataset ######################################

sampleFull <- read.csv("csavarozo1.csv", sep = ";")
sampleFull <- read.csv("C:/Users/dtakacs/Desktop/testpad_2016-08-21.csv", sep = ";")

sampleRate <- 48000
windowsSize <- 4096
timeFrameMilliSecond <- 600000

setupDataFrame(sampleFull, sampleRate, windowsSize, timeFrameMilliSecond)

###################################### legrepzentánsabb Hz kiválasztása #########################
sample <- sampleFull[0:1000, ]
labelRow(sample,143000,144000,1)
labelRow(sampleLabeled,144500,146000,1)
labelRow(sampleLabeled,147500,148500,1)
labelRow(sampleLabeled,156500,157500,1)
labelRow(sampleLabeled,158500,159500,1)
labelRow(sampleLabeled,160500,161500,1)


sample <- sampleFull[1641:1782, ]
sample <- sampleFull[0:1000,]

hertzek <- c("Hz1629","Hz1641","Hz1652","Hz1664","Hz1676")
amps <- c(100000,250000,500000,750000)

result <- data.frame(matrix(NA, nrow = 5, ncol = 4))
names(result) <- amps
rownames(result) <- hertzek

for (hertz in hertzek) {
  for (amp in amps) {
    
    print(paste("Evaluating ",hertz," for value: ",amp,sep=""))
    sampleLabeled <- sample %>% mutate(label = ifelse(sample[[hertz]] > amp,1,0))
    
    # Randomize 
    sampleLabeled <- sampleLabeled[sample(1:nrow(sampleLabeled)),]
    
    #split
    ind <- sample(2, nrow(sampleLabeled), replace=TRUE, prob=c(0.7, 0.3)) 
    sampleLabeled$label <- as.factor(sampleLabeled$label)
    
    sampleLabeled.training <- sampleLabeled[ind == 1, 2:ncol(sampleLabeled)]
    sampleLabeled.test <- sampleLabeled[ind == 2, 2:ncol(sampleLabeled)] 
    
    sampleLabeled.trainLabels <- sampleLabeled[ind==1, 2]
    sampleLabeled.testLabels <- sampleLabeled[ind==2, 2] 
    
    sampleLabeled.training.norm.svm <- sampleLabeled.training
    sampleLabeled.test.norm.svm <- sampleLabeled.test
    sampleLabeled.training.norm.svm[,-1] <- as.data.frame(lapply(sampleLabeled.training[,-1], normalize) )
    sampleLabeled.test.norm.svm[,-1] <- as.data.frame(lapply(sampleLabeled.test[,-1], normalize)) 
    
    ########## compare AUC if needed
    if (max(as.numeric(sampleLabeled.testLabels)) != 1) {
      svmModel  <- ksvm(label ~ ., data = sampleLabeled.training.norm.svm,kernel = "polydot",C = 2)
      svmPrediction  <- predict(svmModel , sampleLabeled.test.norm.svm , type = "response")
      evaluateModel(svmPrediction, sampleLabeled.testLabels)
      
      result[[hertz, toString(amp)]] <- auc
      
    } else {
      print(paste("No valid predictions for: ", hertz, sep = ""))
      result[[hertz, toString(amp)]] <- 0
    }
  }
}
result


###################################### Label dataset for given Hz #########################

# "telefoncsörgés" (1610-1680 Hz) ábrázolása első kb 40 mp-re 1641 Hz-en 
sample <- sampleFull[0:460, ]
sample <- sampleFull[0:3000, ]

createPlotHertz(1641) 

sampleLabeled <- sample %>% mutate(label = ifelse(Hz1641 > 500000,1,0))

# csavarás 1950 Hz körül 
createPlotHertz(1945) + geom_vline(xintercept = c(5000,11750, 26800,33000),
                                   colour="red",
                                   linetype = "longdash",
                                   size = 1,5)


###################################### EDA ######################################

# correlation matrix
corrTable <- cor(sample[,3:100])
corrplot(corrTable, method="color", tl.pos="n")

table(sampleLabeled$label)
round(prop.table(table(sampleLabeled$label)) * 100, digits = 1)

#create plot
p1 <- createPlotHzAvgBar(28000, 29000,1043,19992)
p2 <- createPlotHzAvgBar(31000, 32000,1043,19992)
p3 <- createPlotHzAvgBar(40000, 41000,1043,19992)
p4 <- createPlotHzAvgBar(43000, 44000,1043,19992)

multiplot(p1, p2, p3, p4, cols = 1)

###################################### Split Dataset ######################################

# Randomize 
sampleLabeled <- sampleLabeled[sample(1:nrow(sampleLabeled)),]

#split
ind <- sample(2, nrow(sampleLabeled), replace=TRUE, prob=c(0.7, 0.3)) 
sampleLabeled$label <- as.factor(sampleLabeled$label)

sampleLabeled.training <- sampleLabeled[ind == 1, 2:ncol(sampleLabeled)]
sampleLabeled.test <- sampleLabeled[ind == 2, 2:ncol(sampleLabeled)] 

sampleLabeled.trainLabels <- sampleLabeled[ind==1, 2]
sampleLabeled.testLabels <- sampleLabeled[ind==2, 2] 

###################################### KNN model ######################################

########## simple model: k=5
sampleLabeled.training.norm.knn <- as.data.frame(lapply(sampleLabeled.training[,2:ncol(sampleLabeled.training)], normalize)) 
sampleLabeled.test.norm.knn <- as.data.frame(lapply(sampleLabeled.test[,2:ncol(sampleLabeled.test)], normalize)) 

knnPrediction <-
  knn(
    train = sampleLabeled.training.norm.knn,
    test = sampleLabeled.test.norm.knn,
    cl = sampleLabeled.trainLabels,
    k = 3
  ) 

########## evaluate simple model
evaluateModel(knnPrediction,sampleLabeled.testLabels,"print")

########## param tuning: params = k
result <- rep(0,5)
kMax <- 5
for (i in 1:kMax) {

    knnPrediction <-
    knn(
      train = sampleLabeled.training.norm.knn,
      test = sampleLabeled.test.norm.knn,
      cl = sampleLabeled.trainLabels,
      k = i
    ) 
  evaluateModel(knnPrediction, sampleLabeled.testLabels,"print")
  result[i] <- accuracy
  print(paste("Round: ",i,"/",kMax," -- Accuracy: ", result[i],sep=""))
  
}
result


###################################### SVM ######################################
sampleLabeled.training.norm.svm <- sampleLabeled.training
sampleLabeled.test.norm.svm <- sampleLabeled.test
sampleLabeled.training.norm.svm[,-1] <- as.data.frame(lapply(sampleLabeled.training[,-1], normalize) )
sampleLabeled.test.norm.svm[,-1] <- as.data.frame(lapply(sampleLabeled.test[,-1], normalize)) 


########## simple model: kernel = polydot, c = 2
svmModel  <- ksvm(label ~ .,
                  data = sampleLabeled.training.norm.svm ,
                  kernel ="polydot",
                  C=2)

svmPrediction  <- predict(svmModel ,sampleLabeled.test.norm.svm ,type="response")

########## evaluate simple model
evaluateModel(svmPrediction,sampleLabeled.testLabels,"print")
confusionMatrix(svmPrediction, sampleLabeled.testLabels, positive = "1")

########## param tuning: params = kernel, cost function
compareSvmParamsAccuracy(5)
compareSvmParamsSensitivity(5)
compareSvmParamsAuc(5)

########## k-fold cross-validation: kernel = polydot, c = 2

# Randomize & scale the dataset
sampleLabeled <- sampleLabeled[sample(1:nrow(sampleLabeled)),]
sampleLabeled$label <- as.factor(sampleLabeled$label)
sampleLabeled[,3:ncol(sampleLabeled)] <- as.data.frame(lapply(sampleLabeled[,3:ncol(sampleLabeled)], normalize)) 

# Perform k-fold cross-validation
number_folds <- 5
folds <- cut( seq (1, nrow ( sampleLabeled )),
              breaks = number_folds ,
              labels = FALSE )

svmAccuracy <- rep (0, number_folds)

for (i in 1: number_folds ){
  
  testIndexes <- which ( folds ==i, arr.ind = TRUE )
  testData <- sampleLabeled[testIndexes , ]
  trainData <- sampleLabeled[-testIndexes , ]
  trainData.labels <- trainData[, 2] 
  testData.labels <- testData[, 2] 
  trainData <- trainData[,2:ncol(trainData)]
  testData <- testData[,2:ncol(testData)]
  
  svmModel  <- ksvm(label ~ .,
                     data = trainData,
                     kernel ="polydot",
                     C=2)
  
  svmPrediction  <- predict(svmModel ,testData ,type="response")
  
  # measure accuracy
  svmAccuracy[i] <-sum ( svmPrediction == testData.labels )/ nrow (testData)
  
  confusion_table  <- table(Predicted = svmPrediction, Actual = testData.labels)
  
  print(paste("Round: ",i,"/",number_folds," -- Accuracy: ", round(svmAccuracy[i],4),sep=""))
  print(confusion_table)
} 
svmAccuracy
mean(svmAccuracy)



