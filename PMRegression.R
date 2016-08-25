# load UDFs
source("functionsPlot.r")
source("functionsUtility.r")

packages <- c("ggplot2","dplyr","scales","grid","class","gmodels","caret","ROCR","kernlab","e1071","corrplot")
installAndLoadPackages(packages)

sampleFull <- read.csv("C:/Users/dtakacs/Desktop/testpad_2016-08-21.csv", sep = ";")

sampleRate <- 48000
windowsSize <- 4096
timeFrameMilliSecond <- 600000

setupDataFrame(sampleFull, sampleRate, windowsSize, timeFrameMilliSecond)
colnames(sampleFull)[2] <- "TimeToFailure"

#create label for TimeToFailure

labelVector <- c(seq(6361,1),sampleFull$TimeToFailure[(6361+1):nrow(sampleFull)])
labelVector <- c(seq(5705,1),sampleFull$TimeToFailure[(5705+1):nrow(sampleFull)])
sampleFull$TimeToFailure <- labelVector

# Randomize 
sampleRandom <- sampleFull[sample(1:nrow(sampleFull)),]

#split dataset
samplingVector <- createDataPartition(sampleRandom$TimeToFailure, p = 0.7, list = FALSE)
sampleTrain <- sampleRandom[samplingVector,]
sampleTrainLabels <- sampleRandom$TimeToFailure[samplingVector]
sampleTest <- sampleRandom[-samplingVector,]
sampleTestlabels <- sampleRandom$TimeToFailure[-samplingVector]

#principal component analysis
prin_comp <- prcomp(sampleTrain[,c(-1,-2)], scale. = T)
names(prin_comp)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
sampleTrainPCA <- data.frame(TimeToFailure = sampleTrain$TimeToFailure, prin_comp$x)

#select the first 600 components
sampleTrainPCA <- sampleTrainPCA[,1:600]

#transform test into PCA
sampleTestPCA <- predict(prin_comp, newdata = sampleTest)
sampleTestPCA <- as.data.frame(sampleTestPCA)

#select the first 600 components
sampleTestPCA <-sampleTestPCA[,1:600]

#################################### Linear Regression ####################################

modelLinearRegressionPCA <- lm(RemainingTime ~., data = sampleTrainPCA)
predictionsLinearRegressionPCA <- predict(modelLinearRegressionPCA, sampleTestPCA)

compute_rmse(round(predictionsLinearRegressionPCA), sampleTest$TimeToFailure)

resultLinearRegressionPCA <- cbind.data.frame(Time = sampleTest$timeMs,
                               Predictions = round(predictionsLinearRegressionPCA),
                               Actual = sampleTest$TimeToFailure,
                               Difference = abs(round(predictionsLinearRegressionPCA)-sampleTest$TimeToFailure))
#print median
median(resultLinearRegressionPCA$Difference)

#plot Actual-Predicted Difference
ggplot(data=resultLinearRegressionPCA, aes(x=Time, y=Difference)) +
  geom_line() +
  geom_point() + 
  geom_vline(xintercept = 487000,colour="red",linetype = "longdash",size = 1,5) +
  geom_hline(yintercept = median(comparisonWithPCA$Difference),colour="darkgoldenrod1",linetype = "longdash",size = 1,5) +
  geom_text(aes(0,median(comparisonWithPCA$Difference),label = round(median(comparisonWithPCA$Difference)), vjust = -0.5,hjust = 1.2)) +
  scale_x_continuous(breaks=seq(0,600000,60000)) + ggtitle("Linear Regression with PCA")


#################################### Neural Network without PCA ####################################

modelNeuralNetwork <- nnet(TimeToFailure~.,data=sampleTrain ,size =5,MaxNWts=22000,linout=TRUE)

predictionsNeuralNetwork<-predict(modelNeuralNetwork ,sampleTest ,type = "raw")

compute_rmse(round(predictionsNeuralNetwork), sampleTest$TimeToFailure)

resultNeuralNetwork <- cbind.data.frame(Time = sampleTest$timeMs,
                                   Predictions = round(predictionsNeuralNetwork),
                                   Actual = sampleTest$TimeToFailure,
                                   Difference = abs(round(predictionsNeuralNetwork)-sampleTest$TimeToFailure))


ggplot(data=resultNeuralNetwork, aes(x=Time, y=Difference)) +
  geom_line() +
  geom_point() + 
  geom_vline(xintercept = 487000,colour="red",linetype = "longdash",size = 1,5) +
  geom_hline(yintercept = median(resultNeuralNetwork$Difference),colour="darkgoldenrod1",linetype = "longdash",size = 1,5) +
  geom_text(aes(0,median(resultNeuralNetwork$Difference),label = round(median(resultNeuralNetwork$Difference)), vjust = -0.5,hjust = 1.2)) +
  scale_x_continuous(breaks=seq(0,600000,60000)) + ggtitle("Neural Network")


#################################### Neural Network with PCA ####################################

modelNeuralNetworkPCA <- nnet(TimeToFailure~.,data=sampleTrainPCA ,size =10,MaxNWts=22000,linout=TRUE)

predictionsNeuralNetworkPCA<-predict(modelNeuralNetworkPCA ,sampleTestPCA ,type = "raw")

compute_rmse(round(predictionsNeuralNetworkPCA), sampleTest$TimeToFailure)

comparisonNNet <- cbind.data.frame(Time = sampleTest$timeMs,
                               Predictions = round(classification_NN),
                               Actual = sampleTest$TimeToFailure,
                               Difference = abs(round(classification_NN)-sampleTest$TimeToFailure))

#################################### window ####################################
row <- vector()
df <- data.frame()
sampleTemp <- sampleFull[,c(-1,-2)]
createNewDataFrame <- function(n){
  row <- vector()
  df <<- data.frame()
  for (i in 1:nrow(sampleTemp) ){
    row <- c(row,sampleTemp[i,])
    
    if (i %% n == 0) {
      names(row) <- NULL
      df <<- rbind.data.frame(df,row)
      row <- vector()
      print(paste("Row = ",i,"/",nrow(sampleTemp),sep=""))
    }
  }
}

createNewDataFrame(110)

#create labels

ttf <- c(seq(57,0),0,0,0,0,0)

df$ttf <- ttf



# Randomize 
sampleRandom <- df[sample(1:nrow(df)),]

#split dataset
samplingVector <- createDataPartition(df$ttf, p = 0.7, list = FALSE)
sampleTrain <- sampleRandom[samplingVector,]
sampleTrainLabels <- sampleRandom$TimeToFailure[samplingVector]
sampleTest <- sampleRandom[-samplingVector,]
sampleTestlabels <- sampleRandom$TimeToFailure[-samplingVector]

dfPCA <- prcomp(sampleTrain[,-length(colnames(df))], scale. = T)

#add a training set with principal components
sampleTrainPCA <- data.frame( TimeToFailure = sampleTrain$ttf, dfPCA$x)

#compute standard deviation of each principal component
std_dev <- dfPCA$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#select the first 600 components
sampleTrainPCA <- sampleTrainPCA[,1:40]

#transform test into PCA
sampleTestPCA <- predict(dfPCA, newdata = sampleTest)
sampleTestPCA <- as.data.frame(sampleTestPCA)

#select the first 600 components
sampleTestPCA <-sampleTestPCA[,1:40]

modelNeuralNetworkPCA <- nnet(TimeToFailure~.,data=sampleTrainPCA ,size =10,MaxNWts=22000,linout=TRUE)

predictionsNeuralNetworkPCA<-predict(modelNeuralNetworkPCA ,sampleTestPCA ,type = "raw")

compute_rmse(round(predictionsNeuralNetworkPCA), sampleTest$ttf)

comparisonNNet <- cbind.data.frame(
                                   Predictions = round(predictionsNeuralNetworkPCA),
                                   Actual = sampleTest$ttf,
                                   Difference = abs(round(predictionsNeuralNetworkPCA)-sampleTest$ttf))
