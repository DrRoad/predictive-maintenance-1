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
sampleFull$TimeToFailure <- labelVector

# Randomize 
sampleRandom <- sampleFull[sample(1:nrow(sampleFull)),]

#split dataset
samplingVector <- createDataPartition(sampleRandom$TimeToFailure, p = 0.7, list = FALSE)
sampleTrain <- sampleRandom[samplingVector,]
sampleTrainLabels <- sampleRandom$TimeToFailure[samplingVector]
sampleTest <- sampleRandom[-samplingVector,]
sampleTestlabels <- sampleRandom$TimeToFailure[-samplingVector]

#PCA


#principal component analysis
prin_comp <- prcomp(sampleTrain[,c(-1,-2)], scale. = T)
names(prin_comp)
prin_comp$rotation[1:10,1:8]

biplot(prin_comp, scale = 0)

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
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

#add a training set with principal components
train.data <- data.frame(RemainingTime = sampleTrain$TimeToFailure, prin_comp$x)

#we are interested in first 30 PCAs
train.data <- train.data[,1:600]

#transform test into PCA
test.data <- predict(prin_comp, newdata = sampleTest)
test.data <- as.data.frame(test.data)

#select the first 600 components
test.data <-test.data[,1:600]

sampleModelWithPCa <- lm(RemainingTime ~., data = train.data)
samplePredictionsWithPCa <- predict(sampleModelWithPCa, test.data)

compute_rmse <- function(predictions, actual) { 
  mean( sqrt((predictions - actual) ^ 2 )) 
}

compute_rmse(round(samplePredictionsWithPCa), sampleTest$TimeToFailure)

comparisonWithPCA <- cbind.data.frame(Time = sampleTest$timeMs,
                               Predictions = round(samplePredictionsWithPCa),
                               Actual = sampleTest$TimeToFailure,
                               Difference = abs(round(samplePredictionsWithPCa)-sampleTest$TimeToFailure))

medianWithPca <- median(comparisonWithPCA$Difference)

plot1 <- ggplot(data=comparisonWithPCA, aes(x=Time, y=Difference)) +
  geom_line() +
  geom_point() + geom_vline(xintercept = 542979,colour="red",linetype = "longdash",size = 1,5) +
  geom_hline(yintercept = median(comparisonWithPCA$Difference),colour="darkgoldenrod1",linetype = "longdash",size = 1,5) +
  geom_text(aes(0,median(comparisonWithPCA$Difference),label = round(median(comparisonWithPCA$Difference)), vjust = -0.5,hjust = 1.2)) +
  scale_x_continuous(breaks=seq(0,600000,60000)) + ggtitle("with PCA")


###################################### scale ######################################
sampleTrainScaled <- sampleTrain
sampleTestScaled <- sampleTest
sampleTrainScaled[,c(-1,-2)] <- as.data.frame(lapply(sampleTrain[,c(-1,-2)], normalize) )
sampleTestScaled[,c(-1,-2)] <- as.data.frame(lapply(sampleTest[,c(-1,-2)], normalize)) 


sampleModel <- lm(TimeToFailure ~., data = sampleTrain[,2:ncol(sampleTrain)])
samplePredictions <- predict(sampleModel, sampleTest[,2:ncol(sampleTest)])

compute_rmse <- function(predictions, actual) { 
  mean( sqrt((predictions - actual) ^ 2 )) 
}

compute_rmse(round(samplePredictions), sampleTest$TimeToFailure)

comparison <- cbind.data.frame(Time = sampleTest$timeMs,
                               Predictions = round(samplePredictions),
                               Actual = sampleTest$TimeToFailure,
                               Difference = abs(round(samplePredictions)-sampleTest$TimeToFailure))


plot2 <- ggplot(data=comparisonNNet, aes(x=Time, y=Difference)) +
  geom_line() +
  geom_point() + geom_vline(xintercept = 542979,colour="red",linetype = "longdash",size = 1,5) +
  geom_hline(yintercept = median(comparisonNNet$Difference),colour="darkgoldenrod1",linetype = "longdash",size = 1,5) +
  geom_text(aes(0,median(comparisonNNet$Difference),label = round(median(comparisonNNet$Difference)), vjust = -0.5,hjust = 1.2)) +
  scale_x_continuous(breaks=seq(0,600000,60000)) + ggtitle("without PCA")


multiplot(plot1,plot2,cols = 1)


# Neural  network: 10 nodes  in the  hidden  layer

NN_model <-nnet(TimeToFailure~.,data=sampleTrain ,size =10,MaxNWts=22000,linout=TRUE)

classification_NN<-predict(NN_model ,sampleTest ,type = "raw")

compute_rmse(round(classification_NN), sampleTest$TimeToFailure)

comparisonNNet <- cbind.data.frame(Time = sampleTest$timeMs,
                               Predictions = round(classification_NN),
                               Actual = sampleTest$TimeToFailure,
                               Difference = abs(round(classification_NN)-sampleTest$TimeToFailure))

median(comparisonNNet$Difference)

