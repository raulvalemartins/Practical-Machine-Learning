library(caret)
library(rattle)
library(rpart)
library(randomForest)


trainingURL <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(trainingURL, "pml-training.csv", mode = 'wb', cacheOK = TRUE )
download.file(testingURL, "pml-testing.csv", mode = 'wb', cacheOK = TRUE )

training <- read.csv("pml-training.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c("NA","#DIV/0!","") )
testing <- read.csv("pml-testing.csv", header = TRUE, stringsAsFactors = FALSE, na.strings=c("NA","#DIV/0!",""))

summary(training)

nzv <- nearZeroVar(training)
training <- training[,-nzv]
testing <- testing[,-nzv]

nav <- which(apply(training,2,function(x) {sum(is.na(x))}) == 0)
training <- training[,nav]
testing <- testing[,nav]
training <- training[,-c(1:6)]

training$classe <- as.factor(training$classe)

set.seed(999)
inTrain <- createDataPartition( y=training$class, p=3/4, list=FALSE)
CRtraining <- training[inTrain,]
CRtesting <- training[-inTrain,]

modFitTrees <- train(classe ~ ., method="rpart", data=CRtraining)
confusionMatrix(predict(modFitTrees, newdata = CRtesting), CRtesting$classe)
m <- rbind(table(CRtesting$classe),  table(predict(modFitTrees, newdata = CRtesting)))
barplot(m, beside = TRUE, main="Compare of 'classe' variable in Training Set versus Predition",
        col=c("darkblue","red"), legend = c("Cross Reference Data", "Prediction of Cross Reference Data"))

modFitRF <- randomForest(classe ~ ., data=CRtraining)
confusionMatrix(predict(modFitRF, newdata = CRtesting), CRtesting$classe)
m <- rbind(table(CRtesting$classe),  table(predict(modFitRF, newdata = CRtesting)))
barplot(m, beside = TRUE, main="Compare of 'classe' variable in Training Set versus Predition",
        col=c("darkblue","red"), legend = c("Cross Reference Data", "Prediction of Cross Reference Data"))

##Prediction of Test Data
predict(modFitRF, newdata = testing)
