library(readxl)

data <- read_excel("D:\\THESIS\\DATASET\\HASIL GLOBAL ENCODING\\L=2\\DATAREADYFORRTFL=2.xlsx")


set.seed(123)
datatarget <- as.factor(data$TARGET)
#MEMPERSIAPKAN DATA TRAINING DAN DATA TESTING
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(data), size = floor(.7*nrow(data)), replace = F)
trainingdata <- data[sample, ]
testingdata  <- data[-sample, ]

x_train <- trainingdata[,!colnames(trainingdata) %in% "TARGET"]
y_train <- as.factor(trainingdata$TARGET)

#y <- as.factor(ifelse(iris$Species[1:100]=="setosa",0,1))

x_test <- testingdata[,!colnames(testingdata) %in% "TARGET"]
y_test <- as.factor(testingdata$TARGET)

#ROTATIONFOREST
library(mixOmics)
library(rotationForest)
library(rpart)
library(caret)
library(SDMTools)
library(randomForest)
library(rpart)


rFmodel <- rotationForest(x_train,y_train, 5,10)
prediksi <- predict(rFmodel, x_test)
prediksi1 <-as.factor(ifelse(prediksi> 0.5,1,0))
cm <- confusionMatrix(y_test, prediksi1)

#RANDOMFOREST
randomF <- randomForest(x_train,y_train, ntree = 10)
prediksirandomF <- predict(randomF, x_test)
cmrandom <- confusionMatrix(y_test, prediksirandomF)
