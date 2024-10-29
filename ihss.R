#KNN
data<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data)
normalize<-function(x){
  ((x-min(x))/(max(x)-min(x)))
}
str(data)
data<-na.omit(data)
View(data)

data_n_knn<-as.data.frame(lapply(data[5:37],normalize))
View(data_n_knn)
str(data_n_knn)



train_data_knn<-data_n_knn[1:20000,]
test_data_knn<-data_n_knn[20001:25192,]

train_data_knn_labels<-data[1:20000,42]
test_data_knn_labels<-data[20001:25192,42]

data_test_pred<-knn(train = train_data_knn,test = test_data_knn,cl=train_data_knn_labels,k=500)


library(class)
library(caret)

data <- read.csv(file.choose())

data$protocol_type <- as.numeric(as.factor(data$protocol_type))
data$service <- as.numeric(as.factor(data$service))
data$flag <- as.numeric(as.factor(data$flag))
data$class <- as.factor(data$class)  
View(data)

trainIndex <- createDataPartition(data$class, p = 0.8, list = FALSE)
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

k <- 5  
knn_pred <- knn(train = trainData[, -ncol(trainData)], test = testData[, -ncol(testData)], cl = trainData$class, k = k)

confusionMatrix(knn_pred, testData$class)



