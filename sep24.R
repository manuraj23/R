#KNN
install.packages("lattice")
library(lattice)
library(ggplot2)
View(diamonds)

data<-as.data.frame(diamonds)
normalize<-function(x){
  ((x-min(x))/(max(x)-min(x)))
}

data_n<-as.data.frame(lapply(data[5:10],normalize))
View(data_n)
str(data_n)

train_data<-data_n[1:45000,]
test_data<-data_n[45001:53940,]

train_data_labels<-data[1:45000,2]
test_data_labels<-data[45001:53940,2]

knn_pred<-knn(train = train_data,test = test_data,cl=train_data_labels,k=50)

library(gmodels)

CrossTable(x=test_data_labels,y=knn_pred,prop.chisq = FALSE)
aa=table(test_data_labels,knn_pred)
library(caret)
confusionMatrix(aa)

#Naive Baise

library(e1071)
library(caret)
library(ggplot2)
data <- as.data.frame(diamonds)
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
data_n_naive <- as.data.frame(lapply(data[5:10], normalize))
View(data_n_naive)
train_data_naive <- data_n_naive[1:45000, ]
test_data_naive <- data_n_naive[45001:53940, ]
train_data_labels_naive <- data[1:45000, 2]
test_data_labels_naive <- data[45001:53940, 2]
naive_model_naive <- naiveBayes(train_data_naive, train_data_labels_naive)
nb_pred <- predict(naive_model_naive, test_data_naive)
conf_matrix <- confusionMatrix(table(test_data_labels_naive, nb_pred))
print(conf_matrix)

