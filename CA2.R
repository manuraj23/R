#Q1
#I have used class to predict decison tree model
data1<-as.data.frame(Titanic)
View(data1)
set.seed(123)

train_index <- sample(1:nrow(data1), 0.7 * nrow(data1))
train_data <- data1[train_index, ]
test_data <- data1[-train_index, ]

library(rpart)
library(rpart.plot)
decision_tree<-rpart(Survived ~ ., data=train_data, method="class")
decision_tree_pred<-predict(decision_tree,test_data, type="class")
print(decision_tree)
rpart.plot(decision_tree, main="Decision Tree Titanic", type=2, extra=10)
predction1<-predict(decision_tree,test_data,type="class")
table(predction1,test_data$Survived)
accuracy1<-sum(predction1==test_data$Survived)/nrow(test_data)
print(paste("Accuracy:", round(accuracy1 * 100, 2), "%"))


#Q2
data2<-as.data.frame(read.csv(file.choose(),stringsAsFactors = FALSE))
View(data2)
str(data2)
hist(data2$radius)

normalise<-function(x){return ((x-min(x))/(max(x)-min(x)))}
data2_norm<-as.data.frame(lapply(data2[3:10],normalise))
View(data2_norm)
summary(data2_norm)
data2_train<-data2_norm[1:75,]
data2_test<-data2_norm[76:100,]

library(neuralnet)
data2_model<-neuralnet(radius ~ texture+perimeter+area+smoothness+compactness+symmetry+fractal_dimension,data = data2_train)
plot(data2_model)
model_result=compute(data2_model,data2_test[1:7])
predicted_radius<-model_result$net.result
cor(predicted_radius,data2_test$radius)

data2_model2<-neuralnet(radius ~ texture+perimeter+area+smoothness+compactness+symmety+fractal_dimension,data = data2_train,hidden = 5)
plot(data2_model2)
model_result2<-compute(data2_model2,data2_test[1:7])
predicted_radius2<-model_result2$net.result
cor(predicted_radius2,data2_test$radius)
?cor


