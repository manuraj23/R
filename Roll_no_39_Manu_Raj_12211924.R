data<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data)
str(data)
data=data[3:5]
View(data)
data$Purchased=factor(data$Purchased,levels = c(0,1))
install.packages("caTools")
library(caTools)
set.seed(1)
split=sample.split(data$Purchased,SplitRatio = 0.75)
training_set<-subset(data,split==TRUE)
test_set<-subset(data,split==FALSE)
training_set[-3]=scale(training_set[-3])
test_set[-3]=scale(test_set[-3])
?scale
View(training_set)
library(e1071)
classifier<-svm(formula=Purchased ~ .,
                data=training_set,
                type="C-classification",
                kernel='linear')
y_pred=predict(classifier,newdata = test_set[-3])
(cm=table(test_set[,3],y_pred))

#SVM on iris dataset
data_iris <- as.data.frame(iris)
View(data_iris)
set.seed(123)
train_indices <- sample(1:nrow(data_iris), 0.75 * nrow(iris))
train_datairis <- data_iris[train_indices, ]
test_data_iris <- data_iris[-train_indices, ]
library(e1071)
classifier_iris <- svm(formula = Species ~ .,
                       data = train_datairis,
                       type = "C-classification",
                       kernel = 'linear')
y_pred_iris <- predict(classifier_iris, newdata = test_data_iris[-5])
cm_iris <- table(test_data_iris[,5], y_pred_iris)
cm_iris
error <- mean(y_pred_iris != test_data_iris$Species)
error

#SVM  on Boston
library(MASS)
data_boston <- as.data.frame(Boston)
set.seed(123)
train_indices <- sample(1:nrow(data_boston), 0.75 * nrow(data_boston))
train_data_boston <- data_boston[train_indices, ]
test_data_boston <- data_boston[-train_indices, ]
library(e1071)
regressor_boston <- svm(formula = medv ~ .,
                        data = train_data_boston,
                        type = "C-classification",
                        kernel = 'linear')
y_pred_boston <- predict(regressor_boston, newdata = test_data_boston[-14])
cm_boston<-table(test_data_boston[,-14],y_pred_boston)
cm_boston
