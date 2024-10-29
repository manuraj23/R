# Load the data
data_iris <- as.data.frame(iris)
View(data_iris)
str(data_iris)
table(data_iris$Species)

# Normalize the data
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
data_iris_n <- as.data.frame(lapply(data_iris[1:4], normalize))
View(data_iris_n)

# Split the data into training and testing sets
data_train_iris <- rbind(data_iris_n[1:35, ], data_iris_n[51:85, ], data_iris_n[101:135, ])
data_test_iris <- rbind(data_iris_n[36:50, ], data_iris_n[86:100, ], data_iris_n[136:150, ])
View(data_train_iris)
View(data_test_iris)

# Split the labels 
data_train_labels_iris <- c(data_iris[1:35, 5], data_iris[51:85, 5], data_iris[101:135, 5])
data_test_labels_iris <- c(data_iris[36:50, 5], data_iris[86:100, 5], data_iris[136:150, 5])

# Perform k-NN classification
library(class)
data_iris_test_pred <- knn(train = data_train_iris, test = data_test_iris, cl = data_train_labels_iris, k = 30)

# Create confusion matrix and calculate error rate
library(gmodels)
CrossTable(x = data_test_labels_iris, y = data_iris_test_pred, prop.chisq = FALSE)

aa <- table(data_test_labels_iris, data_iris_test_pred)
library(caret)
cm <- confusionMatrix(aa)
print(cm)








# Calculate error rate
accuracy <- cm$overall['Accuracy']
error_rate <- 1 - accuracy
print(paste("Error Rate:", error_rate))
