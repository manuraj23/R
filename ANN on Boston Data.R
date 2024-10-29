#ANN on Boston Data
library(MASS)

?Boston
data(Boston)
str(Boston)
View(Boston)
hist(Boston$medv)

normalize_data <- function(y) {
  return((y - min(y)) / (max(y) - min(y)))
}

normalized_data <- as.data.frame(lapply(Boston, normalize_data))
training_data <- normalized_data[1:375, ]
testing_data <- normalized_data[376:506, ]

library(neuralnet)
nn_model <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm +age+dis+rad+tax+ptratio+black+lstat,
                      data=training_data)
plot(nn_model)

model_output <- compute(nn_model, testing_data[1:13])
prediction_strength <- model_output$net.result
prediction_strength
cor(testing_data$price, prediction_strength)

nn_model_2 <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm +age+dis+rad+tax+ptratio+black+lstat,
                        data=training_data, hidden=c(6, 3), linear.output=TRUE)
plot(nn_model_2)

model_output_2 <- compute(nn_model_2, testing_data[1:13])
prediction_strength_2 <- model_output_2$net.result
prediction_strength_2
cor(prediction_strength_2, testing_data$price)

#KNN on Boston

# Define the normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize and split the data
data_knn <- as.data.frame(Boston[-4])
data_knn_norm <- as.data.frame(lapply(data_knn, normalize))
data_train_knn <- data_knn_norm[1:375, ]
data_test_knn <- data_knn_norm[376:506, ]

# Set up labels for train and test
data_train_knn_labels <- factor(Boston[1:375, 4])
data_test_knn_labels <- factor(Boston[376:506, 4])

# Run KNN
library(class)
data_test_knn_pred <- knn(train = data_train_knn, test = data_test_knn, cl = data_train_knn_labels, k = 6)

# Generate CrossTable and confusion matrix
library(gmodels)
CrossTable(x = data_test_knn_labels, y = data_test_knn_pred, prop.chisq = FALSE)

# Create confusion matrix
aa_knn <- table(data_test_knn_labels, data_test_knn_pred)
library(caret)
confusionMatrix(aa_knn)

