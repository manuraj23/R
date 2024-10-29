library(e1071)
library(caret)

data(iris)

# Splitting the data into training (60%) and testing (40%) sets
set.seed(1)
train_index <- createDataPartition(iris$Species, p = 0.6, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# Training the Gaussian Naive Bayes model
model <- naiveBayes(Species ~ ., data = train_data)

# Making predictions on the testing set
predictions <- predict(model, test_data)

# Comparing actual response values with predicted response values
accuracy <- sum(predictions == test_data$Species) / nrow(test_data)
print(paste("Gaussian Naive Bayes model accuracy (in %):", accuracy * 100))
