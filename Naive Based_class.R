# Load necessary libraries
install.packages("tm")
install.packages("e1071")
install.packages("gmodels")
install.packages("caret")
library(tm)
library(e1071)
library(gmodels)
library(caret)

# Load the dataset
sms_raw <- read.csv(file.choose(), stringsAsFactors = FALSE)

# Convert 'type' column to factor
sms_raw$type <- factor(sms_raw$type)

# Create a corpus from the text messages
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# Clean the corpus
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# Create a Document-Term Matrix (DTM)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# Split the data into training and test sets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

# Create labels for training and test sets
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

# Find frequent terms in the training data (terms appearing 5 or more times)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

# Filter the DTM to only include the frequent terms
sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]

# Convert counts to Yes/No based on term appearance
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# Apply the conversion to training and test data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# Train a Naive Bayes classifier
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# Make predictions on the test data
sms_test_pred <- predict(sms_classifier, sms_test)

# Create a confusion matrix to evaluate predictions
a <- table(sms_test_pred, sms_test_labels)

# Display the cross table of predictions vs actual values
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('Predicted', 'Actual'))

# Calculate the confusion matrix
confusionMatrix(a)


