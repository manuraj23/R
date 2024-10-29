#Q1
library(class)
library(mlbench)
library(sqldf)
library(gmodels)
library(caret)
data(package = "mlbench")
View(airquality)
data1<-as.data.frame(airquality)
View(data1)
mean_ozone=mean(data1$Ozone,na.rm = TRUE)
data1$Ozone[is.na(data1$Ozone)]<-mean_ozone
sqldf("SELECT Ozone, Temp FROM data1 WHERE Month = 7")
sqldf("Select Month,avg(Wind) from data1 group by Month")
no_of_day=sqldf("Select distinct count(Ozone) from data1 where Ozone>100")
print(no_of_day)
sqldf("select Month,avg(Temp) from data1 where Wind>10 group by month")

#Q2
a <- as.data.frame(Sonar)
set.seed(123)
trainIndex <- sample(1:nrow(a), 0.8 * nrow(a))
trainData <- a[trainIndex, ]
testData  <- a[-trainIndex, ]
knn_pred <- knn(train = trainData[,-61], 
                test = testData[,-61], 
                cl = trainData$Class,  
                k = 3)         
conf_matrix <- confusionMatrix(knn_pred, testData$Class)
print(conf_matrix)
accuracy <- sum(knn_pred == testData$Class) / length(testData$Class)
print(paste("Accuracy:", accuracy))
cross_tab <- table(Predicted = knn_pred, Actual = testData$Class)
print(cross_tab)

#Q3
data3<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data3)
data3$Age <- as.numeric(data3$Age)
mean_age <- mean(data3$Age, na.rm = TRUE)
data3$Age[is.na(data3$Age)] <- mean_age
View(data3)
data3$Age<-as.integer(data3$Age)
data3$Age[data3$Age=='thirty'] <- 30
data3$Gender[data3$Gender=='Female'] <- 'F'
data3$Gender[data3$Gender=='Male'] <- 'M'
data3$Score[data3$Score=='four'] <- 4
data3$Salary[data3$Salary=='Fifty Thousand'] <- 50000
data3$Salary<-as.integer(data3$Salary)
mean_salary<-mean(data3$Salary,na.rm = TRUE)
data3$Salary[is.na(data3$Salary)]<-mean_salary
data3$Salary<-as.integer(data3$Salary)
data3$Score<-as.numeric(data3$Score)
mean_score<-mean(data3$Score,na.rm = TRUE)
data3$Score[is.na(data3$Score)]<-mean_score



#Set B
#Q1
tooth_data<-as.data.frame(Toothdata)

#Q2
data_mtcars<-as.data.frame(mtcars)
View(data_mtcars)
?mtcars

normalize<-function(x){
  ((x-min(x))/(max(x)-min(x)))
}
data_mtcars_n<-as.data.frame(lapply(data_mtcars[3:7],normalize))

View(data_mtcars_n)

set.seed(123)
trainIndex <- sample(1:nrow(data_mtcars_n), 0.8 * nrow(data_mtcars_n))

trainData_mtcars <- data_mtcars_n[trainIndex, ] 
testData_mtcars  <- data_mtcars_n[-trainIndex, ]

data_label_mtcars_train<-data_mtcars[trainIndex,2]
data_label_mtcars_test<-data_mtcars[-trainIndex,2]
knn_pred<-knn(train = trainData_mtcars,test = testData_mtcars,cl=data_label_mtcars_train,k=20)
CrossTable(x=data_label_mtcars_test,y=knn_pred,prop.chisq = FALSE)
acc<-table(data_label_mtcars_test,knn_pred)
confusionMatrix(acc)

#Q3
?BostonHousing
