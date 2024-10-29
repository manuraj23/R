#Q1
data1<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data1)
str(data1)
mean_age<-mean(data1$Age,na.rm = TRUE)
data1$Age[is.na(data1$Age)]<-mean_age
View(data1)
data1$Age<-as.integer(data1$Age)

mean_amount<-mean(data1$PurchaseAmount,na.rm = TRUE)
data1$PurchaseAmount[is.na(data1$PurchaseAmount)]<-mean_amount
View(data1)

mean_rating<-mean(data1$SatisfactionScore,na.rm = TRUE)
data1$SatisfactionScore[is.na(data1$SatisfactionScore)]<-mean_rating
data1$SatisfactionScore<-as.integer(data1$SatisfactionScore)
View(data1)
(data1)
library(sqldf)
sqldf("select gender, avg(SatisfactionScore) from data1 group by Gender")


#Q2
data2<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data2)
str(data2)
normalize<-function(x){
  ((x-min(x))/(max(x)-min(x)))
}
data2_n<-as.data.frame(lapply(data2[2:5],normalize))

View(data2_n)
str(data2_n)

train_data2 <- data2_n[1:800, ] 
test_data2  <- data2_n[801:1000, ]

data2_train_labels<-data2[1:800,7]
data2_test_labels<-data2[801:1000,7]

library(gmodels)
library(caret)

knn_pred<-knn(train = train_data2,test =test_data2,cl=data2_train_labels,k=20)
CrossTable(x=data2_test_labels,y=knn_pred,prop.chisq = FALSE)
acc<-table(data2_test_labels,knn_pred)
confusionMatrix(acc)

#Q3
library(sqldf)
data3<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data3)
a=mean(data3$TotalAmount)
a
sqldf("Select OrderID,TotalAmount FROM data3 order by TotalAmount DESC")
sqldf("Select CustomerID,TotalAmount from data3 where totalamount > 67.25")
sqldf("Select * from data3 where productID>=2004")

 














