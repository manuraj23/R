#KNM algorithm
data<-read.csv(file.choose(),stringsAsFactors = FALSE)
View(data)
str(data)
data<-data[-1]
table(data$diagnosis)
data$diagnosis<-factor(data$diagnosi,levels=c("B","M"),labels=c("Benign","Malignant"))
round(prop.table(table(data$diagnosis))*100,digits = 1)
summary(data[c("radius_mean","area_mean","smoothness_mean")])
normalize<-function(x){
  ((x-min(x))/(max(x)-min(x)))
}
data_n<-as.data.frame(lapply(data[2:31],normalize))

View(data_n)
summary(data_n$area_mean)
View(data_n)

data_train<-data_n[1:469,]
data_test<-data_n[470:569,]

data_train_labels<-data[1:469,1]
data_test_labels<-data[470:569,1]

library(class)

data_test_pred<-knn(train = data_train,test = data_test,cl=data_train_labels,k=23)

library(gmodels)

CrossTable(x=data_test_labels,y=data_test_pred,prop.chisq = FALSE)
aa=table(data_test_labels,data_test_pred)
library(caret)
install.packages("caret")
confusionMatrix(aa)

