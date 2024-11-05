library(ggplot2)
install.packages("arules")
installed.packages("cluster")
library(cluster)
iris_data<-iris[,-5]
set.seed(240);
kmeans.re<-kmeans(iris_data,centers = 3,nstart = 20)
?kmeans
kmeans.re
kmeans.re$cluster
kmeans.re$centers
cm<-table(iris$Species,kmeans.re$cluster)
cm
plot(iris_data[c("Sepal.Length", "Sepal.Width")],
     col=kmeans.re$cluster,
     main = "K-mean With 3 Clusters")

kmeans.re$centers
kmeans.re$centers[,c("Sepal.Length", "Sepal.Width")]

points(kmeans.re$centers[,c("Sepal.Length", "Sepal.Width")],
       col=1:3,pch=8,cex=3)
y_kmeans<-kmeans.re$cluster
clusplot(iris_data[,c("Sepal.Length", "Sepal.Width")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main=paste("cluster iris"),
         xlab="Sepal.Length",
         ylab='Sepal.Width')

