concrete=read.csv(file.choose(),stringsAsFactors = FALSE)
View(concrete)
str(concrete)

hist(concrete$strength)
normalise<-function(x){return ((x-min(x))/(max(x)-min(x)))}
concrete_norm<-as.data.frame(lapply(concrete,normalise))
View(concrete_norm)

summary(concrete_norm)
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

install.packages("neuralnet")
library(neuralnet)

concrete_model<-neuralnet(strength ~ cement +slag + ash+water+superplasticizer+coarseagg+fineagg+age,data = concrete_train)

plot(concrete_model)

model_result=compute(concrete_model,concrete_test[1:8])

predicted_strenth<-model_result$net.result

cor(predicted_strenth,concrete_test$strength)

concrete_model2<-neuralnet(strength ~ cement +slag + ash+water+superplasticizer+coarseagg+fineagg+age,data = concrete_train,hidden = 5)

plot(concrete_model2)

model_result2<-compute(concrete_model2,concrete_test[1:8])

predicted_strenth2<-model_result2$net.result

cor(predicted_strenth2,concrete_test$strength)

library(MASS)


View(Boston)
