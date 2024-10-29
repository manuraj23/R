x<-c(5.1,5.5,5.8,6.1,6.4,6.7,6.4,6.1,5.10,5.7)
x<-c(1,2,3,4,5)
y<-c(63,66,69,72,75,78,75,72,69,66)
y<-c(3,4,2,4,5)
relation<-lm(y~x)
summary(relation)
a<-data.frame(x=6.4)
a
result<-predict(relation,a)
result


