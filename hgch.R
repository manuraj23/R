insurance=read.csv(file.choose(),stringsAsFactors = FALSE)
str(insurance)
#completely display the internal structure
#model's dependent variable is charges, which measures the medical costs
#insurance plan for the year
summary(insurance$charges)

hist(insurance$charges)
#computes a histogram of the given values
table(insurance$region)
#build a contingency table of the counts

cor(insurance[c("age","bmi","children","charges")])

pairs(insurance[c("age","bmi","children","charges")])

install.packages("psych")

library(psych)

pairs.panels(insurance[c("age","bmi","children","charges")])

ins_model<-lm(charges ~ age +children + bmi +sex +smoker +region, data = insurance)

ins_pred<-predict(ins_model,data=insurance)

ins_model

summary(ins_model)
