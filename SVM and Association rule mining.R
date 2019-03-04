library(shape)
library(e1071)
library(rgl)
library(arules)

#SVM
svm.fit = svm(data$label ~ ., data = test1, type = "C-classification", cross = 3)
summary(svm.fit)
pred = predict(svm.fit, data[,-1])
table(pred, data$label)

#precision
precision = (11700)/(11700+281)
#recall
recall = (11700/(11700+811))
#F-measure
fmeasure = (2*precision*recall)/(precision+recall)

#grid search
tuned = tune.svm(label~.,data = data,gamma = seq(0, 1, length = 10),cost = seq(1, 10, length = 10), tunecontrol = tune.control(cross = 3))
plot(tuned, type = "perspective", theta = 120, phi = 45)



#Association rule mining
test = read.csv("Test2_Data.csv")
test = as(data.frame(lapply(test2, as.character), stringsAsFactors=T), "transactions")
rules = apriori(test2, parameter = list(supp = 0.1, conf = 0.7, maxlen = 3), appearance = list(rhs = c("Label=TRUE", "Label=FALSE")))
inspect(rule)