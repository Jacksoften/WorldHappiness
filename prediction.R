# split data to training and testing sets. 

df0 = read.table('./data/regrouped_data')
df1 = read.csv('./data/modified_data.csv')
df1 = df1[,-c(2,4,12)]


set.seed(2018)
test_set_size = round(dim(df0)[1]/10)
test_index = sort(sample(1:test_set_size, test_set_size, replace=FALSE))
train_index = (1:dim(df0)[1])[-test_index]

testdf0 = df0[test_index,]
traindf0 = df0[train_index,]

testdf1 = df1[test_index,]
traindf1 = df1[train_index,]
levels(testdf1$Region) = c("group1", "group2", "group2", "group2", "group2", "group1", "group2", "group3", "group3", "group1")
levels(traindf1$Region) = c("group1", "group2", "group2", "group2", "group2", "group1", "group2", "group3", "group3", "group1")


# NOTE: We have already split the data for prediction,
#       then now, we will test each model with their prediction errors.

model1 = lm(Score~Economy+Family+Health+Freedom+Trust+Generosity, data=traindf1)

predict_acc = function(model, testSet) {
	predictions = predict(model, testSet, interval='conf')
	isInInterval = (testSet$Score >= predictions[,2]) & (testSet$Score <= predictions[,3])
	numCorrect = sum(isInInterval)
	accuracy = numCorrect/test_set_size
	return(accuracy)
}
predict_accuracy = predict_acc(model1, testdf1)
print(predict_accuracy)

model2 = lm(Score~(Economy+Health)*Region+Family+Freedom+Trust+Generosity, data=traindf1)
model3 = lm(Score~Economy*Region+Family+Health+Freedom+Trust+Generosity, data=traindf1)
model4 = lm(Score~Economy+Family+Health+Freedom+Trust+Generosity+Region, data=traindf1)

pred2 = predict_acc(model2, testdf1)
cat("AIC ", AIC(model2), " BIC ", BIC(model2), " PA ", pred2, '\n')
pred3 = predict_acc(model3, testdf1)
cat("AIC ", AIC(model3), " BIC ", BIC(model3), " PA ", pred3, '\n')
pred4 = predict_acc(model4, testdf1)
cat("AIC ", AIC(model4), " BIC ", BIC(model4), " PA ", pred4, '\n')
# split data to training and testing sets. 



df0 = read.table('./data/regrouped_data')
df1 = read.csv('./data/modified_data.csv')
df1 = df1[,-c(2,4,12)]
# Rename columns for simplification
names(df1) = c("Country", "Region", "Score", "Economy", "Family", "Health", "Freedom", "Trust", "Generosity")


set.seed(2018)
test_set_size = round(dim(df0)[1]/10)
test_index = sort(sample(1:test_set_size, test_set_size, replace=FALSE))
train_index = (1:dim(df0)[1])[-test_index]

testdf0 = df0[test_index,]
traindf0 = df0[train_index,]

testdf1 = df1[test_index,]
traindf1 = df1[train_index,]
levels(testdf1$Region) = c("group1", "group2", "group2", "group2", "group2", "group1", "group2", "group3", "group3", "group1")
levels(traindf1$Region) = c("group1", "group2", "group2", "group2", "group2", "group1", "group2", "group3", "group3", "group1")


# NOTE: We have already split the data for prediction,
#       then now, we will test each model with their prediction errors.

model1 = lm(Score~Economy+Family+Health+Freedom+Trust+Generosity, data=traindf1)

predict_acc = function(model, testSet) {
	predictions = predict(model, testSet, interval='conf')
	isInInterval = (testSet$Score >= predictions[,2]) & (testSet$Score <= predictions[,3])
	numCorrect = sum(isInInterval)
	accuracy = numCorrect/test_set_size
	return(accuracy)
}
predict_accuracy = predict_acc(model1, testdf1)
print(predict_accuracy)

model2 = lm(Score~(Economy+Health)*Region+Family+Freedom+Trust+Generosity, data=traindf1)
model3 = lm(Score~Economy*Region+Family+Health+Freedom+Trust+Generosity, data=traindf1)
model4 = lm(Score~Economy+Family+Health+Freedom+Trust+Generosity+Region, data=traindf1)

pred2 = predict_acc(model2, testdf1)
cat("AIC ", AIC(model2), " BIC ", BIC(model2), " PA ", pred2, '\n')
pred3 = predict_acc(model3, testdf1)
cat("AIC ", AIC(model3), " BIC ", BIC(model3), " PA ", pred3, '\n')
pred4 = predict_acc(model4, testdf1)
cat("AIC ", AIC(model4), " BIC ", BIC(model4), " PA ", pred4, '\n')


model5 = lm(Score~(PC1+PC2)*Region, data=traindf0)
model6 = lm(Score~PC1*Region+PC2, data=traindf0) 
model7 = lm(Score~PC1+PC2+Region, data=traindf0)

pred5 = predict_acc(model5, testdf0)
pred6 = predict_acc(model6, testdf0)
pred7 = predict_acc(model7, testdf0)
cat("AIC ", AIC(model5), " BIC ", BIC(model5), " PA ", pred5, '\n')
cat("AIC ", AIC(model6), " BIC ", BIC(model6), " PA ", pred6, '\n')
cat("AIC ", AIC(model7), " BIC ", BIC(model7), " PA ", pred7, '\n')
