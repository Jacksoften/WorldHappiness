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


