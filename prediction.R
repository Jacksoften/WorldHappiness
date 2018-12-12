# split data to training and testing sets. 

df = read.table('./data/regrouped_data')

set.seed(2018)
test_index = sort(sample(1:dim(df)[1], round(dim(df)[1]/10), replace=FALSE))
train_index = (1:dim(df)[1])[-test_index]

testdf = df[test_index,]
traindf = df[train_index,]


