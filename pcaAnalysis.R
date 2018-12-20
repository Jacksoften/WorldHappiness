# To make scripts clean and clear, we use this new file
# for analysis on standardized data. 

# TODO:
# 1. Standardize data.
# 2. Make ANCOVA model again.
# 3. Find the significant regions.
# 4. Use developed and undeveloped countries.

full.table = read.csv("./data/modified_data.csv")

normalized.data = data.frame(sapply(full.table[,c(5:11)], function(x) {x / sd(x)}))
standardized.table = cbind(full.table[,c(2,3)], normalized.data)
write.csv(standardized.table, './data/standardized_data.csv')

library(ggplot2)
levels(standardized.table$Region) = c("ANZ", "CEE", "EA", "LAC", "MNA", "NA", "SEA", "SA", "SSA", "WE")
factors.matrix = as.matrix(standardized.table[,-c(1,2,3)])

pca.decomp = prcomp(factors.matrix)
pca.sum = summary(pca.decomp)
pca1 = barplot(pca.sum$importance[2,], ylim=c(0, 0.5), main='Proportion of Variance')
pca2 = barplot(pca.sum$importance[3,], ylim=c(0, 1), main='Cumulative Proportion')

# NOTE: taking four principal components
pca.table = cbind(standardized.table[,c(1,2,3)], pca.decomp$x)
pca.lm.model = lm(Score ~ PC1 + PC2 + PC3 + PC4, data=pca.table)
cat("pca model, AIC: ", AIC(pca.lm.model),
     " BIC: ", BIC(pca.lm.model), '\n')

pca.lm.model1 = lm(Score ~ PC1 + PC2, data=pca.table)
cat("pca model1, AIC: ", AIC(pca.lm.model1),
     " BIC: ", BIC(pca.lm.model1), '\n')

pca.lm.model2 = lm(Score ~ (PC1 + PC2) * Region, data=pca.table)
cat("pca model2, AIC: ", AIC(pca.lm.model2),
     " BIC: ", BIC(pca.lm.model2), '\n')

pca.lm.model3 = lm(Score ~ PC1 * Region + PC2, data=pca.table)
cat("pca model3, AIC: ", AIC(pca.lm.model3),
     " BIC: ", BIC(pca.lm.model3), '\n')

region.model = aov(Score ~ Region, data = standardized.table)
region.CIs = TukeyHSD(region.model)
print(which(region.CIs$Region[,4] > 0.05))
# NOTE:
# Based on the above Tukey test, I can combine
# North America, Western Europe, and Australia and New Zeland together
ggplot(data=pca.table, aes(y=Score, x=PC1, color=Region)) + geom_point() + facet_grid(.~Region)

data.new = pca.table
# NOTE:
# NA-ANZ-WE: NAW
# EA-CEE-MNA-SEA-LAC: ECMSL
# SSA-SA: SS
levels(data.new$Region) = c("NAW", "ECMSL", "ECMSL", "ECMSL", "ECMSL", "NAW", "ECMSL", "SS", "SS", "NAW")
region.model.new = aov(Score ~ Region, data=data.new)
region.CIs.new = TukeyHSD(region.model.new)
model.new1 = lm(Score ~ PC1 * Region, data=data.new)
cat("new model1, AIC: ", AIC(model.new1),
	" BIC: ", BIC(model.new1), '\n')

model.new2 = lm(Score ~ PC1 + Region, data=data.new)
cat("new model2, AIC: ", AIC(model.new2),
	" BIC: ", BIC(model.new2), '\n')

p = ggplot(data=data.new, aes(y=Score, x=PC1, color=Region)) + geom_point()
p1 = p + geom_abline(intercept = model.new1$coefficient[1],
				     slope = model.new1$coefficient[2], color = 'red') +
		 geom_abline(intercept = model.new1$coefficient[1] + model.new1$coefficient[3], 
					 slope = model.new1$coefficient[2] + model.new1$coefficient[5], color = 'green') + 
		 geom_abline(intercept = model.new1$coefficient[1] + model.new1$coefficient[4], 
					 slope = model.new1$coefficient[2] + model.new1$coefficient[6], color = 'blue')


model.new3 = lm(Score ~ PC1 * Region + PC2, data=data.new)
cat("new model 3, AIC: ", AIC(model.new3),
     " BIC: ", BIC(model.new3), '\n')
model.new4 = lm(Score ~ (PC1 + PC2) * Region, data=data.new)
cat("new model 4, AIC: ", AIC(model.new4),
     " BIC: ", BIC(model.new4), '\n')
model.new5 = lm(Score ~ PC1 + PC2 + Region, data=data.new)
cat("new model 5, AIC: ", AIC(model.new5),
     " BIC: ", BIC(model.new5), '\n')

