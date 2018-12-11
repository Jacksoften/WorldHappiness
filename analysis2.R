# To make scripts clean and clear, we use this new file
# for analysis on standardized data. 

# TODO:
# 1. Standardize data.
# 2. Make ANCOVA model again.
# 3. Find the significant regions.
# 4. Use developed and undeveloped countries.

# NOTE: Comment out since data has been saved
# ----------------------------------------------------
# full.table = read.csv("./data/modified_data.csv")
# # select only region factor and six target factors
# selected.table = full.table[,-c(1,2,4,12)]
# # Rename columns for simplification
# names(selected.table) = c("Region", "Score", "Economy", "Family", "Health", "Freedom", "Trust", "Generosity")
#
# normalized.data = data.frame(sapply(selected.table[,-1], function(x) x/sd(x)))
# standardized.table = cbind(Region = selected.table[,1], normalized.data)
# write.csv(standardized.table, './data/standardized_data.csv')
# ----------------------------------------------------

data = read.csv("./data/standardized_data.csv")
levels(data$Region) = c("ANZ", "CEE", "EA", "LAC", "MNA", "NA", "SEA", "SA", "SSA", "WE")
factors.matrix = as.matrix(data[,-c(1,2,3)])

pca.decomp = prcomp(factors.matrix)
pca.sum = summary(pca.decomp)
p1 = barplot(pca.sum$importance[2,], ylim=c(0, 0.5), main='Proportion of Variance')
p2 = barplot(pca.sum$importance[3,], ylim=c(0, 1), main='Cumulative Proportion')

# NOTE: taking four principal components
pca.table = cbind(data[,c(2,3)], pca.decomp$x)
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

region.model = aov(Score ~ Region, data = data)
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
