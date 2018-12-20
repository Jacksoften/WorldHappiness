# NOTE:
# sink() for saving R output to a file
# pdf() for saving R graph to a pdf

# -------------------------ANOVA--------------------------------
# Here we can see that time does not have obvious effects on scores
# but region does have obvious effects.
# We need two-way ANOVA to test it. 
happiness.all = read.csv('./data/happiness_region_year.csv')
interaction.model = aov(score ~ year * Region, data = happiness.all)
interaction.anova = anova(interaction.model)
# The p-value is rounded to 1, so we can conclude there is no interaction effect
# then we drop the interation term from the model
addition.model = aov(score~year+Region, data=happiness.all)
addition.anova = anova(addition.model)
# the p-value for year is large. -> no major effect of year factor
# the p-value for Region is small, there is effect of region factor

# Tukey pairwised comparisons
levels(happiness.all$Region) = c("ANZ", "CEE", "EA", "LAC", "MNA", "NA", "SEA", "SA", "SSA", "WE")
regionOnly.model = aov(score~Region, data=happiness.all)
regionOnly.tukey = TukeyHSD(regionOnly.model)
# We can see if the difference between any pair of regions is statistically significant.

full.table = read.csv("./data/modified_data.csv")
# select only region factor and six target factors
selected.table = full.table[,-c(1,2,4,12)]

# NOTE: Should we check assumptions here? 
#       It is actually a uneqaul sized two-way ANOVA

# ------------------------linear regression-------------------------
# Question: Should I start with multivariate linear regression or ANCOVA

# model selection
# 1. backward
# Model1
full.model = lm(Score~Economy+Family+Health+Freedom+Trust+Generosity, data=selected.table)
drop1(full.model)
# AIC values show that we should not drop any vairable

# 2. Manually, in the plot, we can see that there is the least linear relationship
#    between Score and Generosity, let us see if we can drop it.
# Model2
reduced.model.no.generosity = lm(Score~Economy+Family+Health+Freedom+Trust, data=selected.table)
cat("full model, AIC: ", AIC(full.model), 
	" BIC: ", BIC(full.model), "\n")
cat("reduced model, AIC: ", AIC(reduced.model.no.generosity), 
	" BIC: ", BIC(reduced.model.no.generosity), "\n")
# AIC shows that we do not drop it, BIC shows we do need to drop it
# NOTE: - consider use transformation on generosity
#       - we might also consider transformation on trust since there is some non-linear
#         relationship between trust an score

# A TRY
# we can fit a model with six vairables and region factor at the same time
# Model3
model3 = lm(Score~(Economy+Family+Health+Freedom+Trust+Generosity)*Region, data=selected.table)
cat("full model with region interaction with all factors, AIC: ", AIC(model3), 
	" BIC: ", BIC(model3), "\n")
# The value of AIC dramatically droped, but BIC increased.

# NOTE: We might also need to consider randomized block design
#       for countries, it is very similar comparing to the test
#       students example from class.
# !NO, we cannot use randomized block design here
#   Because, the scores are independent with each other

# NOTE: Should we check assumptions here? 
#       It is actually a uneqaul sized two-way ANOVA

# NOTE: There are relationship between variables.
#       Might need to deal with colinearity
#       p84: Linear models in R

# by EDA, we run model with region effect only on health and economy
# model4
full.model.with.partial.region.effect = lm(Score~(Health+Economy)*Region+Family+Trust+Freedom+Generosity, data=selected.table)

cat("full model with region interaction with health and economy, AIC: ", AIC(full.model.with.partial.region.effect), 
	" BIC: ", BIC(full.model.with.partial.region.effect), "\n")

# model5
reduced.model.with.partial.region.effect = lm(Score~(Health+Economy)*Region+Family+Trust+Freedom, data=selected.table)

cat("reduced model with region interaction with health and economy, AIC: ", AIC(reduced.model.with.partial.region.effect), 
	" BIC: ", BIC(reduced.model.with.partial.region.effect), "\n")

# NOTE: Should we rely on AIC and BIC? They are giving different result for our model. 
# 		Or, should we keep the principal that simplicity of model is the most important

# --------------------------PCA---------------------------------
# NOTE: Why should we use pca here?
#       Creating new variables which is the combination of the 
#       old ones, and fit the model to reduce the colinearity
factors.matrix = as.matrix(selected.table[,-c(1,2)])
factors.cov = cov(factors.matrix)
# NOTE: In this correlation table, there are some variables have large correlation
#       with each other. We might consider use pca to reduce those correlation

pca.model = prcomp(factors.cov)
pca.sum = summary(pca.model)
# NOTE: We can take the first two components

pca.table = cbind(selected.table[,c(1,2)], 
				  factors.matrix %*% pca.sum$x[,1:2])

# factors.eigen = eigen(factors.cor)
# pc1 = t(factors.eigen$vectors[,1] %*% t(factors.matrix))
# pc2 = t(factors.eigen$vectors[,2] %*% t(factors.matrix))

pca.lm.model = lm(Score ~ PC1 + PC2, data=pca.table)
cat("pca model, AIC: ", AIC(pca.lm.model), 
	" BIC: ", BIC(pca.lm.model), '\n')

pca.lm.model.with.region = lm(Score ~ (PC1 + PC2) * Region, data=pca.table)
cat("pca model, AIC: ", AIC(pca.lm.model.with.region), 
	" BIC: ", BIC(pca.lm.model.with.region), '\n')

# NOTE: Since The generosity does not have a strong effect on our model
# 		We can consider to drop this variable for model simplicity.


pca.lm.model1 = lm(Score ~ PC1, data=pca.table)
cat("pca model, AIC: ", AIC(pca.lm.model1), 
	" BIC: ", BIC(pca.lm.model1), '\n')

pca.lm.model.with.region1 = lm(Score ~ PC1 * Region, data=pca.table)
cat("pca model, AIC: ", AIC(pca.lm.model.with.region1), 
	" BIC: ", BIC(pca.lm.model.with.region1), '\n')



levels(selected.table$Region) = c("group1", "group2", "group2", "group2", "group2", "group1", "group2", "group3", "group3", "group1")

p.n = ggplot(data=selected.table)
p1.n = p.n + geom_boxplot(aes(Region, Economy))
p2.n = p.n + geom_boxplot(aes(Region, Family))
p3.n = p.n + geom_boxplot(aes(Region, Freedom))
p4.n = p.n + geom_boxplot(aes(Region, Generosity))
p5.n = p.n + geom_boxplot(aes(Region, Health))
p6.n = p.n + geom_boxplot(aes(Region, Trust))
pfin.n =  grid.arrange(p1.n, p2.n, p3.n, p4.n, p5.n, p6.n, nrow = 2)
