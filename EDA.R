# Data exploration

# default directory is at finalProject where data are saved
data1 = read.csv("./data/2015.csv")
data2 = read.csv("./data/2016.csv")
data3 = read.csv("./data/2017.csv")

# check the number of counties in each region
ncountries_per_region = aggregate(Country ~ Region, data = data1, length)

# parallel boxplots of six factors according to region
# We want to know if six factors behave differently 
#  across regions
p1 = boxplot(Economy..GDP.per.Capita. ~ Region, data=data1)
p2 = boxplot(Family ~ Region, data=data1)
p3 = boxplot(Freedom ~ Region, data=data1)
p4 = boxplot(Generosity ~ Region, data=data1)
p5 = boxplot(Health..Life.Expectancy. ~ Region, data=data1)
p6 = boxplot(Trust..Government.Corruption. ~ Region, data=data1)


# make a new data frame with only happiness score, region, and year.
happiness.2015 = cbind(score = data1$Happiness.Score, year = rep(2015, length(data1$Happiness.Score)))
happiness.2016 = cbind(score = data2$Happiness.Score, year = rep(2016, length(data2$Happiness.Score)))
happiness.2017 = cbind(score = data3$Happiness.Score, year = rep(2017, length(data3$Happiness.Score)))
happiness.all = data.frame(rbind(happiness.2015, happiness.2016, happiness.2017))
# make year variable as a factor
happiness.all$year = factor(happiness.all$year)

# there is no region information in 2017.csv,
# then we match the country with another two files
# to fill the information
Region3_1 = data1$Region[match(data3$Country, data1$Country)]
Region3_2 = data2$Region[match(data3$Country, data2$Country)]
Region.full = Region3_1

# We found there are still two NA's 
# They are Taiwan region and Hongkong region
# so we mannaul put them into Eastern Asia region
Region.full[is.na(Region.full)] = Region3_2[is.na(Region.full)]
Region.full[which(is.na(Region.full))] = "Eastern Asia"
data3$Region = Region.full
happiness.all$Region = unlist(list(data1$Region, data2$Region, data3$Region))

# -------------------------ANOVA--------------------------------
# parallel boxplot with two factors year and region
p7 = boxplot(score~year*Region, data=happiness.all, xlab='year & region', ylab = 'happiness score') # might switch to ggplot2 for better interpretation.
# Here we can see that time does not have obvious effects on scores
# but region does have obvious effects.
# We need two-way ANOVA to test it. 
interation.model = aov(score ~ year * Region, data = happiness.all)
interation.anova = anova(interation.model)
# The p-value is rounded to 1, so we can conclude there is no interaction effect
# then we drop the interation term from the model
addition.model = aov(score~year+Region, data=happiness.all)
addition.anova = anova(addition.model)
# the p-value for year is large. -> no major effect of year factor
# the p-value for Region is small, there is effect of region factor

# Tukey pairwised comparisons
regionOnly.model = aov(score~Region, data=happiness.all)
regionOnly.tukey = TukeyHSD(regionOnly.model)
# We can see if the difference between any pair of regions is statistically significant.

# data modification
# since there is no significant effect of year, we can combine three tables together
# for later work for simplicity
# we drop measure of error variation since they are not standard for different years
variation.names = c("Standard.Error", "Lower.Confidence.Interval", "Upper.Confidence.Interval", 
					"Whisker.high", "Whisker.low")
data1new = data1[, !names(data1) %in% variation.names]
data2new = data2[, !names(data2) %in% variation.names]
data3new = data3[, !names(data3) %in% variation.names]
full.table = rbind(data1new, data2new, data3new)
# select only region factor and six target factors
selected.table = full.table[,c(2,4,5,6,7,8,9,10)]
# Rename columns for simplification
names(selected.table) = c("Region", "Score", "Economy", "Family", "Health", "Freedom", "Trust", "Generosity")
p8 = pairs(selected.table)

# ------------------------linear regression---------------------------------
# Question: Should I start with multivariate linear regression or ANCOVA

# model selection
# 1. backward
full.model = lm(Score~Economy+Family+Health+Freedom+Trust+Generosity, data=selected.table)
drop1(full.model)
# AIC values show that we should not drop any vairable

# 2. Manually, in the plot, we can see that there is the least linear relationship
#    between Score and Generosity, let us see if we can drop it.
reduced.model.no.generosity = lm(Score~Economy+Family+Health+Freedom+Trust, data=selected.table)
cat("full model, AIC: ", AIC(full.model), " BIC: ", BIC(full.model), "\n")
cat("reduced model, AIC: ", AIC(reduced.model.no.generosity), " BIC: ", BIC(reduced.model.no.generosity), "\n")
# AIC shows that we do not drop it, BIC shows we do need to drop it
# NOTE: - consider use transformation on generosity
#       - we might also consider transformation on trust since there is some non-linear
#         relationship between trust an score

# A TRY
# we can fit a model with six vairables and region factor at the same time
full.model.plus = aov(Score~(Economy+Family+Health+Freedom+Trust+Generosity)*Region, data=selected.table)

cat("full model plut, AIC: ", AIC(full.model.plus), " BIC: ", BIC(full.model.plus), "\n")
# The value of AIC dramatically droped, but BIC increased.
