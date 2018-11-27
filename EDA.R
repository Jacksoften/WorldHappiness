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


# ------------------------linear regression---------------------------------
