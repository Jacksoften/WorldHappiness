# Data exploration

# default directory is at finalProject where data are saved
data1 = read.csv("/data/2015.csv")
data2 = read.csv("/data/2016.csv")
data3 = read.csv("/data/2017.csv")

# check the number of counties in each region
aggregate(Country ~ Region, data = data1, length)

# parallel boxplots of six factors according to region
# We want to know if six factors behave differently 
#  across regions
boxplot(Economy..GDP.per.Capita. ~ Region, data=data1)
boxplot(Family ~ Region, data=data1)
boxplot(Freedom ~ Region, data=data1)
boxplot(Generosity ~ Region, data=data1)
boxplot(Health..life.Expectancy ~ Region, data=data1)
boxplot(Trust..Government.Corruption. ~ Region, data=data1)


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
Region.full[is.na(Region.full)] = Region3_1[is.na(Region.full)]
Region.full[which(is.na(Region.full))] = "Eastern Asia"
data3Region = Region.full
happiness.all$Region = unlist(list(data1$Region, data2$Region, data3$Region))


# parallel boxplot with two factors year and region
boxplot(happiness.all~yesr*Region, data=happiness.all, xlab='year & region', ylab = 'happiness score') # might switch to ggplot2 for better interpretation.


