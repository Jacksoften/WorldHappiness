# Data exploration

# default directory is at finalProject where data are saved
data1 = read.csv("./data/2015.csv")
data2 = read.csv("./data/2016.csv")
data3 = read.csv("./data/2017.csv")

# check the number of counties in each region
ncountries_per_region = aggregate(Country ~ Region, data = data1, length)

# First. We just look into the 2015 dataset.
# parallel boxplots of six factors according to region
# We want to know if six factors behave differently 
#  across regions

# p1 = boxplot(Economy..GDP.per.Capita. ~ Region, data=data1)
# p2 = boxplot(Family ~ Region, data=data1)
# p3 = boxplot(Freedom ~ Region, data=data1)
# p4 = boxplot(Generosity ~ Region, data=data1)
# p5 = boxplot(Health..Life.Expectancy. ~ Region, data=data1)
# p6 = boxplot(Trust..Government.Corruption. ~ Region, data=data1)

# For plot display, we use initials of Regions instead of their full names
# Australia and New Zealand -- ANZ
# Central and Eastern Europe -- CEE
# Eastern Asia -- EA
# Latin America and Caribbean -- LAC
# Middle East and Northern Africa -- MNA
# North America -- NA
# Southeastern Asia -- SEA
# Southern Aisa -- SA
# Sub-Saharan Africa -- SSA
# Western Europe -- WE
levels(data1$Region) = c("ANZ", "CEE", "EA", "LAC", "MNA", "NA", "SEA", "SA", "SSA", "WE")
levels(data2$Region) = c("ANZ", "CEE", "EA", "LAC", "MNA", "NA", "SEA", "SA", "SSA", "WE")

# Using ggplot2 
library(ggplot2)
p = ggplot(data=data1) # 2017 Data
p1 = p + geom_boxplot(aes(Region, Economy..GDP.per.Capita.))
p2 = p + geom_boxplot(aes(Region, Family))
p3 = p + geom_boxplot(aes(Region, Freedom))
p4 = p + geom_boxplot(aes(Region, Generosity))
p5 = p + geom_boxplot(aes(Region, Health..Life.Expectancy. ))
p6 = p + geom_boxplot(aes(Region, Trust..Government.Corruption. ))
library(gridExtra)
# pfin1 = grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)


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
Region.full[which(is.na(Region.full))] = "EA"
data3$Region = Region.full
happiness.all$Region = unlist(list(data1$Region, data2$Region, data3$Region))


# parallel boxplot with two factors year and region
# p7 = boxplot(score~year*Region, data=happiness.all, xlab='year & region', ylab = 'happiness score') # might switch to ggplot2 for better interpretation.
p7 = ggplot(data=happiness.all) + geom_boxplot(aes(x=Region, y=score, color=Region))

# interaction plots for two factors year and region
inter.region = interaction.plot(x.factor = happiness.all$year,
								trace.factor=happiness.all$Region,
								response = happiness.all$score, 
								fun = mean, type = "b", legend = TRUE,
								xlab = "year", ylab="Region",
								pch=c(1,19),col=c("blue","red","purple","yellow","green","brown","pink","powderblue","orange","palegreen"))


inter.year = interaction.plot(x.factor = happiness.all$Region,
							  trace.factor=happiness.all$year,
							  response = happiness.all$score,
							  fun = mean, type = "b", legend = TRUE,
							  xlab = "Region", ylab="year",
							  pch=c(1,19),col=c("blue","red","green"))

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
# full table is saved to modified data for analysis

# select only region factor and six target factors
selected.table = full.table[,c(2,4,5,6,7,8,9,10)]
# Rename columns for simplification
names(selected.table) = c("Region", "Score", "Economy", "Family", "Health", "Freedom", "Trust", "Generosity")
library(GGally)
# p8 = ggpairs(selected.table)
# p8 = pairs(selected.table)
# This plot is very nice, but takes a long to process
# and might need some fine-tuning
# NOTE: There are relationships between variables which brings the collinearity to the 
#       model. We need to deal with it

# In order to check the region effects on different variables we have
# new boxplots on the full data.
p.new = ggplot(data=selected.table)
p1.new = p.new + geom_boxplot(aes(Region, Economy))
p2.new = p.new + geom_boxplot(aes(Region, Family))
p3.new = p.new + geom_boxplot(aes(Region, Freedom))
p4.new = p.new + geom_boxplot(aes(Region, Generosity))
p5.new = p.new + geom_boxplot(aes(Region, Health))
p6.new = p.new + geom_boxplot(aes(Region, Trust))
# pfin = grid.arrange(p1.new, p2.new, p3.new, p4.new, p5.new, p6.new, nrow = 2)
# NOTE: I think that the region has effect only on economy and health.
#       Since the first box is for Austrilia region which only contains 
#       two countries, we do not need to pay much attention to them

cor.matrix = cor(selected.table[,-c(1,2)])
cor.matrix.rotate = apply(cor.matrix, 2, rev)
expand.cor = cbind(expand.grid(rownames(cor.matrix.rotate), 
							   colnames(cor.matrix.rotate)),
				   value = matrix(cor.matrix.rotate, ncol=1))
p.cor = ggplot(data=expand.cor, aes(x=Var1, y=Var2, fill=value)) + 
		geom_tile() + scale_x_discrete(position='top') + xlab(NULL) + ylab(NULL)


# ----------------TODO----------------------
# TODO: Polish all plots with labels, color, title, and layout
