# Worldmap plot
library(sp)
library(maptools)
data(wrld_simpl)

data1 = read.table("./data/regrouped_data")
regions = by(data1, data1$Region, function(x) wrld_simpl@data$NAME %in% x$Country)
colors = c(gray(0.80), 'red')[regions[[1]] + 1]
colors[regions[[2]]] = 'blue'
colors[regions[[3]]] = 'green'
# colors[regions[[4]]] = 'pink'
# colors[regions[[5]]] = 'yellow'
# colors[regions[[6]]] = 'beige'
# colors[regions[[7]]] = 'orange'
# colors[regions[[8]]] = 'goldenrod'
# colors[regions[[9]]] = 'purple'
# colors[regions[[10]]] = 'salmon'

# myCountries = wrld_simpl@data$NAME %in% data1$Country
plot(wrld_simpl, col=colors)


