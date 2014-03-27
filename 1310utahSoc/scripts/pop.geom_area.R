##	geom_area for CUEB - pop

windows(record=T)
require(ggplot2)

## setwd("C:/Documents and Settings/Russell/Desktop/VAT/Classes Taught/CUEBsu12")
ex1 <- read.csv('pop.geom_area.csv',header=T) ## ,stringsAsFactors=F)

ex1m <- melt(ex1, id.vars = "region", variable_name = "year")

## Some cleanup of years needed
ex1m$year <- as.character(ex1m$year)
ex1m$year <- sapply(strsplit(ex1m$year, split = "X"), function(x)
               as.numeric(x[2]))

ggplot() +
  	geom_area(aes(x=ex1m$year[22:56],ex1m$value[22:56],fill=ex1m$region[22:56])) +
    scale_fill_brewer(name='Region')+
	scale_x_continuous(
	name='Year C.E.\n Source: Angus Maddison'
	) +
  	opts(title = "Regional Population in levels",legend.position=c(0.3,.75)) +
 	scale_y_continuous("Population, Millions",formatter='comma')

ggsave(filename='../images/geom_area_pop.png')
