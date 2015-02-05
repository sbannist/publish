### to get total pop from year one
### maddison date
### use stinepack interpolation

## load libraries
library(zoo)
library(lattice)
library(stinepack)
library(ggplot2)
require(scales)

## set data path
datapath <- c( '../data/' )

##load madTotPop.csv
madTotPop <- read.csv ( paste ( datapath,"madTotPop.csv" , sep='' ) , 
	header = TRUE , stringsAsFactors = FALSE )

## clean up rows and round to whole thousands
madTotPop <- madTotPop[-c(1:2),]
madTotPop [ , 2 ] <- round(as.numeric(madTotPop[,2],0))

## expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
madTotPopNew <- madTotPop[1,]
for ( i in 2:999 ) {

## add new row
	madTotPopNew <- rbind ( madTotPopNew , madTotPopNew[i,] )

## populate year
	madTotPopNew [ i , 1 ] <- i
}

## rbind madTotPop[3,]
madTotPopNew <- rbind ( madTotPopNew , madTotPop [ 3 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1001:1499 ) {

## add new row
	madTotPopNew <- rbind ( madTotPopNew , madTotPopNew[i,] )

## populate year
	madTotPopNew [ i , 1 ] <- i
}

## rbind madTotPop[3,]
madTotPopNew <- rbind ( madTotPopNew , madTotPop [ 5 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1501:1599 ) {

## add new row
	madTotPopNew <- rbind ( madTotPopNew , madTotPopNew[i,] )

## populate year
	madTotPopNew [ i , 1 ] <- i
}

## rbind madTotPop[3,]
madTotPopNew <- rbind ( madTotPopNew , madTotPop [ 7 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1601:1699 ) {

## add new row
	madTotPopNew <- rbind ( madTotPopNew , madTotPopNew[i,] )

## populate year
	madTotPopNew [ i , 1 ] <- i
}

## rbind madTotPop[3,]
madTotPopNew <- rbind ( madTotPopNew , madTotPop [ 9 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1701:1819 ) {

## add new row
	madTotPopNew <- rbind ( madTotPopNew , madTotPopNew[i,] )

## populate year
	madTotPopNew [ i , 1 ] <- i
}

## rbind madTotPop[3,]
madTotPopNew <- rbind ( madTotPopNew , madTotPop [ 11:nrow(madTotPop) , ] )

## now apply stinterp
na.stinterp ( madTotPopNew [ , 2 ] )

## do a crude plot
plot(madTotPopNew[,1],ts(na.stinterp ( madTotPopNew [ , 2 ] )))

## do a crude log plot
plot(madTotPopNew[,1],log(ts(na.stinterp ( madTotPopNew [ , 2 ] ))))

## a great plot - the first diff of the log, so a sort of second diff
plot(diff(ts(log(na.stinterp(madTotPopNew[,2])))),lwd=2)

## a great plot - the first diff of the log, so a sort of second diff - 1900 on
plot(diff(ts(log(na.stinterp(madTotPopNew[1:nrow(madTotPopNew),2])),start=2)),lwd=2)

### now get the ggplots
require(ggplot2)
require(scales)

### start from existing code block

## first plot levels
## first interpolate
x <- as.numeric(madTotPopNew[,1])
y <- na.stinterp(madTotPopNew[,2])

## now plot levels
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Thousands",labels=comma) +
ggtitle("Global population C.E. 1 - 2014\n Angus Maddison and UN data") #+

## save off
ggsave(file='../images/pop1-2014.jpg') #to save file just printed, so change file name as appropriate


## now logs
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous(trans='log',breaks = trans_breaks("log", function(x) exp(x),
	 n=4),labels = trans_format("log", math_format(e^.x)),
	"Natural log of population, Thousands") +
ggtitle("Global population C.E. 1 - 2014\n Angus Maddison and UN data") #

## now save off
ggsave(file='../images/logPop1-2014.jpg') #to save file just printed, so change file name as appropriate

## now first diff of logs
## trim first observation from x so it will match the diff result of y
x <- x[-1]
## do first a log, then a diff on the existing y
y <- diff(log(y),differences=1)

ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=1.5) +
scale_x_continuous("Year") +
scale_y_continuous("Annual growth rate in log first differences") +
theme( legend.position = "none" ) +

# black death
geom_vline ( xintercept = c ( 1346 , 1353 ),colour='black' ) +
geom_text ( aes ( x = 900, y = 0.002, label = 'Black Death, 1346-1353' ),
	colour = 'black' ) +

# little ice age
geom_vline ( xintercept = c ( 1550 , 1650 ),colour=' blue ' ) +
geom_text ( aes ( x = 1100, y = 0.0035, label = 'Little Ice Age, 1550-1650' ) ,
	colour = 'blue' ) +
geom_rect ( aes( xmin=1550, xmax=1650, ymin=-Inf, ymax=Inf), fill = 'blue',
	alpha = 0.2) +

# industrial revolution
geom_vline ( xintercept = c ( 1750 , 1850 ),colour=' green ' ) +
geom_text ( aes ( x = 1200, y = 0.005, label = 'Industrial Revolution, 1750-1850' ) ,
	colour = 'darkgreen' ) +
geom_rect ( aes( xmin=1750, xmax=1850, ymin=-Inf, ymax=Inf), fill = ' darkgreen ',
	alpha = 0.2) +

# wwI-gd
geom_vline ( xintercept = c ( 1907 , 1949 ),colour=' black ' ) +
geom_text ( aes ( x = 1170, y = 0.007, label = 'Spanish Influenza / WWI / 
	Great Depression / WWII, 1907-1949' ) , colour = 'black' ) +
geom_rect ( aes( xmin=1907, xmax=1949, ymin=-Inf, ymax=Inf), fill = ' black ',
	alpha = 0.2) +

# global demographic transition
geom_vline ( xintercept = c ( 1971 ),colour=' red ' , lwd = 1.2, alpha = 0.5 ) +
geom_text ( aes ( x = 1176, y = 0.0205, label = 'Global demographic transition,
 1971 \n "The Pill" ~ 1960') , colour = ' red ' ) +

## main title
ggtitle("Global population C.E. 1 - 2014\n Angus Maddison and UN data") #

## now save off
ggsave(file='../images/logDiffPop1-2014.jpg') #to save file just printed, so change file name as appropriate


#### now window to 1750
## first plot levels
## set up df
x <- as.numeric(madTotPopNew[1750:nrow(madTotPopNew),1])
y <- na.stinterp(madTotPopNew[,2])
y <- y[1750:length(y)]

## plot levels
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Thousands",labels=comma) +
ggtitle("Global population C.E. 1750 - 2014\n Angus Maddison and UN data") #+

## now save off
ggsave(file='../images/pop1750-2014.jpg') #to save file just printed, so change file name as appropriate


## now logs
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous(trans='log',breaks = trans_breaks("log", function(x) exp(x),
	 n=4),labels = trans_format("log", math_format(e^.x)),
	"Natural log of population, Thousands") +
ggtitle("Global population C.E. 1750 - 2014\n Angus Maddison and UN data") #


## now save off
ggsave(file='../images/logPop1750-2014.jpg') #to save file just printed, so change file name as appropriate

## now first diff of logs
## trim first observation from x so it will match the diff result of y
x <- x[-1]

## do first a log, then a diff on the existing y
y <- diff(log(y),differences=1)

ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Annual growth rate in log differences") +

# industrial revolution
geom_vline ( xintercept = c ( 1750 , 1850 ),colour=' green ' ) +
geom_text ( aes ( x = 1850, y = 0.005, label = 'Industrial Revolution, 1750-1850' ) ,
	colour = 'darkgreen' ) +
geom_rect ( aes( xmin=1750, xmax=1850, ymin=-Inf, ymax=Inf), fill = ' darkgreen ',
	alpha = 0.2) +

# wwI-gd
geom_vline ( xintercept = c ( 1907 , 1949 ),colour=' black ' ) +
geom_text ( aes ( x = 1900, y = 0.0125, label = 'Spanish Influenza / WWI / 
	Great Depression / WWII, 1907-1949' ) , colour = 'black' ) +
geom_rect ( aes( xmin=1907, xmax=1949, ymin=-Inf, ymax=Inf), fill = ' black ',
	alpha = 0.2) +

# global demographic transition
geom_vline ( xintercept = c ( 1971 ),colour=' red ' , lwd = 1.2, alpha = 0.5 ) +
geom_text ( aes ( x = 1900, y = 0.0205, label = 'Global demographic transition,
 1971 \n "The Pill" ~ 1960') , colour = ' red ' ) +


## make title
ggtitle("Global population C.E. 1750 - 2014\n Angus Maddison and UN data") #

## now save off
ggsave(file='../images/logDiffPop1750-2014.jpg') #to save file just printed, so change file name as appropriate


#### now window to 1850
## first plot levels
## set up df
x <- as.numeric(madTotPopNew[1850:nrow(madTotPopNew),1])
y <- na.stinterp(madTotPopNew[,2])
y <- y[1850:length(y)]

## plot away
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Thousands",labels=comma) +
ggtitle("Global population C.E. 1850 - 2014\n Angus Maddison and UN data") #+

## now logs
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous(trans='log',breaks = trans_breaks("log", function(x) exp(x),
	 n=4),labels = trans_format("log", math_format(e^.x)),
	"Natural log of population, Thousands") +
ggtitle("Global population C.E. 1850 - 2014\n Angus Maddison and UN data") #

## now first diff of logs
## trim first observation from x so it will match the diff result of y
x <- x[-1]
## do first a log, then a diff on the existing y
y <- diff(log(y),differences=1)
ggplot() +
geom_line(aes(x=x,y=y),
	linetype='solid',colour='blue',size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Annual growth rate in log differences") +
ggtitle("Global population C.E. 1850 - 2014\n Angus Maddison and UN data") #


#### deprecated code below here ###########################################
#### deprecated code below here ###########################################


geom_line(aes(x=fyeare,y=(y/scaling)[92:191]),colour='blue',size=2,linetype=2)+
geom_ribbon(aes(x=append(fyear,fyeare),ymin=(y/scaling),ymax=popFcstBands$pop85upPct*y/scaling),fill=colour3,alpha=alpha) +
geom_ribbon(aes(x=append(fyear,fyeare),ymax=(y/scaling),ymin=popFcstBands$pop85loPct*y/scaling),fill=colour3,alpha=alpha) +
geom_line(aes(x=ex$year,y=ex$pop/1e6),colour=colour2,size=3) + #this scaling to correct for lowpop
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions",labels=comma) +
ggtitle("World Population Forecast, 2010 - 2200 \n UN 2010 Low Population Forecast 2010 - 2100  \n UN forecast extended after 2100 by author \n Actual population 1980 - 2008") +
geom_text(aes(x=2150,y=7500,label='80% forecast error bands'),colour=colour4)+
geom_text(aes(x=2150,y=6900,label='Peak population \n 8,131,522,000 in 2046'),colour=colour4)

ggsave(file='../images/lowPopFcst.png') #to save file just printed, so change file name as appropriate




#### below here is legacy and can be deleted after examining for print
#### commands

mtoetz=zoo(mtoe$mtoecons,mtoe$year)

lines(window(na.stinterp(ukgdptz),end=c(1873)),col="red")


###	use historical eras---perhaps use structural breaks later

combogdp=cbind(window(ukgdptz,start=c(1300),end=c(1500)),window(ukgdptz,start=c(1500),end=c(1760)),
	window(ukgdptz,start=c(1750),end=c(1873)),window(ukgdptz,start=c(1300),end=c(1873)))

###	rename columns do gdpin main, do eras and years in cnames

cnames=list(
	"Late Middle Ages 1300-1500","Early Modern Period 1500-1750",
	"Industrial Revolution 1750-1873 (Allen time frame)","Black Death through Industrial Revolution 1300-1873"
	)

names(combogdp)=cnames


xyplot(combogdp, 									###	in scale="free"
	main = "Gross Domestic Product scaled per era",
	xlab = "Time, C.E.",
	ylab = "1990 Geary-Khamis Dollar"
	)

xyplot(combogdp, 									###	in scale="same"
	scales = list(y="same",alternating=F),
	main = "Gross Domestic Product scaled to total era",
	xlab = "Time, C.E.",
	ylab = "1990 Geary-Khamis Dollar"
	)
	
###	how show growth rates? logs?

xyplot(combogdp, 									###	in scale="free"
	scales = list(y=list(log="e")),
	main = "ln Gross Domestic Product scaled per era",
	xlab = "Time, C.E.",
	ylab = "ln 1990 Geary-Khamis Dollar"
	)

###	Now do energy/caput only can do from 1500 on


ukpop = read.csv("madukpop.csv")

ukpoptz = zoo(ukpop$compose,ukpop$year)

xyplot(window(ukpoptz,end=c(1873)),type="p")				###	the way to look at the pop data, should start later
lines(na.stinterp(window(ukpoptz,end=c(1873))))


mtoecaptz = na.stinterp(mtoetz)/na.stinterp(ukpoptz)							###	energy/caput - only from 1300
par(old.par)
xyplot(window(mtoecaptz,start=c(1300),end=c(1873)))



combomtoecap = cbind(
	window(mtoecaptz*1000000,start=c(1300),end=c(1500)),
	window(mtoecaptz*1000000,start=c(1500),end=c(1750)),
	window(mtoecaptz*1000000,start=c(1750),end=c(1873)),
	window(mtoecaptz*1000000,start=c(1300),end=c(1873))
	)

###	rename columns do gdpin main, do eras and years in cnames

cnames2=list(
	"Late Middle Ages 1300-1750",
	"Early Modern Period 1500-1750",
	"Industrial Revolution 1750-1873 (Allen time frame)",
	"Late Middle Ages through Industrial Revolution 1300-1873"
	)

names(combomtoecap)=cnames2


xyplot(combomtoecap, 									###	in scale="free"
	main = "Energy Consumption per capita, scaled per era",
	xlab = "Time, C.E.",
	ylab = "Tonnes Oil Equivalent per capita"
	)

xyplot(combomtoecap, 									###	in scale="same"
	scales = list(y="same",alternating=F),
	main = "Energy Consumption per capita, scaled to total era",
	xlab = "Time, C.E.",
	ylab = "Tonnes Oil Equivalent per capita"
	)
	
###	how show growth rates? logs?

xyplot(combomtoecap, 									###	in scale="free"
	scales = list(y=list(log="e")),
	main = "ln Energy Consumption per capita, scaled per era",
	xlab = "Time, C.E.",
	ylab = "ln Tonnes Oil Equivalent per capita"
	)

###	GDP/caput

gdpcaptz = (na.stinterp(ukgdptz)/na.stinterp(ukpoptz))					###	GDP/caput -

xyplot(na.stinterp(window(gdpcaptz,start=c(1300),end=c(1873))),type="p")


combogdpcap = cbind(
	window(gdpcaptz*100000,start=c(1300),end=c(1500)),
	window(gdpcaptz*100000,start=c(1500),end=c(1750)),
	window(gdpcaptz*100000,start=c(1750),end=c(1873)),
	window(gdpcaptz*100000,start=c(1300),end=c(1873))
	)


names(combogdpcap)=cnames2

xyplot(combogdpcap, 									###	in scale="free"
	main = "GDP per capita, scaled per era",
	xlab = "Time, C.E.",
	ylab = "2005 Great Britain Pounds"
	)

xyplot(combogdpcap, 									###	in scale="same"
	scales = list(y="same",alternating=F),
	main = "GDP per capita, scaled to total era",
	xlab = "Time, C.E.",
	ylab = "2005 Great Britain Pounds"
	)

###########################################################################	ln of caput series

combolncap = cbind(
	window(log(mtoecaptz*1000000),start=c(1300),end=c(1873)),
	window(log(gdpcaptz*1000000),start=c(1300),end=c(1873))
	)

names(combolncap)=c("ln Tonnes Oil per Capita","ln GDP per Capita")


xyplot(combolncap, 									###	in scale="same"
	main = "Log Energy per Capita and GDP per Capita, scaled to total era",
	xlab = "1300-1873, C.E.",
	ylab = ""
	)

###########################################################################	ln of caput series


###########################################################################	acres of forest calculation
#	two tons dry wood = 1 ton coal
#	forest yield, favorable conditions, 2 tons /acre
#	Wrigley p 127 in Snooks, quoting White and Plaskett 1981
#	1 tonne coal (toc) = .588 tonne oil-equivalent (toe) - Fouquet p 39
#	1 tonne (t) = .9842 (long) ton - Fouquet p 39
#	land: 241,930 sq km - https://www.cia.gov/library/publications/the-world-factbook/geos/uk.html
#	= 59782204.938 acres - http://www.metric-conversions.org/area/square-kilometers-to-acres.htm
#	1873 mtoe = 66.1 = 66100000
#	acres/toe = acres/tow * tow/toc * toc/tonne * tonne/toe
#	= .5 * 2 * 1/.9842 * .588 = 0.5974395 acres/toe = 597439.5 acres/mtoe
.5*2*(1/.9842)*.588
#	total 1873 acres = 66.1 * 597439.5 =  39490751 acres
#	percent of total UK acres : 39490751/59782204.938 = 0.660577 conservative need to check white and plaskett
#	set up acres/mtoe series:

acres = na.stinterp(mtoetz) * (.5*2*(1/.9842)*.588) * 1000000

plot(acres/1000000,
	ylab = "Million Acres",
	xlab = "1300 - 2008, C.E.",
	main = "Forest Acres Required for Energy Consumption, U.K.",
	col="blue"
	)
abline(h=59782204.938/1000000,col="red")
locate=locator()
text(locate, labels="Total U.K. Land Area = 59.8 Million Acres",col="red")

###########################################################################	acres of forest calculation

###########################################################################	numbers of horses calculation
#	1 horse-hour = 1200 watt-hours = 1.2 kWh- fouquet 396
#	horse-hours = 300 days x 6 hours / day - fouquet 396
#	1 kWh = .086 toe - fouquet 39 MUST BE WRONG.... from http://www.onlineconversion.com/energy.htm
#	its = 1 kilowatt hour = 0.000085984522786 tonne of oil equivalent
#	1 horse-hour/toe = 1 horse-hour/1.2 kWh * 1 kWh/.086 toe
#	horse-hours/toe = horse-hours/kWh * kWh/toe = horse-hours/toe = 9.689922
#	horse-hours/year = horse-hours/toe * toe/mtoe * mtoe/year 
#	horses/year = horse-hours/year * horses/horse-hour


(1/1.200) * (1/0.000085984522786)					### horse-hours/toe = 9691.667


9691.667 * 1000000 * na.stinterp(mtoetz)	### horse-hours/year = 3.768819e+10 in 1714

horses = (9691.667 * 1000000 * na.stinterp(mtoetz))*(1/(300*6))	###	horse pop per year


plot(horses/1000000,
	ylab = "Million Horses",
	xlab = "1300 - 2008, C.E.",
	main = "Million Horses Required for Energy Consumption, U.K.",
	col="blue"
	)
abline(h=.5,col="red",lwd=2)
locate=locator()
text(locate,labels="1714 est. horse population = 500,000",col="red")				### fouquet 396

###########################################################################	numbers of horses calculation


###########################################################################	numbers of people calculation

#	1 human hour = .06 kWh (fouquet 398 via Smil)
#	human-year = 2080 hours (modern estimate)
#	1 human can yield 2080 * .06 = 124.8 kWh in a year
#	this equals 0.000085984522786 * 124.8 toe per human = 0.01073087 toe/human year
#	so humans for energy consumption is

humans = na.stinterp(mtoetz) * 1000000 / 0.01073087

plot(humans/1000000,
	ylab = "Million People",
	xlab = "1300 - 2008, C.E.",
	main = "Million People Required for Energy Consumption, U.K.",
	col="blue"
	)
abline(h=61.1,col="red",lwd=2)
locate=locator()
text(locate,labels="2008 U.K. human population = 61,113,000",col="red")	
	

###########################################################################	simple regression

mtoecaptz = na.stinterp(mtoetz)/na.stinterp(ukpoptz)

gdpcaptz = na.stinterp(ukgdptz)/na.stinterp(ukpoptz)

modgdpmtoe = lm(window(gdpcaptz*1000,start=c(1300),end=c(1873)) ~ window(mtoecaptz*1000000,start=c(1300),end=c(1873)))

summary(modgdpmtoe)


###	Call:
###	lm(formula = window(gdpcaptz * 1000, start = c(1300), end = c(1873)) ~ 
###	window(mtoecaptz * 1e+06, start = c(1300), end = c(1873)))


###	Residuals:
###   1300     1443     1586     1729     1873 
###	-0.03268 -0.39038  0.38375  0.42323 -0.21389 

###	Coefficients:
###	                                                          Estimate Std. Error t value Pr(>|t|)    
###	(Intercept)                                                0.10290    0.02626   3.918    1e-04 ***
###	window(mtoecaptz * 1e+06, start = c(1300), end = c(1873))  1.59469    0.02922  54.576   <2e-16 ***
###	---
###	Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

###	Residual standard error: 0.3693 on 572 degrees of freedom
###	Multiple R-squared: 0.8389,     Adjusted R-squared: 0.8386 
###	F-statistic:  2979 on 1 and 572 DF,  p-value: < 2.2e-16 


identify(
	plot(window(mtoecaptz*1000000,
	start=c(1300),
	end=c(1873)),
	window(gdpcaptz*1000,
	start=c(1300),
	end=c(1873)),
	col="blue",
	ylab="",
	xlab="",
	),
	labels=seq(1300,1873,by=1)
	)
abline(lm(formula = window(
	gdpcaptz*1000, 
	start = c(1300), 
	end = c(1873)) 
	~ 
	window(mtoecaptz*1000000, 
	start = c(1300), 
	end = c(1873))),
	col="red",
	lwd=2
	)
locate=locator()
text(locate,expression(GDP == .10 + 1.59 * TOE))
locate=locator()
text(locate,expression(R^2 == .84))
title(main="Correlation of Energy Consumed to GDP",
	ylab="Per Capita Thousands 2005 GBP",
	xlab="Per Capita Energy Consumption, TOE"
	)

###	book production and consumption barplots
###	from http://www.iisg.nl/bibliometrics/books500-1800.pdf

windows(record=T)
read.csv("bookprod.csv")
bookprodtz=zoo(bookprod$prodtot,bookprod$year)
barplot(bookprodtz/1000,
	col=rainbow(7),
	main="Book Production, Western Europe, 1454 - 1800 C.E.",
	ylab="Thousands of Titles Produced",
	names.arg=c("  1454 -\n1500","  1501 -\n1550","  1551 -\n1600","  1601 -\n1650",
	"  1651 -\n1700","  1701 -\n1750","  1751 -\n1800")
	)
bartext=c(round(bookprod$prodtot/1000,digits=1))
locate=locator()
text(locate$x,locate$y,bartext)


bookcons=read.csv("bookcons.csv")
bookconstz=zoo(bookcons$consperK,bookcons$year)
barplot(bookconstz,
	col=rainbow(7),
	main="Book Consumption, Western Europe, 1454 - 1800 C.E.",
	ylab="Books per Thousand Population",
	names.arg=c("  1454 -\n1500","  1501 -\n1550","  1551 -\n1600","  1601 -\n1650",
	"  1651 -\n1700","  1701 -\n1750","  1751 -\n1800")
	)
bartext=c(bookcons$consperK)
locate=locator()
text(locate$x,locate$y,bartext)

###	Allens' wage to energy price ratio, early 18th c. from p 140

wageenergyratio=c(1.4,1.9,0.6,0.9,5.0,0.15)
citytext=c("Amsterdam","London","Paris","Strasbourg","Newcastle","Beijing")
barplot(wageenergyratio,
	col=rainbow(6),
	main="Relative Labour to Energy Prices, early 1700s",
	ylab="Price Ratios",
	names.arg=citytext,
	cex.names=.9
	)


#######################################################################	start logs for normalizaton ---- experimental -- need to interpolate and adjust scales

#######################################################################	start energy ---- experimental
require(zoo)

xyplot(window(mtoetz,end=c(1873)),					
	type="p",
	col="blue",
	main="Energy Consumption 1300-1873",
	xlab="C.E. (Late Middle Ages through Industrial Revolution)",
	ylab="Million Tonnes Oil Equivalent",
	auto.key = list(corner = c(0,1), points = T, lines = F,text="Fouquet 2008")
	)

### source("energy1300-1873.R")						### will not execute

startindex <- as.numeric(window(mtoetz,start='1300',end='1300')/window(ukgdptz/1000,start='1300',end='1300')) ##the starting index for the spaghetti plot

old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
on.exit(par(old.par))

par(mfrow=c(3,1))



plot(window(mtoetz,							###	version for stacked plot
	start=c(1300),
	end=c(1873)),
	type="n",
	ylab="",
	xlab="",
#	log='y'
	)		
points(window(mtoetz-1.7,						### normalized to 0
	start=c(1300),
	end=c(1873)),
	pch=20,
	col="blue")
points(window(ukgdptz/1000*startindex-1.7,
	start=c(1300),
	end=c(1873)),
	pch=20,
	col="red")
legend("topleft",
	c("Energy consumption, 1300 = 1.7 MTOE/year","Gross domestic product, indexed to 1.7 in 1300"),
#	text.col=c("blue","red"),
	col=c("blue","red"),
	pch=c(19),
	text.col=c("blue","red"),
#	col=c("blue"),
#	pch=c(19)
)
title(main = "Energy consumption and GDP, 1300-1873. \n A spaghetti chart using GDP standardized \n to 1300's energy consumption",
	xlab = "C.E. (Late Middle Ages through Industrial Revolution)",
	ylab = "Standardized levels of energy consumption and GDP")



####################################################################### end energy

#######################################################################	start gdp -- experimental


plot(window(log(ukgdptz/1000),
	start=c(1300),
	end=c(1399)),
	type="n",
	ylab="",
	xlab="",
	log='y')		
#axis(1)
#axis(2,ukgdptz/1000)
points(window(ukgdptz/1000*startindex-1.7,
	start=c(1300),
	end=c(1873)),
	pch=19,
	col="red")
points(window(ukgdptz/1000,
	start=c(1700),
	end=c(1873)),
	pch=22,
	col="red")
legend("topleft",
	c("Snooks 1994 (1300-1700)","Officer 2007 (1700-1873)"),
	text.col=c("blue","red"),
	col=c("blue","red"),
	pch=c(21,22))
abline(v=1700,
	lty=2,
	lwd=2,
	col="purple")
title(main = "Gross Domestic Product 1300-1873",
	xlab = "C.E. (Late Middle Ages through Industrial Revolution)",
	ylab = "Billion 2005 Great Britian Pounds")




#######################################################################	end gdp

