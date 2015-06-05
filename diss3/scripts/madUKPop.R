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
madTotPop <- read.csv ( paste ( datapath,"madUKPop.csv" , sep='' ) , 
	header = TRUE , stringsAsFactors = FALSE )

## clean up rows and round to whole thousands
madUKPop <- madUKPop[-c(1:2),]
madUKPop [ , 2 ] <- round(as.numeric(madUKPop[,2],0))

## expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
madUKPopNew <- madUKPop[1,]
for ( i in 2:999 ) {

## add new row
	madUKPopNew <- rbind ( madUKPopNew , madUKPopNew[i,] )

## populate year
	madUKPopNew [ i , 1 ] <- i
}

## rbind madUKPop[3,]
madUKPopNew <- rbind ( madUKPopNew , madUKPop [ 3 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1001:1499 ) {

## add new row
	madUKPopNew <- rbind ( madUKPopNew , madUKPopNew[i,] )

## populate year
	madUKPopNew [ i , 1 ] <- i
}

## rbind madUKPop[3,]
madUKPopNew <- rbind ( madUKPopNew , madUKPop [ 5 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1501:1599 ) {

## add new row
	madUKPopNew <- rbind ( madUKPopNew , madUKPopNew[i,] )

## populate year
	madUKPopNew [ i , 1 ] <- i
}

## rbind madUKPop[3,]
madUKPopNew <- rbind ( madUKPopNew , madUKPop [ 7 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1601:1699 ) {

## add new row
	madUKPopNew <- rbind ( madUKPopNew , madUKPopNew[i,] )

## populate year
	madUKPopNew [ i , 1 ] <- i
}

## rbind madUKPop[3,]
madUKPopNew <- rbind ( madUKPopNew , madUKPop [ 9 , ] )

## again expand the columns for stinterp - need blanks for each year 
## to be interpolated
## set up loop
for ( i in 1701:1819 ) {

## add new row
	madUKPopNew <- rbind ( madUKPopNew , madUKPopNew[i,] )

## populate year
	madUKPopNew [ i , 1 ] <- i
}

## rbind madUKPop[3,]
madUKPopNew <- rbind ( madUKPopNew , madUKPop [ 11:nrow(madUKPop) , ] )

## now apply stinterp
na.stinterp ( madUKPopNew [ , 2 ] )

## do a crude plot
plot(madUKPopNew[,1],ts(na.stinterp ( madUKPopNew [ , 2 ] )))

## do a crude log plot
plot(madUKPopNew[,1],log(ts(na.stinterp ( madUKPopNew [ , 2 ] ))))

## a great plot - the first diff of the log, so a sort of second diff
plot(diff(ts(log(na.stinterp(madUKPopNew[,2])))),lwd=2)

## a great plot - the first diff of the log, so a sort of second diff - 1900 on
plot(diff(ts(log(na.stinterp(madUKPopNew[1:nrow(madUKPopNew),2])),start=2)),lwd=2)

### now get the ggplots
require(ggplot2)
require(scales)

### start from existing code block

## first plot levels
## first interpolate
x <- as.numeric(madUKPopNew[,1])
y <- na.stinterp(madUKPopNew[,2])

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
x <- as.numeric(madUKPopNew[1750:nrow(madUKPopNew),1])
y <- na.stinterp(madUKPopNew[,2])
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
x <- as.numeric(madUKPopNew[1850:nrow(madUKPopNew),1])
y <- na.stinterp(madUKPopNew[,2])
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
