load("C:\\Documents and Settings\\Russell\\Desktop\\VAT\\Projects\\Gothenburg-Fall 2010 WS Workshop\\gothenburg.RData")

## note using snooks and mitchell gdp data spliced together in 2005 GBP
## based on van Zanden, snooks data may not be good so figure out what to do. look at Maddison.
##Prak, Maarten Roy. Early Modern Capitalism Economic and Social Change in Europe 1400-1800. London; New York: Routledge, 2001. http://public.eblib.com/EBLPublic/PublicView.do?ptiID=237334. van Zanden chapter



mtoe1300=read.csv("mtoecons.csv")
ls()
mtoet=ts(mtoe1300,start=c(1300),deltat=10)
mtoe10yrt=ts(mtoe1300,start=c(1300),deltat=10)

plot(mtoet,main="MTOE consumption 1300-1850")
plot(log(mtoet),main="ln MTOE consumption 1300-1850")

plot(mtoe10yrt,main="MTOE consumption 1300-2000")
plot(log(mtoe10yrt),main="ln MTOE consumption 1300-2000")


##############this is the one to use....zoo rocks
mtoe=read.csv("mtoecons.csv")
mtoetz=zoo(mtoe$mtoecons,mtoe$year)
plot(mtoetz)
plot(log(mtoetz))

plot(log(mtoetz))
plot(window(mtoetz,start=c(1500)))


library(forecast)
ets(mtoet)
plot(ets(mtoet))
plot(ets(log(mtoet)))



mtoe1850=read.csv("mtoe1850.csv")
mtoe1850t=ts(mtoe1850,start=c(1850))
plot(mtoe1850t,main="MTOE consumption 1850-2008")

plot(log(mtoe1850t),main="ln MTOE consumption 1850-2008")

plot(ets(mtoe1850t))
plot(ets(log(mtoe1850t)))

ukpop=read.csv("madukpop.csv")
posixyr=ISOdate(ukpop$year,6,30,12,0,0)

library(tseries)
ukpopt=irts(posixyr,ukpop$ukpop)
plot(ukpopt)

lnukpop=irts(posixyr,log(ukpop$ukpop))
plot(lnukpop)

ukpopzt=as.zoo(ukpopt)
plot(ukpopzt)
plot(log(ukpopzt))

plot(mtoetz/ukpopzt)
plot(log(mtoetz/ukpopzt))


ukgdp=read.csv("madukgdp.csv")
ukgdpzt=zoo(ukgdp$ukgdp,ukgdp$year)


plot(ukgdpzt)
plot(ukgdpzt/ukpopzt)
plot(mtoetz/ukgdpzt)#### energy

### ok, now do graphs for gothenburg paper 8/5/10
### 1.	energy 1300-1500
### 2.	energy 1500-1750
### 3.	energy 1750-1875
### 4.	energy 1300-1875
### 5.	gdp 1300-1500
### 6.	gdp 1500-1750
### 7.	gdp 1750-1875
### 8.	gdp 1300-1875
### 9.	energy/caput 1300-1500
###10.	energy/caput 1500-1750
###11.	energy/caput 1750-1875
###12.	energy/caput 1300-1875
###13.	gdp/caput 1300-1500
###14.	gdp/caput 1500-1750
###15.	gdp/caput 1750-1875
###16.	gdp/caput 1300-1875
###17.	energy/gdp 1300-1500 (energy intensity)
###18.	energy/gdp 1500-1750 (energy intensity)
###19.	energy/gdp 1750-1875 (energy intensity)
###20.	energy/gdp 1300-1875 (energy intensity)
###21.	energy/gdp/caput 1300-1500 (energy intensity per caput)
###22.	energy/gdp/caput 1500-1750 (energy intensity per caput)
###23.	energy/gdp/caput 1750-1875 (energy intensity per caput)
###24.	energy/gdp/caput 1300-1875 (energy intensity per caput)
###25.	forest acres/energy
###26.	pop/energy using human output
###27.	horse/energy using horse output
###28.	added charts from Allen data
###29.	correlations
###30.	cointegration results
###31.	do I need to change units (mtoe to exaj)?
###32.	I think logs may make stuff clearer - need to do equi-scale somehow
###33.	histogram on growth of energy consumption by era


library(zoo)
library(lattice)
library(stinepack)
library(ggplot2)

##############this is the one to use....zoo rocks

mtoe=read.csv("mtoecons.csv")
mtoetz=zoo(mtoe$mtoecons,mtoe$year)

###	look at raw data

#######################################################################	start energy
xyplot(window(mtoetz,end=c(1873)),
	type="p",
	col="blue",
	main="Energy Consumption 1300-1873",
	xlab="C.E. (Late Middle Ages through Industrial Revolution)",
	ylab="Million Tonnes Oil Equivalent",
	auto.key = list(corner = c(0,1), points = T, lines = F,text="Fouquet 2008")
	)

### source("energy1300-1873.R")						### will not execute


old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
on.exit(par(old.par))

par(mfrow=c(3,1))



plot(window(mtoetz,							###	version for stacked plot
	start=c(1086),
	end=c(1873)),
	type="n",
	ylab="",
	xlab="")
points(window(mtoetz,
	start=c(1086),
	end=c(1873)),
	pch=21,
	col="blue")
legend("topleft",
	c("Fouquet 2008"),
	text.col=c("blue"),
	col=c("blue"),
	pch=c(21))
title(main = "Energy Consumption 1300-1873",
	xlab = "C.E. (Late Middle Ages through Industrial Revolution)",
	ylab = "Million Tonnes Oil Equivalent")



####################################################################### end energy

#######################################################################	start gdp

plot(window(ukgdptz/1000,
	start=c(1300),
	end=c(1873)),
	type="n",
	ylab="",
	xlab="")
#axis(1)
#axis(2,ukgdptz/1000)
points(window(ukgdptz/1000,
	start=c(1300),
	end=c(1700)),
	pch=21,
	col="blue")
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


#######################################################################	start pop

plot(window(ukpoptz/1000,
	start=c(1300),
	end=c(1873)),
	type="n",
	ylab="",
	xlab="")
points(window(ukpoptz/1000,
	start=c(1300),
	end=c(1540)),
	pch=21,
	col="blue")
points(window(ukpoptz/1000,
	start=c(1540),
	end=c(1801)),
	pch=22,
	col="red")
points(window(ukpoptz/1000,
	start=c(1801),
	end=c(1873)),
	pch=23,
	col="green")

legend("topleft",
	c("Snooks 1994 (1300-1540)","Mitchell 1988 (1540-1801:England)","Mitchell 1988 (1801-1873:England and Wales)"),
	text.col=c("blue","red","green"),
	col=c("blue","red","green"),
	pch=c(21,22,23))
abline(v=c(1540,1801),
	lty=2,
	lwd=2,
	col="purple")
title(main = "Population 1300-1873",
	xlab = "C.E. (Late Middle Ages through Industrial Revolution)",
	ylab = "Population, Thousands")



#######################################################################	end pop


#######################################################################	start ln

plot(log(window(ukgdptz/1000,							### ln gdp
	start=c(1300),
	end=c(1873))),
	type="n",
	ylab="",
	xlab="")
#axis(1)
#axis(2,ukgdptz/1000)
points(log(window(ukgdptz/1000,
	start=c(1300),
	end=c(1700))),
	pch=21,
	col="blue")
points(log(window(ukgdptz/1000,
	start=c(1700),
	end=c(1873))),
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
title(main = "ln Gross Domestic Product 1300-1873",
	xlab = "C.E. (Late Middle Ages through Industrial Revolution)",
	ylab = "ln Billion 2005 Great Britian Pounds")



###	logs of growth rates stacked by era



#######################################################################	end ln




###	Single plots to get warmed up

xyplot(
	window(mtoetz,start=c(1300),end=c(1500)),
	main="Energy Consumption 1300-1500",
	xlab="C.E. (Late Middle Ages)",
	ylab="Million Tonnes Oil Equivalent"
	)
xyplot(
	window(mtoetz,start=c(1500),end=c(1750)),
	main="Energy Consumption 1500-1750",
	xlab="C.E. (Early Modern Period)",
	ylab="Million Tonnes Oil Equivalent"
	)
xyplot(window(mtoetz,start=c(1750),end=c(1873)),
	main="Energy Consumption 1750-1873",
	xlab="C.E. (Industrial Revolution-Allen time frame)",
	ylab="Million Tonnes Oil Equivalent"
	)
xyplot(window(mtoetz,start=c(1300),end=c(1873)),
	main="Energy Consumption 1300-1873",
	xlab="C.E. (Black Death through Industrial Revolution)",
	ylab="Million Tonnes Oil Equivalent"
	)

#### attempt multiple plots

###	use historical eras---perhaps use structural breaks later

combo=cbind(
	window(mtoetz,start=c(1300),end=c(1500)),
	window(mtoetz,start=c(1500),end=c(1750)),
	window(mtoetz,start=c(1750),end=c(1873)),
	window(mtoetz,start=c(1300),end=c(1873)))

###	rename columns do Energy consumption in main, do eras and years in cnames

cnames=list(
	"Late Middle Ages 1300-1500",
	"Early Modern Period 1500-1750",
	"Industrial Revolution 1750-1873 (Allen time frame)",
	"Black Death through Industrial Revolution 1300-1873"
	)

names(combo)=cnames


xyplot(combo, 									###	in scale="free"
	main = "Energy Consumption scaled per era",
	xlab = "Time, C.E.",
	ylab = "Million Tonnes Oil Equivalent"
	)
xyplot(combo, 									###	in scale="same"
	scales = list(y="same",alternating=F),
	main = "Energy Consumption scaled to total era",
	xlab = "Time, C.E.",
	ylab = "Million Tonnes Oil Equivalent"
	)

###	how show growth rates? logs?

xyplot(combo, 									###	in scale="free"
	scales = list(y=list(log="e")),
	main = "ln Energy Consumption scaled per era",
	xlab = "Time, C.E.",
	ylab = "ln Million Tonnes Oil Equivalent"
	)

###	also calculate growth rates per era
latemiddle = (log(mtoe[201,2])-log(mtoe[1,2])/(1500-1300+1)		###	annual growth (log)
earlymodern = (log(mtoe[451,2])-log(mtoe[201,2]))/(1750-1500+1)	###	ibid
industrev = (log(mtoe[574,2])-log(mtoe[451,2]))/(1873-1750+1)		###	ibid
fullera = (log(mtoe[574,2])-log(mtoe[1,2]))/(1873-1300+1)		###	ibid


globalenergy=read.csv("C:/Documents and Settings/Russell/Desktop/VAT/Classes Taught/4650Fa10/Week 02/model1.csv") ### global
currentera = (log(globalenergy[27,1])-log(globalenergy[1,1]))/(2006-1980+1)	###	log percent annual growth


### overall growth chart in ln growth rates -

bplot = c(latemiddle,earlymodern,industrev)				###	use for y axis label

bnames=c("C.E. 1300-1500","C.E. 1500-1750","C.E. 1750-1873")		###	use for y axis lable

### barplot(bplot,names.arg=bnames,ylim=c(-.005,.025))

barplot(bplot,names.arg=bnames,ylim=c(-.005,.025),xlim=c(0,4),axes=F,col=c("green","yellow","red"))
###	no axes, but right axes size

axis(2, at=bplot,labels=percent(bplot))					###	y axis, use ggplot2::percent

axis(1,pos=c(0,0),labels=F,lwd.ticks=0)					###	so a naked x axis

locate=locator()									###	store points for bar test

text(locate$x,locate$y,labels=percent(bplot))				###	plot bar test

title(main="Annual Percent Energy Consumption change",ylab="Percentage per Year")	###	the main title
mtext("in natural log differences",line=0,font=4)

legend("topleft", c("Late Middle Ages","Early Modern Period","Industrial Revolution"),
	text.col=c("green","yellow","red"),bty="n",inset=.10)		###	no margin colored legend

###	next get current global percent growth and plot

bplot2 = c(industrev,currentera)						###	use for y axis label
bplot2r = c(currentera,industrev)						###	reverse for axis lable

bnames2=c("C.E. 1750-1873","C.E. 1983-2006")				###	use for y axis lable


barplot(bplot2,names.arg=bnames2,ylim=c(0,.022),xlim=c(0,3),axes=F,col=c("green","red"))
###	no axes, but right axes size

axis(2, at=c(0,bplot2r[2]),labels=c("0.0%","2.1%"))					###	y axis, use ggplot2::percent


locate2=locator()									###	store points for bar test

text(locate2$x,locate2$y,labels=percent(bplot2))				###	plot bar test

title(main="Annual Percent Energy Consumption change",ylab="Percentage per Year")	###	the main title
mtext("in natural log differences",line=0,font=4)

legend("topright", c("England","Total World"),
	text.col=c("green","red"),bty="n")		###	no margin colored legend

###	now do gdp CHECK VAN ZANDEN GDP DATA

###	ukgdp = read.csv("file://C:/Documents and Settings/Russell/Desktop/VAT/Projects/
	Gothenburg-Fall 2010 WS Workshop/madukgdp.csv")					###	nb!!!! data for 1759 splined from Officer
ukgdp = read.csv("madukgdp.csv")
ukgdptz = zoo(ukgdp$splicedgdp,ukgdp$year)

xyplot(window(ukgdptz,end=c(1873)),type="p",col="blue")				### the way to look at gdp, use later start
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
	"Late Middle Ages 1300-1500",
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










