###	11110 energy conference with Razai
## us starts at line 552
## chn starts at 1309
## 1765 all screwed up

windows(record=T)

library(forecast)
library(ggplot2)

###	co2 = co2/ec * ec/gdp * gdp/pop * pop

###	co2 metric tonnes

###	co2 <- ts(read.csv("clipboard",header=T),start=1980)
## wec from total_primary_energy_consumption_(quadrillion_btu).xls


ex <- read.csv("extract.csv",header=T); attach(ex)


ggplot(ex,aes(year,wco2))+
#png("us/wco2.png")
geom_line(colour="blue",size=2)+
scale_y_continuous("CO2 emissions, metric tonnes") +
opts(title="U.S. CO2 Emissions")

ggsave(file='wco2.png') #to save file just printed, so change file name as appropriate


## qplot(year, wco2/1000000000, data=nco2, geom="line",log="y")

###	energy BTU

ggplot(ex,aes(year,wec))+
#png("us/wec.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Primay energy consumption, BTU") +
opts(title="World primary energy consumption")

ggsave(file='wec.png') #to save file just printed, so change file name as appropriate

###	gdp 2000 USD

ggplot(ex,aes(year,wgdp2005)) +
#png("wgdp.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("GDP, 1990 USD - Market Exchange Rates") +
opts(title="World gross domestic product")

ggsave(file='wgdp.png') #to save file just printed, so change file name as appropriate

###	pop

p <- ggplot(ex,aes(year,pop))
png("wpop.png")
p +
geom_line(colour="blue",size=2) +
scale_y_continuous("Population") +
opts(title="World population")
dev.off()

### now get ratios

###	carbon intensity of energy

p <- ggplot(ex,aes(year,(wco2/wec*1000000)))		### wco2/1000000 BTU
png("wco2wec.png")
p +
geom_line(colour="blue",size=2) +
scale_y_continuous("CO2/Million BTU EC") +
opts(title="World carbon intensity of energy")
dev.off()


###	energy intensity of gdp

p <- ggplot(ex,aes(year,(wec/wgdp2005)))
png("wecwgdp.png")
p +
geom_line(colour="blue",size=2) +
scale_y_continuous("EC/GDP") +
opts(title="World energy intensity of GDP")
dev.off()

###	living standards

p <- ggplot(ex,aes(year,(wgdp2005/pop)))
png("wgdpwpop.png")
p +
geom_line(colour="blue",size=2) +
scale_y_continuous("GDP/POP") +
opts(title="World living standards, 2005 USD - Market Exchange Rates")
dev.off()

###	energy intensity per capita

p <- ggplot(ex,aes(year,(wec/pop)))
png("wecwpop.png")
p +
geom_line(colour="blue",size=2) +
scale_y_continuous("Energy Consumption/POP") +
opts(title="World Energy Consumption per Capita")
dev.off()


###	now forecast co2

###	first show model fit

mwco2 <- forecast(wco2/1000000,h=100,level=50)
p <- ggplot()
png("wco2mfit.png")
p +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="CO2 State-Space Model Fit \n (AAN)")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("wco2fcst.png")
p +
geom_line(aes(x=fyear,y=mwco2$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwco2$mean+3000,ymax=mwco2$upper[,1]),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwco2$mean-3000,ymin=mwco2$lower[,1]),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="World CO2 Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	now forecast wec

###	first show model fit

mwec <- forecast(wec/1000000000000,h=100,level=50)
p <- ggplot()
png("wecmfit.png")
p +
geom_line(aes(x=year,y=mwec$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
opts(title="EC State-Space Model Fit \n (MMN)")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("wecfcst.png")
p +
geom_line(aes(x=fyear,y=mwec$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwec$mean+3000,ymax=mwec$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwec$mean-3000,ymin=mwec$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwec$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwec$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
opts(title="World Energy Consumption Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	now forecast gdp

###	first show model fit

mgdp <- forecast(wgdp2005/1000000000,h=100,level=50)
p <- ggplot()
png("wgdpmfit.png")
p +
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 2005 USD MER") +
opts(title="GDP State-Space Model Fit \n (MMN)")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("wgdpfcst.png")
p +
geom_line(aes(x=fyear,y=mgdp$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mgdp$mean+3000,ymax=mgdp$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mgdp$mean-3000,ymin=mgdp$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 2005 USD MER") +
opts(title="World Gross Domestic Product Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	now forecast pop

###	first show model fit

mpop <- forecast(pop/1000000,h=100,level=50)
p <- ggplot()
png("wpopmfit.png")
p +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mpop$fitted),colour="red",size=4,alpha=0.5) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="Population State-Space Model Fit \n (MAN)")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("wpopfcst.png")
p +
geom_line(aes(x=fyear,y=mpop$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="World Population Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	now do intensive models

###	co2 intensity of energy
###	first show model fit

p <- ggplot()
png("co2intmfit.png")
p +
geom_line(aes(x=year,y=mwco2$x/mwec$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted/mwec$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2/EC BTU") +
opts(title="Carbon Intensity of Energy Consumption Model Fit")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("co2intfcst.png")
p +
geom_line(aes(x=fyear,y=mwco2$mean/mwec$mean*1000000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwco2$mean/mwec$mean*1000000+30,ymax=mwco2$upper/mwec$upper*1000000),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwco2$mean/mwec$mean*1000000-30,ymin=mwco2$lower/mwec$lower*1000000),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2$x/mwec$x*1000000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted/mwec$fitted*1000000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / Million BTU EC") +
opts(title="World Carbon Intensity of Energy Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	energy intensity of gdp
###	first show model fit

p <- ggplot()
png("ecintmfit.png")
p +
geom_line(aes(x=year,y=mwec$x/mgdp$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted/mgdp$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("EC BTU / GDP 2005 USD MER") +
opts(title="Energy Consumption Intensity of GDP Model Fit")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("ecintmfcst.png")
p +
geom_line(aes(x=fyear,y=mwec$mean/mgdp$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwec$mean/mgdp$mean*1000,ymax=mwec$upper/mgdp$upper*1000),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=mwec$mean/mgdp$mean*1000,ymin=mwec$lower/mgdp$lower*1000),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwec$x/mgdp$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted/mgdp$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("BTU EC / GDP 2005 USD MER") +
opts(title="World Energy Intensity of Gross Domestic Product \n Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	gdp intensity of population
###	first show model fit

p <- ggplot()
png("gdpintmfit.png")
p +
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 2005 USD MER / Population") +
opts(title="GDP Intensity of Population Model Fit")
dev.off()

###	now do forecast plot with error bands

fyear=seq(2009,2108)
p <- ggplot()
png("gdpintmfcst.png")
p +
geom_line(aes(x=fyear,y=mgdp$mean/mpop$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean*1000,ymax=mgdp$upper/mpop$upper*1000),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean*1000,ymin=mgdp$lower/mpop$lower*1000),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 2005 USD MER / Population") +
opts(title="World Gross Domestic Product Intensity of Population \n Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")
dev.off()

###	forecast 100 in variable, plot

###	do ratios from base forecasts, plot

###	factor in UN pop forecast from http://esa.un.org/unpd/wpp/unpp/p2k0data.asp

###	get UN 2010 lowpop, replace NA with na.stinterp

lowpop <- read.csv("lowpop.csv",header=T)

library(stinepack)

lowpop$lowpop <- na.stinterp(lowpop$lowpop)

###	show un lowpop with pop forecast

fyear=seq(2010,2100)
p <- ggplot()
png("unlowpopfcst.png")
p +
geom_line(aes(x=fyear,y=lowpop$lowpop/1000),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="World Population Forecast, 2010 - 2100 \n (UN 2010 Low Population Forecast)")
dev.off()


###	now run the forecast

fyear=seq(2010,2100)
yco2 <- lowpop$lowpop*mgdp$mean[2:92]/mpop$mean[2:92]*mwec$mean[2:92]/mgdp$mean[2:92]*mwco2$mean[2:92]/mwec$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
p <- ggplot()
png("unlowco2fcst.png")
p +
geom_line(aes(x=fyear,y=yco2),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="World Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Low Population Estimate)")
dev.off()

###	get UN 2010 medpop, replace NA

medpop <- read.csv("medpop.csv",header=T)

library(stinepack)

medpop$medpop <- na.stinterp(medpop$medpop)

###	show un medpop with pop forecast

fyear=seq(2010,2100)
p <- ggplot()
png("unmedpopfcst.png")
p +
geom_line(aes(x=fyear,y=medpop$medpop/1000),colour="yellow",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="World Population Forecast, 2010 - 2100 \n (UN 2010 Medium Population Forecast)")
dev.off()


###	now run the forecast

fyear=seq(2010,2100)
yco2 <- medpop$medpop*mgdp$mean[2:92]/mpop$mean[2:92]*mwec$mean[2:92]/mgdp$mean[2:92]*mwco2$mean[2:92]/mwec$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
p <- ggplot()
png("unmedco2fcst.png")
p +
geom_line(aes(x=fyear,y=yco2),colour="yellow",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="World Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Medium Population Estimate)")
dev.off()

###	get UN 2010 highpop, replace NA

highpop <- read.csv("highpop.csv",header=T)

library(stinepack)

highpop$highpop <- na.stinterp(highpop$highpop)

###	show un highpop with pop forecast

fyear=seq(2010,2100)
p <- ggplot()
png("unhighpopfcst.png")
p +
geom_line(aes(x=fyear,y=highpop$highpop/1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="World Population Forecast, 2010 - 2100 \n (UN 2010 High Population Forecast)")
dev.off()


###	now run the forecast

fyear=seq(2010,2100)
yco2 <- highpop$highpop*mgdp$mean[2:92]/mpop$mean[2:92]*mwec$mean[2:92]/mgdp$mean[2:92]*mwco2$mean[2:92]/mwec$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
p <- ggplot()
png("unhighco2fcst.png")
p +
geom_line(aes(x=fyear,y=yco2),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="World Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 High Population Estimate)")
dev.off()


###	now run the comparative co2 -- do not run all three yco2 at once -- run one at a time


fyear=seq(2010,2100)
yco2h <- highpop$highpop*mgdp$mean[2:92]/mpop$mean[2:92]*mwec$mean[2:92]/mgdp$mean[2:92]*mwco2$mean[2:92]/mwec$mean[2:92]
yco2h <- yco2h  /1000 ###	to scale properly to million tonnes of co2

yco2m <- medpop$medpop*mgdp$mean[2:92]/mpop$mean[2:92]*mwec$mean[2:92]/mgdp$mean[2:92]*mwco2$mean[2:92]/mwec$mean[2:92]
yco2m <- yco2m  /1000 ###	to scale properly to million tonnes of co2

yco2 <- lowpop$lowpop*mgdp$mean[2:92]/mpop$mean[2:92]*mwec$mean[2:92]/mgdp$mean[2:92]*mwco2$mean[2:92]/mwec$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2

p <- ggplot()
png("uncompco2fcst.png")
p +
geom_line(aes(x=fyear,y=yco2h),colour="red",size=2) +
geom_line(aes(x=fyear,y=yco2m),colour="yellow",size=2) +
geom_line(aes(x=fyear,y=yco2),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="World Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Population Estimates)")
dev.off()


###	now do a combined pop graph for parsimony

library(stinepack)
lowpop <- read.csv("lowpop.csv",header=T)
lowpop$lowpop <- na.stinterp(lowpop$lowpop)
medpop <- read.csv("medpop.csv",header=T)
medpop$medpop <- na.stinterp(medpop$medpop)
highpop <- read.csv("highpop.csv",header=T)
highpop$highpop <- na.stinterp(highpop$highpop)


fyear=seq(2010,2100)
p <- ggplot()
png("unpopfcst.png")
p +
geom_line(aes(x=fyear,y=lowpop$lowpop/1000),colour="green",size=2) +
geom_line(aes(x=fyear,y=medpop$medpop/1000),colour="yellow",size=2) +
geom_line(aes(x=fyear,y=highpop$highpop/1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="World Population Forecast, 2010 - 2100 \n (UN 2010 Population Forecast)")
dev.off()



###	carbon decoupling?


p <- ggplot()
png("wco2decoup.png")
p +
geom_line(aes(x=year,y=wco2/wgdp2005),colour="blue",size=2) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous(" Tonnes of CO2 Emissions / GDP 2005 USD MER") +
opts(title="Global Carbon Intensity of Gross Domestic Product \n (A Measure of 'Decoupling')")
dev.off()

###	the elasticity calcs

elas <- lm(log(wco2) ~ log(wco2/wec) + log(wec/wgdp2005) + log(wgdp2005/pop))
summary(elas)
library(xtable)
xtable(elas)

###	do co2 ppm calcs and charts

###	absorbtion = 3 billion tonnes per year,
###	2,130,000,000 metric tons of CO2 = 1ppm
###	so take emissions per year, subtract 3 billion, convert into ppm
###	then temp 2010 ppm 392 July 2011


mwco2n <- mwco2$mean*1000000
mwco2a <- mwco2n - 3000000000
mwco2ppm <- mwco2a/2130000000
mwco2ppm
sum(mwco2ppm)

###	http://answers.yahoo.com/question/index?qid=20070906124908AAXV6Bx

###	off by factor of 10

mwco2ppm <- mwco2ppm / 10

## ########################################US

windows(record=T)

library(forecast)
library(ggplot2)

###	co2 = co2/ec * ec/gdp * gdp/pop * pop

###	co2 metric tonnes

###	co2 <- ts(read.csv("clipboard",header=T),start=1980)
## wec from total_primary_energy_consumption_(quadrillion_btu).xls


ex <- read.csv("extractus.csv",header=T); attach(ex)

## set up models, both ets and arima
mwco2 <- forecast(wco2/1000000,h=100,level=50) #ets(A,A,N)
mwco2a <- forecast.Arima(auto.arima(wco2/1000000),h=100,level=50) #arima(0,1,0) with drift
mwec <- forecast(wec/1000000000000,h=100,level=50) #ets(A,A,N)
mweca <- forecast.Arima(auto.arima(wec/1000000000000),h=100,level=50) #arima(0,1,0) with drift
mgdp <- forecast(wgdp2005/1000000000,h=100,level=50) #ets(A,A,N)
mgdpa <- forecast.Arima(auto.arima(wgdp2005/1000000000),h=100,level=50) #arima(0,1,1)
mpop <- forecast(pop/1000000,h=100,level=50)                    #ets(A,A,N)
mpopa <- forecast.Arima(auto.arima(pop/1000000),h=100,level=50) #arima(0,2,1)

gggplot(ex,aes(year,wco2))+
#png("us/wco2.png")
geom_line(colour="blue",size=2)+
scale_y_continuous("CO2 emissions, metric tonnes") +
opts(title="U.S. CO2 Emissions")

ggsave(file='us/wco2.png') #to save file just printed, so change file name as appropriate

## qplot(year, wco2/1000000000, data=nco2, geom="line",log="y")

###	energy BTU

ggplot(ex,aes(year,wec))+
#png("wec.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Primay energy consumption, BTU") +
opts(title="U.S. primary energy consumption")

ggsave(file='us/wec.png') #to save file just printed, so change file name as appropriate


###	gdp 2000 USD

ggplot(ex,aes(year,wgdp2005))+
#png("wgdp.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("GDP, 1990 USD - Market Exchange Rates") +
opts(title="U.S. gross domestic product")

ggsave(file='us/wgdp.png') #to save file just printed, so change file name as appropriate

###	pop

ggplot(ex,aes(year,pop))+
#png("wpop.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Population") +
opts(title="U.S. population")

ggsave(file='us/wpop.png') #to save file just printed, so change file name as appropriate

### now get ratios

###	carbon intensity of energy

ggplot(ex,aes(year,(wco2/wec*1000000)))+		### wco2/1000000 BTU
#png("wco2wec.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("CO2 / Million BTU EC") +
opts(title="U.S. carbon intensity of energy")

ggsave(file='us/wco2wec.png') #to save file just printed, so change file name as appropriate


###	energy intensity of gdp

ggplot(ex,aes(year,(wec/wgdp2005)))+
#png("wecwgdp.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("EC / GDP") +
opts(title="U.S. energy intensity of GDP")

ggsave(file='us/wecwgdp.png') #to save file just printed, so change file name as appropriate

###	living standards

ggplot(ex,aes(year,(wgdp2005/pop)))+
#png("wgdpwpop.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("GDP / POP") +
opts(title="U.S. living standards, 1990 USD - Market Exchange Rates")

ggsave(file='us/wgdpwpop.png') #to save file just printed, so change file name as appropriate

###	energy intensity per capita

ggplot(ex,aes(year,(wec/pop)))+
#png("wecwpop.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Energy Consumption / POP") +
opts(title="U.S. Energy Consumption per Capita")

ggsave(file='us/wecwpop.png') #to save file just printed, so change file name as appropriate


###	now forecast co2

###	first show model fit



ggplot()+
#png("wco2mfit.png")
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=1985,y=6000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=5900,label='red = fitted',colour='red')+
opts(title="U.S. CO2 State-Space Model Fit \n (ANN)")

ggsave(file='us/wco2mfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wco2fcst.png")
geom_line(aes(x=fyear,y=mwco2$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwco2$mean,ymax=mwco2$upper[,1]),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwco2$mean,ymin=mwco2$lower[,1]),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2020,y=6750,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=6625,label='red = fitted',colour='red')+
    opts(title="U.S. CO2 Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wco2fcst.png")
geom_line(aes(x=fyear,y=mwco2a$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mwco2a$mean),ymax=as.vector(mwco2a$upper[,1])),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mwco2a$mean),ymin=as.vector(mwco2a$lower[,1])),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2a$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwco2a$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
#annotate('text',x=2020,y=10250,label='blue = actual',colour='blue')+
#    annotate('text',x=2020,y=10000,label='red = fitted',colour='red')+
annotate('text',x=2020,y=10250,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=10000,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=9750,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=9500,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="U.S. CO2 Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='us/wco2fcst.png') #to save file just printed, so change file name as appropriate


###	now forecast wec

###	first show model fit


ggplot()+
#png("wecmfit.png")
geom_line(aes(x=year,y=mwec$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
annotate('text',x=1985,y=100000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=99000,label='red = fitted',colour='red')+
    opts(title="U.S. EC State-Space Model Fit \n (ANN)")

ggsave(file='us/wecmfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

fyear=seq(2009,2108)
ggplot()+
#png("wecfcst.png")
geom_line(aes(x=fyear,y=mwec$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwec$mean+3000,ymax=mwec$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwec$mean-3000,ymin=mwec$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwec$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwec$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
opts(title="U.S. Energy Consumption Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")

ggsave(file='us/wecfcst.png') #to save file just printed, so change file name as appropriate


###	now forecast gdp

###	first show model fit

## ets
ggplot()+
#png("wgdpmfit.png")
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=1985,y=9500,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=9250,label='red = fitted',colour='red')+
    opts(title="U.S. GDP Model Fit \n ETS (AAN)")

## arima
ggplot()+
#png("wgdpmfit.png")
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=1985,y=9500,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=9250,label='red = fitted',colour='red')+
    opts(title="U.S. GDP Model Fit \n ARIMA (0,1,1) with drift")


ggsave(file='us/wgdpmfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wgdpfcst.png")
geom_line(aes(x=fyear,y=mgdp$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mgdp$mean,ymax=mgdp$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mgdp$mean,ymin=mgdp$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=2030,y=32500,label='blue = actual',colour='blue')+
    annotate('text',x=2030,y=31250,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2030,y=30000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=28750,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="U.S. Gross Domestic Product Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wgdpfcst.png")
geom_line(aes(x=fyear,y=mgdpa$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mgdpa$mean),ymax=as.vector(mgdpa$upper)),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mgdpa$mean),ymin=as.vector(mgdpa$lower)),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mgdpa$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mgdpa$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=2030,y=32500,label='blue = actual',colour='blue')+
    annotate('text',x=2030,y=31250,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2030,y=30000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=28750,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="U.S. Gross Domestic Product Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='us/wgdpfcst.png') #to save file just printed, so change file name as appropriate

###	now forecast pop

###	first show model fit

## ets
ggplot()+
#png("wpopmfit.png")
geom_line(aes(x=year,y=mpop$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mpop$fitted),colour="red",size=4,alpha=0.5) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=1985,y=300,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=297,label='red = fitted',colour='red')+
    opts(title="U.S. Population Model Fit \n ETS (AAN)")

## arima
ggplot()+
#png("wpopmfit.png")
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mpopa$fitted),colour="red",size=4,alpha=0.5) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=1985,y=300,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=297,label='red = fitted',colour='red')+
    opts(title="U.S. Population Model Fit \n ARIMA (0,2,1)")


ggsave(file='us/wpopmfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wpopfcst.png")
geom_line(aes(x=fyear,y=mpop$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mpop$mean,ymax=mpop$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mpop$mean,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2030,y=650,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=625,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=600,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=575,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="U.S. Population Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wpopfcst.png")
geom_line(aes(x=fyear,y=mpopa$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mpopa$mean),ymax=as.vector(mpopa$upper)),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mpopa$mean),ymin=as.vector(mpopa$lower)),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mpopa$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2030,y=650,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=625,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=600,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=575,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="U.S. Population Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='us/wpopfcst.png') #to save file just printed, so change file name as appropriate


###	now do intensive models

###	co2 intensity of energy
###	first show model fit

## ets
ggplot()+
#png("co2intmfit.png")
geom_line(aes(x=year,y=mwco2$x/mwec$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted/mwec$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2/EC BTU") +
annotate('text',x=1995,y=0.0610,label='blue = actual',colour='blue')+
    annotate('text',x=1995,y=0.0608,label='red = fitted',colour='red')+
    opts(title="U.S. Carbon Intensity of Energy Consumption Model Fit \n ETS")

## arima
ggplot()+
#png("co2intmfit.png")
geom_line(aes(x=year,y=mwco2a$x/mweca$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2a$fitted/mweca$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / EC BTU") +
annotate('text',x=1995,y=0.0610,label='blue = actual',colour='blue')+
    annotate('text',x=1995,y=0.0608,label='red = fitted',colour='red')+
    opts(title="U.S. Carbon Intensity of Energy Consumption Model Fit \n ARIMA")

ggsave(file='us/co2intmfit.png') #to save file just printed, so change file name as appropriate

###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("co2intfcst.png")
geom_line(aes(x=fyear,y=mwco2$mean/mwec$mean*1000000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwco2$mean/mwec$mean*1000000+30,ymax=mwco2$upper/mwec$upper*1000000),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwco2$mean/mwec$mean*1000000-30,ymin=mwco2$lower/mwec$lower*1000000),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2$x/mwec$x*1000000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted/mwec$fitted*1000000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / Million BTU EC") +
    annotate('text',x=2030,y=650,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=625,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=600,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=575,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.1)+
opts(title="U.S. Carbon Intensity of Energy Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("co2intfcst.png")
geom_line(aes(x=year,y=mwco2a$x/mweca$x*1000000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2a$fitted/mweca$fitted*1000000),colour="red",size=1) +
geom_line(aes(x=fyear,y=mwco2a$mean/mweca$mean*1000000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mwco2a$mean/mweca$mean*1000000),ymax=as.vector(mwco2a$upper/mweca$upper*1000000)),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mwco2a$mean/mweca$mean*1000000),ymin=as.vector(mwco2a$lower/mweca$lower*1000000)),fill="blue",alpha=0.1) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / Million BTU EC") +
annotate('text',x=2050,y=61000,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=60500,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2040,y=60000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2050,y=59550,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.5)+
    opts(title="U.S. Carbon Intensity of Energy Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='us/co2intfcst.png') #to save file just printed, so change file name as appropriate

###	energy intensity of gdp
###	first show model fit

## ets
ggplot()+
#png("ecintmfit.png")
geom_line(aes(x=year,y=mwec$x/mgdp$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted/mgdp$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("EC BTU / GDP 2005 USD MER") +
opts(title="U.S. Energy Consumption Intensity of GDP Model Fit \n ETS")

## arima
ggplot()+
#png("ecintmfit.png")
geom_line(aes(x=year,y=mweca$x/mgdpa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mweca$fitted/mgdpa$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("EC BTU / GDP 1990 USD MER") +
opts(title="U.S. Energy Consumption Intensity of GDP Model Fit \n ARIMA")


ggsave(file='us/ecintmfit.png') #to save file just printed, so change file name as appropriate

###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("ecintmfcst.png")
geom_line(aes(x=fyear,y=mwec$mean/mgdp$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwec$mean/mgdp$mean*1000,ymax=mwec$upper/mgdp$upper*1000),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=mwec$mean/mgdp$mean*1000,ymin=mwec$lower/mgdp$lower*1000),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwec$x/mgdp$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted/mgdp$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("BTU EC / GDP 1990 USD MER") +
opts(title="U.S. Energy Intensity of Gross Domestic Product \n ETS Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("ecintmfcst.png")
geom_line(aes(x=fyear,y=mweca$mean/mgdpa$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mweca$mean/mgdpa$mean*1000),ymax=as.vector(mweca$upper/mgdpa$upper*1000)),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mweca$mean/mgdpa$mean*1000),ymin=as.vector(mweca$lower/mgdpa$lower*1000)),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mweca$x/mgdpa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mweca$fitted/mgdpa$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("BTU EC / GDP 1990 USD MER") +
    annotate('text',x=2040,y=18500,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=18000,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2040,y=17500,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2040,y=17000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.5)+
    opts(title="U.S. Energy Intensity of Gross Domestic Product \n ARIMA Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")


ggsave(file='us/ecintmfcst.png') #to save file just printed, so change file name as appropriate

###	gdp intensity of population
###	first show model fit

## ets
ggplot()+
#png("gdpintmfit.png")
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
annotate('text',x=1985,y=31000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=30500,label='red = fitted',colour='red')+
    opts(title="U.S. GDP Intensity of Population Model Fit \n ETS")

## arima
ggplot()+
#png("gdpintmfit.png")
geom_line(aes(x=year,y=mgdpa$x/mpopa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdpa$fitted/mpopa$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
annotate('text',x=1985,y=31000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=30500,label='red = fitted',colour='red')+
    opts(title="U.S. GDP Intensity of Population Model Fit \n ARIMA")

ggsave(file='us/gdpintmfit.png') #to save file just printed, so change file name as appropriate

###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("gdpintmfcst.png")
geom_line(aes(x=fyear,y=mgdp$mean/mpop$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean*1000,ymax=mgdp$upper/mpop$upper*1000),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean*1000,ymin=mgdp$lower/mpop$lower*1000),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
opts(title="U.S. Gross Domestic Product Intensity of Population \n Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("gdpintmfcst.png")
geom_line(aes(x=fyear,y=mgdpa$mean/mpopa$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mgdpa$mean/mpopa$mean*1000),ymax=as.vector(mgdpa$upper/mpopa$upper*1000)),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mgdpa$mean/mpopa$mean*1000),ymin=as.vector(mgdpa$lower/mpopa$lower*1000)),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
    annotate('text',x=2030,y=57500,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=56000,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=54500,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=53000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.5)+
    opts(title="U.S. Gross Domestic Product Intensity of Population \n Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")


ggsave(file='us/gdpintmfcst.png') #to save file just printed, so change file name as appropriate


###	forecast 100 in variable, plot

###	do ratios from base forecasts, plot

###	factor in UN pop forecast from http://esa.un.org/unpd/wpp/unpp/p2k0data.asp

###	get UN 2010 lowpop, replace NA with na.stinterp

lowpop <- read.csv("usalowpop.csv",header=T)

library(stinepack)

lowpop$lowpop <- na.stinterp(lowpop$lowpop)

###	show un lowpop with pop forecast

fyear=seq(2010,2100)
ggplot()+
#png("unlowpopfcst.png")
geom_line(aes(x=fyear,y=lowpop$lowpop/1000),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2040,y=240,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=235,label='green = low forecast',colour='green')+
    opts(title="U.S. Population Forecast, 2010 - 2100 \n (UN 2010 Low Population Forecast)")

ggsave(file='us/unlowpopfcst.png') #to save file just printed, so change file name as appropriate

###	now run the forecast

fyear=seq(2010,2100)
yco2 <- lowpop$lowpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
ggplot()+
#png("unlowco2fcst.png")
geom_line(aes(x=fyear,y=yco2),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2040,y=4500,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=4400,label='green = low forecast',colour='green')+
    opts(title="U.S. Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Low Population Estimate)")

ggsave(file='us/unlowco2fcst.png') #to save file just printed, so change file name as appropriate

###	get UN 2010 medpop, replace NA

medpop <- read.csv("usamedpop.csv",header=T)

library(stinepack)

medpop$medpop <- na.stinterp(medpop$medpop)

###	show un medpop with pop forecast

## all arima
fyear=seq(2010,2100)
ggplot()+
#png("unmedpopfcst.png")
geom_line(aes(x=fyear,y=medpop$medpop/1000),colour="yellow",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2040,y=240,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=230,label='yellow = medium forecast',colour='yellow')+
    opts(title="U.S. Population Forecast, 2010 - 2100 \n (UN 2010 Medium Population Forecast)")

ggsave(file='us/unmedpopfcst.png') #to save file just printed, so change file name as appropriate

###	now run the forecast

## arima
fyear=seq(2010,2100)
yco2 <- medpop$medpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
ggplot()+
#png("unmedco2fcst.png")
geom_line(aes(x=fyear,y=yco2),colour="yellow",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2040,y=4500,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=4350,label='yellow = medium forecast',colour='yellow')+
    opts(title="U.S. Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Medium Population Estimate)")

ggsave(file='us/unmedco2fcst.png') #to save file just printed, so change file name as appropriate

###	get UN 2010 highpop, replace NA

highpop <- read.csv("usahighpop.csv",header=T)

library(stinepack)

highpop$highpop <- na.stinterp(highpop$highpop)

###	show un highpop with pop forecast

## arima
fyear=seq(2010,2100)
ggplot()+
#png("unhighpopfcst.png")
geom_line(aes(x=fyear,y=highpop$highpop/1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2040,y=250,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=235,label='red = high forecast',colour='red')+
    opts(title="U.S. Population Forecast, 2010 - 2100 \n (UN 2010 High Population Forecast)")

ggsave(file='us/unhighpopfcst.png') #to save file just printed, so change file name as appropriate

###	now run the forecast

## arima
fyear=seq(2010,2100)
yco2 <- highpop$highpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
ggplot()+
#png("unhighco2fcst.png")
geom_line(aes(x=fyear,y=yco2),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2a$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2040,y=4600,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=4350,label='red = high forecast',colour='red')+
    opts(title="U.S. Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 High Population Estimate)")

ggsave(file='us/unhighco2fcst.png') #to save file just printed, so change file name as appropriate

###	now run the comparative co2 -- do not run all three yco2 at once -- run one at a time


fyear=seq(2010,2100)
yco2h <- highpop$highpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2h <- yco2h  /1000 ###	to scale properly to million tonnes of co2

yco2m <- medpop$medpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2m <- yco2m  /1000 ###	to scale properly to million tonnes of co2

yco2 <- lowpop$lowpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2

ggplot()+
#png("uncompco2fcst.png")
geom_line(aes(x=fyear,y=yco2h),colour="red",size=2) +
geom_line(aes(x=fyear,y=yco2m),colour="yellow",size=2) +
geom_line(aes(x=fyear,y=yco2),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="U.S. Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Population Estimates)")

ggsave(file='us/uncompco2fcst.png') #to save file just printed, so change file name as appropriate

###	now do a combined pop graph for parsimony

library(stinepack)
lowpop <- read.csv("usalowpop.csv",header=T)
lowpop$lowpop <- na.stinterp(lowpop$lowpop)
medpop <- read.csv("usamedpop.csv",header=T)
medpop$medpop <- na.stinterp(medpop$medpop)
highpop <- read.csv("usahighpop.csv",header=T)
highpop$highpop <- na.stinterp(highpop$highpop)


fyear=seq(2010,2100)
ggplot()+
#png("unpopfcst.png")
geom_line(aes(x=fyear,y=lowpop$lowpop/1000),colour="green",size=2) +
geom_line(aes(x=fyear,y=medpop$medpop/1000),colour="yellow",size=2) +
geom_line(aes(x=fyear,y=highpop$highpop/1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="U.S. Population Forecast, 2010 - 2100 \n (UN 2010 Population Forecast)")

ggsave(file='us/unpopfcst.png') #to save file just printed, so change file name as appropriate


###	carbon decoupling?


p <- ggplot()
png("wco2decoup.png")
p +
geom_line(aes(x=year,y=wco2/wgdp2005),colour="blue",size=2) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous(" Tonnes of CO2 Emissions / GDP 2005 USD MER") +
opts(title="Global Carbon Intensity of Gross Domestic Product \n (A Measure of 'Decoupling')")
dev.off()

###	the elasticity calcs

elas <- lm(log(wco2) ~ log(wco2/wec) + log(wec/wgdp2005) + log(wgdp2005/pop))
summary(elas)
library(xtable)
xtable(elas)


## ########################################CHN

windows(record=T)

library(forecast)
library(ggplot2)

###	co2 = co2/ec * ec/gdp * gdp/pop * pop

###	co2 metric tonnes

###	co2 <- ts(read.csv("clipboard",header=T),start=1980)
## wec from total_primary_energy_consumption_(quadrillion_btu).xls


ex <- read.csv("extractchn.csv",header=T); attach(ex)

## set up models, both ets and arima
mwco2 <- forecast(wco2/1000000,h=100,level=50) #ets(M,A,N)
mwco2a <- forecast.Arima(auto.arima(wco2/1000000),h=100,level=50) #arima(0,2,0)
mwec <- forecast(wec/1000000000000,h=100,level=50) #ets(M,A,N)
mweca <- forecast.Arima(auto.arima(wec/1000000000000),h=100,level=50) #arima(0,2,0)
mgdp <- forecast(wgdp2005/1000000000,h=100,level=50) #ets(A,A,N)
mgdpa <- forecast.Arima(auto.arima(wgdp2005/1000000000),h=100,level=50) #arima(0,1,1) with drift
mpop <- forecast(pop/1000000,h=100,level=50)                    #ets(A,A,N)
mpopa <- forecast.Arima(auto.arima(pop/1000000),h=100,level=50) #arima(0,2,1)

ggplot(ex,aes(year,wco2))+
#png("chn/wco2.png")
geom_line(colour="blue",size=2)+
scale_y_continuous("CO2 emissions, metric tonnes") +
opts(title="China CO2 Emissions")

ggsave(file='chn/wco2.png') #to save file just printed, so change file name as appropriate

## qplot(year, wco2/1000000000, data=nco2, geom="line",log="y")

###	energy BTU

ggplot(ex,aes(year,wec))+
#png("wec.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Primay energy consumption, BTU") +
opts(title="China primary energy consumption")

ggsave(file='chn/wec.png') #to save file just printed, so change file name as appropriate


###	gdp 2000 USD

ggplot(ex,aes(year,wgdp2005))+
#png("wgdp.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("GDP, 1990 USD - Market Exchange Rates") +
opts(title="China gross domestic product")

ggsave(file='chn/wgdp.png') #to save file just printed, so change file name as appropriate

###	pop

ggplot(ex,aes(year,pop))+
#png("wpop.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Population") +
opts(title="China population")

ggsave(file='chn/wpop.png') #to save file just printed, so change file name as appropriate

### now get ratios

###	carbon intensity of energy

ggplot(ex,aes(year,(wco2/wec*1000000)))+		### wco2/1000000 BTU
#png("wco2wec.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("CO2 / Million BTU EC") +
opts(title="China carbon intensity of energy")

ggsave(file='chn/wco2wec.png') #to save file just printed, so change file name as appropriate


###	energy intensity of gdp

ggplot(ex,aes(year,(wec/wgdp2005)))+
#png("wecwgdp.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("EC / GDP") +
opts(title="China energy intensity of GDP")

ggsave(file='chn/wecwgdp.png') #to save file just printed, so change file name as appropriate

###	living standards

ggplot(ex,aes(year,(wgdp2005/pop)))+
#png("wgdpwpop.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("GDP / POP") +
opts(title="China living standards, 1990 USD - Market Exchange Rates")

ggsave(file='chn/wgdpwpop.png') #to save file just printed, so change file name as appropriate

###	energy intensity per capita

ggplot(ex,aes(year,(wec/pop)))+
#png("wecwpop.png")
geom_line(colour="blue",size=2) +
scale_y_continuous("Energy Consumption / POP") +
opts(title="China Energy Consumption per Capita")

ggsave(file='chn/wecwpop.png') #to save file just printed, so change file name as appropriate


###	now forecast co2

###	first show model fit



ggplot()+
#png("wco2mfit.png")
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=1985,y=6000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=5900,label='red = fitted',colour='red')+
opts(title="China CO2 State-Space Model Fit \n (ANN)")

ggsave(file='chn/wco2mfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wco2fcst.png")
geom_line(aes(x=fyear,y=mwco2$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwco2$mean,ymax=mwco2$upper[,1]),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwco2$mean,ymin=mwco2$lower[,1]),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2020,y=6750,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=6625,label='red = fitted',colour='red')+
    opts(title="China CO2 Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wco2fcst.png")
geom_line(aes(x=fyear,y=mwco2a$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mwco2a$mean),ymax=as.vector(mwco2a$upper[,1])),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mwco2a$mean),ymin=as.vector(mwco2a$lower[,1])),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2a$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwco2a$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
#annotate('text',x=2020,y=10250,label='blue = actual',colour='blue')+
#    annotate('text',x=2020,y=10000,label='red = fitted',colour='red')+
annotate('text',x=2020,y=125000,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=120000,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=115000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=110000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="China CO2 Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='chn/wco2fcst.png') #to save file just printed, so change file name as appropriate


###	now forecast wec

###	first show model fit


ggplot()+
#png("wecmfit.png")
geom_line(aes(x=year,y=mwec$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
annotate('text',x=1985,y=100000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=99000,label='red = fitted',colour='red')+
    opts(title="China EC State-Space Model Fit \n (ANN)")

ggsave(file='chn/wecmfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wecfcst.png")
geom_line(aes(x=fyear,y=mwec$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwec$mean+3000,ymax=mwec$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwec$mean-3000,ymin=mwec$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwec$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mwec$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
opts(title="China Energy Consumption Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wecfcst.png")
geom_line(aes(x=fyear,y=mweca$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mweca$mean),ymax=as.vector(mweca$upper)),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mweca$mean),ymin=as.vector(mweca$lower)),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mweca$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mweca$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Energy Consumption, Trillion BTU") +
annotate('text',x=2020,y=1700000,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=1600000,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=1500000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=1400000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="China Energy Consumption Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")


ggsave(file='chn/wecfcst.png') #to save file just printed, so change file name as appropriate


###	now forecast gdp

###	first show model fit

## ets
ggplot()+
#png("wgdpmfit.png")
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=1985,y=2500,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=2400,label='red = fitted',colour='red')+
    opts(title="China GDP Model Fit \n ETS (AAN)")

## arima
ggplot()+
#png("wgdpmfit.png")
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=1985,y=2500,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=2400,label='red = fitted',colour='red')+
    opts(title="China GDP Model Fit \n ARIMA (0,1,1) with drift")


ggsave(file='chn/wgdpmfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wgdpfcst.png")
geom_line(aes(x=fyear,y=mgdp$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mgdp$mean,ymax=mgdp$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mgdp$mean,ymin=mgdp$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mgdp$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mgdp$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=2030,y=32500,label='blue = actual',colour='blue')+
    annotate('text',x=2030,y=31250,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2030,y=30000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=28750,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="China Gross Domestic Product Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wgdpfcst.png")
geom_line(aes(x=fyear,y=mgdpa$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mgdpa$mean),ymax=as.vector(mgdpa$upper)),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mgdpa$mean),ymin=as.vector(mgdpa$lower)),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mgdpa$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mgdpa$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP, Billion 1990 USD MER") +
annotate('text',x=2030,y=29000,label='blue = actual',colour='blue')+
    annotate('text',x=2030,y=28000,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2030,y=27000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=26000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="China Gross Domestic Product Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='chn/wgdpfcst.png') #to save file just printed, so change file name as appropriate

###	now forecast pop

###	first show model fit

## ets
ggplot()+
#png("wpopmfit.png")
geom_line(aes(x=year,y=mpop$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mpop$fitted),colour="red",size=4,alpha=0.5) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=1985,y=1300,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=1290,label='red = fitted',colour='red')+
    opts(title="China Population Model Fit \n ETS (AAN)")

## arima
ggplot()+
#png("wpopmfit.png")
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mpopa$fitted),colour="red",size=4,alpha=0.5) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=1985,y=1300,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=1290,label='red = fitted',colour='red')+
    opts(title="China Population Model Fit \n ARIMA (0,2,1)")


ggsave(file='chn/wpopmfit.png') #to save file just printed, so change file name as appropriate


###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("wpopfcst.png")
geom_line(aes(x=fyear,y=mpop$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mpop$mean,ymax=mpop$upper),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mpop$mean,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2030,y=2400,label='blue = actual',colour='blue')+
    annotate('text',x=2030,y=2350,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2030,y=2300,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=2250,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="China Population Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("wpopfcst.png")
geom_line(aes(x=fyear,y=mpopa$mean),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mpopa$mean),ymax=as.vector(mpopa$upper)),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mpopa$mean),ymin=as.vector(mpopa$lower)),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=3) +
geom_line(aes(x=year,y=mpopa$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2030,y=2500,label='blue = actual',colour='blue')+
    annotate('text',x=2030,y=2450,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2030,y=2400,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=2350,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.3)+
    opts(title="China Population Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='chn/wpopfcst.png') #to save file just printed, so change file name as appropriate


###	now do intensive models

###	co2 intensity of energy
###	first show model fit

## ets
ggplot()+
#png("co2intmfit.png")
geom_line(aes(x=year,y=mwco2$x/mwec$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted/mwec$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2/EC BTU") +
annotate('text',x=2004,y=0.090,label='blue = actual',colour='blue')+
    annotate('text',x=2004,y=0.0895,label='red = fitted',colour='red')+
    opts(title="China Carbon Intensity of Energy Consumption Model Fit \n ETS")

## arima
ggplot()+
#png("co2intmfit.png")
geom_line(aes(x=year,y=mwco2a$x/mweca$x),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2a$fitted/mweca$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / EC BTU") +
annotate('text',x=2004,y=0.090,label='blue = actual',colour='blue')+
    annotate('text',x=2004,y=0.0895,label='red = fitted',colour='red')+
    opts(title="China Carbon Intensity of Energy Consumption Model Fit \n ARIMA")

ggsave(file='us/co2intmfit.png') #to save file just printed, so change file name as appropriate

###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("co2intfcst.png")
geom_line(aes(x=fyear,y=mwco2$mean/mwec$mean*1000000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=mwco2$mean/mwec$mean*1000000+30,ymax=mwco2$upper/mwec$upper*1000000),fill="blue",alpha=0.1) +
geom_ribbon(aes(x=fyear,ymax=mwco2$mean/mwec$mean*1000000-30,ymin=mwco2$lower/mwec$lower*1000000),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mwco2$x/mwec$x*1000000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2$fitted/mwec$fitted*1000000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / Million BTU EC") +
    annotate('text',x=2030,y=650,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=625,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=600,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2030,y=575,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.1)+
opts(title="China Carbon Intensity of Energy Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("co2intfcst.png")
geom_line(aes(x=year,y=mwco2a$x/mweca$x*1000000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwco2a$fitted/mweca$fitted*1000000),colour="red",size=1) +
geom_line(aes(x=fyear,y=mwco2a$mean/mweca$mean*1000000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=as.vector(mwco2a$mean/mweca$mean*1000000),ymax=as.vector(mwco2a$upper/mweca$upper*1000000)),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=as.vector(mwco2a$mean/mweca$mean*1000000),ymin=as.vector(mwco2a$lower/mweca$lower*1000000)),fill="blue",alpha=0.1) +
scale_x_continuous("Year") +
scale_y_continuous("Tonnes CO2 / Million BTU EC") +
annotate('text',x=2040,y=90000,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=89400,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2040,y=88800,label='red (out-of-sample) = forecast',colour='red')+
#    annotate('text',x=2040,y=59550,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.4)+
    annotate('text',x=2040, y=88200,label='error bands not useful due to extreme noise in data',colour='blue', alphap=0.4)+
    opts(title="China Carbon Intensity of Energy Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")

ggsave(file='chn/co2intfcst.png') #to save file just printed, so change file name as appropriate

###	energy intensity of gdp
###	first show model fit

## ets
ggplot()+
#png("ecintmfit.png")
geom_line(aes(x=year,y=mwec$x/mgdp$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted/mgdp$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("EC BTU / GDP 2005 USD MER") +
opts(title="China Energy Consumption Intensity of GDP Model Fit \n ETS")

## arima
ggplot()+
#png("ecintmfit.png")
geom_line(aes(x=year,y=mweca$x/mgdpa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mweca$fitted/mgdpa$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("EC BTU / GDP 1990 USD MER") +
opts(title="China Energy Consumption Intensity of GDP Model Fit \n ARIMA")


ggsave(file='chn/ecintmfit.png') #to save file just printed, so change file name as appropriate

###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("ecintmfcst.png")
geom_line(aes(x=fyear,y=mwec$mean/mgdp$mean*1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mwec$mean/mgdp$mean*1000,ymax=mwec$upper/mgdp$upper*1000),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mwec$mean/mgdp$mean*1000,ymin=mwec$lower/mgdp$lower*1000),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwec$x/mgdp$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mwec$fitted/mgdp$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("BTU EC / GDP 1990 USD MER") +
    annotate('text',x=2040,y=100000,label='blue = actual',colour='blue')+
    annotate('text',x=2040,y=97500,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2040,y=95000,label='red (out-of-sample) = forecast',colour='red')+
#    annotate('text',x=2040,y=17000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.5)+
    annotate('text',x=2040,y=92500,label='error bands not useful due to extreme noise in data',colour='blue', alphap=0.4)+
    opts(title="China Energy Intensity of Gross Domestic Product \n ETS Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("ecintmfcst.png")
geom_line(aes(x=fyear,y=mweca$mean/mgdpa$mean*1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=as.vector(mweca$mean/mgdpa$mean*1000),ymax=as.vector(mweca$upper/mgdpa$upper*1000)),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=as.vector(mweca$mean/mgdpa$mean*1000),ymin=as.vector(mweca$lower/mgdpa$lower*1000)),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mweca$x/mgdpa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mweca$fitted/mgdpa$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("BTU EC / GDP 1990 USD MER") +
#    annotate('text',x=2040,y=18500,label='blue = actual',colour='blue')+
#    annotate('text',x=2040,y=18000,label='red (in-sample) = fitted',colour='red')+
#    annotate('text',x=2040,y=17500,label='red (out-of-sample) = forecast',colour='red')+
#    annotate('text',x=2040,y=17000,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.5)+
    opts(title="China Energy Intensity of Gross Domestic Product \n ARIMA Forecast, 2009 - 2108 \n (50% Forecast Error Bands)")


ggsave(file='chn/ecintmfcst.png') #to save file just printed, so change file name as appropriate

###	gdp intensity of population
###	first show model fit

## ets
ggplot()+
#png("gdpintmfit.png")
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
annotate('text',x=1985,y=31000,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=30500,label='red = fitted',colour='red')+
    opts(title="China GDP Intensity of Population Model Fit \n ETS")

## arima
ggplot()+
#png("gdpintmfit.png")
geom_line(aes(x=year,y=mgdpa$x/mpopa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdpa$fitted/mpopa$fitted*1000),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
annotate('text',x=1985,y=1750,label='blue = actual',colour='blue')+
    annotate('text',x=1985,y=1650,label='red = fitted',colour='red')+
    opts(title="China GDP Intensity of Population Model Fit \n ARIMA")

ggsave(file='chn/gdpintmfit.png') #to save file just printed, so change file name as appropriate

###	now do forecast plot with error bands

## ets
fyear=seq(2009,2108)
ggplot()+
#png("gdpintmfcst.png")
geom_line(aes(x=fyear,y=mgdp$mean/mpop$mean*1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean*1000,ymax=mgdp$upper/mpop$upper*1000),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean*1000,ymin=mgdp$lower/mpop$lower*1000),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mgdp$x/mpop$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdp$fitted/mpop$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
opts(title="China Gross Domestic Product Intensity of Population \n Forecast, 2009 - 2108 \n ETS \n (50% Forecast Error Bands)")

## arima
fyear=seq(2009,2108)
ggplot()+
#png("gdpintmfcst.png")
geom_line(aes(x=fyear,y=mgdpa$mean/mpopa$mean*1000),colour="red",size=2) +
geom_ribbon(aes(x=fyear,ymin=as.vector(mgdpa$mean/mpopa$mean*1000),ymax=as.vector(mgdpa$upper/mpopa$upper*1000)),fill="blue",alpha=0.2) +
geom_ribbon(aes(x=fyear,ymax=as.vector(mgdpa$mean/mpopa$mean*1000),ymin=as.vector(mgdpa$lower/mpopa$lower*1000)),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mgdpa$x/mpopa$x*1000),colour="blue",size=2) +
geom_line(aes(x=year,y=mgdpa$fitted/mpopa$fitted*1000),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("GDP 1990 USD MER / Population") +
    annotate('text',x=2020,y=11000,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=10500,label='red (in-sample) = fitted',colour='red')+
    annotate('text',x=2020,y=10000,label='red (out-of-sample) = forecast',colour='red')+
    annotate('text',x=2020,y=9500,label='lt.blue (out-of-sample) = forecast error bands',colour='blue',alpha=0.4)+
    opts(title="China Gross Domestic Product Intensity of Population \n Forecast, 2009 - 2108 \n ARIMA \n (50% Forecast Error Bands)")


ggsave(file='chn/gdpintmfcst.png') #to save file just printed, so change file name as appropriate


###	forecast 100 in variable, plot

###	do ratios from base forecasts, plot

###	factor in UN pop forecast from http://esa.un.org/unpd/wpp/unpp/p2k0data.asp

###	get UN 2010 lowpop, replace NA with na.stinterp

lowpop <- read.csv("chnlowpop.csv",header=T)

library(stinepack)

lowpop$lowpop <- na.stinterp(lowpop$lowpop)

###	show un lowpop with pop forecast

fyear=seq(2010,2100)
ggplot()+
#png("unlowpopfcst.png")
geom_line(aes(x=fyear,y=lowpop$lowpop/1000),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2080,y=1350,label='blue = actual',colour='blue')+
    annotate('text',x=2080,y=1300,label='green = low forecast',colour='green')+
    opts(title="China Population Forecast, 2010 - 2100 \n (UN 2010 Low Population Forecast)")

ggsave(file='chn/unlowpopfcst.png') #to save file just printed, so change file name as appropriate

###	now run the forecast

fyear=seq(2010,2100)
yco2 <- lowpop$lowpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
ggplot()+
#png("unlowco2fcst.png")
geom_line(aes(x=fyear,y=yco2),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2000,y=21250,label='blue = actual',colour='blue')+
    annotate('text',x=2000,y=20000,label='green = low forecast',colour='green')+
    opts(title="China Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Low Population Estimate)")

ggsave(file='chn/unlowco2fcst.png') #to save file just printed, so change file name as appropriate

###	get UN 2010 medpop, replace NA

medpop <- read.csv("chnmedpop.csv",header=T)

library(stinepack)

medpop$medpop <- na.stinterp(medpop$medpop)

###	show un medpop with pop forecast

## all arima
fyear=seq(2010,2100)
ggplot()+
#png("unmedpopfcst.png")
geom_line(aes(x=fyear,y=medpop$medpop/1000),colour="yellow",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2080,y=1400,label='blue = actual',colour='blue')+
    annotate('text',x=2080,y=1350,label='yellow = medium forecast',colour='yellow')+
    opts(title="China Population Forecast, 2010 - 2100 \n (UN 2010 Medium Population Forecast)")

ggsave(file='chn/unmedpopfcst.png') #to save file just printed, so change file name as appropriate

###	now run the forecast

## arima
fyear=seq(2010,2100)
yco2 <- medpop$medpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
ggplot()+
#png("unmedco2fcst.png")
geom_line(aes(x=fyear,y=yco2),colour="yellow",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2000,y=27500,label='blue = actual',colour='blue')+
    annotate('text',x=2000,y=26250,label='yellow = medium forecast',colour='yellow')+
    opts(title="China Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Medium Population Estimate)")

ggsave(file='chn/unmedco2fcst.png') #to save file just printed, so change file name as appropriate

###	get UN 2010 highpop, replace NA

highpop <- read.csv("chnhighpop.csv",header=T)

library(stinepack)

highpop$highpop <- na.stinterp(highpop$highpop)

###	show un highpop with pop forecast

## arima
fyear=seq(2010,2100)
ggplot()+
#png("unhighpopfcst.png")
geom_line(aes(x=fyear,y=highpop$highpop/1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpopa$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
annotate('text',x=2000,y=1600,label='blue = actual',colour='blue')+
    annotate('text',x=2000,y=1550,label='red = high forecast',colour='red')+
    opts(title="China Population Forecast, 2010 - 2100 \n (UN 2010 High Population Forecast)")

ggsave(file='chn/unhighpopfcst.png') #to save file just printed, so change file name as appropriate

###	now run the forecast

## arima
fyear=seq(2010,2100)
yco2 <- highpop$highpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2
ggplot()+
#png("unhighco2fcst.png")
geom_line(aes(x=fyear,y=yco2),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2a$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
annotate('text',x=2020,y=45000,label='blue = actual',colour='blue')+
    annotate('text',x=2020,y=43000,label='red = high forecast',colour='red')+
    opts(title="China Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 High Population Estimate)")

ggsave(file='chn/unhighco2fcst.png') #to save file just printed, so change file name as appropriate

###	now run the comparative co2 -- do not run all three yco2 at once -- run one at a time


fyear=seq(2010,2100)
yco2h <- highpop$highpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2h <- yco2h  /1000 ###	to scale properly to million tonnes of co2

yco2m <- medpop$medpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2m <- yco2m  /1000 ###	to scale properly to million tonnes of co2

yco2 <- lowpop$lowpop*mgdpa$mean[2:92]/mpopa$mean[2:92]*mweca$mean[2:92]/mgdpa$mean[2:92]*mwco2a$mean[2:92]/mweca$mean[2:92]
yco2 <- yco2  /1000 ###	to scale properly to million tonnes of co2

ggplot()+
#png("uncompco2fcst.png")
geom_line(aes(x=fyear,y=yco2h),colour="red",size=2) +
geom_line(aes(x=fyear,y=yco2m),colour="yellow",size=2) +
geom_line(aes(x=fyear,y=yco2),colour="green",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mgdp$mean/mpop$mean,ymax=mgdp$upper/mpop$upper),fill="blue",alpha=0.2) +
#geom_ribbon(aes(x=fyear,ymax=mgdp$mean/mpop$mean,ymin=mgdp$lower/mpop$lower),fill="blue",alpha=0.2) +
geom_line(aes(x=year,y=mwco2$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Million Tonnes of CO2 Emissions") +
opts(title="China Carbon Dioxide Forecast, 2010 - 2100 \n (UN 2010 Population Estimates)")

ggsave(file='chn/uncompco2fcst.png') #to save file just printed, so change file name as appropriate

###	now do a combined pop graph for parsimony

library(stinepack)
lowpop <- read.csv("chnlowpop.csv",header=T)
lowpop$lowpop <- na.stinterp(lowpop$lowpop)
medpop <- read.csv("chnmedpop.csv",header=T)
medpop$medpop <- na.stinterp(medpop$medpop)
highpop <- read.csv("chnhighpop.csv",header=T)
highpop$highpop <- na.stinterp(highpop$highpop)


fyear=seq(2010,2100)
ggplot()+
#png("unpopfcst.png")
geom_line(aes(x=fyear,y=lowpop$lowpop/1000),colour="green",size=2) +
geom_line(aes(x=fyear,y=medpop$medpop/1000),colour="yellow",size=2) +
geom_line(aes(x=fyear,y=highpop$highpop/1000),colour="red",size=2) +
#geom_ribbon(aes(x=fyear,ymin=mpop$mean+30,ymax=mpop$upper),fill="blue",alpha=0.1) +
#geom_ribbon(aes(x=fyear,ymax=mpop$mean-30,ymin=mpop$lower),fill="blue",alpha=0.1) +
geom_line(aes(x=year,y=mpop$x),colour="blue",size=3) +
#geom_line(aes(x=year,y=mpop$fitted),colour="red",size=1) +
scale_x_continuous("Year") +
scale_y_continuous("Population, Millions") +
opts(title="China Population Forecast, 2010 - 2100 \n (UN 2010 Population Forecast)")

ggsave(file='chn/unpopfcst.png') #to save file just printed, so change file name as appropriate


###	carbon decoupling?


p <- ggplot()
png("wco2decoup.png")
p +
geom_line(aes(x=year,y=wco2/wgdp2005),colour="blue",size=2) +
#geom_line(aes(x=year,y=mwco2$fitted),colour="red",size=2) +
scale_x_continuous("Year") +
scale_y_continuous(" Tonnes of CO2 Emissions / GDP 2005 USD MER") +
opts(title="Global Carbon Intensity of Gross Domestic Product \n (A Measure of 'Decoupling')")
dev.off()

###	the elasticity calcs

elas <- lm(log(wco2) ~ log(wco2/wec) + log(wec/wgdp2005) + log(wgdp2005/pop))
summary(elas)
library(xtable)
xtable(elas)
