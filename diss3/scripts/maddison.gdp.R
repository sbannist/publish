### Maddison gdp data for area chart
## do in percents, remember to take out individual countries from regions
## want pop in proportions, want total world gdp, pop

windows(record=T)

library(ggplot2)
library(plyr)
library(reshape)

## gdp area chart in proportions

#filepath <- c('../data/')
filepath <- c ( 'C:/Users/Steve/Documents/GitHub/publish/1310utahSoc/data/' )

onepraw <- read.csv(paste(filepath,"maddison.reg.gdp.pct.csv",sep=""), stringsAsFactors = F)
onep <- t(onepraw[c(1:12,70),])                      #transpose in prep for melt, also just pick of rows
rownames(onep) <- NULL                  #wipe out row names for clarity
onep <- onep[,-1]                       #get rid off unneded column
onep <- onep[-13,]                      #get rid of total row
onep[1,1] <- 'region'                   #label first column
colnames(onep) <- onep[1,]              #clean up names
onep <- onep[-1,]                       #clean up
onep <- as.data.frame(onep,stringsAsFactors=FALSE) #convert to df

onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "year")
onepm$year <- as.integer(levels(onepm$year))[onepm$year]
onepm$value <- as.numeric(levels(onepm$value))[onepm$value]

p4 <-                                   #for multiplot

ggplot(onepm, aes(x=year,y=value,fill=region )) +
    geom_area() +
    scale_fill_brewer(type="seq",palette="Paired")+ ## for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle("Gross Domestic Product Proportions, C.E. 0 - 2008")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.20,0.48),legend.background=element_rect(fill="transparent"))+
    labs(fill="Region or Country")+
    ylab("GDP Proportions")

filename <- c('maddisonreggdppct.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))

## gdp area chart in levels

#filepath <- c('../data/')
filepath <- c ( 'C:/Users/Steve/Documents/GitHub/publish/1310utahSoc/data/' )

onepraw <- read.csv(paste(filepath,"maddison.reg.gdp.csv",sep=""), stringsAsFactors = F)
onep <- t(onepraw[c(1:12,70),])                      #transpose in prep for melt, also just pick of rows
rownames(onep) <- NULL                  #wipe out row names for clarity
onep <- onep[,-1]                       #get rid off unneeded column
onep <- onep[-13,]                      #get rid of total row
onep[1,1] <- 'region'                   #label first column
colnames(onep) <- onep[1,]              #clean up names
onep <- onep[-1,]                       #clean up
onep <- onep[,-c(2:3)]                  #get rid of 1 and 1000
onep <- as.data.frame(onep,stringsAsFactors=FALSE) #convert to df

onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "year")
onepm$year <- as.integer(levels(onepm$year))[onepm$year]
onepm$value <- as.numeric(levels(onepm$value))[onepm$value]

ggplot(onepm, aes(x=year,y=value/1000000,fill=region )) +
    geom_area() +
    scale_fill_brewer(type="seq",palette="Paired")+ ## for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle("GDP Levels, C.E. 1500 - 2008")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.15,0.7),legend.background=element_rect(fill="transparent"))+
    labs(fill="Region or Country")+
    ylab("GDP, Trillion 1990 GK $")

filename <- c('maddisonreggdplevels.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))

## gdp area chart in levels through 1900

filepath <- c('../data/')

onepraw <- read.csv(paste(filepath,"maddison.reg.gdp.csv",sep=""), stringsAsFactors = F)
onep <- t(onepraw[c(1:12,70),])                      #transpose in prep for melt, also just pick of rows
rownames(onep) <- NULL                  #wipe out row names for clarity
onep <- onep[,-1]                       #get rid off unneeded column
onep <- onep[-13,]                      #get rid of total row
onep[1,1] <- 'region'                   #label first column
colnames(onep) <- onep[1,]              #clean up names
onep <- onep[-1,]                       #clean up
onep <- onep[,-c(2:3,10:12)]                  #get rid of 1 and 1000 and after 1900
onep <- as.data.frame(onep,stringsAsFactors=FALSE) #convert to df

onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "year")
onepm$year <- as.integer(levels(onepm$year))[onepm$year]
onepm$value <- as.numeric(levels(onepm$value))[onepm$value]

p2 <-                                   #for multiplot

ggplot(onepm, aes(x=year,y=value/1000000,fill=region )) +
    geom_area() +
    scale_fill_brewer(type="seq",palette="Paired")+ ## for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle("GDP Levels, C.E. 1500 - 1900")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.15,0.7),legend.background=element_rect(fill="transparent"))+
    labs(fill="Region or Country")+
    ylab("GDP, Trillion 1990 GK $")

filename <- c('maddisonreggdplevels1900.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))

## population area chart in proportions

filepath <- c('../data/')

onepraw <- read.csv(paste(filepath,"maddison.reg.pop.pct.csv",sep=""), stringsAsFactors = F)
onep <- t(onepraw[c(1:13),])                      #transpose in prep for melt, also just pick of rows
rownames(onep) <- NULL                  #wipe out row names for clarity
onep <- onep[,-1]                       #get rid off unneded column
onep <- onep[-13,]                      #get rid of total row
onep[1,1] <- 'region'                   #label first column
colnames(onep) <- onep[1,]              #clean up names
onep <- onep[-1,]                       #clean up
onep <- as.data.frame(onep,stringsAsFactors=FALSE) #convert to df

onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "year")
onepm$year <- as.integer(levels(onepm$year))[onepm$year]
onepm$value <- as.numeric(levels(onepm$value))[onepm$value]

ggplot(onepm, aes(x=year,y=value,fill=region )) +
    geom_area() +
    scale_fill_brewer(type="seq",palette="Paired")+ ## for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle("Population Proportions, C.E. 0 - 2008")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.20,0.48),legend.background=element_rect(fill="transparent"))+
    labs(fill="Region or Country")+
    ylab("Population Proportions")

filename <- c('maddisonregpoppct.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))

## population area chart in levels

filepath <- c('../data/')

onepraw <- read.csv(paste(filepath,"maddison.reg.pop.csv",sep=""), stringsAsFactors = F)
onep <- t(onepraw[c(1:13),])                      #transpose in prep for melt, also just pick of rows
rownames(onep) <- NULL                  #wipe out row names for clarity
onep <- onep[,-1]                       #get rid off unneeded column
onep <- onep[-13,]                      #get rid of total row
onep[1,1] <- 'region'                   #label first column
colnames(onep) <- onep[1,]              #clean up names
onep <- onep[-1,]                       #clean up
onep <- onep[,-c(2:3)]                  #get rid of 1 and 1000
onep <- as.data.frame(onep,stringsAsFactors=FALSE) #convert to df

onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "year")
onepm$year <- as.integer(levels(onepm$year))[onepm$year]
onepm$value <- as.numeric(levels(onepm$value))[onepm$value]

p3 <-                                   #for multiplot

ggplot(onepm, aes(x=year,y=value/1000000,fill=region )) +
    geom_area() +
    scale_fill_brewer(type="seq",palette="Paired")+ ## for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle("Population Levels, C.E. 1500 - 2008")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.15,0.7),legend.background=element_rect(fill="transparent"))+
    labs(fill="Region or Country")+
    ylab("Population, Billions")

filename <- c('maddisonregpoplevels.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))


## population area chart in levels through 1900

filepath <- c('../data/')

onepraw <- read.csv(paste(filepath,"maddison.reg.pop.csv",sep=""), stringsAsFactors = F)
onep <- t(onepraw[c(1:13),])                      #transpose in prep for melt, also just pick of rows
rownames(onep) <- NULL                  #wipe out row names for clarity
onep <- onep[,-1]                       #get rid off unneeded column
onep <- onep[-13,]                      #get rid of total row
onep[1,1] <- 'region'                   #label first column
colnames(onep) <- onep[1,]              #clean up names
onep <- onep[-1,]                       #clean up
onep <- onep[,-c(2:3,10:12)]                  #get rid of 1 and 1000, after 1900
onep <- as.data.frame(onep,stringsAsFactors=FALSE) #convert to df

onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "year")
onepm$year <- as.integer(levels(onepm$year))[onepm$year]
onepm$value <- as.numeric(levels(onepm$value))[onepm$value]

p1 <-                                   #for multiplot

ggplot(onepm, aes(x=year,y=value/1000000,fill=region )) +
    geom_area() +
    scale_fill_brewer(type="seq",palette="Paired")+ ## for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle("Population Levels, C.E. 1500 - 1900")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.15,0.7),legend.background=element_rect(fill="transparent"))+
    labs(fill="Region or Country")+
    ylab("Population, Billions")

filename <- c('maddisonregpoplevels1900.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))

## multiplot
source('multiplot.R')

multiplot(p1, p2, p3, p4, cols=2)


## experimental theme updates for publications 12/28/12 from http://mcfromnz.wordpress.com/2012/11/06/ggplot-graphs-in-publications/  need to make into callable function

    tsize <- 9; tsize2 <- 8; pchsize <- 1; lkey <- 3.5 # lsize <- 0.3

    theme_set(theme_bw())
    theme_update(
    plot.margin = unit(c(0.4,0.5,0.1,0), "lines"),
    panel.margin = unit(0.25, "lines"),
    panel.grid.minor = theme_line(colour = NA),
    panel.grid.major = theme_line(colour = NA),
    panel.background=theme_rect(fill = NA, colour = "black"),
    panel.border = theme_rect(colour="black", size=0),
    plot.title = theme_text(size = tsize, vjust = 0.5, hjust=0),
    axis.title.x = theme_text(size = tsize, vjust = 0.35),
    axis.title.y = theme_text(size = tsize, hjust = 0.5, vjust = 0.4, angle = 90),
    axis.text.x = theme_text(size = tsize2),
    axis.text.y = theme_text(size = tsize2),
    axis.ticks.margin=unit(0.5, units="mm"),
    axis.ticks.length=unit(1, units="mm"),
    #axis.line = theme_segment(colour = "black", size = 1),
    legend.key=theme_rect(colour = NA),
    legend.title=theme_text(size = tsize2-1, hjust = 0),
    legend.text=theme_text(size = tsize2-1, hjust = 0),
    legend.background=theme_rect(colour=NA, size=0),
    legend.margin = unit(0, "mm"),
    legend.key.size=unit(lkey,"mm"),
    legend.key.width = unit(lkey*1.5, "mm"),
    strip.background = theme_rect(fill = NA, linetype=NULL, size=0, colour="white"),
    strip.text.x = theme_text(size=tsize, vjust=0.7, hjust= 0.5)
    )
