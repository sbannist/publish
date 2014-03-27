###	allen_energy

windows(record=T)

library(ggplot2)
library(plyr)
library(reshape)

allen <- read.csv("data/allen_energy.csv",header=T,stringsAsFactors = F)
allen$region <- factor(allen$region, levels = allen$region)

## Some cleanup of years needed

allen_head <- as.character(colnames(allen))
allen_head <- sapply(strsplit(allen_head, split = "X"), function(x)
               as.character(x[2]))
allen_head[1] <- "region"

colnames(allen) <- allen_head

allenm <- as.matrix(allen[,-1])

barplot(allenm[,-1], beside=T,col=topo.colors(nrow(allenm)))

###	western UK coal,london coal,  beijing cheap, canton cheap
barplot(allenm[c(4,1,23,24),-1],
	beside=T,col=rainbow(4),
	legend.text=allen[c(4,1,23,24),1],
	args.legend = list(x = "topleft"),
	ylab="grams of silver per million BTU, deflated",
	main="Real silver cost of energy")

## ######################################## use allen df, select out the four countries 4/18/12

require(ggplot2)

df.allen.sub <- allen[c(4,1,23,24),]    #Western UK, coal; London, coal;Beijing;Canton
df.allen.sub$region <- as.character(df.allen.sub$region)

## fix up region names to tag with wood energy source

df.allen.sub[3,1] <- 'Beijing, wood'
df.allen.sub[4,1] <- 'Canton, wood'

## fix up data - remove 1400 column (no data)
df.allen.sub <- df.allen.sub[,-2]
df.allen.sub.m <- melt(df.allen.sub,id.vars=c('region'))
colnames(df.allen.sub.m) <- c('Region','variable','value')

## need to fix up factors on region

df.allen.sub.m$Region <- as.factor(df.allen.sub.m$Region)

ggplot(df.allen.sub.m, aes(x=df.allen.sub.m$Region, fill=Region,y=df.allen.sub.m$value)) +
    geom_bar(aes(df.allen.sub.m$Region),stat='identity',position='dodge') +
    facet_grid(. ~ variable) +
    scale_y_continuous(name='grams of silver per million BTU, deflated')+
    scale_x_discrete(name='Countries/Regions',breaks='',labels='')+
    opts(title='Real silver cost of energy', legend.position=c(0.22,0.65))

ggsave(file='C:/Documents and Settings/Russell/Desktop/VAT/Projects/Research/Conferences/1204CSBS/images/gworldenergycost.png')

## ######################################## world wages

filepath <- c('../data/')

df.allen.wage <- read.csv(paste(filepath,'allen_wages1.csv',sep=""),header=TRUE,stringsAsFactors=FALSE) #with cities across columns

df.allen.wage.m <- melt(df.allen.wage,id.vars='Year')
colnames(df.allen.wage.m) <- c('Year','City','value')

ggplot(df.allen.wage.m, aes(x=Year,y=value,group=City)) +
    geom_line(aes(colour=City),size=2.5)+ #for presentation
#    scale_fill_grey(start=0.0,end=0.95)+ #for publication
    ggtitle('World Daily Wage \n grams of silver')+
    theme(legend.position=c(0.20,0.69),legend.background=element_rect(fill="transparent"))+
    ylab('grams of silver per day')

filename <- c('gworldwages.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))
