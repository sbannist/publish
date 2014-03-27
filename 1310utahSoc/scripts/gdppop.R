## todo gdp bar chart like pop, gdp/cap bar chart like pop
## todo faceted bar plot like http://had.co.nz/ggplot2/geom_bar.html, using:
## todo ggplot(diamonds, aes(cut, fill=cut)) + geom_bar() +  facet_grid(. ~ clarity) form
## todo ggplot(gdpcapita, aes(variable, fill=variable)) + geom_bar() +  facet_grid(. ~ date) form
## ######################################## to reform maddison data for capita gdp tables and graphs.

## libraries

library(zoo)
require(stinepack)
require(reshape)
require(ggplot2)

## read data

filepath <- c('../data/')

gdp <- read.csv(paste(filepath,'gdp.ex.csv',sep=""),header=TRUE,stringsAsFactors=FALSE)
nrow(gdp)                               #194 rows from CE 1 through CE 2030, angus maddison
pop <- read.csv(paste(filepath,'pop.ex.csv',sep=""),header=TRUE,stringsAsFactors=FALSE)
nrow(pop)                               #196 rows -- need to drop last 2 rows which are 2009 and 2030 fcst
pop <- pop[-c(195,196),];nrow(pop)      #194 rows


offset <- as.Date('0001-06-30') - as.Date(as.character(gdp$ce[1]),format='%Y') #note dependency on starting at ce 1

## zoo object. note that gdp is in thousands, per maddison original

gdp.data <- gdp[,-1];pop.data <- pop[,-1]                    #strip maddison's year column
gdp.index <- as.Date(as.character(gdp$ce),format='%Y')+ offset
pop.index <- as.Date(as.character(pop$ce),format='%Y')+ offset
gdp.z <- zoo(gdp.data,gdp.index)        #set up df
pop.z <- zoo(pop.data,pop.index)        #set up df

## fix up eu12 totals and row totals before do divsion

gdp.z[,1] <- gdp.z[,1] - gdp.z[,2];pop.z[,1] <- pop.z[,1] - pop.z[,2] #splits out UK from eu12 oops, do before division
gdpsub <- gdp.z[,-7];popsub <- pop.z[,-7] #because of rowSums behavior,need strict subset
gdp.z$row <- gdp.z$world.total - rowSums(gdpsub);pop.z$row <- rowSums(popsub) #to create and calc row column
gdp.z <- gdp.z[,-7];pop.z <- pop.z[,-7] #strip world.total so easier to plot

## ######################################## should create new df at some point...here?
## note that gdp is in 000000 1990 GK USD, pop is 000, so gdp/pop is 000000/000=000, so mult by 1000

gdpcapita <- (gdp.z/pop.z*1000);nrow(gdpcapita)                  #so in 1990 GK USD per capita. this is a zoo object


## ######################################## need to melt data with rownames being id.var

df.gdpcapita.z <- data.frame(date=rownames(as.data.frame(gdpcapita)),gdpcapita,stringsAsFactors=FALSE) #reset every time with this

## do for total gdp
df.gdpcapita.z.y <- df.gdpcapita.z[df.gdpcapita.z$date=='0001-06-30',c(1,8)]
for(i in c('1000-06-30','1500-06-30','1600-06-30','1700-06-30','1820-06-30','1870-06-30','1900-06-30')){
    df.gdpcapita.z.y <- rbind(df.gdpcapita.z.y,df.gdpcapita.z[df.gdpcapita.z$date==i,c(1,8)])
}

## now figure out how to plot this
p <- ggplot(df.gdpcapita.z.y,aes(x=date,y=world.total))+
    geom_bar(aes(stat='identity',fill=df.gdpcapita.z.y$world.total),position='dodge')+
#    scale_fill_continuous(name='Year gdpcapita.')+#,
#                      breaks=levels(as.factor(df.gdpcapita.z.y$date)),
#                      labels=c('CE 1','1000','1500','1600','1700','1820','1870','1900')+
    scale_x_discrete(name='Maddison Benchmark Years',
                     breaks=df.gdpcapita.z.y$date,
                     labels=c('CE 1','1000','1500','1600','1700','1820','1870','1900'))+
    scale_y_continuous(name='GDP/Capita, in 1990 Geary-Khamis USD',formatter='comma')+
    opts(title=paste('World GDP per Capita by Selected Year'),legend.position='none')
print(p)

ggsave(file='../images/gworldgdpcapitabar.png') #to save file just printed, so change file name as appropriate

## do this for each year.

df.gdpcapita.z <- data.frame(date=rownames(as.data.frame(gdpcapita)),gdpcapita,stringsAsFactors=FALSE) #reset every time with this

## need to rbind the right dates

df.gdpcapita.z.sub <- df.gdpcapita.z[df.gdpcapita.z$date=='0001-06-30',];pyear <- substr(df.gdpcapita.z[1,1],start=1,stop=4)
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1000-06-30',])
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1500-06-30',])
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1600-06-30',])
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1700-06-30',])
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1820-06-30',])
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1870-06-30',])
df.gdpcapita.z.sub <- rbind(df.gdpcapita.z.sub,df.gdpcapita.z[df.gdpcapita.z$date=='1900-06-30',])

colnames(df.gdpcapita.z.sub) <- c('date','EU-11','UK','USA','China','India','Japan','ROW')

head(melt(df.gdpcapita.z,id.vars=c('date')))
df.gdpcapita.z.m <- melt(df.gdpcapita.z.sub,id.vars=c('date')) #this is the one that works for interim
#z <- colnames(df.gdpcapita.z.m)
#z[2] <- 'Country/Region'
#colnames(df.gdpcapita.z.m) <- z
head(df.gdpcapita.z.m)

## ######################################## now ready to do graphs

vname <- c('EU-11','UK','USA','China','India','Japan','ROW')

ggplot(df.gdpcapita.z.m, aes(variable, fill=variable,y=value)) +
    geom_bar(stat='identity',position='dodge') +
    facet_grid(. ~ date) +
    ggtitle("GDP per Capita, Selected Countries/Regions \n C.E. 0 - 1900")+
    ylab('GDP per Capita, 1990 Geary-Khamis USD')+
    scale_x_discrete(name='Countries/Regions',breaks='',labels='')+
    labs(fill="Region or Country")+
    theme(legend.position=c(0.18,0.725),legend.background=element_rect(fill="transparent"))

filename <- c('ggdpcapitadodge.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))

ggsave(file='../images/ggdpcapitadodge.png') #to save file just printed, so change file name as appropriate

## this does it with ggplot

value <- df.gdpcapita.z.m$value
p <- ggplot(df.gdpcapita.z.m,aes(df.gdpcapita.z.m$date,value))+
    geom_bar(aes(stat='identity',fill=df.gdpcapita.z.m$variable),position='dodge')+
    scale_fill_brewer(pal = "Accent",name='Country/Region',
                      breaks=df.gdpcapita.z.m$variable,
                      labels=c('EU-11','UK','USA','China','India','Japan','ROW'))+
    scale_x_discrete(name='Selected Countries or Regions')+
    scale_y_continuous(name='GDP, in Thousands')+
    opts(title=paste('CE Year',pyear,'GDP, Selected Countries or Regions'))
print(p)


## lets try pie charts...stacked bar charts with polar coordinates

datap<-(p<-cumsum(df.gdpcapita.z.m$value)-diff(c(0,cumsum(df.gdpcapita.z.m$value)))*(1-0.5))
p <- ggplot(df.gdpcapita.z.m,aes(x=factor(1))) + geom_bar(aes(fill = variable,weight=value))+
    ## p <- ggplot(df.gdpcapita.z.m,aes(x=factor(1))) + geom_bar(aes(fill = variable,weight=value), position = 'fill')+
#    geom_text(aes(x= 1.15, y=datap,angle=-datap*360,label=paste(round(100*value/sum(value),1),'%')),vjust=0)+
    geom_text(aes(x= 1.15, y=datap,label=paste(round(100*value/sum(value),1),'%')),vjust=0)+
    coord_polar(theta = 'y')+
    scale_x_discrete(name='')+
    scale_y_continuous(name='')+
    scale_fill_brewer(pal = "Accent",name='Country/Region',
                  breaks=df.gdpcapita.z.m$variable,
                  labels=c('EU-11','UK','USA','China','India','Japan','ROW'))+
    opts(title=paste('GDP Shares, Selected Countries/Regions,',pyear))
print(p)

ggsave(file='../images/g1900gdppie.png') #to save file just printed, so change file name as appropriate

value <- df.gdpcapita.z.y$world.total
p <- ggplot(df.gdpcapita.z.y,aes(df.gdpcapita.z.y$date,value))+
#p <- ggplot(df.gdpcapita.z.y,aes(factor(value)))+
    geom_bar(aes(stat='identity',fill=df.gdpcapita.z.y$world.total,width=1))#+
#    geom_bar(aes(width=1))#+
#    coord_polar(theta = 'y')
print(p)

## to experiment with for pie charts
## ggplot(dat, aes(x = fruit)) + geom_bar(aes(fill = variable), position = 'fill')

x=factor(df.gdpcapita.z.y$date)
ggplot(df.gdpcapita.z.y,aes(x=df.gdpcapita.z.y$date)) + geom_histogram(aes(fill = world.total), position = 'fill')

## great to get colour sets

RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)
