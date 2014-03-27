## ######################################## to reform maddison data for gdp tables and graphs.

## libraries

library(zoo)
require(stinepack)
require(reshape)
require(ggplot2)

## read data

gdp <- read.csv('gdp.ex.csv',header=TRUE,stringsAsFactors=FALSE)
nrow(gdp)                               #196 rows from CE 1 through CE 2030, angus maddison

## head(format(as.Date(as.character(gdp$ce),format='%Y')),format='%Y')
head(as.Date(as.character(gdp$ce),format='%Y',origin=as.Date('0001-06-15')))
## now need to fix date for error in glibc on this machine
## first calculate offset based on today

offset <- as.Date('0001-06-30') - as.Date(as.character(gdp$ce[1]),format='%Y') #note dependency on starting at ce 1
head(as.Date(as.character(gdp$ce),format='%Y')+ offset) #to check if we got it right...maddison data is centered on mid-year per his documentation
tail(as.Date(as.character(gdp$ce),format='%Y')+ offset) #so it works at least today. next set up zoo object

## zoo object. note that gdp is in thousands, per maddison original

gdp.data <- gdp[,-1]                    #strip maddison's year column
head(gdp.data)                          #check
gdp.index <- as.Date(as.character(gdp$ce),format='%Y')+ offset
head(gdp.index)                         #check
gdp.z <- zoo(gdp.data,gdp.index)        #set up df
head(gdp.z)                             #check. note the annoying first colname due to beginning with numeric. but ok.

## ######################################## need to melt data with rownames being id.var

df.gdp.z <- data.frame(date=rownames(as.data.frame(gdp.z)),gdp.z,stringsAsFactors=FALSE) #reset every time with this

## do for total gdp
df.gdp.z.y <- df.gdp.z[df.gdp.z$date=='0001-06-30',c(1,8)]
for(i in c('1000-06-30','1500-06-30','1600-06-30','1700-06-30','1820-06-30','1870-06-30','1900-06-30')){
    df.gdp.z.y <- rbind(df.gdp.z.y,df.gdp.z[df.gdp.z$date==i,c(1,8)])
}

## now figure out how to plot this
p <- ggplot(df.gdp.z.y,aes(x=date,y=world.total))+
    geom_bar(aes(stat='identity',fill=df.gdp.z.y$world.total),position='dodge')+
#    scale_fill_continuous(name='Year gdp.')+#,
#                      breaks=levels(as.factor(df.gdp.z.y$date)),
#                      labels=c('CE 1','1000','1500','1600','1700','1820','1870','1900')+
    scale_x_discrete(name='Maddison Benchmark Years',
                     breaks=df.gdp.z.y$date,
                     labels=c('CE 1','1000','1500','1600','1700','1820','1870','1900'))+
    scale_y_continuous(name='1990 Geary-Khamis USD, in Millions',formatter='comma')+
    opts(title=paste('World GDP by Selected Year'),legend.position='none')
print(p)

ggsave(file='../images/gworldgdpbar.png') #to save file just printed, so change file name as appropriate

## do this for each year.

df.gdp.z <- data.frame(date=rownames(as.data.frame(gdp.z)),gdp.z,stringsAsFactors=FALSE) #reset every time with this

df.gdp.z <- df.gdp.z[df.gdp.z$date=='0001-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1000-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1500-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1600-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1700-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1820-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1870-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)
df.gdp.z <- df.gdp.z[df.gdp.z$date=='1900-06-30',];pyear <- substr(df.gdp.z[1,1],start=1,stop=4)

df.gdp.z[,2] <- df.gdp.z[,2] - df.gdp.z[,3]
df.gdp.z <- df.gdp.z[1,c(1,2,3,4,5,6,7,8)] #strips world.total which I may nead for pie charts, so add back 8
row <- df.gdp.z[,2]+df.gdp.z[,3]+df.gdp.z[,4]+df.gdp.z[,5]+df.gdp.z[,6]+df.gdp.z[,7]
df.gdp.z[,8] <- df.gdp.z[,8] - row
head(df.gdp.z)
head(melt(df.gdp.z,id.vars=c('date')))
df.gdp.z.m <- melt(df.gdp.z,id.vars=c('date')) #this is the one that works for interim
head(df.gdp.z.m)

## ######################################## now ready to do graphs


## this does it with ggplot

value <- df.gdp.z.m$value
p <- ggplot(df.gdp.z.m,aes(df.gdp.z.m$date,value))+
    geom_bar(aes(stat='identity',fill=df.gdp.z.m$variable),position='dodge')+
    scale_fill_brewer(pal = "Accent",name='Country/Region',
                      breaks=df.gdp.z.m$variable,
                      labels=c('EU-11','UK','USA','China','India','Japan','ROW'))+
    scale_x_discrete(name='Selected Countries or Regions')+
    scale_y_continuous(name='GDP, in Thousands')+
    opts(title=paste('CE Year',pyear,'GDP, Selected Countries or Regions'))
print(p)

## lets try pie charts...stacked bar charts with polar coordinates

datap<-(p<-cumsum(df.gdp.z.m$value)-diff(c(0,cumsum(df.gdp.z.m$value)))*(1-0.5))
p <- ggplot(df.gdp.z.m,aes(x=factor(1))) + geom_bar(aes(fill = variable,weight=value))+
    ## p <- ggplot(df.gdp.z.m,aes(x=factor(1))) + geom_bar(aes(fill = variable,weight=value), position = 'fill')+
#    geom_text(aes(x= 1.15, y=datap,angle=-datap*360,label=paste(round(100*value/sum(value),1),'%')),vjust=0)+
    geom_text(aes(x= 1.15, y=datap,label=paste(round(100*value/sum(value),1),'%')),vjust=0)+
    coord_polar(theta = 'y')+
    scale_x_discrete(name='')+
    scale_y_continuous(name='')+
    scale_fill_brewer(pal = "Accent",name='Country/Region',
                  breaks=df.gdp.z.m$variable,
                  labels=c('EU-11','UK','USA','China','India','Japan','ROW'))+
    opts(title=paste('GDP Shares, Selected Countries/Regions,',pyear))
print(p)

ggsave(file='../images/g1900gdppie.png') #to save file just printed, so change file name as appropriate

value <- df.gdp.z.y$world.total
p <- ggplot(df.gdp.z.y,aes(df.gdp.z.y$date,value))+
#p <- ggplot(df.gdp.z.y,aes(factor(value)))+
    geom_bar(aes(stat='identity',fill=df.gdp.z.y$world.total,width=1))#+
#    geom_bar(aes(width=1))#+
#    coord_polar(theta = 'y')
print(p)

## to experiment with for pie charts
## ggplot(dat, aes(x = fruit)) + geom_bar(aes(fill = variable), position = 'fill')

x=factor(df.gdp.z.y$date)
ggplot(df.gdp.z.y,aes(x=df.gdp.z.y$date)) + geom_histogram(aes(fill = world.total), position = 'fill')

## great to get colour sets

RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)
