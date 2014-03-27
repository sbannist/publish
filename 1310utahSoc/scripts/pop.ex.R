## ######################################## to reform maddison data for pop tables and graphs.

## libraries

library(zoo)
require(stinepack)
require(reshape)
require(ggplot2)

## read data

pop <- read.csv('pop.ex.csv',header=TRUE,stringsAsFactors=FALSE)
nrow(pop)                               #196 rows from CE 1 through CE 2030, angus maddison

## head(format(as.Date(as.character(pop$ce),format='%Y')),format='%Y')
head(as.Date(as.character(pop$ce),format='%Y',origin=as.Date('0001-06-15')))
## now need to fix date for error in glibc on this machine
## first calculate offset based on today

offset <- as.Date('0001-06-30') - as.Date(as.character(pop$ce[1]),format='%Y') #note dependency on starting at ce 1
head(as.Date(as.character(pop$ce),format='%Y')+ offset) #to check if we got it right...maddison data is centered on mid-year per his documentation
tail(as.Date(as.character(pop$ce),format='%Y')+ offset) #so it works at least today. next set up zoo object

## zoo object. note that pop is in thousands, per maddison original

pop.data <- pop[,-1]                    #strip maddison's year column
head(pop.data)                          #check
pop.index <- as.Date(as.character(pop$ce),format='%Y')+ offset
head(pop.index)                         #check
pop.z <- zoo(pop.data,pop.index)        #set up df
head(pop.z)                             #check. note the annoying first colname due to beginning with numeric. but ok.

## ######################################## need to melt data with rownames being id.var

df.pop.z <- data.frame(date=rownames(as.data.frame(pop.z)),pop.z,stringsAsFactors=FALSE) #reset every time with this

## do for total pop
df.pop.z.y <- df.pop.z[df.pop.z$date=='0001-06-30',c(1,8)]
for(i in c('1000-06-30','1500-06-30','1600-06-30','1700-06-30','1820-06-30','1870-06-30','1900-06-30')){
    df.pop.z.y <- rbind(df.pop.z.y,df.pop.z[df.pop.z$date==i,c(1,8)])
}

## now figure out how to plot this
p <- ggplot(df.pop.z.y,aes(x=date,y=world.total))+
    geom_bar(aes(stat='identity',fill=df.pop.z.y$world.total),position='dodge')+
#    scale_fill_continuous(name='Year Pop.')+#,
#                      breaks=levels(as.factor(df.pop.z.y$date)),
#                      labels=c('CE 1','1000','1500','1600','1700','1820','1870','1900')+
    scale_x_discrete(name='Maddison Benchmark Years',
                     breaks=df.pop.z.y$date,
                     labels=c('CE 1','1000','1500','1600','1700','1820','1870','1900'))+
    scale_y_continuous(name='Population, in Thousands',formatter='comma')+
    opts(title=paste('World Population by Selected Year'),legend.position='none')
print(p)
ggsave(file='../images/g.world.pop.bar.png') #to save file just printed, so change file name as appropriate

## do this for each year.

df.pop.z <- data.frame(date=rownames(as.data.frame(pop.z)),pop.z,stringsAsFactors=FALSE) #reset every time with this

df.pop.z <- df.pop.z[df.pop.z$date=='0001-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1000-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1500-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1600-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1700-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1820-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1870-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)
df.pop.z <- df.pop.z[df.pop.z$date=='1900-06-30',];pyear <- substr(df.pop.z[1,1],start=1,stop=4)

df.pop.z[,2] <- df.pop.z[,2] - df.pop.z[,3]
df.pop.z <- df.pop.z[1,c(1,2,3,4,5,6,7,8)] #strips world.total which I may nead for pie charts, so add back 8
row <- df.pop.z[,2]+df.pop.z[,3]+df.pop.z[,4]+df.pop.z[,5]+df.pop.z[,6]+df.pop.z[,7]
df.pop.z[,8] <- df.pop.z[,8] - row
head(df.pop.z)
head(melt(df.pop.z,id.vars=c('date')))
df.pop.z.m <- melt(df.pop.z,id.vars=c('date')) #this is the one that works for interim
head(df.pop.z.m)

## ######################################## now ready to do graphs


## this does it with ggplot

value <- df.pop.z.m$value
p <- ggplot(df.pop.z.m,aes(df.pop.z.m$date,value))+
    geom_bar(aes(stat='identity',fill=df.pop.z.m$variable),position='dodge')+
    scale_fill_brewer(pal = "Accent",name='Country/Region',
                      breaks=df.pop.z.m$variable,
                      labels=c('EU-11','UK','USA','China','India','Japan','ROW'))+
    scale_x_discrete(name='Selected Countries or Regions')+
    scale_y_continuous(name='Population, in Thousands')+
    opts(title=paste('CE Year',pyear,'Population, Selected Countries or Regions'))
print(p)

## lets try pie charts...stacked bar charts with polar coordinates

datap<-(p<-cumsum(df.pop.z.m$value)-diff(c(0,cumsum(df.pop.z.m$value)))*(1-0.5))
p <- ggplot(df.pop.z.m,aes(x=factor(1))) + geom_bar(aes(fill = variable,weight=value))+
    ## p <- ggplot(df.pop.z.m,aes(x=factor(1))) + geom_bar(aes(fill = variable,weight=value), position = 'fill')+
    geom_text(aes(x= 1.15, y=datap,label=paste(round(100*value/sum(value),1),'%')),vjust=1.0,hjust=0.5)+
#    geom_text(aes(x= 1.15, y=datap,label=paste(df.pop.z.m$value,'\n',round(100*value/sum(value),1),'%')),vjust=0,hjust=0)+
#    geom_text(aes(x= 1.15, y=datap,angle=-datap*360,label=paste(round(100*value/sum(value),1),'%')),vjust=0,hjust=0)+
    coord_polar(theta = 'y')+
    scale_x_discrete(name='')+
    scale_y_continuous(name='')+
    scale_fill_brewer(pal = "Accent",name='Country/Region',
                  breaks=df.pop.z.m$variable,
                  labels=c('EU-11','UK','USA','China','India','Japan','ROW'))+
    opts(title=paste('Population Shares, Selected Countries/Regions,',pyear))
print(p)

ggsave(file='../images/g1700poppie.png') #to save file just printed, so change file name as appropriate

value <- df.pop.z.y$world.total
p <- ggplot(df.pop.z.y,aes(df.pop.z.y$date,value))+
#p <- ggplot(df.pop.z.y,aes(factor(value)))+
    geom_bar(aes(stat='identity',fill=df.pop.z.y$world.total,width=1))#+
#    geom_bar(aes(width=1))#+
#    coord_polar(theta = 'y')
print(p)

## to experiment with for pie charts
## ggplot(dat, aes(x = fruit)) + geom_bar(aes(fill = variable), position = 'fill')

x=factor(df.pop.z.y$date)
ggplot(df.pop.z.y,aes(x=df.pop.z.y$date)) + geom_histogram(aes(fill = world.total), position = 'fill')

## great to get colour sets

RColorBrewer::display.brewer.all(n=8, exact.n=FALSE)
