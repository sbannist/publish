#woodPrice
#wood = read.csv('../data/woodPrice.csv')

filepath <- c('../data/')
imagepath <- c('../images/')

wood <- read.csv(paste(filepath,'woodPrice.csv',sep=''),header=TRUE,stringsAsFactors=FALSE)

require(ggplot2)

ggplot(data=wood)+
    geom_line(aes(x=midPoint,y=price,colour='blue'),size=1)+
#    geom_point(aes(x=midPoint,y=price),shape=2,colour='blue',size=4)+
    geom_line(aes(x=midPoint,y=wood,colour='red'),size=1)+
#    geom_point(aes(x=midPoint,y=wood),shape=8,colour='red',size=4)+
    scale_colour_manual(name='Price index',values=c('blue','red'),	
#				  breaks=c('price','wood'),
				  labels=c('General prices','Wood prices'),
				   guide = guide_legend(reverse=TRUE))+
#    scale_shape_manual(values=c(2,8))+
#    scale_shape_identity()+	
    geom_point(aes(x=midPoint,y=price),colour='blue',shape=2,size=4)+
#    geom_point(aes(shape=factor(price)))+
#    scale_shape(solid=FALSE)+		
    geom_point(aes(x=midPoint,y=wood),colour='red',shape=8,size=4)+
    theme(legend.position=c(0.1,0.9),legend.background = element_rect(fill="gray90"))+
    labs(title='English general and wood price indices, 1451 - 1702,\n 1451 - 1500 = 100',
	y='Price indices, 1451-1500 = 100',x='Year')

ggsave(file=paste(imagepath,'woodPrice.png',sep='')) #to save file just printed, so change file name as appropriate

ggplot(data=wood)+
#    geom_line(aes(x=midPoint,y=price,colour='blue'),size=1)+
    geom_point(aes(x=midPoint,y=price,colour='blue'),shape=2,size=4)+
#    geom_line(aes(x=midPoint,y=wood,colour='red'),size=1)+
    geom_point(aes(x=midPoint,y=wood,colour='red'),shape=8,size=4)+
    scale_colour_manual(name='Price index',values=c('blue','red'),	
#				  breaks=c('price','wood'),
				  labels=c('General prices','Wood prices'),
				   guide = guide_legend(reverse=TRUE))+
#    scale_shape_manual(values=c('blue','red'))+
    theme(legend.position=c(0.1,0.9),legend.background = element_rect(fill="gray90"))+
    labs(title='English general and wood price indices, 1451 - 1702,\n 1451 - 1500 = 100',
	y='Price indices',x='Year')

