### allen p. 82

windows(record=T)

require(ggplot2)
require(reshape)

filepath <- c('../data/')

onepraw <- read.csv(paste(filepath,"allen_coal.csv",sep=""), stringsAsFactors = FALSE)
onep <- onepraw
#onep$region <- factor(onep$region, rev(levels = onep$region))
onep$region <- factor(onep$region, levels = onep$region)
onepm <- melt(onep, id.vars = "region", variable_name = "Year")

## Some cleanup of years needed
onepm$Year <- as.character(onepm$Year)
onepm$Year <- sapply(strsplit(onepm$Year, split = "X"), function(x)
               as.numeric(x[2]))

## Generate dataframe and placement for text labels in plot
textvals <- onep[c("region", "X1700")]
mids <- c(0, cumsum(textvals$X1700)) ### original doesn't work
##mids <- c(cumsum(textvals$X1700))
textvals$mids <- (mids[-1] + mids[-length(mids)]) / 2 - 0.3
##textvals$mids <-  mids[-length(mids)] / 2 - 0.3
textvals$region <- as.character(textvals$region)
#textvals$region <- c("Executives, managers, supervisors\n(non-finance)",
#                         "Medical workers",
#                        "Finance professions,\nincluding management",
#                         "Lawyers",
#                         "Tech")

ggplot(onepm, aes(Year, value, fill = region)) +
    geom_area() +
#  geom_text(data = textvals,
#            aes(label = region, y = mids), x = 1700, size = 4.5) +
    scale_fill_brewer(type="seq", palette  = "Paired") +
    ggtitle("British coal production by region, 1560 - 1800") +
    ylab("000 tons of coal")+
    guides(fill=guide_legend(reverse=TRUE))+
    theme(legend.position=c(0.14,0.66),legend.background=element_rect(fill="transparent"))+
    labs(fill='Region')

filename <- c('allen_coal.png')

## for presentation with colour
image.path <- c('../images/')
ggsave(file=paste(image.path,filename,sep=""))

## for publication, essentially enhance theme_bw
image.path <- c('../images_pub/')
ggsave(file=paste(image.path,filename,sep=""))
