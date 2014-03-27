### allen p. 82

windows(record=T)

library(ggplot2)

onepraw <- read.csv("data/allen_coal.csv", stringsAsFactors = F)
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
                        
png("pics/oneplottime.png")


oneplottime <- ggplot(onepm, aes(Year, value, fill = region)) +
  geom_area() +
#  geom_text(data = textvals,
#            aes(label = region, y = mids), x = 1700, size = 4.5) +
  scale_fill_brewer(pal = "Paired",legend=T ) +
  opts(title = "British coal production by region, 1560 - 1800",legend.position=c(.25,.5)) +
scale_y_continuous("000 tons of coal")
print(oneplottime)
dev.off()

