# library(maps)
# library(maptools)

library(XML)
library(tidyverse)

# power outages in maine

cmp.url <- "http://www3.cmpco.com/OutageReports/CMP.html"
# get outage table (first one on the cmp.url page)
cmp.node <- getNodeSet(htmlParse(cmp.url),"//table")[[1]]
cmp.tab <- readHTMLTable(cmp.node,
                         header=c("subregion","total.customers","without.power"),
                         skip.rows=c(1,2,3),
                         trim=TRUE, stringsAsFactors=FALSE)
# clean up the table to it's easier to work with
cmp.tab <- cmp.tab[-nrow(cmp.tab),] # get rid of last row
cmp.tab$subregion <- tolower(cmp.tab$subregion)

cmp.tab$total.customers <- as.numeric(gsub(",","",cmp.tab$total.customers))
cmp.tab$without.power <- as.numeric(gsub(",","",cmp.tab$without.power))
# get maine map with counties
county.df <- map_data('county')
me <- subset(county.df, region=="maine")
# get a copy with just the affected counties
out <- subset(me, subregion %in% cmp.tab$subregion)
# add outage into to it
out <- left_join(out, cmp.tab)
# plot the map
gg <- ggplot(me, aes(long, lat, group=group))
gg <- gg + geom_polygon(fill=NA, colour='gray50', size=0.25)
gg <- gg + geom_polygon(data=out, aes(long, lat, group=group, fill=without.power),
                        colour='gray50', size=0.25)
gg <- gg + scale_fill_gradient2(low="#FFFFCC", mid="#FD8D3C", high="#800026")
gg <- gg + coord_map()
gg <- gg + theme_bw()
gg <- gg + labs(x="", y="", title="CMP (Maine) Customers Without Power by County")
gg <- gg + theme(panel.border = element_blank(),
                 panel.background = element_blank(),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position="left",
                 legend.title=element_blank())
gg











