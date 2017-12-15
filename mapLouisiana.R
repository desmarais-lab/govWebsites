library("dplyr")
library("ggplot2")
library("ggmap")

#load data
load("rfiles/d_coords_LA.rdata")
d <- d[!duplicated(d$City),]
d$latitude <- as.numeric(d$latitude)
d$longitude <- as.numeric(d$longitude)
d$winner[is.na(d$winner)==T] <- "NA"

## Indiana
# get the map from Google maps
states <- map_data("state")
louisiana <- subset(states, region %in% c("louisiana"))
map1 <- ggplot(data = louisiana) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "papayawhip", color = "black") + 
  coord_fixed(1.3)
map1
#overlay points on map
map2 <- map1 + 
  geom_point(data = d, aes(x = longitude, y = latitude, color = winner), size = 2) + 
  theme(legend.position = "none") + 
  scale_color_manual(values=c("blue", "gray", "red")) +
  labs(x = "Longitude", y = "Latitude")
map2

ggsave(filename = "paper/figures/louisiana_map.pdf", plot = map2, width = 5, height = 8)
