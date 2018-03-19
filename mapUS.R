library("dplyr")
library("ggplot2")
library("ggmap")

#load data
load("rfiles/allURLs.rdata")
websiteUrls <- websiteUrls[!websiteUrls$State=="Alaska",]
websiteUrls <- websiteUrls[!websiteUrls$State=="Hawaii",]

## Louisiana
# get the map from Google maps
states <- map_data("state")
map1 <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "papayawhip", color = "black") + 
  coord_fixed(1.3)
map1
#overlay points on map
map2 <- map1 + 
  geom_point(data = websiteUrls, aes(x = -longitude, y = latitude, color = party), size = .5) + 
  theme(legend.position = "none") + 
  scale_color_manual(values=c("blue", "red")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_void() + theme(legend.position = "none")
map2

ggsave(filename = "paper/figures/us_map.pdf", plot = map2, width = 8, height = 6)
