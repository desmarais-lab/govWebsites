library("dplyr")
library("ggplot2")
library("ggmap")

#load data
load("rfiles/allURLs.rdata")
websiteUrls <- websiteUrls[!websiteUrls$State=="Alaska",]
websiteUrls <- websiteUrls[!websiteUrls$State=="Hawaii",]
websiteUrls$latitude <- as.numeric(as.character(websiteUrls$latitude))
websiteUrls$longitude <- as.numeric(as.character(websiteUrls$longitude))
websiteUrls <- websiteUrls[is.na(websiteUrls$longitude)==F,]
websiteUrls <- websiteUrls[is.na(websiteUrls$latitude)==F,]

# make a basic map of the us states
states <- map_data("state")
map1 <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "papayawhip", color = "black") + 
  coord_fixed(1.3)
map1
#overlay points on map
map2 <- map1 + 
  geom_point(data = websiteUrls, aes(x = longitude, y = latitude, color = party), size = .5) + 
  theme(legend.position = "none") + 
  scale_color_manual(values=c("blue", "red")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_void() + theme(legend.position = "none")
map2

ggsave(filename = "paper/figures/us_map.pdf", plot = map2, width = 8, height = 6)

#same map, but only the states we use
states_use <- c("Indiana", "Louisiana", "Texas", "New York", "Washington", "California")
websiteUrls <- websiteUrls[websiteUrls$State%in%states_use,]

#overlay points on map
map2 <- map1 + 
  geom_point(data = websiteUrls, aes(x = longitude, y = latitude, color = party), size = .5) + 
  theme(legend.position = "none") + 
  scale_color_manual(values=c("blue", "red")) +
  labs(x = "Longitude", y = "Latitude") +
  theme_void() + theme(legend.position = "none")
map2

ggsave(filename = "paper/figures/us_map_sample_states.pdf", plot = map2, width = 8, height = 6)
