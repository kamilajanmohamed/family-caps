#make a map for capped states
library(ggplot2)
library(maps)
library(mapdata)
library(tidyverse)
library(numform)
library(ggrepel)

state <- map_data("state")
state <- state %>%
  mutate(yearCapped = as.integer(case_when(
    region %in% c("new jersey") ~ 1992,
    region %in% c("arkansas", "georgia") ~ 1994,
    region %in% c("arizona", "delaware", "indiana", "massachusetts", "mississippi", "nebraska", "virginia") ~ 1995,
    region %in% c("connecticut", "florida", "illinois", "maryland", "north carolina", "tennessee", "wisconsin") ~ 1996,
    region %in% c("california", "idaho", "oklahoma", "south carolina", "wyoming") ~ 1997,
    region %in% c("north dakota") ~ 1998,
    region %in% c("minnesota") ~ 2003)),
    region = f_title(region))

#add state abbreviations
centroids <- data.frame(region=tolower(state.name), long=state.center$x, lat=state.center$y)
centroids$abb<-state.abb[match(centroids$region,tolower(state.name))]
centroids <- centroids %>%
  filter(abb != "DC" & abb != "AK" & abb != "HI")

map <- ggplot() + 
  geom_polygon(data=state, aes(x=long, y=lat, fill=yearCapped, group=group), color = "black") + 
  #guides(fill=FALSE) + 
  scale_fill_gradient(limits = c(1992, 2003), low = "orange", high = "steelblue", na.value = "white", space = "Lab", breaks = c(1992, 1994, 1996, 1998, 2000, 2002)) +
  geom_segment(data = centroids[centroids$abb == "MD",], aes(x = long, y = lat, xend= long+2, yend = lat-2)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "MD"]+2.5, y = centroids$lat[centroids$abb == "MD"]-2.5, label = "MD", size = 2.5) +
  geom_segment(data = centroids[centroids$abb == "DE",], aes(x = long, y = lat, xend= long+2, yend = lat)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "DE"]+2.5, y = centroids$lat[centroids$abb == "DE"], label = "DE", size = 2.5) +
  geom_segment(data = centroids[centroids$abb == "NJ",], aes(x = long, y = lat, xend= long+2, yend = lat)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "NJ"]+2.5, y = centroids$lat[centroids$abb == "NJ"], label = "NJ", size = 2.5) +
  geom_segment(data = centroids[centroids$abb == "CT",], aes(x = long, y = lat, xend= long+2, yend = lat-2)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "CT"]+2.5, y = centroids$lat[centroids$abb == "CT"]-2.5, label = "CT", size = 2.5) +
  geom_segment(data = centroids[centroids$abb == "MA",], aes(x = long, y = lat, xend= long+2, yend = lat)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "MA"]+2.5, y = centroids$lat[centroids$abb == "MA"], label = "MA", size = 2.5) +
  geom_segment(data = centroids[centroids$abb == "NH",], aes(x = long, y = lat, xend= long+2, yend = lat)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "NH"]+2.5, y = centroids$lat[centroids$abb == "NH"], label = "NH", size = 2.5) +
  geom_segment(data = centroids[centroids$abb == "VT",], aes(x = long, y = lat, xend= long, yend = lat+2)) +
  annotate(geom = "text", x = centroids$long[centroids$abb == "VT"], y = centroids$lat[centroids$abb == "VT"]+2.5, label = "VT", size = 2.5) +
  labs(fill = "Year capped") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.9, 0.5)) +
  with(centroids[centroids$abb != "MD" & centroids$abb != "DE" & centroids$abb != "NJ" & centroids$abb != "CT" & centroids$abb != "MA" & centroids$abb != "NH" & centroids$abb != "VT",], 
       annotate(geom="text", x = long, y=lat, label = abb, 
                size = 2.5)
  )

ggsave("Output/Figures/capMap.png", map, height = 6, width = 9)
