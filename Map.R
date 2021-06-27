# Libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(ggrepel)
# Get the world polygon and extract UK
library(maps)

library(viridis)

# UK <- map_data("world") %>% filter(region=="UK")
Taiwan <- map_data("world") %>% filter(region=="Taiwan")

data <- read_excel("Geo.xlsx", sheet="Sheet1",col_names = T)
# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
# data <- world.cities %>% filter(country.etc=="UK")
# data <- world.cities %>% filter(country.etc=="Taiwan") %>% arrange(desc(pop)) %>% head(10)
# data <- add_row(data,name="Taipei",country.etc="Taiwan",pop=111110000,lat=25.02,long=121.45)
# data <- data %>% select(pop,long,lat)

# Left chart
# ggplot() +
#   geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
#   geom_point( data=data, aes(x=long, y=lat)) +
#   theme_void() + ylim(50,59) + coord_map() 
p<-ggplot() +
  geom_polygon(data = Taiwan, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data %>% arrange(desc(N)), aes(size=N, color=N, x=long, y=lat)) +
  geom_text_repel( data=data, aes(x=long, y=lat, label=University), size=5) +
  scale_color_viridis(trans="log") +
  labs(title="The number of new international students") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # theme_void() +
  coord_map() 
p
ggsave(file="geo.png",plot=p)
# Second graphic with names of the 10 biggest cities
library(ggrepel)
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")
