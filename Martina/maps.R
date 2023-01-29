library(sf)
library(tidyverse)
library(tidygeocoder)
library(ggspatial)
library(viridis)

datos <- read_csv("datos-map.csv") %>% 
  mutate(
    adress = paste0(
      str_to_upper(Country),
      ",",
      str_to_upper(City))
    ) %>% 
  geocode(
    address = adress,
    method = "arcgis",
    lat = latitude,
    long = longitude
  ) %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) 


input <- datos %>% 
  pivot_longer(
    cols = gB1_n:gBoth_p,
    names_to = "variables",
    values_to = "value"
    ) %>% 
  mutate(
    value = gsub(",",".",value) %>% as.double(),
    variables = as.factor(variables)) %>% 
  st_transform(
    "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
    ylim=c(-55,80),
    xlim=c(-180,180)
  )

world <- spData::world %>% 
  st_as_sf() %>% 
  st_transform(
    "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
    ylim=c(-55,80),
    xlim=c(-180,180)
  )

ggplot() + 
  geom_sf(
    data = world,
    fill = "white",
    color = "black",
    stroke= 0.01
    ) +
  geom_sf(
    data = input,
    aes(
      fill = value,
      size = `Sample size`/450
      ),
    alpha = 0.8,
    shape = 21,
    stroke= 0.05
    ) +
  geom_point() +
  scale_size("Sample size") + 
  scale_fill_viridis("Values",option = "viridis",direction = 1)+
  facet_wrap(~variables,ncol = 2) +
  theme_minimal()

ggsave(
  filename = "final_map.png",
  height = 10,
  width = 5  
)