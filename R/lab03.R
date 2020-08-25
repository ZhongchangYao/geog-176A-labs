########################
#Author: Zhongchang Yao
#Project: Lab 03
#Date: August 20, 2020
########################

library(tidyverse)
library(sf)
library(units)

#R already know the variable state.region/state.name
region = data.frame(region = state.region,
                    state_name = state.name)

south = USAboundaries::us_states()%>%
  right_join(region, by = "state_name")%>%
  filter(region == "South")

plot(south)

#dollar sign to access the column
plot(south$geometry)

plot(south$aland)

plot(south['aland'])
plot(south['awater'])

cities = readr::read_csv("data/uscities.csv")%>%
  st_as_sf(coords = c("lng","lat"), crs = 4326)%>%
  st_filter(south, .predicate = st_intersects)


plot(south$geometry)
plot(cities$geometry, add = TRUE, pch = 16, cex = .1)


south_c = st_combine(south)%>%
  st_cast("MULTILINESTRING")

south_c = st_transform(south_c, 5070)
cities = st_transform(cities, 5070)

x = st_distance(cities, south_c)

x = cities %>%
  mutate(dist_state = st_distance(cities, south_c))

ggplot() +
  geom_sf(data = south_c)+
  geom_sf(data = cities, aes(col = dist_to_state))





