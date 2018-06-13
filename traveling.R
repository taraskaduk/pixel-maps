library(tidyverse)
library(ggmap)
library(gganimate)
library(lubridate)
library(maps)

url_cities <- 'https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv'

cities_import <- read_csv(url_cities) 
write_csv(cities_import, "cities.csv")

cities <- cities_import %>% 
  select(city = city_ascii,
         state = state_id,
         latitude = lat,
         longitude = lng)

locations <- read_csv("locations2.csv", 
                      col_types = cols(date_from = col_date(format = "%Y-%m-%d"), 
                                       date_to = col_date(format = "%Y-%m-%d")))


# existing_coords <- locations %>% 
#   filter(!(is.na(longitude) | is.na(latitude)))
# 
# missing_coords <- locations %>% 
#   filter(is.na(longitude) | is.na(latitude))
# 
# coords_merged <- missing_coords %>% 
#   select(-c(longitude, latitude)) %>% 
#   left_join(cities, by = c("city", "state"))
# 
# coords_geocodes <- coords_merged %>% 
#   filter(is.na(longitude) | is.na(latitude)) %>% 
#   select(-c(latitude, longitude)) %>% 
#   mutate(string = paste(city, state, country, sep = ", ")) %>% 
#   mutate(latitude = geocode(string) %>% .$lat,
#          longitude = geocode(string) %>% .$lon) %>% 
#   select(-string)
# 
# locations_all <- existing_coords %>% 
#   bind_rows(coords_geocodes) %>% 
#   bind_rows(coords_merged %>% filter(!(is.na(longitude) | is.na(latitude))))


locations_all <- locations

write_csv(locations_all, "locations2.csv")

locations_final <- locations_all %>% 
  mutate(lon = round(longitude*2,0)/2,
         lat = round(latitude*2,0)/2,
         year =year(date_from)) %>% 
  filter(city != "Kyiv")


# dot map -----------------------------------------------------------------

lat <- data_frame(lat = seq(-90, 90, by = 0.5))
lon <- data_frame(lon = seq(-180, 180, by = 0.5))
dots <- lat %>% 
  merge(lon, all = TRUE)

## Only include dots that are within borders. Also, exclude lakes.
map <- dots %>% 
  mutate(country = map.where('world', lon, lat),
         state = map.where("state", lon, lat),
         lakes = map.where('lakes', lon, lat)) %>% #USA doesn't include AK, HI, PR
  filter((str_detect(country, "USA") == TRUE | str_detect(country, "Puerto Rico") == TRUE) & lon < 0 & lon > -125 & is.na(lakes)) %>% 
  select(-lakes)

ggplot(map, aes(x=lon, y=lat)) + geom_point(size = .5)


p <- ggplot() + 
  geom_point(data=map, aes(x=lon, y=lat), size = 0.5, alpha = 0.2, col = "grey70") +
  geom_point(data=locations_final, aes(x=lon, y=lat, frame = date_from, cumulative = TRUE, col = as.factor(year)), size = 1, alpha = 0.3)+
  geom_point(data=locations_final, aes(x=lon, y=lat, frame = date_from), size = 5, col = 'red') +
  theme_void()

gganimate(p, interval = .5)
