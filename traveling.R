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
         year =year(date_from),
         month = floor_date(date_from, unit = "month")) %>% 
  filter(city != "Kyiv") %>% 
  mutate(dday = day(date_from),
         dmonth = as.factor(month(date_from)),
         dyear = as.factor(year(date_from)))

ggplot(locations_final, aes(x=month)) + geom_dotplot(dotsize = 0.5)

locations_final %>% 
  group_by(dyear, dmonth) %>% 
  count(.drop = FALSE) %>% 
ggplot(aes(x=dmonth, y=dyear, fill = as.factor(n))) + 
  geom_tile(col = "black") +
  theme_minimal() +
  coord_fixed()+
  scale_fill_brewer(type = "seq")

dates <- tibble(date = seq(as.Date('2012-08-01'),
                           as.Date(Sys.Date()),
                           by = 'months')) %>% 
  mutate(year = year(date),
         home = if_else(date < "2014-09-01" | date > "2016-04-01", "Jacksonville", "Seattle"),
         lon = if_else(home == 'Jacksonville', -81.5, -122.5),
         lat = if_else(home == 'Jacksonville', 30.5, 47.5))

dates_year <- dates %>% 
  select(-date) %>% 
  distinct()

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

theme <- theme_void() +
  theme(panel.background = element_rect(fill="grey95", color = NULL),
        plot.background = element_rect(fill="grey90", color = NULL),
        plot.title=element_text(face="bold", colour="grey10",size=26, hjust = 0.5),
        plot.subtitle=element_text(colour="grey20",size=12),
        plot.caption = element_text(colour="grey30",size=14))

p_year <- ggplot() + 
  geom_point(data=map, aes(x=lon, y=lat), size = 2, col = "grey80") +
  geom_point(data=dates_year,aes(x=lon, y=lat, frame = year, cumulative = TRUE), size=2, col = "black") +
  geom_point(data=dates_year,aes(x=lon, y=lat, frame = year), size=20, col = "blue", alpha = 0.4) +
  geom_point(data=locations_final, aes(x=lon, y=lat, frame = year, cumulative = TRUE), col = "black", size = 2, alpha = 0.4)+
  geom_point(data=locations_final, aes(x=lon, y=lat, frame = year), size = 12, col = 'red', alpha=0.3) +
  theme +
  labs(caption = "My traveling around the United States (+Canada) since first arriving to the country")+
  coord_equal()

gganimate(p_year, interval = 3, "output_year.gif", ani.width = 1000, ani.height = 560)

p_month <- ggplot() + 
  geom_point(data=map, aes(x=lon, y=lat), size = 2, col = "grey80") +
  geom_point(data=dates,aes(x=lon, y=lat, frame = date, cumulative = TRUE), size=2, col = "black") +
  geom_point(data=dates,aes(x=lon, y=lat, frame = date), size=20, col = "blue", alpha = 0.4) +
  geom_point(data=locations_final, aes(x=lon, y=lat, frame = month, cumulative = TRUE), col = "black", size = 2, alpha = 0.4)+
  geom_point(data=locations_final, aes(x=lon, y=lat, frame = month), size = 12, col = 'red', alpha=0.3) +
  theme +
  coord_equal()

gganimate(p_month, interval = 0.2, "output_month.gif", ani.width = 1000, ani.height = 560)

