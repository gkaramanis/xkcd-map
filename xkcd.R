library(tidyverse)
library(terra)
library(sf)
library(camcorder)

gg_record(dir = here::here("xkcd-map-temp"), device = "png", width = 8, height = 8, units = "in", dpi = 320)

# Get world geojson and join countries
world <- rnaturalearth::ne_countries() %>% 
  st_as_sf() %>% 
  rmapshaper::ms_dissolve()

# Convert to raster
r <- rast(world, nrow = 2000, ncol = 2000)
rr <- rasterize(vect(world), r)

# List of cities
places <- tribble(
  ~place, ~label,
  "Reykjavík", "",
  "Dublin", "",
  "London", "",
  "Paris", "",
  "Riyadh", "",
  "Cairo", "",
  "Dallas", "",
  "Warsaw", "",
  "Istanbul", "",
  "Lagos", "",
  "Kinshasa", "",
  "Luanda", "",
  "Addis Ababa", "",
  "Nairobi", "",
  "Johannesburg", "",
  "Moscow", "",
  "St. John's", "",
  "Baghdad", "",
  "Riyadh", "",
  "Tehran", "",
  "Bogotá", "",
  "Rio de Janeiro", "",
  "São Paulo", "",
  "Buenos Aires", "",
  "Lima", "",
  "Santiago", "",
  "Halifax", "",
  "Tashkent", "",
  "Karachi", "",
  "New Delhi", "",
  "Miami", "",
  "Havana", "",
  "Cancún", "",
  "Dhaka", "",
  "Mexico City", "",
  "Bangkok", "",
  "Guangzhou", "",
  "Hong Kong", "",
  "Shanghai", "",
  "Los Angeles", "",
  "Tokyo", "",
  "Beijing", "",
  "Seoul", "",
  "San Francisco", "",
  "Vladivostok", "",
  "Salt Lake City", "",
  "Chicago", "",
  "Winnipeg", "",
  "Irkutsk", "",
  "Yellowknife", "",
  "Boston", "",
  "Toronto", "",
  "New York City", "NYC",
  "Washington", "DC",
  "Mumbai", "",
  "Manila", ""
  ) %>% 
  mutate(label = if_else(label == "", place, label))

# Get cities from GeoNames 
col_names = c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "feature class", "feature code", "country code", "cc2", "admin1 code", "admin2 code", "admin3 code", "admin4 code", "population", "elevation", "dem", "timezone", "modification date")

cities5000 <- read_tsv(here::here("cities5000.txt"), col_names = col_names)

places_map <- cities5000 %>% 
  select(name, population, longitude, latitude) %>% 
  right_join(places, by = c("name" = "place")) %>% 
  group_by(name) %>% 
  filter(population == max(population)) %>% 
  ungroup() %>% 
  distinct()

# List of other places
other_places <- tribble(
  ~place, ~lat, ~long,
  "Antarctica",	-80,	90,
  "Australia",	-25.2744,	133.7751,
  "Greenland",	76.7069,	-40.6043,
  "Sahara",	22,	16,
  "Alaska",	64.2008,	-149.4937,
  "Congo-Amazon rainforest", -8, 45,
  "Quebec",	55.9399,	-65.5491,
  "Hudson Plain",	59.3366,	-78.6504,
  "Siberia",	68.5240,	108.3188,
  "Kazakhstan",	50.0196,	66.9237,
  "Madagascar",	-28.8792,	46.5079,
  "French Southern\nand Antarctic Lands",	-47.2804,	54.3486,
  "Falkland\nIslands",	-55.7963,	-59.5236,
  "Trinidad\n& Tobago",	10.6918,	-61.2225,
  "Dominican Rep.",	18.7357,	-60.1627,
  "Maldives",	-2.2028,	73.2207,
  "Andes",	-10.8895,	-68.8458,
  "Rapa Nui", -27.1212,	-109.3662,
  "Hawaii",	24.8968,	-158.5828,
  "Jakarta",	-6.2088,	110.8456,
  "Galápagos",	-0.9537,	-90.9656,
  "French\nPolynesia",	-17.6797,	-156.4068,
  "Fiji",	-20.7134,	178.0650,
  "Rocky Mountains",	48.5501,	-110.9027,
  "Tunguska\nEvent",	59.8956, 101.8933,
  "Titanic Wreck", 40.7269,	-49.9483,
  "Aleutian\nIslands",	51.9419,	-174.4421,
  "Franklin's\nVery Lost\nExpedition",	69, -97,
  "Kamchatka", 	53.0219,	150.6497,
  "St. Helena",	-15.9583, -5.7020,
  "Kara-Baffin Sea", 75, 70,
  "Central America", 13, -87,
  "Palk-Panama Canal", 10, -79
  )

f1 <- "Outfit"
f2 <- "DIN Condensed"

rr %>% 
  terra::as.data.frame(xy = TRUE) %>% 
  mutate(
    x = abs(round(x, 2))
  ) %>% 
  ggplot() +
  geom_raster(aes(x, y), fill = "white") +
  geom_point(data = places_map, aes(abs(longitude), latitude), size = 1.2) +
  ggrepel::geom_text_repel(data = places_map, aes(abs(longitude), latitude, label = label), family = f1, size = 3, segment.size = 0.15, seed = 399) +
  geom_text(data = other_places, aes(abs(long), lat, label = toupper(place)), family = f2, size = 3.5, lineheight = 0.8, color = "grey30") +
  labs(
    title = "LONGITUDE=ABS(LONGITUDE)",
    subtitle = "(https://xkcd.com/2807)",
    caption = "Sources: Natural Earth & GeoNames · Graphic: Georgios Karamanis"
  ) +
  coord_fixed(expand = FALSE) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#D8D8D8", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(2, 0, 10, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 10, 0))
  )
  
  

  
