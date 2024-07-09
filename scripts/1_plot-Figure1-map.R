# make a map 
library(ggmap)
library(ggplot2)
library(sf)
library(rgdal)
library(maps)
library(mapdata)

`%notin%` <- function(x, y) {
  !(x %in% y)
}

# Set your Google Maps API key here
register_google(key = "AIzaSyAzKAajDAHybjStRcm9Ckch3E3IKrb4jmw")

# Define the data
data <- data.frame(
  lat = c(36.057702, 36.10588,36.200674),
  lon = c(-112.1405119, -112.094753,	-112.0532175),
  shape = c("triangle", "circle", "triangle")
)

# Get state and country map data
states <- map_data("state")
world <- map_data("world")

# Filter the state data for Arizona
arizona <- subset(states, region == "arizona")

# Filter the world data for the contigous US
us <- subset(world, region == "USA")
us_contiguous <- subset(us, subregion %notin% c("Alaska", "Hawaii")) 

# Load the shapefile for Grand Canyon National Park
grand_canyon <- st_read("data/grca_tracts/GRCA_boundary.shp")

GRCA <- readOGR("data/grca_tracts/GRCA_boundary.shp")
# Convert the shapefile to the same projection as Google Maps
grand_canyon <- st_transform(grand_canyon, crs = st_crs("+proj=longlat +datum=WGS84"))

# Plot the US map with Arizona highlighted
ggplot() +
  geom_polygon(data = us_contiguous , aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.5) +
  geom_polygon(data = arizona, aes(x = long, y = lat, group = group), fill = "lightgrey", color = "black", size = 0.5) +
  geom_polygon(data=GRCA, aes(x = long, y = lat, group = group), fill ="darkred", size = 0.7) +
  theme_classic()


# Define the center coordinates and zoom level
lon_center <- -112.1081
lat_center <- 36.12842
zoom <- 12

# Get the map
map <- get_map(location = c(lon = lon_center, lat = lat_center), zoom = zoom, maptype = "terrain")

# Load the shapefile for Grand Canyon National Park
grand_canyon <- st_read("data/grca_tracts/GRCA_boundary.shp")

# Convert the shapefile to the same projection as Google Maps
grand_canyon <- st_transform(grand_canyon, crs = st_crs("+proj=longlat +datum=WGS84"))

# Create a ggmap plot
ggmap(map) +
  geom_sf(data = grand_canyon, color = "black", size = 0.5, alpha = 0.5) +
  geom_rect(aes(xmin = lon - 0.1, xmax = lon + 0.1, ymin = lat - 0.1, ymax = lat + 0.1),
            fill = "red", alpha = 0.5) +
  labs(title = "Grand Canyon National Park with Box",
       x = "Longitude",
       y = "Latitude")


# Get the map
map <- get_map(location = c(lon = mean(data$lon), lat = mean(data$lat)), 
               zoom = 12, 
               style = c(feature = "all", element = "labels", visibility = "off"),
               maptype ="terrain")

# Create the plot
ggmap(map) +
  geom_point(data = data, aes(x = lon, y = lat, shape = shape), size = 5) +
  scale_shape_manual(labels = c("Wilderness", "Vehicle-Access"),
                     values = c("triangle" = 15, "circle" = 17)) +
  labs(x = "Longitude",
       y = "Latitude", 
       shape = "Tourist Access") +
  theme_minimal()


## USING ggspatial 
+
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
