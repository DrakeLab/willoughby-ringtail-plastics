# make a map 
library(ggmap)
library(ggplot2)
library(sf)
library(rgdal) # needed do download and install from zip as no longer on CRAN
library(maps)
library(mapdata)
library(ggthemes) # for removing unnecessary grid lines 
library(cowplot) # plotting inset 

`%notin%` <- function(x, y) {
  !(x %in% y)
}

# Set your Google Maps API key here
register_google(key = "AIzaSyAzKAajDAHybjStRcm9Ckch3E3IKrb4jmw")

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
plot1 <- ggplot() +
  geom_polygon(data = us_contiguous , aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.5) +
  geom_polygon(data = arizona, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA, size = 0.5) +
  geom_polygon(data=GRCA, aes(x = long, y = lat, group = group), fill ="lightgrey", size = 0.7) +
  geom_rect(aes(xmin = -112.0081, xmax = -112.2081, ymin = 36.02842, ymax = 36.22842), color = "black", fill = "black")  +
  theme_map() 
  
# Define the center coordinates and zoom level for study area map
lon_center <- -112.1081
lat_center <- 36.12842
zoom <- 12

# Get the map
map <- get_map(location = c(lon = lon_center, lat = lat_center), zoom = zoom, maptype = "stamen_terrain_background")

# Load the shapefile for Bright Angel Trail
NPS_trails <- st_read("data/grca_tracts/GRCA_TRANS_Trail_ln/GRCA_TRANS_Trail_ln.shp")
bat <- subset(NPS_trails, TrailName == "Bright Angel")


# Convert the shapefile to the same projection as Google Maps
bat  <- st_transform(bat, crs = st_crs("+proj=longlat +datum=WGS84"))
bat  <- st_zm(bat)
 
# load data of study point 
# Define the data 
data <- data.frame(
  lat = c(36.053524, 36.10588, 36.200674),
  lon = c(-112.138886, -112.094753,	-112.0532175),
 #  shape = c("triangle", "circle", "triangle"), 
 #  color = c("#68534D", "#D2D68D","#68534D")
)

# Create a inset map with study points plot
plot2 <- ggmap(map) + # background map
  geom_sf(data = grand_canyon, color = "black", size = 0.5, alpha = 0.5) + # provides white overlay to mute background terrain
  geom_sf(data=bat, fill ="darkgreen",color ="darkgreen",  size = 0.7) +# bright angel trail
  geom_point(data = data, aes(x = lon, y = lat, shape = shape, color = color), size = 4) +
  scale_color_manual(values = c("#68534D", "darkgreen","#68534D"), guide = "none") +
  scale_shape_manual(labels = c("backcountry", "frontcountry"),
                     values = c("triangle" = 15, "circle" = 17)) +
  labs(x = "Longitude",
       y = "Latitude", 
       shape = "Tourist Access") +
  theme_minimal()

ggdraw()+
  draw_plot(plot1)+
  draw_plot(plot2,height=0.6,x=0.1,y=0.3)
