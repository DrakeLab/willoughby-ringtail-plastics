# Load libraries 
library(ggmap) #for map making 
library(tidyverse) # data wrangling and plotting
library(sf)
library(rgdal) # needed do download and install from zip as no longer on CRAN
library(maps)
library(mapdata)
library(ggthemes) # for removing unnecessary grid lines 
library(cowplot) # plotting inset 

# custom functions
`%notin%` <- function(x, y) {
  !(x %in% y)
}

# make pie charts for Figure 1A inset 
## read in data
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv(file = "data/2_scat_data.csv") # scat segment data
scat <- left_join(scat, site, by = "site_id")

## remove scats not confirmed as ringtail 
scat <- scat %>% 
  dplyr::filter(ringtail_confirmed == "yes")

## group by site type 
plastic_site <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(plastic_positive = sum(as.numeric(plastic)),
            plastic_negative  = sum(plastic == 0, na.rm=TRUE)) 

## transform for plotting purposes 
plastic_site <- plastic_site %>% 
  pivot_longer(cols = starts_with("plastic_"), 
               names_to = "plastic_status", 
               values_to = "scat_samples")

## rename for legend 
plastic_site$plastic_status <- str_sub(plastic_site$plastic_status,9,16)

# filter for separate plots 
bc_piechart <- plastic_site %>% dplyr::filter(tourism_level == "backcountry")
bc_piechart$percent <- paste(round(bc_piechart$scat_samples/sum(bc_piechart$scat_samples)*100, 0), "%", sep="")
fc_piechart <- plastic_site %>% dplyr::filter(tourism_level == "frontcountry")
fc_piechart$percent <- paste(round(fc_piechart$scat_samples/sum(fc_piechart$scat_samples)*100, 0), "%", sep="") 

## create backcountry plot object
bc_pc_plot <- ggplot(bc_piechart, aes(x="", y=scat_samples, fill=plastic_status)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(y = scat_samples/2 +  0.3+  c(0, cumsum(scat_samples)[-length(scat_samples)]), label = percent), size=8) + 
  scale_fill_manual(values=c("#10A870", "#F5A6E6")) +
  labs(title="backcountry") + 
  theme_void() +  # remove background, grid, numeric labels
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        legend.position="none")  

## save the plot as png
png(filename="figures/fig1/Figure1Aiii.png", width = 4, height = 4, unit = 'in', res = 300)
par(bg=NA)
bc_pc_plot
dev.off()

## create frontcountry plot object
fc_pc_plot <- ggplot(fc_piechart, aes(x="", y=scat_samples, fill=plastic_status)) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(y = scat_samples/2 + 0.3 + c(0, cumsum(scat_samples)[-length(scat_samples)]), label = percent), size=8) + 
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#8D7068", "#F5A6E6")) + 
  labs(title="frontcountry") + 
  theme_void() + # remove background, grid, numeric labels
  theme(plot.title = element_text(hjust = 0.5, face="bold"), 
        legend.position="none")  
  
png(filename="figures/fig1/Figure1Aiv.png", width = 4, height = 4, unit = 'in', res = 300)
par(bg=NA)
fc_pc_plot
dev.off()

# Make map for Figure 1A background - Completed the background 

## get state and country map data from ggplot
states <- map_data("state")
world <- map_data("world")

## filter the state data for Arizona
arizona <- subset(states, region == "arizona")

## filter the world data for the contigous US
us <- subset(world, region == "USA")
us_contiguous <- subset(us, subregion %notin% c("Alaska", "Hawaii")) 

## load the shapefile for Grand Canyon National Park
grand_canyon <- st_read("data/grca_tracts/GRCA_boundary.shp")
GRCA <- readOGR("data/grca_tracts/GRCA_boundary.shp")

## convert the shapefile to the same projection as Google Maps
grand_canyon <- st_transform(grand_canyon, crs = st_crs("+proj=longlat +datum=WGS84"))

## create background admin plot object 
plot1 <- ggplot() +
  geom_polygon(data = us_contiguous , aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.5) +
  geom_polygon(data = arizona, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA, size = 0.5) +
  geom_polygon(data=GRCA, aes(x = long, y = lat, group = group), fill ="lightgrey", size = 0.7) +
  geom_rect(aes(xmin = -112.0081, xmax = -112.2081, ymin = 36.02842, ymax = 36.22842), color = "black", fill = "black")  +
  theme_map() 
## plot and save the US map with Arizona highlighted
png(filename="figures/fig1/Figure1Ai.png", width = 4, height =3, unit = 'in', res = 300)
plot1
dev.off()

## QUERIED STAMEN Maps on 8/9/24 
## set your Google Maps API key here
## register_google(key = "AIzaSyAzKAajDAHybjStRcm9Ckch3E3IKrb4jmw")
## register_stadiamaps(key = "da6d10ef-d122-4fdd-a9f7-8037e18ffd7a") # this expires 8/13/2024
## get the background map from Stadia with center coordinates and zoom level for study area map
## define the center coordinates and zoom level for study area map
#### old centroid value
#### lon_center <- -112.1081
#### lat_center <- 36.12842
#### zoom <- 12

## map <- get_stadiamap(bbox = c(-112.187920,36.039938,-112.027588,36.228212), zoom =zoom, maptype = "stamen_terrain_background", source = "stadia")
## save(map, file = "data/4_background_map.RData")
load(file = "data/4_background_map.RData") # this loads the above background "map"

## load the shapefile for Bright Angel Trail
NPS_trails <- st_read("data/grca_tracts/GRCA_TRANS_Trail_ln/GRCA_TRANS_Trail_ln.shp")
bat <- subset(NPS_trails, TrailName == "Bright Angel")

# Convert the shapefile to the same projection as Google Maps
bat  <- st_transform(bat, crs = st_crs("+proj=longlat +datum=WGS84"))
bat  <- st_zm(bat)

# remove trail before 1.5 mile house (36°03'37.5"N 112°08'21.4"W)
bat_bc <- bat 
bat_bc$geometry[[1]][[1]]  <- bat_bc$geometry[[1]][[1]][c(455:1991),c(1:2)]
 
### define the data for study sites 
data <- data.frame(
  lat = c(36.053524, 36.10588, 36.200674),
  lon = c(-112.138886, -112.094753,	-112.0532175),
 shape = c("circle", "triangle", "circle"), 
 color = c("#8D7068", "#10A870","#8D7068")
)

# Create a inset map with study points plot
plot2 <- ggmap(map) + # background map
  geom_sf(data = grand_canyon, color = "black", size = 0.5, alpha = 0.5, inherit.aes = FALSE) + # provides white overlay to mute background terrain
  #scale_x_discrete(breaks=c(-112.16,-112.12,-112.08, -112.04), labels=c("-112.16°W","-112.12°W","-112.08°W", "-112.04°W")) +
  geom_sf(data = bat_bc, fill ="#10A870",color ="#10A870",  size = 0.7, inherit.aes = FALSE) +# bright angel trail
  geom_point(data = data, aes(x = lon, y = lat, shape = shape, color = color), size = 4) +
  scale_color_manual(values = c( "#10A870","#8D7068","#8D7068"), guide = "none") +
  scale_shape_manual(labels = c("backcountry", "frontcountry"),
                     values = c("triangle" = 15, "circle" = 17), guide = "none") +
  labs(x = "Longitude",
       y = "Latitude", 
       # shape = "Tourist Access"
  ) +
  theme_minimal()

png(filename="figures/Figure1Aii.png", width = 4, height = 4, unit = 'in', res = 300)
plot2
dev.off()

# save the figure combined
# png(filename="figures/Figure1_SiteMap.png", width = 4, height = 3, unit = 'in', res = 300)
# ggdraw()+
#   draw_plot(plot1)+
#   draw_plot(plot2,height=0.6,x=0.1,y=0.3)
# dev.off()

