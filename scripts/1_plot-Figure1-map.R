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
png(filename="figures/Figure1Aiii.png", width = 4, height = 4, unit = 'in', res = 300)
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
  
png(filename="figures/Figure1Aiv.png", width = 4, height = 4, unit = 'in', res = 300)
par(bg=NA)
fc_pc_plot
dev.off()

# Make map for Figure 1A background 
## set your Google Maps API key here
register_google(key = "AIzaSyAzKAajDAHybjStRcm9Ckch3E3IKrb4jmw")
register_stadiamaps(key = "bb626491-1172-4176-8659-714d7fe2d859") # this expires 8/13/2024
  
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

## plot the US map with Arizona highlighted
png(filename="figures/Figure1Ai.png", width = 4, height =3, unit = 'in', res = 300)
plot1 <- ggplot() +
  geom_polygon(data = us_contiguous , aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.5) +
  geom_polygon(data = arizona, aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA, size = 0.5) +
  geom_polygon(data=GRCA, aes(x = long, y = lat, group = group), fill ="lightgrey", size = 0.7) +
  geom_rect(aes(xmin = -112.0081, xmax = -112.2081, ymin = 36.02842, ymax = 36.22842), color = "black", fill = "black")  +
  theme_map() 
dev.off()
  
## define the center coordinates and zoom level for study area map
lon_center <- -112.1081
lat_center <- 36.12842
zoom <- 12

## get the background map from Stadia
map <- get_map(location = c(lon = lon_center, lat = lat_center), zoom = zoom, maptype = "stamen_terrain_background")

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
  lon = c(-112.138886, -112.094753,	-112.0532175)
 #  shape = c("triangle", "circle", "triangle"), 
 #  color = c("#68534D", "#D2D68D","#68534D")
)

# Create a inset map with study points plot
png(filename="figures/Figure1Aii.png", width = 4, height = 4, unit = 'in', res = 300)
plot2 <- ggmap(map) + # background map
  geom_sf(data = grand_canyon, color = "black", size = 0.5, alpha = 0.5) + # provides white overlay to mute background terrain
  geom_sf(data = bat_bc, fill ="#10A870",color ="#10A870",  size = 0.7) +# bright angel trail
  geom_point(data = data, aes(x = lon, y = lat, shape = shape, color = color), size = 4) +
  scale_color_manual(values = c("#8D7068", "#10A870","#8D7068"), guide = "none") +
  scale_shape_manual(labels = c("backcountry", "frontcountry"),
                     values = c("triangle" = 15, "circle" = 17), guide = "none") +
  labs(x = "Longitude",
       y = "Latitude", 
      # shape = "Tourist Access"
      ) +
  theme_minimal()
dev.off()

# save the figure 
png(filename="figures/Figure1_SiteMap.png", width = 4, height = 3, unit = 'in', res = 300)
ggdraw()+
  draw_plot(plot1)+
  draw_plot(plot2,height=0.6,x=0.1,y=0.3)
dev.off()
