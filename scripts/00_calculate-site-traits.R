# merge all detections, filter to carnivore 
library(tidyverse)

# load site data 
sites <- read.csv(file = "data/0_site_data.csv")

# read camera data
camera_data <- read.csv(file = "data/5_camera_data.csv") # camera check events 
site_key <- dplyr::select(camera_data, unique_id, site_id) %>% unique()

# calculate trap days
trap_days <- sum(camera_data$trap_days) 
print(trap_days)

# read in identity data - this is camera data prefiltered to carnivores. Sites without carnivores will not show up. 
carnivores <- read.csv(file = "data/camera_data/carnivores_with_site-data_daily.csv")

# group carnivore species by site and count frequency
site_species <- carnivores %>% 
  group_by(site_id, top_identity) %>% 
  summarise(n_detections = n())

# merge with camera data to get unique sites 
site_species <- left_join(site_species, camera_data, by = "site_id")
# merge to unique sites
site_species <- site_species %>% 
  group_by(unique_id, top_identity) %>% 
  summarise(n_detections = sum(n_detections))
site_species_wide <- site_species %>%
  pivot_wider(
    names_from = top_identity,
    values_from = n_detections, 
    values_fill = 0 #fill zeros
  )

# merge in camera data to site data
sites <- left_join(sites, site_species_wide, by = c("unique_site" = "unique_id")) %>%
  mutate_if(is.numeric,coalesce,0) # fill zeros

# define scat identity based on camera trap data
carnivore_cols <- c("BOBCAT", "GRAYFOX", "RACCOON", "SPOTTEDSKUNK", "STRIPEDSKUNK", 
                    "RINGTAIL", "DOMESTICCAT", "COYOTE")
sites$scat_identity <- apply(sites, 1, function(row) {
  row_vals <- as.numeric(row[carnivore_cols])
  names(row_vals) <- carnivore_cols
  
  ringtail <- row_vals["RINGTAIL"]
  raccoon <- row_vals["RACCOON"]
  grayfox <- row_vals["GRAYFOX"]
  skunks <- row_vals["SPOTTEDSKUNK"] + row_vals["STRIPEDSKUNK"]
  other_carnivores <- sum(row_vals) - ringtail
  
  if (sum(row_vals) == 0) {
    return("no carnivore")
  } else if (ringtail > 0 && raccoon > 0) {
    return("procyonid")
  } else if (ringtail > 0 && other_carnivores == skunks) {
    return("ringtail")
  } else if (ringtail > 0 && other_carnivores == 0) {
    return("ringtail")
  } else if (ringtail > 0 && grayfox > 4) {
    return("carnivore")
  } else if (ringtail == 0 && other_carnivores > 0) {
    return("other mesocarnivore")
  } else {
    return("ringtail")
  }
})

# load in scat data 
scat <- read.csv(file = "data/2_scat_data.csv") 
scat <- left_join(scat, site_key, by = "site_id")
scat_by_site <- scat %>% 
  group_by(unique_id) %>% 
  summarise(n_scat = n())

sites <- left_join(sites, scat_by_site, c("unique_site" = "unique_id")) %>%
  mutate_if(is.numeric,coalesce,0) # fill zeros

write.csv(sites, file = "data/1_site_traits.csv")

