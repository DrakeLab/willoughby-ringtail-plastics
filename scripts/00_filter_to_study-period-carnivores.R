# filter main data to cces and carnivores

# load libraries
library(tidyverse) # data wrangling
library(magrittr) # piping

# load data HOURLY
detections <- read.csv(file = "data/camera_data/GRCA_filtered_60min.csv")
detections <- detections[,c(1:20)]
cam_data <- read.csv(file = "data/5_camera_data.csv")

# filter to only cce of interest 
detections <- detections %>% dplyr::filter(cce_id %in% cam_data$check_event)

# filter to carnivore detections 
carnivores <- detections %>% 
  dplyr::filter(top_identity %in% c("BOBCAT", "COYOTE", "DOMESTICCAT", "DOMESTICDOGCAT", "GRAYFOX", "RACCOON", "RINGTAIL", "SPOTTEDSKUNK", "STRIPEDSKUNK"))

# save the file 
write.csv(carnivores, file = "data/camera_data/carnivores_hourly.csv")
# merge in site data
site_data <- read.csv(file = "data/1_site_data.csv")

carnivores <- left_join(carnivores, site_data, by = "site_id")
write.csv(carnivores, file = "data/camera_data/carnivores_with_site-data_hourly.csv")

# load data DAILY
detections <- read.csv(file = "data/camera_data/GRCA_filtered_daily.csv")
detections <- detections[,c(1:21)]

# filter to only cce of interest 
detections <- detections %>% dplyr::filter(cce_id %in% cam_data$check_event)

# filter to carnivore detections 
carnivores <- detections %>% 
  dplyr::filter(top_identity %in% c("BOBCAT", "COYOTE", "DOMESTICCAT", "DOMESTICDOGCAT", "GRAYFOX", "RACCOON", "RINGTAIL", "SPOTTEDSKUNK", "STRIPEDSKUNK"))

# save the file 
write.csv(carnivores, file = "data/camera_data/carnivores_daily.csv")
# merge in site data
site_data <- read.csv(file = "data/1_site_data.csv")

carnivores <- left_join(carnivores, site_data, by = "site_id")
write.csv(carnivores, file = "data/camera_data/carnivores_with_site-data_daily.csv")



