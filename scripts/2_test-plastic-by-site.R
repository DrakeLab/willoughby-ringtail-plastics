# Script to compare sites by plastic presence and composition
# load libraries
library(tidyverse)

# read in data
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv(file = "data/2_scat_data.csv") # scat segment data
scat <- left_join(scat, site, by = "site_id")

# remove scats not confirmed as ringtail 
scat <- scat %>% 
  dplyr::filter(ringtail_confirmed == "yes")

# format contingency table of site (vehicle access YN) by plastic (YN)
site_plastic_tbl <- scat %>% 
  group_by(vehicle_access, plastic_presence) %>% 
  summarise(n = n())
site_plastic_mat <- site_plastic_tbl %>% 
  pivot_wider(names_from = plastic_presence,
              values_from = n)
site_plastic_mat <- site_plastic_mat %>% 
  rename(
    plastic_no = no,
    plastic_yes = yes
  )
site_plastic_mat$vehicle_access <- NULL
# make a matrix 
site_plastic_mat <- as.matrix(site_plastic_mat)

# chi square of site by plastic presence
# not using Yates's correction for continuity
chisq.test(site_plastic_mat, correct = F)

# format contingency table of site (vehicle access YN) by plant (YN)
site_plant_tbl <- scat %>% 
  group_by(vehicle_access, plant_presence) %>% 
  summarise(n = n())
site_plant_mat <- site_plant_tbl %>% 
  pivot_wider(names_from = plant_presence,
              values_from = n)
site_plant_mat <- site_plant_mat %>% 
  rename(
    plant_no = no,
    plant_yes = yes
  )
site_plant_mat$vehicle_access <- NULL

# make a matrix 
site_plant_mat <- as.matrix(site_plant_mat)

# chi square of site by plant presence
# not using Yates's correction for continuity
chisq.test(site_plant_mat, correct = F)

# format contingency table of site (vehicle access YN) by invertebrate (YN)
site_inv_tbl <- scat %>% 
  group_by(vehicle_access, invertebrate_presence) %>% 
  summarise(n = n())
site_inv_mat <- site_inv_tbl %>% 
  pivot_wider(names_from = invertebrate_presence,
              values_from = n)
site_inv_mat <- site_inv_mat %>% 
  rename(
    inv_no = no,
    inv_yes = yes
  )
site_inv_mat$vehicle_access <- NULL
# make a matrix 
site_inv_mat <- as.matrix(site_inv_mat)

# chi square of site by invertebrate presence
# not using Yates's correction for continuity
chisq.test(site_inv_mat, correct = F)

# format contingency table of site (vehicle access YN) by vertebrate (YN)
site_vert_tbl <- scat %>% 
  group_by(vehicle_access, vertebrate_presence) %>% 
  summarise(n = n())
site_vert_mat <- site_vert_tbl %>% 
  pivot_wider(names_from = vertebrate_presence,
              values_from = n)
site_vert_mat <- site_vert_mat %>% 
  rename(
    vert_no = no,
    vert_yes = yes
  )
site_vert_mat$vehicle_access <- NULL
# make a matrix 
site_vert_mat <- as.matrix(site_vert_mat)

# chi square of site by vertebrate presence
# not using Yates's correction for continuity
chisq.test(site_vert_mat, correct = F)

# format contingency table of site (vehicle access YN) by anthropogenic (YN)
site_anthro_tbl <- scat %>% 
  group_by(vehicle_access, anthropogenic_presence) %>% 
  summarise(n = n())
site_anthro_mat <- site_anthro_tbl %>% 
  pivot_wider(names_from = anthropogenic_presence,
              values_from = n)
site_anthro_mat <- site_anthro_mat %>% 
  rename(
    anthro_no = no,
    anthro_yes = yes
  )
site_anthro_mat$vehicle_access <- NULL
# make a matrix 
site_anthro_mat <- as.matrix(site_anthro_mat)

# chi square of site by vertebrate presence
# not using Yates's correction for continuity
chisq.test(site_anthro_mat, correct = F)


# data visualizations by site of plastic presence 
## plot stacked barplot 
site_plastic_tbl$vehicle_access <- as.factor(site_plastic_tbl$vehicle_access)
levels(site_plastic_tbl$vehicle_access) <- c("no", "yes")

site_by_plastic_plot <- ggplot(site_plastic_tbl, aes(x = vehicle_access, fill = plastic_presence, y = n)) + 
geom_bar(position="fill", stat="identity") + 
  theme_classic() +
  scale_x_discrete(name ="", 
                  limits=c("yes", "no"), 
                   labels=c("Vehicle Access", "Wilderness")) + 
  scale_fill_manual(values = c("lightgray", "black")) + 
  labs(y = "Proportion of scat segments") + 
site_by_plastic_plot 
# save plot 
png(filename="figures/Figure1.png")
site_by_plastic_plot 
dev.off()

# colors 

# fix order of sites 
# site names 

# statistics of site by plastic type 

# data visualizations of site by plastic type 

# summary statistics of plastic morphology by site 

# data visualizations of plastic morphology by site 

# statistics of scat morphology by site 

# data visualizations of scat morphology by site 
