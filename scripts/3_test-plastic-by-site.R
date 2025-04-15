# Script to compare sites by plastic presence and composition
# load libraries
library(tidyverse)

# read in data
site <- read.csv("data/0_site_data.csv", strip.white = T)
scat <- read.csv(file = "data/2_scat_data.csv") # scat segment data
scat <- left_join(scat, site, by = "unique_site")

# format contingency table of site by site type by plastic (YN)
site_plastic_tbl <- scat %>% 
  group_by(tourism_level, plastic) %>% 
  summarise(n = n())
site_plastic_mat <- site_plastic_tbl %>% 
  pivot_wider(names_from = plastic,
              values_from = n)
site_plastic_mat <- site_plastic_mat %>% 
  rename(
    plastic_no = no,
    plastic_yes = yes
  )
site_plastic_mat$tourism_level <- NULL
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

# Plot Figure 3 
# calculate fragment weights by site type 
frags <-  frags %>% 
  dplyr::filter(fragment_type != "Rocks")
scat_fw <- left_join(scat, frags, by = "sample_id")

scat_fw_sum <- scat_fw %>% 
  group_by(tourism_level) %>% 
  summarise(weight = sum(fragment_weight))

scat_fw$pct_W <- scat_fw$fragment_weight / scat_fw$dryweight_grams*100 

# compare % weight of Plastics in front country vs backcountry 
plastics <- read.csv(file = "data/4_plastic_data.csv", strip.white = T)
plastics <- left_join( plastics,scat, by = "sample_id")
plastics$pct_W <- plastics$fragment_weight / plastics$dry_weight *100

plastic_pctW_by_site <- ggplot(plastics, aes(x =tourism_level.x , y = pct_W)) + 
  geom_boxplot() + 
  geom_point(aes(shape = scat_identity, color = plastic_type),size = 5, position = position_jitterdodge(0.5)) + 
  scale_shape_manual(name = "scat depositor identity", 
                       values = c("ringtail" = 18, "carnivore" = 15, "other mesocarnivore" = 17)) + 
  scale_color_manual(
    name = "plastic polymer",
    values = c(
      "pe" = "blue",
      "pp" = "orange",
      "ps" = "darkgrey"
    ),
    labels = c(
      "pe" = "polyethylene",
      "pp" = "polypropylene",
      "ps" = "polystyrene"
    )
  ) +
  scale_y_continuous(breaks = c(0,15,30,45,60), limits = c(0,60)) + 
  labs(x = "Site Area", y = "Weight (%)") + 
  labs(x = "Tourist access") +
  theme_classic() + 
  theme(axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        axis.title=element_text(size=24,face="bold")) 

png(filename="figures/Figure3.png", width = 4, height = 4, units = 'in', res = 300)
plastic_pctW_by_site
dev.off()

