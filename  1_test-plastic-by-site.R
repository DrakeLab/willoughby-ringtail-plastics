# Script to compare sites by plastic presence and composition
# load libraries
library(tidyverse)

# read in data
scat <- read.csv(file = "data/2_scat_data.csv")

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

# data visualizations by site of plastic presence 
## plot stacked barplot 
site_plastic_tbl$vehicle_access <- as.factor(site_plastic_tbl$vehicle_access)
levels(site_plastic_tbl$vehicle_access) <- c("yes", "no")



site_by_plastic_plot <- ggplot(site_plastic_tbl, aes(x = vehicle_access, fill = plastic_presence, y = n)) + 
geom_bar(position="fill", stat="identity") + 
  theme_classic() +
  scale_x_discrete(name ="", 
                  limits=c("yes", "no"), 
                   labels=c("Vehicle Access", "Wilderness")) + 
  labs(y = "Proportion of scat segments (n = 83)")

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
