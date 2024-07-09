# bootstrap proportions of diet categories 
library(tidyverse)
library(boot)

scat <- read.csv(file = "data/2_scat_data.csv")
ringtail_scat <- dplyr::filter(scat, ringtail_confirmed == "yes") 
ringtail_scat$plastic <- as.numeric(ringtail_scat$plastic)
ringtail_scat$anthropogenic <- as.numeric(ringtail_scat$anthropogenic)
ringtail_scat$invertebrate <- as.numeric(ringtail_scat$invertebrate)
ringtail_scat$vertebrate <- as.numeric(ringtail_scat$vertebrate)
ringtail_scat$plant <- as.numeric(ringtail_scat$plant)

wild_scats <- dplyr::filter(ringtail_scat, tourism_level == "wilderness") 
auto_scats <- dplyr::filter(ringtail_scat , tourism_level == "vehicle")

# Load the boot library
library(boot)

# Define a function to calculate the column means
col_mean <- function(data, indices, col) {
  return(mean(data[indices, col]))
}

# Set the seed for reproducibility
set.seed(1993)
level <- 0.95 # confidence level 

# WILDERNESS SCATS 
# Perform bootstrap sampling for each column
boot_results_wild_plastic <- boot(wild_scats, col_mean, R = 1000, col = 12)
boot_results_wild_anthro <- boot(wild_scats, col_mean, R = 1000, col = 13)
boot_results_wild_inv <- boot(wild_scats, col_mean, R = 1000, col = 14)
boot_results_wild_vert <- boot(wild_scats, col_mean, R = 1000, col = 15)
boot_results_wild_plant <- boot(wild_scats, col_mean, R = 1000, col = 16)

# Create a data frame for each diet category with the proportion of all boot strap replicates
## PLASTIC
wild_plastic_bs <- data.frame(
  site_type = rep("wilderness", 1000),
  diet_category = rep("plastic", 1000),
  bootstrap  = boot_results_wild_plastic$t)
## ANTHROPOGENIC
wild_anthro_bs <- data.frame(
  site_type = rep("wilderness", 1000),
  diet_category = rep("anthropogenic", 1000),
  bootstrap  = boot_results_wild_anthro$t)
## INVERTS
wild_inv_bs <- data.frame(
  site_type = rep("wilderness", 1000),
  diet_category = rep("invertebrate", 1000),
  bootstrap  = boot_results_wild_inv$t)
## VERTS
wild_vert_bs <- data.frame(
  site_type = rep("wilderness", 1000),
  diet_category = rep("vertebrate", 1000),
  bootstrap  = boot_results_wild_vert$t)
## PLANTS
wild_plant_bs <- data.frame(
  site_type = rep("wilderness", 1000),
  diet_category = rep("plant", 1000),
  bootstrap  = boot_results_wild_plant$t)
# group all wilderness together for plotting 
wild_cats_bs <- rbind(wild_plastic_bs, wild_anthro_bs, wild_inv_bs, wild_vert_bs, wild_plant_bs)


# Calculate the 95% confidence intervals for each column
conf_intervals_wild_plastic <- boot.ci(boot_results_wild_plastic, conf = level, type = "perc")
conf_intervals_wild_anthro <- boot.ci(boot_results_wild_anthro, conf = level, type = "perc")
conf_intervals_wild_inv <- boot.ci(boot_results_wild_inv, conf = level, type = "perc")
conf_intervals_wild_vert <- boot.ci(boot_results_wild_vert, conf = level, type = "perc")
conf_intervals_wild_plant <- boot.ci(boot_results_wild_plant, conf = level, type = "perc")

wild_report <- data.frame(
  site_type = rep("wilderness", 5),
  diet_category = colnames(wild_scats[12:16]),
  est = c(
    boot_results_wild_plastic$t0,
    boot_results_wild_anthro$t0,
    boot_results_wild_inv$t0,
    boot_results_wild_vert$t0,
    boot_results_wild_plant$t0
  ),
  level = rep(level, 5),
  lower = c(
    conf_intervals_wild_plastic$percent[4],
    conf_intervals_wild_anthro$percent[4],
    conf_intervals_wild_inv$percent[4],
    conf_intervals_wild_vert$percent[4],
    conf_intervals_wild_plant$percent[4]
  ),
  upper = c(
    conf_intervals_wild_plastic$percent[5],
    conf_intervals_wild_anthro$percent[5],
    conf_intervals_wild_inv$percent[5],
    conf_intervals_wild_vert$percent[5],
    conf_intervals_wild_plant$percent[5]
  ), 
  std_dev = c(
    sd(boot_results_wild_plastic$t), 
    sd(boot_results_wild_anthro$t),
    sd(boot_results_wild_inv$t),
    sd(boot_results_wild_vert$t),
    sd(boot_results_wild_plant$t)
  )
)

# VEHICLE SCATS 
# Perform bootstrap sampling for each column
boot_results_auto_plastic <- boot(auto_scats, col_mean, R = 1000, col = 12)
boot_results_auto_anthro <- boot(auto_scats, col_mean, R = 1000, col = 13)
boot_results_auto_inv <- boot(auto_scats, col_mean, R = 1000, col = 14)
boot_results_auto_vert <- boot(auto_scats, col_mean, R = 1000, col = 15)
boot_results_auto_plant <- boot(auto_scats, col_mean, R = 1000, col = 16)

# Create a data frame for each diet category with the proportion of all boot strap replicates
## PLASTIC
auto_plastic_bs <- data.frame(
  site_type = rep("vehicle-access", 1000),
  diet_category = rep("plastic", 1000),
  bootstrap  = boot_results_auto_plastic$t)
## ANTHROPOGENIC
auto_anthro_bs <- data.frame(
  site_type = rep("vehicle-access", 1000),
  diet_category = rep("anthropogenic", 1000),
  bootstrap  = boot_results_auto_anthro$t)
## INVERTS
auto_inv_bs <- data.frame(
  site_type = rep("vehicle-access", 1000),
  diet_category = rep("invertebrate", 1000),
  bootstrap  = boot_results_auto_inv$t)
## VERTS
auto_vert_bs <- data.frame(
  site_type = rep("vehicle-access", 1000),
  diet_category = rep("vertebrate", 1000),
  bootstrap  = boot_results_auto_vert$t)
## PLANTS
auto_plant_bs <- data.frame(
  site_type = rep("vehicle-access", 1000),
  diet_category = rep("plant", 1000),
  bootstrap  = boot_results_auto_plant$t)
# group all auto together for plotting 
auto_cats_bs <- rbind(auto_plastic_bs, auto_anthro_bs, auto_inv_bs, auto_vert_bs, auto_plant_bs)

# Calculate the 95% confidence intervals for each column
conf_intervals_auto_plastic <- boot.ci(boot_results_auto_plastic , conf = level, type = "perc")
conf_intervals_auto_anthro <- boot.ci(boot_results_auto_anthro, conf = level, type = "perc")
conf_intervals_auto_inv <- boot.ci(boot_results_auto_inv, conf = level, type = "perc")
conf_intervals_auto_vert <- boot.ci(boot_results_auto_vert, conf = level, type = "perc")
conf_intervals_auto_plant <- boot.ci(boot_results_auto_plant, conf = level, type = "perc")

auto_report <- data.frame(
  site_type = rep("vehicle-access", 5),
  diet_category = colnames(auto_scats[12:16]),
  est = c(
    boot_results_auto_plastic$t0,
    boot_results_auto_anthro$t0,
    boot_results_auto_inv$t0,
    boot_results_auto_vert$t0,
    boot_results_auto_plant$t0
  ),
  level = rep(level, 5),
  lower = c(
    conf_intervals_auto_plastic$percent[4],
    conf_intervals_auto_anthro$percent[4],
    conf_intervals_auto_inv$percent[4],
    conf_intervals_auto_vert$percent[4],
    conf_intervals_auto_plant$percent[4]
  ),
  upper = c(
    conf_intervals_auto_plastic$percent[5],
    conf_intervals_auto_anthro$percent[5],
    conf_intervals_auto_inv$percent[5],
    conf_intervals_auto_vert$percent[5],
    conf_intervals_auto_plant$percent[5]
  ), 
  std_dev = c(
    sd(boot_results_auto_plastic$t), 
    sd(boot_results_auto_anthro$t),
    sd(boot_results_auto_inv$t),
    sd(boot_results_auto_vert$t),
    sd(boot_results_auto_plant$t)
  )
)

diet_cat_report <- rbind(wild_report, auto_report)
# transform character to level so we can order facet and assign levels in decreasing %FO for wilderness scats 
diet_cat_report$diet_category <- factor(diet_cat_report$diet_category, 
                                        levels= c("plant",
                                                  "vertebrate",
                                                  "anthropogenic",
                                                  "invertebrate", 
                                                  "plastic"))

bootstrap_merge <- rbind(wild_cats_bs, auto_cats_bs)
# transform proportion to percentage as Frequency of Occcurence 
bootstrap_merge$FO_percent <- round(bootstrap_merge$bootstrap*100,2)
# transform character to level so we can order facet and assign levels in decreasing %FO for wilderness scats 
bootstrap_merge$diet_category <- factor(bootstrap_merge$diet_category, levels= c("plant","vertebrate","anthropogenic","invertebrate", "plastic") )

# Plot the density of bootstrap replicates 
diet_category_density_plot <- ggplot(bootstrap_merge, aes(x = FO_percent, fill = site_type)) + 
  geom_density(bw="bcv", alpha = 0.4) + 
  facet_wrap(~ diet_category,  ncol=1) +
  geom_vline(data = diet_cat_report, aes(xintercept=round(est*100, 2)), color="black", linetype="dashed", size=0.8) + 
  theme_classic() + 
  scale_fill_manual(values=c("#47434C", "#A9C5A0")) +
  #scale_y_continuous(breaks=seq(0,0.,by=0.05)) + 
  theme(legend.position="none") +
  labs(x = "Frequency of Occurence (%)", y = "Proportion of bootstrap replicates (n = 1,000)")
diet_category_density_plot

# Plot bar graph of bootstrap mean with standard deviation intervals from bootstrap process 
diet_cat_bp <- ggplot(diet_cat_report, aes(x = diet_category, y = round(est*100,2), fill = site_type)) + 
  geom_bar(stat = "identity", position =position_dodge(), width = 0.8) + 
  geom_errorbar(aes(ymax = round((est + std_dev)*100,2), ymin = round((est-std_dev)*100,2)), position = position_dodge(0.8), width = 0.2) + 
  scale_fill_manual(values=c("#ADA9B2", "#A9C5A0")) + 
  labs(y = "Frequency of Occurence (%)", x = "Diet Category") + 
  theme_classic() +
  theme(legend.position="none") 
diet_cat_bp 
