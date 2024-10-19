# load libraries
library(tidyverse)
library(boot)

# load data 
scat <- read.csv(file = "data/2_scat_data.csv")
# filter to only ringtail confirmed scat 
rscat <- dplyr::filter(scat, ringtail_confirmed == "yes") 
# transform to numeric 
rscat$plastic <- as.numeric(rscat$plastic)
rscat$anthropogenic <- as.numeric(rscat$anthropogenic)
rscat$invertebrate <- as.numeric(rscat$invertebrate)
rscat$vertebrate <- as.numeric(rscat$vertebrate)
rscat$plant <- as.numeric(rscat$plant)

dc_raw <- rscat %>% 
  group_by(tourism_level) %>% 
  summarise(scat = n(), 
            plasticFO = sum(plastic), 
            anthropogenicFO =sum(anthropogenic),
            invertebrateFO =  sum(invertebrate),
            vertebrateFO = sum(vertebrate),
            plantFO =  sum(plant))
dc_raw_long <- dc_raw %>%
  pivot_longer(cols = ends_with("FO"), 
               names_to = "diet_category", 
               values_to = "frequency")
dc_raw_long$pct_FO <- round((dc_raw_long$frequency / dc_raw_long$scat)*100,2)
dc_raw_long$SE <-  round(sqrt((dc_raw_long$frequency / dc_raw_long$scat)*(1-(dc_raw_long$frequency / dc_raw_long$scat))/dc_raw_long$scat)*100,2)
dc_raw_long$diet_category <- substr(dc_raw_long$diet_category,1,nchar(dc_raw_long$diet_category)-2)
dc_raw_long$significant <- c("yes","yes","no","yes","yes","yes","yes","no","yes","yes")

# plot the raw values and calculated SE 
dc_bp <- ggplot(dc_raw_long, aes(x = reorder(diet_category, -pct_FO), y = pct_FO, fill = tourism_level, shape = significant)) + 
  geom_bar(stat = "identity", position =position_dodge(), width = 0.8) + 
  geom_errorbar(aes(ymax = (pct_FO + SE), ymin = (pct_FO-SE)), position = position_dodge(0.8), width = 0.2) + 
  scale_fill_manual(values=c("#A9C5A0", "#ADA9B2")) +
  labs(y = "Frequency of Occurence (%)", x = "Diet Category") + 
  theme_classic() +
  theme(legend.position="none") + 
  geom_point(aes(y = pct_FO + 11),
             position = position_dodge(0.9), 
             show.legend = FALSE) +
  scale_shape_manual(values = c(NA, 8)) + 
  ylim(0,100)

# save the plot
png(filename="figures/Figure2a.png", width = 6, height = 4, unit = 'in', res = 300)
dc_bp
dev.off()

# bootstrap proportions of diet categories for %FO 

# Define a function to calculate the column means
# col_mean <- function(data, indices, col) {
#  return(mean(data[indices, col]))
#}

# Set the seed for reproducibility
#set.seed(1993)
#level <- 0.95 # confidence level 

# BACKCOUNTRY SCATS 
# Perform bootstrap sampling for each column
#boot_results_bc_plastic <- boot(bc_scats, col_mean, R = 1000, col = 12)
#boot_results_bc_anthro <- boot(bc_scats, col_mean, R = 1000, col = 13)
#boot_results_bc_inv <- boot(bc_scats, col_mean, R = 1000, col = 14)
#boot_results_bc_vert <- boot(bc_scats, col_mean, R = 1000, col = 15)
#boot_results_bc_plant <- boot(bc_scats, col_mean, R = 1000, col = 16)

# Create a data frame for each diet category with the proportion of all boot strap replicates
## PLASTIC
#bc_plastic_bs <- data.frame(
#  site_type = rep("backcountry", 1000),
#  diet_category = rep("plastic", 1000),
#  bootstrap  = boot_results_bc_plastic$t)
## ANTHROPOGENIC
#bc_anthro_bs <- data.frame(
#  site_type = rep("backcountry", 1000),
#  diet_category = rep("anthropogenic", 1000),
#  bootstrap  = boot_results_bc_anthro$t)
## INVERTS
#bc_inv_bs <- data.frame(
#  site_type = rep("backcountry", 1000),
#  diet_category = rep("invertebrate", 1000),
#  bootstrap  = boot_results_bc_inv$t)
## VERTS
#bc_vert_bs <- data.frame(
#  site_type = rep("backcountry", 1000),
#  diet_category = rep("vertebrate", 1000),
#  bootstrap  = boot_results_bc_vert$t)
## PLANTS
#bc_plant_bs <- data.frame(
#  site_type = rep("backcountry", 1000),
#  diet_category = rep("plant", 1000),
#  bootstrap  = boot_results_bc_plant$t)
# group all backcountry together for plotting 
#bc_cats_bs <- rbind(bc_plastic_bs, bc_anthro_bs, bc_inv_bs, bc_vert_bs, bc_plant_bs)

# Calculate the 95% confidence intervals for each column
#conf_intervals_bc_plastic <- boot.ci(boot_results_bc_plastic, conf = level, type = "perc")
#conf_intervals_bc_anthro <- boot.ci(boot_results_bc_anthro, conf = level, type = "perc")
#conf_intervals_bc_inv <- boot.ci(boot_results_bc_inv, conf = level, type = "perc")
#conf_intervals_bc_vert <- boot.ci(boot_results_bc_vert, conf = level, type = "perc")
#conf_intervals_bc_plant <- boot.ci(boot_results_bc_plant, conf = level, type = "perc")

#bc_report <- data.frame(
#  site_type = rep("backcountry", 5),
#  diet_category = colnames(bc_scats[12:16]),
#  est = c(
#    boot_results_bc_plastic$t0,
#    boot_results_bc_anthro$t0,
#    boot_results_bc_inv$t0,
#    boot_results_bc_vert$t0,
#    boot_results_bc_plant$t0
#  ),
#  level = rep(level, 5),
#  lower = c(
#    conf_intervals_bc_plastic$percent[4],
#    conf_intervals_bc_anthro$percent[4],
#    conf_intervals_bc_inv$percent[4],
#    conf_intervals_bc_vert$percent[4],
#    conf_intervals_bc_plant$percent[4]
#  ),
#  upper = c(
#    conf_intervals_bc_plastic$percent[5],
#    conf_intervals_bc_anthro$percent[5],
#    conf_intervals_bc_inv$percent[5],
#    conf_intervals_bc_vert$percent[5],
#    conf_intervals_bc_plant$percent[5]
#  ), 
#  std_dev = c(
 #   sd(boot_results_bc_plastic$t), 
 #   sd(boot_results_bc_anthro$t),
  #  sd(boot_results_bc_inv$t),
#    sd(boot_results_bc_vert$t),
#    sd(boot_results_bc_plant$t)
#  ),
#  std_error = c(
#   sd(boot_results_bc_plastic$t)/sqrt(nrow(boot_results_bc_plastic$data)), 
#    sd(boot_results_bc_anthro$t)/sqrt(nrow(boot_results_bc_anthro$data)),
#    sd(boot_results_bc_inv$t)/sqrt(nrow(boot_results_bc_inv$data)),
#   sd(boot_results_bc_vert$t)/sqrt(nrow(boot_results_bc_vert$data)),
#    sd(boot_results_bc_plant$t/sqrt(nrow(boot_results_bc_plant$data)))
 # )
#)

# FRONTCOUNTRY SCATS 
# Perform bootstrap sampling for each column
#boot_results_fc_plastic <- boot(fc_scats, col_mean, R = 1000, col = 12)
#boot_results_fc_anthro <- boot(fc_scats, col_mean, R = 1000, col = 13)
#boot_results_fc_inv <- boot(fc_scats, col_mean, R = 1000, col = 14)
#boot_results_fc_vert <- boot(fc_scats, col_mean, R = 1000, col = 15)
#boot_results_fc_plant <- boot(fc_scats, col_mean, R = 1000, col = 16)

# Create a data frame for each diet category with the proportion of all boot strap replicates
## PLASTIC
#fc_plastic_bs <- data.frame(
#  site_type = rep("frontcountry", 1000),
#  diet_category = rep("plastic", 1000),
#  bootstrap  = boot_results_fc_plastic$t)
### ANTHROPOGENIC
#fc_anthro_bs <- data.frame(
#  site_type = rep("frontcountry", 1000),
#  diet_category = rep("anthropogenic", 1000),
#  bootstrap  = boot_results_fc_anthro$t)
## INVERTS
#fc_inv_bs <- data.frame(
#  site_type = rep("frontcountry", 1000),
#  diet_category = rep("invertebrate", 1000),
#  bootstrap  = boot_results_fc_inv$t)
## VERTS
#fc_vert_bs <- data.frame(
#  site_type = rep("frontcountry", 1000),
#  diet_category = rep("vertebrate", 1000),
#  bootstrap  = boot_results_fc_vert$t)
## PLANTS
#fc_plant_bs <- data.frame(
#  site_type = rep("frontcountry", 1000),
#  diet_category = rep("plant", 1000),
#  bootstrap  = boot_results_fc_plant$t)
# group all auto together for plotting 
#fc_cats_bs <- rbind(fc_plastic_bs, fc_anthro_bs, fc_inv_bs, fc_vert_bs, fc_plant_bs)

# Calculate the 95% confidence intervals for each column
#conf_intervals_fc_plastic <- boot.ci(boot_results_fc_plastic , conf = level, type = "perc")
#conf_intervals_fc_anthro <- boot.ci(boot_results_fc_anthro, conf = level, type = "perc")
#conf_intervals_fc_inv <- boot.ci(boot_results_fc_inv, conf = level, type = "perc")
#conf_intervals_fc_vert <- boot.ci(boot_results_fc_vert, conf = level, type = "perc")
#conf_intervals_fc_plant <- boot.ci(boot_results_fc_plant, conf = level, type = "perc")

#fc_report <- data.frame(
#  site_type = rep("frontcountry", 5),
#  diet_category = colnames(fc_scats[12:16]),
#  est = c(
#    boot_results_fc_plastic$t0,
#    boot_results_fc_anthro$t0,
#    boot_results_fc_inv$t0,
#    boot_results_fc_vert$t0,
#    boot_results_fc_plant$t0
#  ),
#  level = rep(level, 5),
#  lower = c(
#    conf_intervals_fc_plastic$percent[4],
#    conf_intervals_fc_anthro$percent[4],
#    conf_intervals_fc_inv$percent[4],
#    conf_intervals_fc_vert$percent[4],
#    conf_intervals_fc_plant$percent[4]
#  ),
#  upper = c(
#    conf_intervals_fc_plastic$percent[5],
#    conf_intervals_fc_anthro$percent[5],
#    conf_intervals_fc_inv$percent[5],
 #   conf_intervals_fc_vert$percent[5],
#    conf_intervals_fc_plant$percent[5]
#  ), 
#  std_dev = c(
#    sd(boot_results_fc_plastic$t), 
#    sd(boot_results_fc_anthro$t),
#    sd(boot_results_fc_inv$t),
#    sd(boot_results_fc_vert$t),
#    sd(boot_results_fc_plant$t)
#  ), 
#  std_error = c(
#    sd(boot_results_fc_plastic$t)/sqrt(nrow(boot_results_fc_plastic$data)), 
#    sd(boot_results_fc_anthro$t)/sqrt(nrow(boot_results_fc_anthro$data)),
#    sd(boot_results_fc_inv$t)/sqrt(nrow(boot_results_fc_inv$data)),
 #   sd(boot_results_fc_vert$t)/sqrt(nrow(boot_results_fc_vert$data)),
#    sd(boot_results_fc_plant$t)/sqrt(nrow(boot_results_fc_plant$data))
#))

#diet_cat_report <- rbind(bc_report, fc_report)

# perform statistical comparison between the bootstrap replicates 
# anthropogenic t test 
#t.test(fc_anthro_bs$bootstrap, bc_anthro_bs$bootstrap, alternative = "two.sided")
# invertebrate t test 
#t.test(fc_inv_bs$bootstrap, bc_inv_bs$bootstrap, alternative = "two.sided")
# plant t test 
#t.test(fc_plant_bs$bootstrap, bc_plant_bs$bootstrap, alternative = "two.sided")
# plastic t test 
#t.test(fc_plastic_bs$bootstrap, bc_plastic_bs$bootstrap, alternative = "two.sided")
# vertebrate t test 
#t.test(fc_vert_bs$bootstrap, bc_vert_bs$bootstrap, alternative = "two.sided")

# transform character to level so we can order facet and assign levels in decreasing %FO for wilderness scats 
#diet_cat_report$diet_category <- factor(diet_cat_report$diet_category, 
#                                        levels= c("plant",
#                                                  "vertebrate",
#                                                  "anthropogenic",
#                                                  "invertebrate", 
#                                                  "plastic"))
#bootstrap_merge <- rbind(bc_cats_bs, fc_cats_bs)
# transform proportion to percentage as Frequency of Occcurence 
#bootstrap_merge$FO_percent <- round(bootstrap_merge$bootstrap*100,2)
# transform character to level so we can order facet and assign levels in decreasing %FO for wilderness scats 
#bootstrap_merge$diet_category <- factor(bootstrap_merge$diet_category, levels= c("plant","vertebrate","anthropogenic","invertebrate", "plastic") )

# Plot the density of bootstrap replicates 
# png(filename="figures/Figure2OptA.png", width = 8, height = 6, unit = 'in', res = 300)
# diet_category_density_plot <- ggplot(bootstrap_merge, aes(x = FO_percent, fill = site_type)) + 
#  geom_density(bw="bcv", alpha = 0.4) + 
#  facet_wrap(~ diet_category,  ncol=1) +
#  geom_vline(data = diet_cat_report, aes(xintercept=round(est*100, 2)), color="black", linetype="dashed", size=0.8) + 
#  theme_classic() + 
#  scale_fill_manual(values=c("#47434C", "#A9C5A0")) +
#  #scale_y_continuous(breaks=seq(0,0.,by=0.05)) + 
#  theme(legend.position="none") +
#  labs(x = "Frequency of Occurence (%)", y = "Proportion of bootstrap replicates (n = 1,000)")
# diet_category_density_plot
# dev.off()

# Plot bar graph of bootstrap mean with standard error intervals from bootstrap process 
#diet_cat_bp <- ggplot(diet_cat_report, aes(x = diet_category, y = round(est*100,2), fill = site_type)) + 
#  geom_bar(stat = "identity", position =position_dodge(), width = 0.8) + 
#  geom_errorbar(aes(ymax = round((est + std_error)*100,2), ymin = round((est-std_dev)*100,2)), position = position_dodge(0.8), width = 0.2) + 
#  scale_fill_manual(values=c("#A9C5A0", "#ADA9B2")) + 
#  labs(y = "Frequency of Occurence (%)", x = "Diet Category") + 
#  theme_classic() +
#  theme(legend.position="none") + 
 # ylim(0,100)

# save the plot
#png(filename="figures/Figure2b.png", width = 6, height = 4, unit = 'in', res = 300)
#diet_cat_bp 
#dev.off()
