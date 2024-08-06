# bootstrap proportions of diet categories for %FO 

# load libraries
library(tidyverse)
library(boot)

# load data 
site <- read.csv(file = "data/1_site_data.csv")
# scat <- read.csv(file = "data/2_scat_data.csv")
# filter to only ringtail confirmed scat 
# ringtail_scat <- dplyr::filter(scat, ringtail_confirmed == "yes") 
# transform to numeric 
fragment <- read.csv(file = "data/3_fragment_data.csv")

# calculate total analyzed segment weight 
segment <- fragment %>% 
  group_by(segment_id) %>% 
  summarise(total_weight = sum(fragment_weight))

# merge in site type 
fragment <- left_join(fragment, site, by = "site_id")
# merge in total analyzed weight 
fragment <- left_join(fragment, segment, by = "segment_id")
#calculate relative weight 
fragment$relative_weight <- (fragment$fragment_weight / fragment$total_weight)*100

# convert into a matrix to ensure 0s 
fragment_ss <- fragment %>% 
  dplyr::select(tourism_type, segment_id, fragment_id, fragment_type, relative_weight)
fragment_ss <- fragment_ss %>% 
  pivot_wider(names_from = fragment_type, 
              values_from = relative_weight, 
              values_fn = sum)
# replace NAs with zero 
fragment_ss_wz <- fragment_ss
fragment_ss_wz [is.na(fragment_ss_wz )] <- 0

# pivot long again with Zeros 
fragment_ss_wz_wide <- fragment_ss_wz %>% 
  pivot_longer(cols = Vertebrate:Plastic,
               names_to = "diet_category",
               values_to = "relative_weight")

# plot density plot 


# pivot long again without Zeros 
fragment_ss_wide <- fragment_ss %>% 
  pivot_longer(cols = Vertebrate:Plastic,
               names_to = "diet_category",
               values_to = "relative_weight")
fragment_ss_wide <- fragment_ss_wide %>% 
  dplyr::filter(is.na(relative_weight) == FALSE)

# summarise at the site type level
segment_rw <- fragment_ss_wide %>% 
  group_by(tourism_type, diet_category) %>% 
  summarise(n_fragments = n(),
            mean_RW = mean(relative_weight), 
            sd_RW = sd(relative_weight))

# plot bar graph 
# Plot bar graph of bootstrap mean with standard deviation intervals from bootstrap process 
png(filename="figures/Figure3OptB.png", width = 8, height = 4, unit = 'in', res = 300)
diet_cat_RW_bp <- ggplot(segment_rw, aes(x = diet_category, y = round(mean_RW,2), fill = tourism_type)) + 
  geom_bar(stat = "identity", position =position_dodge(), width = 0.8) + 
  geom_errorbar(aes(ymax = round((mean_RW + sd_RW),2), 
                    ymin = round((mean_RW - sd_RW),2)), position = position_dodge(0.8), width = 0.2) + 
  scale_fill_manual(values=c("#ADA9B2", "#A9C5A0")) + 
  labs(y = "Relative weight (%)", x = "Diet Category") + 
  geom_point(data = fragment_ss_wide, aes(x = diet_category, y = relative_weight, fill = tourism_type)) + 
  geom_jitter() + 
  theme_classic()
diet_cat_RW_bp 
dev.off()

# filter to site type
bc_frags <- dplyr::filter(fragment_ss, tourism_type == "backcountry") 
fc_frags <- dplyr::filter(fragment_ss, tourism_type == "frontcountry")

# perform statistical comparison between the bootstrap replicates 
# anthropogenic t test 
t.test(fc_frags$Anthropogenic, bc_frags$Anthropogenic, alternative = "two.sided")
# invertebrate t test 
t.test(fc_frags$Invertebrate, bc_frags$Invertebrate, alternative = "two.sided")
# plant t test 
t.test(fc_frags$Plant, bc_frags$Plant, alternative = "two.sided")
# plastic t test 
t.test(fc_frags$Plastic, bc_frags$Plastic, alternative = "two.sided")
# vertebrate t test 
t.test(fc_vert_bs$bootstrap, bc_vert_bs$bootstrap, alternative = "two.sided")

# transform character to level so we can order facet and assign levels in decreasing %FO for wilderness scats 
diet_cat_report$diet_category <- factor(diet_cat_report$diet_category, 
                                        levels= c("plant",
                                                  "vertebrate",
                                                  "anthropogenic",
                                                  "invertebrate", 
                                                  "plastic"))

bootstrap_merge <- rbind(bc_cats_bs, fc_cats_bs)
# transform proportion to percentage as Frequency of Occcurence 
bootstrap_merge$FO_percent <- round(bootstrap_merge$bootstrap*100,2)
# transform character to level so we can order facet and assign levels in decreasing %FO for wilderness scats 
bootstrap_merge$diet_category <- factor(bootstrap_merge$diet_category, levels= c("plant","vertebrate","anthropogenic","invertebrate", "plastic") )

# Plot the density of bootstrap replicates 
png(filename="figures/Figure2OptA.png", width = 8, height = 6, unit = 'in', res = 300)
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
dev.off()

