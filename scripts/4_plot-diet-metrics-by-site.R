# create site metric tables and plot them as a stacked bar plot 

# set up libraries 
library(tidyverse)

# load data 
site <- read.csv("data/0_site_data.csv", strip.white = T)
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)
frags <- dplyr::filter(frags, fragment_type != "Rocks")

# construct Supplementary Table 2.: full population frequency
F_frags <- frags %>% 
  group_by(fragment_type) %>% 
  summarise(`F` = n_distinct(sample_id),
            frags = n(),
            weight = sum(fragment_weight))
total_frags <- sum(F_frags$`F`)
total_weight <- sum(F_frags$weight)

# make into percentages 
F_frags$FO <- round(F_frags$`F`/91*100, 2)
F_frags$RFO <- round(F_frags$`F` / total_frags*100, 2)
F_frags$`%W` <- round(F_frags$weight /total_weight*100, 2)

# Ouptut Table 1 
print(F_frags[c(2,3,5,1,4), c(1,2,5:7)]) 

# Create diet category %W by site type 

pctW_frags <- frags %>% 
  dplyr::select(unique_site, sample_id, fragment_type, fragment_weight)

pctW_frags_assess <- pctW_frags %>% 
  group_by(sample_id) %>% 
  summarise(wght_assessed = sum(fragment_weight))

pctW_frags <- left_join(pctW_frags, pctW_frags_assess, by = "sample_id") 
pctW_frags$pct_W <- round(pctW_frags$fragment_weight / pctW_frags$wght_assessed *100,2)
pctW_frags <- pctW_frags %>% 
  group_by(unique_site, sample_id, fragment_type) %>% 
  summarise(pct_W = sum(pct_W))

pctW_frags_wide <- pctW_frags %>% 
  pivot_wider(names_from = fragment_type,
              values_from = pct_W, 
              values_fill = 0)

pctW_frags_wide <- left_join(pctW_frags_wide , scat, by = "sample_id")

# Perform site comparisons of diet type %W (this includes the zeros)
## PLANT
t_test_pctW_plant <- t.test(Plant ~ tourism_level, data = pctW_frags_wide)
t_test_pctW_plant

## VERTEBRATE
t_test_pctW_vert <- t.test(Vertebrate ~ tourism_level, data = pctW_frags_wide)
t_test_pctW_vert

## ANTHROPOGENIC
t_test_pctW_anthro <- t.test(Anthropogenic ~ tourism_level, data = pctW_frags_wide)
t_test_pctW_anthro

## INV
t_test_pctW_inv <- t.test(Invertebrate ~ tourism_level, data = pctW_frags_wide)
t_test_pctW_inv

## PLASTIC
t_test_pctW_plastic <- t.test(Plastic ~ tourism_level, data = pctW_frags_wide)
t_test_pctW_plastic

# Plot results with significance

## revert wide back to long for plotting 
pctW_frags_long <- pctW_frags_wide[,c(2:7)]

pctW_frags_long <- pctW_frags_long %>% 
  pivot_longer(!sample_id, names_to = "diet_category", values_to = "pct_w")

pctW_frags_long <- left_join(pctW_frags_long , scat, by = "sample_id")
pctW_frags_long$diet_category <- factor(pctW_frags_long$diet_category, 
                                        levels = c("Plant", "Vertebrate", "Anthropogenic", "Invertebrate", "Plastic"))

# plot as boxplot
site_by_pctW <- ggplot(pctW_frags_long, aes(x = diet_category, y = pct_w, fill = tourism_level)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("frontcountry" = "#DF2626", 
                                "backcountry" = "gray50")) +
  scale_x_discrete(labels = c("Anthropogenic" = "Non-plastic contaminants", 
                              "Invertebrate" = "Invertebrate",
                              "Plant" = "Plant",
                              "Plastic" = "Plastic",
                              "Vertebrate" = "Vertebrate")) +
  #geom_jitter() + 
  labs(
    x = "Diet category",
    y = "Weight (%)",
    fill = "Tourist access"
  ) +
  theme_classic()
# save plot
png(filename="figures/supp/FigureS2.png", width = 7.5, height = 4, unit = 'in', res = 300)
site_by_pctW
dev.off()

# calculate frequency of occurrence by site
site_F <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(n_plastic = sum(plastic), 
            n_np_contaminant = sum(anthropogenic),
            n_invertebrate = sum(invertebrate),
            n_vertebrate = sum(vertebrate),
            n_plant = sum(plant))

site_F_long <- site_F %>% 
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "diet_category",
    values_to = "frequency",
    values_drop_na = TRUE
  )

site_F_long$FO <- ifelse(site_F_long$tourism_level == "backcountry", round(site_F_long$frequency/47*100,2),round(site_F_long$frequency/44*100,2))
