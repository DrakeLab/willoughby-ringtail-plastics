# create site metric tables and plot them as a stacked bar plot 

# set up libraries 
library(tidyverse)

# load data 
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)

# merge scat and site data 
scat <- left_join(scat, site, by = "site_id")
scat <- scat %>% dplyr::filter(is.na(anthropogenic_presence) == F)
n_scat <- nrow(scat)

# calculate frequency of occurrence by site
site_F <- scat %>% 
  group_by(vehicle_access) %>% 
  summarise(n_plastic = sum(plastic_presence =="yes"), 
            n_anthropogenic = sum(anthropogenic_presence == "yes"),
            n_invertebrate = sum(invertebrate_presence == "yes"),
            n_vertebrate = sum(vertebrate_presence == "yes"),
            n_plant = sum(plant_presence== "yes"))

## make a stacked bar plot of % relative frequency of occurrence by site
# merge frags and site data 
frags <- left_join(frags, site, by = "site_id")
frags <- frags %>% 
  dplyr::filter(date_added != "")
n_frags <- nrow(frags)
sum_frags <- sum(frags$fragment_weight)

# make a site type table with frequency and sum weight 
site_F <- frags %>% 
  group_by(vehicle_access) %>% 
  summarise(scats = n_distinct(segment_id),
            frags = n(),
            weight = sum(fragment_weight),
            n_frag_plastic = length(fragment_type[fragment_type=="Plastic"]), 
            weight_plastic = sum(fragment_weight[fragment_type=="Plastic"]),
            n_frag_anthro = length(fragment_type[fragment_type=="Anthropogenic"]),
            weight_anthro = sum(fragment_weight[fragment_type=="Anthropogenic"]),
            n_frag_inv =  length(fragment_type[fragment_type=="Invertebrate"]),
            weight_inv = sum(fragment_weight[fragment_type=="Invertebrate"]),
            n_frag_vert =length(fragment_type[fragment_type=="Vertebrate"]),
            weight_vert = sum(fragment_weight[fragment_type=="Vertebrate"]),
            n_frag_plant = length(fragment_type[fragment_type=="Plant"]), 
            weight_plant = sum(fragment_weight[fragment_type=="Plant"]))

# make longer for stacked bar plot to calculate relative frequency 
site_FR <- site_F %>% 
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "diet_category",
    values_to = "frequency",
    values_drop_na = TRUE
)
site_FR <- site_FR[,c(1,2,3,10,11)]
site_FR$diet_category <- str_sub(site_FR$diet_category, 8, -1)
site_FR$pct_frequency_occurrence <- round(site_FR$frequency/site_FR$scats*100, 2)
site_FR$pct_relative_frequency <- round(site_FR$frequency/site_FR$frags*100, 2)

# make longer for stacked bar plot to calculate % weight 
site_W <- site_F %>% 
  pivot_longer(
    cols = starts_with("weight_"),
    names_to = "diet_category",
    values_to = "frag_weight",
    values_drop_na = TRUE
  )
site_W <- site_W[,c(1,2,4,10,11)]
site_W$diet_category <- str_sub(site_W$diet_category, 8, -1)
site_W$pct_weight <- round(site_W$frag_weight/site_W$weight*100, 2)

# merge 
site_diet_metrics <- full_join(site_FR, site_W, by = c("vehicle_access", "scats", "diet_category"))
site_diet_metrics <-site_diet_metrics  %>% 
  pivot_longer(
    cols = frequency:pct_weight,
    names_to = "metric",
    values_to = "value",
    values_drop_na = TRUE
)
# retain only percentages 
site_diet_metrics <- site_diet_metrics %>% 
  dplyr::filter(metric %in% c("pct_frequency_occurrence", "pct_relative_frequency", "pct_weight"))

# plot a stacked bar plot

# facetted verion 
site_by_metric <- ggplot(site_diet_metrics , aes(x = vehicle_access, y = value, fill = diet_category)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_grid(.~metric, scales = "free_y") +
  theme_classic() +
  scale_x_discrete(name ="", 
                   limits=c("yes", "no"), 
                   labels=c("Vehicle Access", "Wilderness")) + 
  labs(y = "Proportion of scat segments (n = 83)")

# side by side version 
library(gridExtra)
site_by_pctFO <- ggplot(subset(site_diet_metrics, metric=="pct_frequency_occurrence"), aes(x = vehicle_access, y = value, fill = diet_category)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_classic() + 
  theme(legend.position="none") + 
  labs(title="Frequency of Occurrence (%)", y = "Scat (n = 71)") + 
  scale_x_discrete(name ="", limits=c("yes", "no"), 
                   labels=c("Vehicle Access", "Wilderness")) 
site_by_pctFR <- ggplot(subset(site_diet_metrics,metric=="pct_relative_frequency"), aes(x = vehicle_access, y = value, fill = diet_category)) + 
   geom_bar(position="stack", stat="identity") + 
  theme_classic() + 
  theme(legend.position="none")  + 
  labs(title="Relative Frequency of Occurrence (%)", y = "Diet items (n = 183)") + 
  scale_x_discrete(name ="", limits=c("yes", "no"), 
                   labels=c("Vehicle Access", "Wilderness")) 
site_by_pctW <- ggplot(subset(site_diet_metrics,metric=="pct_weight"), aes(x = vehicle_access, y = value, fill = diet_category)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_classic()  + 
  labs(title="Weight (%)", y = "Diet Items (n = 22.009 grams)") + 
  scale_x_discrete(name ="", limits=c("yes", "no"), 
                   labels=c("Vehicle Access", "Wilderness")) 
# make the plot 

png(filename="figures/Figure1.png")
grid.arrange(site_by_pctFO, site_by_pctFR, site_by_pctW, nrow = 1)
dev.off()
