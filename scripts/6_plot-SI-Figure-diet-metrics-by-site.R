# create site metric tables and plot them as a stacked bar plot 

# set up libraries 
library(tidyverse)

# load data 
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
# remove scats not confirmed as ringtail 
scat <- scat %>% 
  dplyr::filter(ringtail_confirmed == "yes")
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)

# merge scat and site data
scat <- left_join(scat, site, by = "site_id")
n_scat <- nrow(scat)

# construct Table 1.: full population frequency
F_frags <- frags %>% 
  group_by(fragment_type) %>% 
  summarise(`F` = n_distinct(segment_id),
            frags = n(),
            weight = sum(fragment_weight))
total_frags <- sum(F_frags$`F`)
total_weight <- sum(F_frags$weight)

# make into percentages 
F_frags$FO <- round(F_frags$`F`/83*100, 2)
F_frags$RFO <- round(F_frags$`F` / total_frags*100, 2)
F_frags$RW <- round(F_frags$weight /total_weight*100, 2)

# Ouptut Table 1 
print(F_frags[c(2,3,5,1,4), c(1,2,5:7)]) 

# calculate frequency of occurrence by site
site_F <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(n_plastic = sum(plastic == 1), 
            n_anthropogenic = sum(anthropogenic == 1),
            n_invertebrate = sum(invertebrate == 1),
            n_vertebrate = sum(vertebrate == 1),
            n_plant = sum(plant == 1))

site_F_long <- site_F %>% 
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "diet_category",
    values_to = "frequency",
    values_drop_na = TRUE
  )

site_F_long$site_type = ifelse(site_F_long$tourism_level == "backcountry", "backcountry (n = 43)","frontcountry(n = 40)")
site_F_long$FO <- ifelse(site_F_long$tourism_level == "backcountry", round(site_F_long$frequency/43*100,2),round(site_F_long$frequency/40*100,2))

site_by_diet_plot <- ggplot(site_F_long, aes(x = reorder(diet_category, -FO), y = FO, fill = diet_category)) + 
  geom_bar(stat="identity") + 
  facet_grid(~site_type) + 
  labs(x = "diet category", y = "Frequency of Occurrence (%)") + 
  scale_y_continuous(limits = c(0,100)) + 
  theme_classic() 
  # annotate("text", )
site_by_diet_plot 


## make a stacked bar plot of % relative frequency of occurrence by site
# merge frags and site data 
frags <- left_join(frags, site, by = "site_id")
n_frags <- nrow(frags)
frags$fragment_weight <- as.numeric(frags$fragment_weight)
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
                   labels=c("frontcountry", "backcountry")) + 
  labs(y = "Proportion of scat segments (n = 83)")

# side by side version 
library(gridExtra)
site_by_pctFO <- ggplot(subset(site_diet_metrics, metric=="pct_frequency_occurrence"), aes(x = vehicle_access, y = value, fill = diet_category)) + 
  geom_bar(position="dodge", stat="identity") + 
  theme_classic() + 
  theme(legend.position="none") + 
  labs(title="Frequency of Occurrence (%)", y = "Scat (n = 83)") + 
  scale_x_discrete(name ="", limits=c("yes", "no"), 
                   labels=c("frontcountry", "backcountry")) 
site_by_pctFR <- ggplot(subset(site_diet_metrics,metric=="pct_relative_frequency"), aes(x = vehicle_access, y = value, fill = diet_category)) + 
   geom_bar(position="stack", stat="identity") + 
  theme_classic() + 
  theme(legend.position="none")  + 
  labs(title="Relative Frequency of Occurrence (%)", y = "Percent of Diet items (n = 205)") + 
  scale_x_discrete(name ="", limits=c("yes", "no"), 
                   labels=c("frontcountry", "backcountry")) 
site_by_pctW <- ggplot(subset(site_diet_metrics,metric=="pct_weight"), aes(x = vehicle_access, y = value, fill = diet_category)) + 
  geom_bar(position="stack", stat="identity") + 
  theme_classic()  + 
  labs(title="Relative Weight (%)", y = "Diet Items (n = 22.009 grams)") + 
  scale_x_discrete(name ="", limits=c("yes", "no"), 
                   labels=c("frontcountry", "backcountry")) 
# make the plot 

png(filename="figures/supp/FigureS2.png", width = 16, height = 6, units = 'in', res = 300)
grid.arrange(site_by_pctFO, site_by_pctFR, site_by_pctW, nrow = 1)
dev.off()
