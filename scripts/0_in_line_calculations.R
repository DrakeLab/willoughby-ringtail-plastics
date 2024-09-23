# Calculation of In-line values 

# set up libraries 
library(dplyr) # data wrangling

# read in data 
site <- read.csv(file = "data/1_site_data.csv")
scat <- read.csv(file = "data/2_scat_data.csv")
scat <- left_join(scat, site, by = "site_id")

# frags <- read.csv("data/3_fragment_data.csv", strip.white = T)
#plastic <- read.csv("data/4_plastic_data.csv", strip.white = T)

# of unique scat samples 
scat_count <- n_distinct(scat$segment_id)
print(scat_count)

# calculate any anthropogenic items in the scat 
scat$any_anthro <- ifelse(scat$anthropogenic == "yes", "yes", 
                          ifelse(scat$plastic_presence == "yes", "yes", "no")) 
scat_by_anthropogenic <- scat %>% 
  group_by(any_anthro) %>% 
  summarise(n_scat = n(),
            weight_mean = mean(wetweight_grams),
            weight_sd = sd(wetweight_grams),
            diameter_mean = mean(diameter_mm), 
            diameter_sd = sd(diameter_mm))

# calculate grouped statistics for scat 
scat_by_site <- scat %>% 
  group_by(vehicle_access) %>% 
  summarise(n_scat = n(),
            weight_mean = mean(wetweight_grams),
            weight_sd = sd(wetweight_grams),
            diameter_mean = mean(diameter_mm), 
            diameter_sd = sd(diameter_mm))

# of unique scat samples by site 
## frontcountry scats 
scat_vehicle <- scat_by_site[2,2] 
print(scat_vehicle)
## backcountry scats 
scat_wild <- scat_by_site[1,2] 
print(scat_wild)

# morphometric differences by site 
## weights by site type 
## vehicle-access scats 
wght_mean_vehicle <- scat_by_site[2,3] 
wght_sd_vehicle <- scat_by_site[2,4] 
paste(round(wght_mean_vehicle,2), "+/-", round(wght_sd_vehicle,2))

## wilderness scats 
wght_mean_wild <- scat_by_site[1,3] 
wght_sd_wild <- scat_by_site[1,4] 
paste(round(wght_mean_wild,2), "+/-", round(wght_sd_wild,2))

## mann whitney U test between site types 
wilcox.test(wetweight_grams ~ vehicle_access, data=scat) 

## diameter by site type
### vehicle access sites
diam_mean_vehicle <- scat_by_site[2,5] 
diam_sd_vehicle <- scat_by_site[2,6] 
paste(round(diam_mean_vehicle,2), "+/-", round(diam_sd_vehicle,2))

### wilderness scats 
diam_mean_wild <- scat_by_site[1,5] 
diam_sd_wild <- scat_by_site[1,6] 
paste(round(diam_mean_wild,2), "+/-", round(diam_sd_wild,2))

## mann whitney U test between site types 
wilcox.test(diameter_mm ~ vehicle_access, data=scat) 

# morphometric differences by plastic 
# calculate grouped statistics for scat 
scat_by_plasticYN <- scat %>% 
  group_by(plastic_presence) %>% 
  summarise(n_scat = n_distinct(segment_id),
            weight_mean = mean(wetweight_grams),
            weight_sd = sd(wetweight_grams),
            diameter_mean = mean(diameter_mm), 
            diameter_sd = sd(diameter_mm))

# of unique scat samples by plastic dection
## scats with plastic  
scat_Yplastic <- scat_by_plasticYN[2,2] 
print(scat_Yplastic)

## scats without plastic
scat_Nplastic <- scat_by_plasticYN[1,2] 
print(scat_Nplastic)

## percent of scat with plastic 
round(scat_Yplastic / (scat_Yplastic + scat_Nplastic) *100,2)

## weights by plastic content 
## plastic scats
wght_mean_Yplastic <- scat_by_plasticYN[2,3] 
wght_sd_Yplastic <- scat_by_plasticYN[2,4] 
paste(round(wght_mean_Yplastic,2), "+/-", round(wght_sd_Yplastic,2))

## plastic-free scat  
wght_mean_Nplastic <- scat_by_plasticYN[1,3] 
wght_sd_Nplastic <- scat_by_plasticYN[1,4] 
paste(round(wght_mean_Nplastic,2), "+/-", round(wght_sd_Nplastic ,2))

## WEIGHT mann whitney U test between plastic content
wilcox.test(wetweight_grams ~ plastic_presence, data=scat) 

## diameter by plastic content
### scats with plastic 
diam_mean_Yplastic <- scat_by_plasticYN[2,5] 
diam_sd_Yplastic <- scat_by_plasticYN[2,6] 
paste(round(diam_mean_Yplastic,2), "+/-", round(diam_sd_Yplastic,2))

### plastic-free scat  
diam_mean_Nplastic <- scat_by_plasticYN[1,5] 
diam_sd_Nplastic <- scat_by_plasticYN[1,6] 
paste(round(diam_mean_Nplastic,2), "+/-", round(diam_sd_Nplastic,2))

## mann whitney U test between site types 
wilcox.test(diameter_mm ~ plastic_presence, data=scat)

scat_summary <- scat %>% group_by(site_area, month) %>% 
  summarise(n_scat = n_distinct(segment_id))
scat_summary <- scat_summary %>% 
  pivot_wider(names_from = month, values_from = n_scat)
View(scat_summary)

## RFW for plastic content

# load data 
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)
plastic <- read.csv("data/4_plastic_data.csv", strip.white = T)
plastic_type <- dplyr::select(plastic, fragment_id, plastic_type)

# filter to only plastics
plastic_frags <- frags %>%
  dplyr::filter(fragment_type == "Plastic")

# merge dataframes
plastic_frags <- left_join(plastic_frags, scat, by= c("site_id", "segment_id"))
plastic_frags <- left_join(plastic_frags, site, by= "site_id")
plastic_frags <- left_join(plastic_frags, plastic_type, by= "fragment_id") 

# calculate % relative frequency by weight of plastics 
plastic_frags$plasticRFW <- round((plastic_frags$fragment_weight / plastic_frags$dryweight_grams)*100, 2)
wilcox.test(plasticRFW  ~ vehicle_access, data=plastic_frags)

plastic_frags_sitetype <- plastic_frags %>%
  group_by(vehicle_access) %>%
  summarise(n_fragments = n(), 
            RFW_mean = mean(plasticRFW),
            RFW_sd = sd(plasticRFW))

# frequency of plastic types by site type 

