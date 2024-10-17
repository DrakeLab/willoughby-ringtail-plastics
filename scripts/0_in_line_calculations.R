# Calculation of In-line values 

# set up libraries 
library(dplyr) # data wrangling

# read in data 
site <- read.csv(file = "data/1_site_data.csv")
scat <- read.csv(file = "data/2_scat_data.csv")
scat <- left_join(scat, site, by = "site_id") # link scat and collection site data 
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)
# plastic <- read.csv("data/4_plastic_data.csv", strip.white = T)

# filter to only ringtail scats
rscat <- scat %>% 
  dplyr::filter(ringtail_confirmed== "yes")

### ABSTRACT VALUES ###

# of unique scat samples
scat_count <- n_distinct(rscat$segment_id)
print(scat_count) # ABSTRACT 

# calculate any anthropogenic items in the scat
rscat$any_anthro <- ifelse(rscat$anthropogenic == 1, "yes", 
                           ifelse(rscat$plastic == 1, "yes", "no")) 
anthro_scat <- filter(rscat,any_anthro == "yes")
# count how many scats trash items 
nrow(anthro_scat) # ABSTRACT 
# calculate percentage of scats with trash 
round(nrow(anthro_scat) / nrow(rscat) *100, 2)  # ABSTRACT

# how many paper contaminants? 
frag_type_anthro <- left_join(anthro_scat, frags, by = "segment_id")  # ABSTRACT
frag_type_anthro <- filter(frag_type_anthro, fragment_type %in% c("Anthropogenic", "Plastic"))
paper_frags <- filter(frag_type_anthro, fragment_description %in% c("paper", "paper_wrapping")) 

# count the scats with paper fragments as percent 
print(round(n_distinct(paper_frags$segment_id)/nrow(rscat)*100),2) # ABSTRACT 

# count the scats with plastics as percent 
plastic_frags <-  filter(frag_type_anthro, fragment_description == "plastic")
print(nrow(plastic_frags)) # ABSTRACT
# frequency 
print(n_distinct(plastic_frags$segment_id)) # ABSTRACT
# percent 
print(round(n_distinct(plastic_frags$segment_id)/nrow(rscat)*100),2) # ABSTRACT

### METHODS ###

### RESULTS ###

# break down of unique scat samples by tourism level
table(rscat$tourism_level) # RESULTS P1 
# break down of unique scat samples by site 
table(rscat$site_area) # RESULTS P1 
# break down of unique scat samples by season 
table(rscat$month) # RESULTS P1

# calculate scat morphometrics by tourism type 

## separate out the groups 
bc_scat <- rscat %>% 
  dplyr::filter(tourism_level == "backcountry")
fc_scat <- rscat %>% 
  dplyr::filter(tourism_level == "frontcountry")

## Each group is >30, but we will still do a visual inspection for normality 
## test  each group diameter 
car::qqPlot(as.numeric(bc_scat$diameter_mm)) # some outliers but ok
car::qqPlot(as.numeric(fc_scat$diameter_mm))

## test normality of each group wet weight
car::qqPlot(as.numeric(bc_scat$wetweight_grams)) # some outliers but ok
car::qqPlot(as.numeric(fc_scat$wetweight_grams))

# T Test to compare groups 
## diameter
t.test(as.numeric(diameter_mm) ~ tourism_level, data = rscat)
## wet weight 
t.test(as.numeric(wetweight_grams) ~ tourism_level, data = rscat)

scat_by_site <- rscat %>% 
  group_by(vehicle_access) %>% 
  summarise(n_scat = n(),
            weight_mean = mean(wetweight_grams),
            weight_sd = sd(wetweight_grams),
            diameter_mean = mean(as.numeric(diameter_mm)), 
            diameter_sd = sd(as.numeric(diameter_mm)))

scat_fc <- scat_by_site[2,2] ## frontcountry scats 
scat_bc <- scat_by_site[1,2] ## backcountry scats 

# morphometric differences by site 
## weights by site type 
## frontcountry scats 
wght_mean_fc <- scat_by_site[2,3] 
wght_sd_fc <- scat_by_site[2,4] 
paste(round(wght_mean_fc,2), "+/-", round(wght_sd_fc,2)) # SUP TABLE 1 

## backcountry scats 
wght_mean_bc <- scat_by_site[1,3] 
wght_sd_bc <- scat_by_site[1,4] 
paste(round(wght_mean_bc,2), "+/-", round(wght_sd_bc,2)) # SUP TABLE 1 

# perform statistics for Supplement 
## mann whitney U test between site types 
wilcox.test(wetweight_grams ~ tourism_level, data=rscat) # SUP TABLE 1 

## diameter by site type
### frontcountry sites
diam_mean_fc <- scat_by_site[2,5] 
diam_sd_fc <- scat_by_site[2,6] 
paste(round(diam_mean_fc,2), "+/-", round(diam_sd_fc,2)) # SUP TABLE 1 

### backcountry scats 
diam_mean_bc <- scat_by_site[1,5] 
diam_sd_bc <- scat_by_site[1,6] 
paste(round(diam_mean_bc,2), "+/-", round(diam_sd_bc,2)) # SUP TABLE 1 

## mann whitney U test between site types 
wilcox.test(as.numeric(diameter_mm) ~ vehicle_access, data=rscat) # SUP TABLE 1 

# prepare scat morphometrics by anthropogenic content
rscat_by_anthropogenic <- rscat %>% 
  group_by(any_anthro) %>% 
  summarise(n_scat = n(),
            weight_mean = mean(wetweight_grams),
            weight_sd = sd(wetweight_grams),
            diameter_mean = mean(as.numeric(diameter_mm)), 
            diameter_sd = sd(as.numeric(diameter_mm)))

## mann whitney U test of diameter by anthropogenic content
wilcox.test(as.numeric(diameter_mm) ~ any_anthro, data=rscat) # SUP TABLE 1 
t.test(as.numeric(diameter_mm)~any_anthro, data=rscat)

## of weight 
wilcox.test(as.numeric(wetweight_grams) ~ any_anthro, data=rscat) # SUP TABLE 1 
t.test(as.numeric(wetweight_grams)~any_anthro, data=rscat)

## diameter by contamination
### anthropogenic scat
diam_mean_anthro <- rscat_by_anthropogenic [2,5] 
diam_sd_anthro <- rscat_by_anthropogenic [2,6] 
paste(round(diam_mean_anthro,2), "+/-", round(diam_sd_anthro,2)) # SUP TABLE 1 

### uncontaminated scats 
diam_mean_uc <- rscat_by_anthropogenic[1,5] 
diam_sd_uc <- rscat_by_anthropogenic[1,6] 
paste(round(diam_mean_uc,2), "+/-", round(diam_sd_uc,2)) # SUP TABLE 1 

## weight by contamination
### anthropogenic scat
wght_mean_anthro <- rscat_by_anthropogenic [2,3] 
wght_sd_anthro <- rscat_by_anthropogenic [2,4] 
paste(round(wght_mean_anthro,2), "+/-", round(wght_sd_anthro,2)) # SUP TABLE 1 

### uncontaminated scats 
wght_mean_uc <- rscat_by_anthropogenic[1,3] 
wght_sd_uc <- rscat_by_anthropogenic[1,4] 
paste(round(wght_mean_uc,2), "+/-", round(wght_sd_uc,2)) # SUP TABLE 1 

# morphometric differences by plastic 
# calculate grouped statistics for scat 
scat_by_plasticYN <- rscat %>% 
  group_by(plastic) %>% 
  summarise(n_scat = n_distinct(segment_id),
            weight_mean = mean(wetweight_grams),
            weight_sd = sd(wetweight_grams),
            diameter_mean = mean(as.numeric(diameter_mm)), 
            diameter_sd = sd(as.numeric(diameter_mm)))

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
wilcox.test(wetweight_grams ~ plastic, data=rscat) 

## diameter by plastic content
### scats with plastic 
diam_mean_Yplastic <- scat_by_plasticYN[2,5] 
diam_sd_Yplastic <- scat_by_plasticYN[2,6] 
paste(round(diam_mean_Yplastic,2), "+/-", round(diam_sd_Yplastic,2))

### plastic-free scat  
diam_mean_Nplastic <- scat_by_plasticYN[1,5] 
diam_sd_Nplastic <- scat_by_plasticYN[1,6] 
paste(round(diam_mean_Nplastic,2), "+/-", round(diam_sd_Nplastic,2))

## mann whitney U test between plastic contamination
wilcox.test(as.numeric(diameter_mm) ~ plastic, data=rscat)

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

