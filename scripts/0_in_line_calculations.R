# Calculation of In-line values 

# set up libraries 
library(dplyr) # data wrangling

# read in data 
site <- read.csv(file = "data/0_site_data.csv")
site_traits <- read.csv(file = "data/1_site_traits.csv")
scat <- read.csv(file = "data/2_scat_data.csv")
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)
# plastic <- read.csv("data/4_plastic_data.csv", strip.white = T)
cam <- read.csv(file = "data/5_camera_data.csv")

### ABSTRACT VALUES ###
scat_count <- n_distinct(scat$sample_id) 

# of unique scat samples
print(scat_count) # ABSTRACT 

# calculate scat count by site type 
table(scat$tourism_level)

# calculate any anthropogenic items in the scat
scat$any_anthro <- ifelse(scat$anthropogenic == 1, "yes", 
                           ifelse(scat$plastic == 1, "yes", "no")) 
anthro_scat <- filter(scat,any_anthro == "yes")
# count how many scats trash items 
nrow(anthro_scat) # ABSTRACT 
# calculate percentage of scats with trash 
round(nrow(anthro_scat) / nrow(scat) *100, 2)  # ABSTRACT

# how many paper contaminants? 
frag_type_anthro <- left_join(anthro_scat, frags, by = "sample_id")  # ABSTRACT
frag_type_anthro <- filter(frag_type_anthro, fragment_type %in% c("Anthropogenic", "Plastic"))
paper_frags <- filter(frag_type_anthro, fragment_description %in% c("paper", "paper_wrapping")) 

# count the scats with paper fragments as percent 
print(round(n_distinct(paper_frags$sample_id)/nrow(scat)*100),2) # ABSTRACT 

# count the scats with plastics as percent 
plastic_frags <-  filter(frag_type_anthro, fragment_description == "plastic")
# plastic frequency
nrow(plastic_frags)
# unique scat count
print(n_distinct(plastic_frags$sample_id)) # ABSTRACT
# percent 
print(round(n_distinct(plastic_frags$sample_id)/nrow(scat)*100),2) # ABSTRACT

### METHODS ###
# calculate the total value of weighed fragments
cat_material <- dplyr::filter(frags, fragment_type != "Rocks")
cat_material_wght <- sum(cat_material$fragment_weight)
print(cat_material_wght)

### RESULTS ###

# calculate trap days
trap_days <- sum(cam$trap_days) 
print(trap_days)

# break down of unique scat samples by site 
table(scat$tourism_level) # RESULTS P2 
table(scat$month) # RESULTS P2

# break down of unique scat samples by season 
table(scat$site_area) # RESULTS P1

scat_fc <- scat_by_site[2,2] ## frontcountry scats 
scat_bc <- scat_by_site[1,2] ## backcountry scats 

# break down of unique scat samples by scat depositor identity
table(scat$scat_identity)

# how many categorized fragments? This does not include rocks
nrow(cat_material)

# Calculate each category
sum(scat$plant)/nrow(scat) # plant 
sum(scat$vertebrate)/nrow(scat) # animal
nrow(anthro_scat)/nrow(scat) # contaminants
sum(scat$invertebrate)/nrow(scat) # invertebrate

# calculate frequency of more than one diet category
solos <- dplyr::filter(scat, diet_richness == 1)
print((nrow(scat)-nrow(solos)) / nrow(scat)*100)

# compare the diet richness between site types 
t_test_result <- t.test(diet_richness ~ tourism_level, data = scat)
sd_back <- sd(scat$diet_richness[scat$tourism_level == "backcountry"])
sd_front <- sd(scat$diet_richness[scat$tourism_level == "frontcountry"])

## RESULTS 
# compare plants between site types 
plant_data <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(scat_w_plant = sum(plant), 
            scats = n())
plant_data$scats <- plant_data$scats - plant_data$scat_w_plant 
names(plant_data)[names(plant_data) == 'scats'] <- 'scat_no_plant'
plant_data <- plant_data %>% 
  remove_rownames %>% 
  column_to_rownames(var="tourism_level")
print(chisq.test(plant_data, correct = FALSE))

# compare vertebrates between site types
vert_data <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(scat_w_vert = sum(vertebrate), 
            scats = n())
vert_data$scats <- vert_data$scats - vert_data$scat_w_vert 
names(vert_data)[names(vert_data) == 'scats'] <- 'scat_no_vert'
vert_data <- vert_data %>% 
  remove_rownames %>% 
  column_to_rownames(var="tourism_level")
print(chisq.test(vert_data, correct = FALSE))

# compare invertebrates between site types
invert_data <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(scat_w_invert = sum(invertebrate), 
            scats = n())
invert_data$scats <- invert_data$scats - invert_data$scat_w_invert 
names(invert_data)[names(invert_data) == 'scats'] <- 'scat_no_invert'
invert_data <- invert_data %>% 
  remove_rownames %>% 
  column_to_rownames(var="tourism_level")
print(chisq.test(invert_data, correct = FALSE))

# compare plastic between site types
plastic_data <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(scat_w_plastic = sum(plastic), 
            scats = n())
plastic_data$scats <- plastic_data$scats - plastic_data$scat_w_plastic
names(plastic_data)[names(plastic_data) == 'scats'] <- 'scat_no_plastic'
plastic_data <- plastic_data %>% 
  remove_rownames %>% 
  column_to_rownames(var="tourism_level")
print(chisq.test(plastic_data))

# compare anthropogenic between site types
anthro_data <- scat %>% 
  group_by(tourism_level) %>% 
  summarise(scat_w_anthro = sum(anthropogenic), 
            scats = n())
anthro_data$scats <- anthro_data$scats - anthro_data$scat_w_anthro
names(anthro_data)[names(anthro_data) == 'scats'] <- 'scat_no_anthro'
anthro_data <- anthro_data %>% 
  remove_rownames %>% 
  column_to_rownames(var="tourism_level")
print(chisq.test(anthro_data, correct = FALSE))

# calculate fragment weights by site type 
frags <-  frags %>% 
  dplyr::filter(fragment_type != "Rocks")
scat_fw <- left_join(scat, frags, by = "sample_id")

scat_fw_sum <- scat_fw %>% 
  group_by(tourism_level) %>% 
  summarise(weight = sum(fragment_weight))

scat_fw$pct_W <- scat_fw$fragment_weight / scat_fw$dryweight_grams*100 

# compare % weight of Plastics in front country vs backcountry 
plastics <- scat_fw %>% 
  dplyr::filter(fragment_type == "Plastic") 

## first check normality 
shapiro.test(plastics$pct_W[plastics$tourism_level == "frontcountry"])
shapiro.test(plastics$pct_W[plastics$tourism_level == "backcountry"]) 

# failed normality: use Mann Whitney U 
wilcox.test(pct_W ~ tourism_level, data = plastics)

plastic_sum <- plastics %>%
  group_by(tourism_level) %>%
  summarise(
    mean = mean(pct_W, na.rm = TRUE),
    sd = sd(pct_W, na.rm = TRUE),
    n = sum(!is.na(pct_W)),
    se = sd / sqrt(n)
  )
 
## DELETE ALL BELOW?

# of unique scat samples by plastic detection
## scats with plastic  
scat_Yplastic <- scat_by_plasticYN[2,2] 
print(scat_Yplastic)

## scats without plastic
scat_Nplastic <- scat_by_plasticYN[1,2] 
print(scat_Nplastic)

## percent of scat with plastic 
round(scat_Yplastic / (scat_Yplastic + scat_Nplastic) *100,2)

## mann whitney U test between plastic contamination
wilcox.test(as.numeric(diameter_mm) ~ plastic, data=rscat)

scat_summary <- scat %>% group_by(site_area, month) %>% 
  summarise(n_scat = n_distinct(segment_id))
scat_summary <- scat_summary %>% 
  pivot_wider(names_from = month, values_from = n_scat)
View(scat_summary)

## RFW for plastic content

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

