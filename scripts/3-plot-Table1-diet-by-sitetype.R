# raw proportions of diet categories for %FO 

# load libraries
library(tidyverse)

# load data 
scat <- read.csv(file = "data/2_scat_data.csv")
rscat <- dplyr::filter(scat, ringtail_confirmed == "yes") # filter to only ringtail confirmed scat 

# transform to numeric 
rscat$plastic <- as.numeric(rscat$plastic)
rscat$anthropogenic <- as.numeric(rscat$anthropogenic)
rscat$invertebrate <- as.numeric(rscat$invertebrate)
rscat$vertebrate <- as.numeric(rscat$vertebrate)
rscat$plant <- as.numeric(rscat$plant)
rscat$inorganic <- as.numeric(rscat$inorganic)

# calculate diet richness 
rscat$diet_richness <- rscat$plastic + 
  rscat$plant + 
  rscat$vertebrate + 
  rscat$invertebrate + 
  rscat$anthropogenic + 
  rscat$inorganic
single_scats <- dplyr::filter(rscat, diet_richness == 1)

# filter to site type
bc_scats <- dplyr::filter(rscat, tourism_level == "backcountry") 
bc_dcat <- bc_scats[,c(12:17)]
bc_dcat <- as.data.frame(colSums(bc_dcat))
names(bc_dcat)[names(bc_dcat) == 'colSums(bc_dcat)'] <- 'n_scat' # frequency
bc_dcat$FO <- bc_dcat$n_scat/nrow(bc_scats) # frequency of occurrence
bc_dcat$pct_FO <- round(bc_dcat$FO*100, 2) # percent frquency of occurrence 
bc_dcat$sd <- sqrt((1-(bc_dcat$FO/nrow(bc_scats)))*(bc_dcat$FO/nrow(bc_scats))) # standard deviation
bc_dcat$se <- bc_dcat$sd / sqrt(nrow(bc_scats)) # standard error
bc_dcat$pct_se <- round(bc_dcat$se *100,2) # transform for percent
bc_dcat
# FRONTCOUNTRY SCATS 
fc_scats <- dplyr::filter(rscat , tourism_level == "frontcountry")
fc_dcat <- fc_scats[,c(12:17)]
fc_dcat <- as.data.frame(colSums(fc_dcat))
names(fc_dcat)[names(fc_dcat) == 'colSums(fc_dcat)'] <- 'n_scat'

## calculate the values 
fc_dcat$FO <- fc_dcat$n_scat/nrow(fc_scats)
fc_dcat$pct_FO <- round(fc_dcat$FO*100, 2)
fc_dcat$sd <- sqrt((1-(fc_dcat$FO/nrow(fc_scats)))*(fc_dcat$FO/nrow(fc_scats))) # standard deviation
fc_dcat$se <- fc_dcat$sd / sqrt(nrow(fc_scats)) # standard error + tranformation for percetn 
fc_dcat$pct_se <- round(fc_dcat$se *100,2)
fc_dcat

## RESULTS 
# compare plants between site types 
plant_data <- rscat %>% 
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
vert_data <- rscat %>% 
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
invert_data <- rscat %>% 
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
plastic_data <- rscat %>% 
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
anthro_data <- rscat %>% 
  group_by(tourism_level) %>% 
  summarise(scat_w_anthro = sum(anthropogenic), 
            scats = n())
anthro_data$scats <- anthro_data$scats - anthro_data$scat_w_anthro
names(anthro_data)[names(anthro_data) == 'scats'] <- 'scat_no_anthro'
anthro_data <- anthro_data %>% 
  remove_rownames %>% 
  column_to_rownames(var="tourism_level")
print(chisq.test(anthro_data, correct = FALSE))