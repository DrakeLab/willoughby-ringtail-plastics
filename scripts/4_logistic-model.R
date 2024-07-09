# model to predict plastic presence 
library(lubridate)


# load data 
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
scat <- left_join(scat, site, by = "site_id")

scat$plastic_YN <- ifelse(scat$plastic_presence == "yes", 1, 0)

plasticYN_model <- glm(plastic_YN ~ vehicle_access + month + anthropogenic_presence + 
  invertebrate_presence + vertebrate_presence + plant_presence + diameter_mm, 
  family=binomial(link=logit), 
  data = scat) 
summary(plasticYN_model)

plasticYN_model2 <- glm(plastic_YN ~ vehicle_access, 
                       family=binomial(link=logit), 
                       data = scat) 
summary(plasticYN_model2)