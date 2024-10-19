# load libraries
library(tidyverse)

site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv(file = "data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)

# Table S3: create a study-wide table of diet category metrics
table_S3 <- frags %>% 
  group_by(fragment_type) %>% 
  summarise(f = n_distinct(fragment_id), 
            FO = round(n_distinct(segment_id)/83*100,2),
            RFO = round((n_distinct(fragment_id) / 206)*100,2), 
            weight = sum(fragment_weight))
table_S3$RW <- round(table_S3$weight / sum(frags$fragment_weight)*100,2)
# Print Table S3
table_s3

# Table S4: create table for relative weight by site type 
frag_scat <- left_join(frags, scat, by = "segment_id")
# scat level RFW 
# frag_scat$rfw <- frag_scat$fragment_weight / as.numeric(frag_scat$dryweight_grams)

# population level RFW
table_s4 <- frag_scat %>% 
  group_by(tourism_level, fragment_type) %>% 
  summarise(f = n_distinct(fragment_id), 
            frag_sum_g = sum(fragment_weight), 
            scat_sum_g = sum(as.numeric(dryweight_grams)))
table_s4$site_RFW <- round((table_s4$frag_sum_g /table_s4$scat_sum_g)*100,2) # calculate percent relative weight 
# calculate standard error
table_s4$site_RFW_sd <- sqrt((table_s4$frag_sum_g *(table_s4$scat_sum_g - table_s4$frag_sum_g))/table_s4$scat_sum_g)
table_s4$site_RFW_se <- table_s4$site_RFW_sd / sqrt(table_s4$scat_sum_g) 
table_s4$pct_site_RFW_se <-round(table_s4$site_RFW_se *100,2)

# Print Table s4
table_s4