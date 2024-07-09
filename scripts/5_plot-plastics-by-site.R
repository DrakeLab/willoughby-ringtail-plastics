# plot morphometrics by site

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

# calculate relative frequency by weight of plastics 
plastic_frags$plasticRFW <- plastic_frags$fragment_weight / plastic_frags$dryweight_grams

plasticRFW_by_site <- ggplot(plastic_frags, aes(x =vehicle_access , y = plasticRFW)) + 
  geom_boxplot() + 
  geom_point(aes(shape = plastic_type), position = position_jitterdodge(0.5)) + 
  scale_shape_discrete(labels = c("polyethylene", "polypropylene", "polystyrene")) +
 # guides(shape=guide_legend("Plastic Type")) + 
  labs(x = "Site Type", y = "Relative Frequency by Weight (grams)", 
       shape = "plastic type") + 
  theme(axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        axis.title=element_text(size=30,face="bold")) + 
  theme_classic()
  
png(filename="figures/Figure3.png", width = 4, height = 4, units = 'in', res = 300)
plasticRFW_by_site 
dev.off()
