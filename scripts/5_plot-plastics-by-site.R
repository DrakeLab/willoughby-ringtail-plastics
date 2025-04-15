# plot morphometrics by site

# load data 
plastic <- read.csv("data/4_plastic_data.csv", strip.white = T)
plastic_type <- dplyr::select(plastic, fragment_id, plastic_type)

# working from plastics sheet
plastic$plasticRFW <- plastic$fragment_weight / as.numeric(plastic$dry_weight)
plastic$pct_plasticRFW <- round(plastic$plasticRFW *100,2)

plasticRFW_by_site <- ggplot(plastic, aes(x =tourism_level , y =pct_plasticRFW)) + 
  geom_boxplot() + 
  geom_point(aes(shape = plastic_type, color = plastic_type), position = position_jitterdodge(0.5)) + 
  scale_shape_manual(name = "plastic polymer", 
                     values = c(15,17,18), 
                     labels = c("polyethylene", "polypropylene", "polystyrene")) + 
  scale_color_manual(name = "plastic polymer",
                     values = c("blue", "darkorange", "darkgrey"), 
                     labels = c("polyethylene", "polypropylene", "polystyrene")) +
  scale_y_continuous(breaks = c(0,15,30,45,60), limits = c(0,60)) + 
  labs(x = "Site Area", y = "Weight (%)") + 
  theme(axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        axis.title=element_text(size=30,face="bold")) + 
  theme_classic()
  
png(filename="figures/Figure3.png", width = 4, height = 4, units = 'in', res = 300)
plasticRFW_by_site 
dev.off()
