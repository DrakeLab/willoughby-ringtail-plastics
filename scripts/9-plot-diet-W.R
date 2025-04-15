# plot diet RFW by site

# load data 
site <- read.csv("data/1_site_data.csv", strip.white = T)
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)

# merge dataframes
frags <- left_join(frags, scat, by= c("site_id", "segment_id"))

# calculate percent weight of plastics 
frags$`%W` <- frags$fragment_weight / as.numeric(frags$dryweight_grams)
frags$`%W` <- round(frags$`%W`  *100,2)

# calculate relative frequency by weight of plastics 

# plot Figure Supp diet RFW by site 
pctW_by_site <- ggplot(frags, aes(x =fragment_type , y =`%W`, fill = tourism_level)) + 
  geom_boxplot() +
 # facet_wrap(~tourism_level) + 
  scale_fill_manual(values=c("#A9C5A0", "#ADA9B2")) +
  geom_jitter() + 
  scale_y_continuous(breaks = c(0,15,30,45,60), limits = c(0,60)) + 
  labs(x = "Site Area", y = "Relative Frequency by Weight (%)") + 
  theme(axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        axis.title=element_text(size=30,face="bold")) + 
  theme_classic()

png(filename="figures/FigureSX.png", width = 4, height = 4, units = 'in', res = 300)
pctW_by_site 
dev.off()

# perform statistical tests 
# vertebrates
t.test(pct_W ~ tourism_level, data = frags[frags$fragment_type == "Vertebrate",])
# invertebrates
t.test(pct_W ~ tourism_level, data = frags[frags$fragment_type == "Invertebrate",])
# plant
t.test(pct_W ~ tourism_level, data = frags[frags$fragment_type == "Plant",])
# anthropogenic
t.test(pct_W ~ tourism_level, data = frags[frags$fragment_type == "Anthropogenic",])


