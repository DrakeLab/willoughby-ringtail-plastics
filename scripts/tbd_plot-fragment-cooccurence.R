# create network of fragment co-occurrence 

# setup library 
library(circlize) # tranforming lists and matrices
library(igraph) # network analysis

# read in data 
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)

# calculate diet richness 
scat$diet_richness <- ifelse(scat$anthropogenic_presence == 'yes', 1,0) +
  ifelse(scat$invertebrate_presence == 'yes', 1,0)   + 
  ifelse(scat$plant_presence == 'yes',  1,0) +
  ifelse(scat$plastic_presence == 'yes', 1,0) + 
  ifelse(scat$vertebrate_presence == 'yes', 1,0)  

# plot histogram of diet richness 
dc_hist <- ggplot(scat, aes(x=diet_richness)) + 
  geom_histogram(binwidth=1) + 
  theme_classic() + 
  labs(x = "count of unique diet types", y = "scat segments") 

seg_frag_1 <- dplyr::select(frags, site_id, segment_id, fragment_type)
seg_frag_2 <- dplyr::select(frags, segment_id, fragment_type)

seg_frag <- full_join(seg_frag_1, seg_frag_2, by = "segment_id")

# create a full co-occurence network 
seg_frag_edges <- seg_frag %>% 
  group_by(seg_frag$fragment_type.x, seg_frag$fragment_type.y) %>% 
  summarise(n_distinct(segment_id))
seg_frag_matrix <- adjacencyList2Matrix(seg_frag_edges)

cat_net <- graph_from_adjacency_matrix(seg_frag_matrix,
  mode = "upper",
  weighted = TRUE,
  diag = FALSE,
  add.colnames = NULL,
  add.rownames = NA
)

#set seed so always get same network 
set.seed(1997)
# plot 
plot.igraph(cat_net, 
            edge.width=0.5*E(cat_net)$weight,
            vertex.size =1.5*diag(seg_frag_matrix), 
            vertex.label.color="black",
            vertex.color = NA, 
            layout=layout.fruchterman.reingold)

png(filename="figures/Figure2A.png", width = 6.5, height = 4, units = 'in', res = 300)
dc_hist
dev.off()

png(filename="figures/Figure2B.png", width = 10.5, height = 8, units = 'in', res = 300)
set.seed(1997)
plot.igraph(cat_net, 
            edge.width=0.5*E(cat_net)$weight,
            vertex.size =1.5*diag(seg_frag_matrix), 
            vertex.label.color="black",
            vertex.color = NA, 
            layout=layout.fruchterman.reingold)
dev.off()

# create a network from vehicle access sites 
# seg_frag2 <- left_join(seg_frag, site, by = "site_id")
# seg_frag_Vedges <- seg_frag2 %>% 
#   group_by(vehicle_access, fragment_type.x, fragment_type.y) %>% 
#   summarise(segments = n_distinct(segment_id))
# seg_frag_Vedges$color <-ifelse(seg_frag_Vedges$vehicle_access == "no","black", "blue")
# seg_frag_Vedges$vehicle_access <- NULL

# get rid of loops 
# seg_frag_Vedges$same <- ifelse(seg_frag_Vedges$fragment_type.x == seg_frag_Vedges$fragment_type.y, "yes", "no")
# seg_frag_Vedges <- seg_frag_Vedges %>% 
#   dplyr::filter(same == "no")
# seg_frag_Vedges$same <- NULL
# seg_frag_Vedges <- seg_frag_Vedges[c(1:4,6:8,11:12,16,25,29:30,33:35,37:40),]
# seg_frag_Vedges$int <- paste(seg_frag_Vedges$fragment_type.x, seg_frag_Vedges$fragment_type.y, sep = "-")

# remove all non-alphabetic combos 
# seg_frag_Vedges <- seg_frag_Vedges %>%
#  dplyr::filter(int %in% 
#                   c("Anthropogenic-Invertebrate", 
#                    "Anthropogenic-Plant", 
#                  "Anthropogenic-Plastic", 
#                     "Anthropogenic-Vertebrate", 
#                     "Invertebrate-Plant", 
#                   "Invertebrate-Plastic", 
#                   "Invertebrate-Vertebrate", 
#                     "Plant-Plastic", 
#                     "Plant-Vertebrate", 
#                     "Plastic-Vertebrate"))
# seg_frag_Vedges$int <- NULL
# cat_net_by_site <-graph_from_data_frame(seg_frag_Vedges)
# set.seed(1997)

# E(cat_net_by_site)$curved <- 0.2
# E(cat_net_by_site)$arrow.mode="-"

# plot.igraph(cat_net_by_site, 
#             edge.width=seg_frag_Vedges$segments,
#             edge.color = seg_frag_Vedges$color,
#             vertex.label.color="black",
#             vertex.color = NA, 
#             layout=layout_nicely)