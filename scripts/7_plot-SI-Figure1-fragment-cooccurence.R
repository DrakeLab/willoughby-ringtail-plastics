# create data visualization for fragment co-occurrence 

# setup library 
library(circlize) # tranforming lists and matrices
library(igraph) # network analysis

# read in data 
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)

# filter to only ringtail scats
rscat <- scat %>% 
  dplyr::filter(ringtail_confirmed== "yes")

# calculate diet richness 
rscat$diet_richness <- as.numeric(rscat$anthropogenic) +
  as.numeric(rscat$invertebrate)  + 
  as.numeric(rscat$plant) +
  as.numeric(rscat$plastic) + 
  as.numeric(rscat$vertebrate) + 
  as.numeric(rscat$inorganic)

# plot histogram of diet richness 
dc_hist <- ggplot(rscat, aes(x=diet_richness)) + 
  geom_histogram(binwidth=1) + 
  theme_classic() + 
  labs(x = "count of unique diet types", y = "scat segments") + 
  geom_text(stat= "count", aes(label=..count..), vjust=-1, size=3) +
  ylim(c(0,40))

## save the plot as png
png(filename="figures/supp/SFig1.png", width = 4, height = 4, unit = 'in', res = 300)
par(bg=NA)
dc_hist
dev.off()

# create a full co-occurence network 
seg_frag_1 <- dplyr::select(frags, site_id, segment_id, fragment_type) # select fewer columns for clarity
seg_frag_2 <- dplyr::select(frags, segment_id, fragment_type) # select fewer columns for clarity
seg_frag <- full_join(seg_frag_1, seg_frag_2, by = "segment_id", relationship = "many-to-many") # merge them 

# make a matrix 
seg_frag_edges <- seg_frag %>% 
  group_by(seg_frag$fragment_type.x, seg_frag$fragment_type.y) %>% 
  summarise(n_distinct(segment_id))
seg_frag_matrix <- adjacencyList2Matrix(seg_frag_edges)

# make the diagonal to be the solo detections
seg_frag_matrix[1,1] <- 1 # anthropogenic solos 
seg_frag_matrix[2,2] <- 0 # invertebrates solos 
seg_frag_matrix[3,3] <- 0 # organic solos 
seg_frag_matrix[4,4] <- 9 # plant solos 
seg_frag_matrix[5,5] <- 0 # plastic solos 
seg_frag_matrix[6,6] <- 4 # vertebrates solos 


get_upper_tri <- function(CorMat){
  CorMat[upper.tri(CorMat, diag = )]<- NA
  return(CorMat)
}

co_frags <-seg_frag_matrix 
co_frags[upper.tri(co_frags)] <- NA # mask upper triangle

library(reshape2)
co_frags_long <- melt(co_frags) # make long format for ggplot

# Plot the heatmap
library(viridis) # for color scale 
# Plot the heatmap with reordered factors
cf_plot <- ggplot(co_frags_long, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("gray", viridis(10, direction = -1)),  # Gray for 0, viridis for 1 and above
    values = c(0, 1, max(co_frags_long$value, na.rm = TRUE)),  # Break at 1
    na.value = "white"
  ) +
  theme_minimal() +
  labs(x = "diet category", y = "diet category", fill = "co-occurrences") +
  coord_fixed()

# make a network 
# cat_net <- graph_from_adjacency_matrix(seg_frag_matrix,
#  mode = "upper",
#  weighted = TRUE,
#  diag = FALSE,
#  add.colnames = NULL,
#  add.rownames = NA
#)

#set seed so always get same network 
#set.seed(1997)
# plot 
#plot.igraph(cat_net, 
#            edge.width=0.5*E(cat_net)$weight,
#            vertex.size =1.5*diag(seg_frag_matrix), 
#            vertex.label.color="black",
#            vertex.color = NA, 
#            layout=layout.fruchterman.reingold)

png(filename="figures/supp/Figure2A.png", width = 6.5, height = 4, units = 'in', res = 300)
dc_hist
dev.off()

png(filename="figures/supp/Figure2B.png", width = 10.5, height = 8, units = 'in', res = 300)
#set.seed(1997)
#plot.igraph(cat_net, 
 #           edge.width=0.5*E(cat_net)$weight,
  #          vertex.size =1.5*diag(seg_frag_matrix), 
   #         vertex.label.color="black",
    #        vertex.color = NA, 
     #       layout=layout.fruchterman.reingold)
cf_plot 
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


# Test for co-occurence with vertebrates 
# filter to plastic scat 
pv_compare <- rscat %>% 
  group_by(plastic) %>% 
  summarise(scat_w_vert = sum(as.numeric(vertebrate)), 
            scats = n())
pv_compare$scats <- pv_compare$scats - pv_compare$scat_w_vert
names(pv_compare)[names(pv_compare) == 'scats'] <- 'scat_no_vert'
pv_compare <- pv_compare %>% 
  remove_rownames %>% 
  column_to_rownames(var="plastic")
print(chisq.test(pv_compare, correct = FALSE))

# Test for co-occurence with anthropogenic content
pa_compare <- rscat %>% 
  group_by(plastic) %>% 
  summarise(scat_w_anthro = sum(as.numeric(anthropogenic)), 
            scats = n())
pa_compare$scats <- pa_compare$scats - pa_compare$scat_w_anthro
names(pa_compare)[names(pa_compare) == 'scats'] <- 'scat_no_anthro'
pa_compare <- pa_compare %>% 
  remove_rownames %>% 
  column_to_rownames(var="plastic")
print(chisq.test(pa_compare,correct = FALSE))
