# create data visualization for fragment co-occurrence 

# setup library 
library(circlize) # tranforming lists and matrices
library(igraph) # network analysis
library(tidyverse) # data wrangling

# read in data 
scat <- read.csv("data/2_scat_data.csv", strip.white = T)
frags <- read.csv("data/3_fragment_data.csv", strip.white = T)
frags <- dplyr::filter(frags, fragment_type != "Rocks") # remove rocks 

# calculate diet richness 
scat$diet_richness <- as.numeric(scat$anthropogenic) +
  as.numeric(scat$invertebrate)  + 
  as.numeric(scat$plant) +
  as.numeric(scat$plastic) + 
  as.numeric(scat$vertebrate)

# plot histogram of diet richness 
dc_hist <- ggplot(scat, aes(x=diet_richness)) + 
  geom_histogram(binwidth=1) + 
  theme_classic() + 
  labs(x = "count of unique diet types", y = "scat samples") + 
  geom_text(stat= "count", aes(label=..count..), vjust=-1, size=3) +
  ylim(c(0,40))

# create a full co-occurence network 
seg_frag_1 <- dplyr::select(frags, unique_site, sample_id, fragment_type) # select fewer columns for clarity
seg_frag_2 <- dplyr::select(frags, sample_id, fragment_type) # select fewer columns for clarity
seg_frag <- full_join(seg_frag_1, seg_frag_2, by = "sample_id", relationship = "many-to-many") # merge them 

# make a matrix 
seg_frag_edges <- seg_frag %>% 
  group_by(seg_frag$fragment_type.x, seg_frag$fragment_type.y) %>% 
  summarise(n_distinct(sample_id))
seg_frag_matrix <- adjacencyList2Matrix(seg_frag_edges)

# calculate the frequency scats with one diet category per diet category 
solos <- dplyr::filter(scat, diet_richness == 1)

print(nrow(solos)) # count of single diet categories 

anthro_solos <- sum(solos$anthropogenic)
inv_solos <- sum(solos$invertebrate)
plant_solos <- sum(solos$plant)
plastic_solos <- sum(solos$plastic)
vert_solos <- sum(solos$vertebrate)

# make the diagonal to be the solo detections
seg_frag_matrix[1,1] <- anthro_solos # anthropogenic solos 
seg_frag_matrix[2,2] <- inv_solos # invertebrates solos 
seg_frag_matrix[3,3] <- plant_solos # plant solos 
seg_frag_matrix[4,4] <- plastic_solos # plastic solos 
seg_frag_matrix[5,5] <- vert_solos # vertebrates solos 

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

png(filename="figures/supp/FigureS1A.png", width = 6.5, height = 4, units = 'in', res = 300)
dc_hist
dev.off()

png(filename="figures/supp/Figure1B.png", width = 10.5, height = 8, units = 'in', res = 300)
cf_plot 
dev.off()

# Test for co-occurrence with vertebrates 
# filter to plastic scat 
pv_compare <- scat %>% 
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
pa_compare <- scat %>% 
  group_by(plastic) %>% 
  summarise(scat_w_anthro = sum(as.numeric(anthropogenic)), 
            scats = n())
pa_compare$scats <- pa_compare$scats - pa_compare$scat_w_anthro
names(pa_compare)[names(pa_compare) == 'scats'] <- 'scat_no_anthro'
pa_compare <- pa_compare %>% 
  remove_rownames %>% 
  column_to_rownames(var="plastic")
print(chisq.test(pa_compare,correct = FALSE))
