# merge all detections, filter to carnivore 
library(tidyverse)
library(igraph)
library(ggraph)
library(ggimage) # For PhyloPic silhouette images
library(cowplot) # graphing on top of each other

# load site data 
sites <- read.csv(file = "data/1_site_traits.csv")

# read camera data
camera_data <- read.csv(file = "data/5_camera_data.csv") # camera check events 

# read in identity data - this is camera data prefiltered to carnivores. Sites without carnivores will not show up. 
carnivores <- read.csv(file = "data/camera_data/carnivores_with_site-data_daily.csv")

# Ensure top_identity and vehicle_access are treated as factors
carnivores$top_identity <- as.factor(carnivores$top_identity)
carnivores <- carnivores%>%
  mutate(species = recode(top_identity,
                          "RINGTAIL" = "Ringtail",
                          "BOBCAT" = "Bobcat",
                          "GRAYFOX" = "Gray fox",
                          "COYOTE" = "Coyote",
                          "SPOTTEDSKUNK" = "Spotted skunk",
                          "STRIPEDSKUNK" = "Striped skunk",
                          "DOMESTICCAT" = "Domestic cat",
                          "RACCOON" = "Raccoon"
  ))
carnivores$vehicle_access <- as.factor(carnivores$vehicle_access)

# Recode vehicle_access values
carnivores <- carnivores %>%
  mutate(`tourism access` = recode(vehicle_access,
                            "yes" = "frontcountry",
                            "no" = "backcountry"))
carnivores$`tourism access` <- factor(carnivores$`tourism access`, levels = c("backcountry", "frontcountry"))

# summarise detection count by sites generally
carn_freq <- carnivores %>% 
  group_by(species) %>%
  summarise(visits = n(),
            sites = n_distinct(site_id))

# Summarize count by site type and complete missing combinations with 0s
carn_by_sitetype <- carnivores %>%
  group_by(species, `tourism access`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(species, `tourism access`, fill = list(count = 0))

# flip 
carn_by_sitetype_w <- carn_by_sitetype  %>%
  pivot_wider(names_from = `tourism access`,
              values_from = count)
carn_by_sitetype_w$visits = carn_by_sitetype_w$backcountry + carn_by_sitetype_w$frontcountry

# Compute total count per species for ordering
ordering <- carn_by_sitetype_w %>%
  group_by(species) %>%
  summarise(total = sum(visits), .groups = 'drop') %>%
  arrange(total)

# Turn species into an ordered factor by total count
carn_by_sitetype$species <- factor(carn_by_sitetype$species, levels = ordering$species)


# Plot barplot for background plot 
detection_barplot <- ggplot(carn_by_sitetype, aes(x = species, y = count, fill = `tourism access`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.76) +
  coord_flip() +
  scale_fill_manual(
    values = c("backcountry" = "#857377", "frontcountry" = "#DF2626")
  ) +
  labs(
    x = "Mesocarnivore species",
    y = "Species independent detection events",
    fill = "Tourism access",  # <-- Legend title
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
          axis.text.x = element_text(size = 14, color = "black"),  # X-axis text
          axis.text.y = element_text(size = 14, color = "black"),  # Y-axis text
          axis.title.x = element_text(size = 18, face = "bold"),  # X-axis label
          axis.title.y = element_text(size = 18, face = "bold")   # Y-axis label
        )
detection_barplot # check all good

# group by sites 
carn_by_site <- carnivores %>%
  group_by(species, unique_site, `tourism access`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(species, `tourism access`, unique_site, fill = list(count = 0))

carn_by_site_nz <- carn_by_site %>% 
  dplyr::filter(count != 0)

carn_by_site_nz  <- carn_by_site_nz  %>%
  group_by(unique_site, `tourism access`) %>%
  summarise(
    ringtail_present = any(species == "Ringtail"),
    other_mesocarnivore = list(species[species != "Ringtail"])
  )

# View result
carn_by_site_nz 

# create site overlap unipartite
# Summarize count and complete missing combinations with 0s
carn_by_uniquesite <- carnivores %>%
  group_by(species, unique_site, `tourism access`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(species, `tourism access`, fill = list(count = 0)) %>% 
  dplyr::filter(count != 0)
carn_cooccurence_by_site <- left_join(carn_by_uniquesite, carn_by_uniquesite, by = c("unique_site", "tourism access"))
carn_cooccurence_by_site$count.x <- NULL
carn_cooccurence_by_site$count.y <- NULL
carn_cooccurence_by_site <- unique(carn_cooccurence_by_site)

# organize by site overlap 
carn_cooccurence_by_site <- carn_cooccurence_by_site %>% 
  group_by(species.x,species.y, `tourism access`) %>% 
  summarise(n_sites = n())

carn_cooccurence_by_site <- carn_cooccurence_by_site %>% 
  dplyr::filter(species.x != species.y)

carn_cooc_clean <- carn_cooccurence_by_site  %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(species.x, species.y)), collapse = "_")) %>%
  ungroup() %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

# 2. Create graph and layout
graph <- graph_from_data_frame(carn_cooc_clean, directed = FALSE)
node_order <- c("Ringtail", setdiff(V(graph)$name, "Ringtail")) # create Ringtail in far left 

# Reorder vertices in the graph
graph_reordered <- permute(graph, match(node_order, V(graph)$name))

# Apply circular layout to reordered graph
layout <- as.data.frame(layout_in_circle(graph_reordered))
layout$name <- V(graph_reordered)$name
layout$image <- paste0("data/phylopic/", toupper(gsub(" ", "", layout$name)), ".png")

# 3. Build edge data with node coordinates
edges <- carn_cooc_clean %>%
  rename(from = species.x, to = species.y) %>%
  left_join(layout, by = c("from" = "name")) %>%
  rename(x = V1, y = V2) %>%
  left_join(layout, by = c("to" = "name")) %>%
  dplyr::rename(xend = V1, yend = V2)
edges$n_sites <- as.factor(edges$n_sites) # making integer for scale 

# 4. Plot network as inset
cooc_network <- ggplot() +
  geom_segment(data = edges, aes(x = x, y = y, xend = xend, yend = yend,
                                 color = `tourism access`, size = n_sites),
               alpha = 0.8) +
  geom_image(data = layout, aes(x = V1, y = V2, image = image), size = c(0.3, 0.15, 0.35,0.25,0.25,0.3,0.2,0.15)) +
  geom_text(data = layout, aes(x = V1, y = V2, label = name), 
            vjust =5.5, size = 4, fontface = "bold",  color = "black") +
  # Hide the color legend from the lines
  scale_color_manual(
    values = c("backcountry" = "#857377", "frontcountry" = "#DF2626")) +
  
  # Keep edge width legend
  scale_size_manual(
   # name = "Number of Sites",
    values = c("1" = 1, "2" = 2, "3" = 3)
  ) +
  theme_void() +
  # theme(legend.position = c(x = 1, y = 1.5)) +
  theme(plot.margin = margin(t = 50, r = 50, b = 200, l = 50)) + # adjust bottom (b) as needed
  coord_cartesian(clip = "off") +  # allow labels outside plot bounds 
  theme(legend.position = "none")

inset_border <- ggplot() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.02))

custom_legend <- ggplot() +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(0, "lines")
  ) +
  # --- Tourism Access ----
  # Optional legend title
  geom_text(aes(x = 0, y = 1.0), label = "Tourism access", fontface = "bold", hjust = 0, size = 6.5) +
  
  # Backcountry
  geom_point(aes(x = 0.02, y = 0.76), shape = 22, size = 8, fill = "#857377", color = "black") +
  geom_text(aes(x = 0.48, y = 0.76), label = "Backcountry", hjust = 2, size = 5) +
  
  # Frontcountry
  geom_point(aes(x = 0.02, y = 0.86), shape = 22, size = 8, fill = "#DF2626", color = "black") +
  geom_text(aes(x = 0.48, y = 0.86), label = "Frontcountry", hjust = 2, size = 5) +

# --- Number of Sites ---
geom_text(aes(x = 0, y = 0.62), label = "Co-occuring sites (#)", fontface = "bold", hjust = 0, size = 6.5) +
  geom_segment(aes(x = 0, xend = 0.05, y = 0.48, yend = 0.48), color = "gray30", size = 1) +
  geom_text(aes(x = 0.052, y = 0.48), label = "1 site", hjust = 0, size = 5) +
  
  geom_segment(aes(x = 0, xend = 0.05, y = 0.36, yend = 0.36), color = "gray30", size = 2) +
  geom_text(aes(x = 0.052, y = 0.36), label = "2 sites", hjust = 0, size = 5) +
  
  geom_segment(aes(x = 0, xend = 0.05, y = 0.24, yend = 0.24), color = "gray30", size = 3) +
  geom_text(aes(x = 0.052, y = 0.24), label = "3 sites", hjust = 0, size = 5)

# plot them all together
final_plot <- ggdraw() +
  draw_plot(detection_barplot) +  # full-size main plot
  draw_plot(cooc_network,  x = 0.25, y = -0.1,   # bottom-right placement
            width = 0.7, height = 0.7)  +  # inset
  draw_plot_label(
    label = c("a.", "b."),
    x = c(0.02, 0.3),  # x = left edge of each plot
    y = c(0.99, 0.53),  # y = top of main plot and inset plot
    hjust = 0, vjust = 1,
    fontface = "bold", size = 20
  ) + 
  draw_plot(custom_legend, x = 0.60, y = 0.60, width = 0.26, height = 0.25) + 
  draw_plot(inset_border, x = 0.275, y = 0.1, width = 0.65, height = 0.46) 

# 4. Save or display
final_plot # check all good 
  
ggsave("figures/Figure2.png", final_plot, width = 11, height = 10, dpi = 300)