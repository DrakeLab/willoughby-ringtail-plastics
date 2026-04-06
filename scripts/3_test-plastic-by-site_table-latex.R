# Script to compare sites by plastic presence and composition
# load libraries
library(tidyverse)

# read in data
site <- read.csv("data/0_site_data.csv", strip.white = T)
scat <- read.csv(file = "data/2_scat_data.csv") # scat segment data
scat <- left_join(scat, site, by = "unique_site")

# scat counts per site 
scat_tbl <- scat %>% 
  group_by(tourism_level) %>%
  summarise(n = n())

# format contingency table of site by site type by plastic (YN)
site_plastic_tbl <- scat %>% 
  group_by(tourism_level, plastic) %>% 
  summarise(n = n())

site_plastic_mat <- site_plastic_tbl %>% 
  pivot_wider(names_from = plastic,
              values_from = n)
colnames(site_plastic_mat)[colnames(site_plastic_mat) == 0] <- "plastic_no"
colnames(site_plastic_mat)[colnames(site_plastic_mat) == 1] <- "plastic_yes"

site_plastic_mat$tourism_level <- NULL
# make a matrix 
site_plastic_mat <- as.matrix(site_plastic_mat)

# chi square of site by plastic presence
# not using Yates's correction for continuity
plastic_chi <- chisq.test(site_plastic_mat, correct = F)

# format contingency table of site (frontcountry vs backcountry) by plant (YN)
site_plant_tbl <- scat %>% 
  group_by(tourism_level, plant) %>% 
  summarise(n = n())
site_plant_mat <- site_plant_tbl %>% 
  pivot_wider(names_from = plant,
              values_from = n)
colnames(site_plant_mat)[colnames(site_plant_mat) == 0] <- "plant_no"
colnames(site_plant_mat)[colnames(site_plant_mat) == 1] <- "plant_yes"
site_plant_mat$tourism_level <- NULL

# make a matrix 
site_plant_mat <- as.matrix(site_plant_mat)

# chi square of site by plant presence
# not using Yates's correction for continuity
plant_chi <- chisq.test(site_plant_mat, correct = F)

# format contingency table of site (vehicle access YN) by invertebrate (YN)
site_inv_tbl <- scat %>% 
  group_by(tourism_level, invertebrate) %>% 
  summarise(n = n())
site_inv_mat <- site_inv_tbl %>% 
  pivot_wider(names_from = invertebrate,
              values_from = n)
colnames(site_inv_mat)[colnames(site_inv_mat) == 0] <- "inv_no"
colnames(site_inv_mat)[colnames(site_inv_mat) == 1] <- "inv_yes"
site_inv_mat$tourism_level <- NULL

# make a matrix 
site_inv_mat <- as.matrix(site_inv_mat)

# chi square of site by invertebrate presence
# not using Yates's correction for continuity
inv_chi <-  chisq.test(site_inv_mat, correct = F)

# format contingency table of site (vehicle access YN) by vertebrate (YN)
site_vert_tbl <- scat %>% 
  group_by(tourism_level, vertebrate) %>% 
  summarise(n = n())
site_vert_mat <- site_vert_tbl %>% 
  pivot_wider(names_from = vertebrate,
              values_from = n)
colnames(site_vert_mat)[colnames(site_vert_mat) == 0] <- "vert_no"
colnames(site_vert_mat)[colnames(site_vert_mat) == 1] <- "vert_yes"
site_vert_mat$tourism_level <- NULL
# make a matrix 
site_vert_mat <- as.matrix(site_vert_mat)

# chi square of site by vertebrate presence
# not using Yates's correction for continuity
vert_chi <- chisq.test(site_vert_mat, correct = F)

# format contingency table of site (vehicle access YN) by anthropogenic (YN)
site_anthro_tbl <- scat %>% 
  group_by(tourism_level, anthropogenic) %>% 
  summarise(n = n())
site_anthro_mat <- site_anthro_tbl %>% 
  pivot_wider(names_from = anthropogenic,
              values_from = n)
colnames(site_anthro_mat)[colnames(site_anthro_mat) == 0] <- "anthro_no"
colnames(site_anthro_mat)[colnames(site_anthro_mat) == 1] <- "anthro_yes"
site_anthro_mat$tourism_level <- NULL
# make a matrix 
site_anthro_mat <- as.matrix(site_anthro_mat)

# chi square of site by vertebrate presence
# not using Yates's correction for continuity
anthro_chi <- chisq.test(site_anthro_mat, correct = F)

# Create Table 1 
##  Build table dataframe 
fc_total_n <- scat_tbl[[2,2]]
bc_total_n <- scat_tbl[[1,2]]
table_df <- data.frame(
  Diet = c("Plant", "Vertebrate", "Non-plastic contaminants", "Invertebrate", "Plastic"),
  
  fc_scat = c(site_plant_tbl[[4,3]], site_vert_tbl[[4,3]], site_anthro_tbl[[4,3]], site_inv_tbl[[4,3]], site_plastic_tbl[[4,3]]), 
  fc_FO = c(site_plant_tbl[[4,3]]/fc_total_n*100, site_vert_tbl[[4,3]]/fc_total_n*100, site_anthro_tbl[[4,3]]/fc_total_n*100, site_inv_tbl[[4,3]]/fc_total_n*100, site_plastic_tbl[[4,3]]/fc_total_n*100), 
  fc_SE =  c(sqrt((site_plant_tbl[[4,3]]/fc_total_n)*(1-(site_plant_tbl[[4,3]]/fc_total_n))/fc_total_n)*100,
             sqrt((site_vert_tbl[[4,3]]/fc_total_n)*(1-(site_vert_tbl[[4,3]]/fc_total_n))/fc_total_n)*100,
             sqrt((site_anthro_tbl[[4,3]]/fc_total_n)*(1-(site_anthro_tbl[[4,3]]/fc_total_n))/fc_total_n)*100,
             sqrt((site_inv_tbl[[4,3]]/fc_total_n)*(1-(site_inv_tbl[[4,3]]/fc_total_n))/fc_total_n)*100,
             sqrt((site_plastic_tbl[[4,3]]/fc_total_n)*(1-(site_plastic_tbl[[4,3]]/fc_total_n))/fc_total_n)*100),
  
  bc_scat =  c(site_plant_tbl[[2,3]], site_vert_tbl[[2,3]], site_anthro_tbl[[2,3]], site_inv_tbl[[2,3]], site_plastic_tbl[[2,3]]), 
  bc_FO = c(site_plant_tbl[[2,3]]/bc_total_n*100, site_vert_tbl[[2,3]]/bc_total_n*100, site_anthro_tbl[[2,3]]/bc_total_n*100, site_inv_tbl[[2,3]]/bc_total_n*100, site_plastic_tbl[[2,3]]/bc_total_n*100), 
  bc_SE = c(sqrt((site_plant_tbl[[2,3]]/bc_total_n)*(1-(site_plant_tbl[[2,3]]/bc_total_n))/bc_total_n)*100,
            sqrt((site_vert_tbl[[2,3]]/bc_total_n)*(1-(site_vert_tbl[[2,3]]/bc_total_n))/bc_total_n)*100,
            sqrt((site_anthro_tbl[[2,3]]/bc_total_n)*(1-(site_anthro_tbl[[2,3]]/bc_total_n))/bc_total_n)*100,
            sqrt((site_inv_tbl[[2,3]]/bc_total_n)*(1-(site_inv_tbl[[2,3]]/bc_total_n))/bc_total_n)*100,
            sqrt((site_plant_tbl[[2,3]]/bc_total_n)*(1-(site_plastic_tbl[[2,3]]/bc_total_n))/bc_total_n)*100),
  
  chi2 = c(plant_chi$statistic, vert_chi$statistic, anthro_chi$statistic, inv_chi$statistic, plastic_chi$statistic),
  p = c(plant_chi$p.value, vert_chi$p.value, anthro_chi$p.value, inv_chi$p.value, plastic_chi$p.value)
)

# Round colums 

table_df <- table_df %>% 
  mutate(
    across(c(fc_FO, fc_SE, bc_FO, bc_SE, chi2, p),
           ~round(.,2))
  )

# Plot Table using kable
library(knitr)
library(kableExtra)

table_1 <- kable(table_df, 
      col.names = c("Diet category",
                    "no. scat", "FO", "SE",
                    "no. scat", "FO", "SE",
                    "$χ2$", "$p$"),
      align = "rcccccccc",
      booktabs = TRUE,
      escape = FALSE) %>%
  add_header_above(c(" " = 1,
                     "Frontcountry ($n$=44)" = 3,
                     "Backcountry ($n$=47)" = 3,
                     " " = 2), 
                   align = "l") %>%
  add_header_above(c(" " = 1, 
                     "Site type" = 8), align = "l") %>%
  kable_styling(latex_options = c("hold_position"),
                font_size = 11) %>%
  #column_spec(1, 
      #        width = "5cm", 
       #      latex_column_spec = ">{\\raggedleft\\arraybackslash}p{5cm}") %>%
  row_spec(0, extra_latex_after = "\\midrule") %>%
  row_spec(5, bold = TRUE) %>%
  row_spec(nrow(table_df), extra_latex_after = "\\bottomrule")
table_1 # check all good

# save the table for LaTeX 
writeLines(table_1, "figures/Table1.tex")
                     

# Plot Figure 3 
# calculate fragment weights by site type 
frags <-  frags %>% 
  dplyr::filter(fragment_type != "Rocks")
scat_fw <- left_join(scat, frags, by = "sample_id")

scat_fw_sum <- scat_fw %>% 
  group_by(tourism_level) %>% 
  summarise(weight = sum(fragment_weight))

scat_fw$pct_W <- scat_fw$fragment_weight / scat_fw$dryweight_grams*100 

# compare % weight of Plastics in front country vs backcountry 
plastics <- read.csv(file = "data/4_plastic_data.csv", strip.white = T)
plastics <- left_join( plastics,scat, by = "sample_id")
plastics$pct_W <- plastics$fragment_weight / plastics$dry_weight *100

plastic_pctW_by_site <- ggplot(plastics, aes(x =tourism_level.x , y = pct_W)) + 
  geom_boxplot() + 
  geom_point(aes(shape = scat_identity, color = plastic_type),size = 5, position = position_jitterdodge(0.5)) + 
  scale_shape_manual(name = "scat depositor identity", 
                       values = c("ringtail" = 18, "carnivore" = 15, "other mesocarnivore" = 17)) + 
  scale_color_manual(
    name = "plastic polymer",
    values = c(
      "pe" = "blue",
      "pp" = "orange",
      "ps" = "darkgrey"
    ),
    labels = c(
      "pe" = "polyethylene",
      "pp" = "polypropylene",
      "ps" = "polystyrene"
    )
  ) +
  scale_y_continuous(breaks = c(0,15,30,45,60), limits = c(0,60)) + 
  labs(x = "Site Area", y = "Weight (%)") + 
  labs(x = "Tourist access") +
  theme_classic() + 
  theme(axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12), 
        axis.title=element_text(size=24,face="bold")) 

png(filename="figures/Figure3.png", width = 6, height = 7, units = 'in', res = 300)
plastic_pctW_by_site
dev.off()

