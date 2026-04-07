library(knitr)
library(kableExtra)

plastics <- data.frame(
  Polymer = c(
    "Polyethylene Terephthalate",
    "High-density Polyethylene",
    "Low-density Polyethylene",
    "Polypropylene",
    "Polystyrene",
    "Polyvinyl Chloride"
  ),
  Abbreviation = c("PET", "HDPE", "LDPE", "PP", "PS", "PVC"),
  Flame_Color = c(
    "Yellow-orange, blue edge",
    "Yellow-orange, blue edge",
    "Orange, blue edge",
    "Yellow",
    "Red, sooty",
    "Red, blue edge"
  ),
  Burn_Odor = c(
    "Sweetly aromatic",
    "Burning rubber",
    "Burning rubber",
    "Sweet, chocolate",
    "Sweetish, natural gas",
    "Acidic"
  ),
  Control_Item = c(
    "Guacamole container lid (a); Coca-Cola bottle; Produce plastic basket",
    "Cashew container lid (b); Milk jug; Half-and-half container lid",
    "Ziploc sandwich bag (c); AirPlus packing air pillow; Bubble wrap",
    "Starbucks drink lid (d); Chobani yogurt container; Milk jug lid",
    "Packing material (e); Clothing hanger; Scotch tape roll",
    "Plumbing pipe (f); Electrical pipe"
  ),
  stringsAsFactors = FALSE
)

supp_t1 <- kable(plastics,
      booktabs = TRUE,
      linesep = "") %>%
  kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE)

supp_t1  # check all good

writeLines(supp_t1, "figures/supp/Table1.tex")

# SUPPLEMENTARY TABLE 2 

# Park wide diet frequency 
pw_diet <- data.frame(
  Diet_Type = c("Plant", "Vertebrate", "Non-Plastic contaminants", "Invertebrate", "Plastic"),
  F = c(64, 62, 36, 33, 21),
  FO_percent = c(70.30, 68.10, 39.60, 36.30, 23.10),
  RFO_percent = c(29.60, 28.70, 16.70, 15.30, 9.72),
  W_percent = c(48.2, 32.8, 9.71, 2.24, 7.02)
)

supp_T2 <- kable(pw_diet,
      col.names = c("Diet category", "F", "FO (%)", "RFO (%)", "W (%)"),
      align = c("r", "c", "c", "c", "c"), 
      booktabs = TRUE,
      linesep = "") %>%
  kable_styling(
    latex_options = "hold_position",
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE)
supp_T2  # check all good

writeLines(supp_T2, "figures/supp/Table2.tex")

# SUPPLEMENTARY TABLE 3 

# Latrine depositor traits 
depositor_tab <- data.frame(
  common_name = c("Ringtail", "Gray fox", "Striped skunk", "Spotted skunk", "Coyote", "Domestic cat", "Raccoon", "Bobcat"),
  species = c("Bassariscus astutus", "Urocyon cinereoargenteus", "Mephitis mephitis", "Spilogale gracilis", "Canis latrans", "Felis catus", "Procyon lotor","Lynx rufus"),
  latrine  = c("yes", "yes", "no", "no", "yes", "yes", "yes", "yes"),
  diam_range = c("0.50 - 1.60", "1.00 - 1.90", "1.00 - 2.20", "0.60 - 1.79", "1.00 - 3.50", "1.00 - 2.20", "0.80 - 3.00", "1.10 - 2.50"),
  shape = c("twisted, tapered ropes", "tubular, blunt or tapered ends", "tubular, blunt ends", "tubular, blunt ends with smooth surface", "tubular or twisted, blunt or tapered ends", "tubular ropes, blunt or one-end tapered", "Lack of taper at the end", "Marked off in short segments by constrictions; pelleted; may cover scat with soil"),
  sources = c("Trapp 1978; Elbroch et al. 2012", "Trapp 1978; Elbroch 2003; Elbroch et al. 2012", "Elbroch 2003", "Elbroch et al. 2012", "Ralls & Smith 2004; Elbroch 2003; Elbroch et al. 2012", "Molsher 1999; Murie 1975; Elbroch et al. 2012", "Hirsch et al. 2014; Elbroch 2003; Murie 1975", "Bailey 1974; Elbroch 2003; Murie 1975")
)

supp_T3 <- kable(depositor_tab,
                 col.names = c("Common name", "Species", "Latrine depositor", "Diameter (cm)", "Scat shape", "Sources"),
                 booktabs = TRUE,
                 linesep = "") %>%
  kable_styling(
    latex_options = "hold_position",
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(2, italic = TRUE)
supp_T3  # check all good

writeLines(supp_T3, "figures/supp/Table3.tex")