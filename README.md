Plastic ingestion by an Omnivorous Mammal, the Ringtail (*Bassariscus astutus*), in a National Park

Authors: 
- Logan Owens
- Anna Willoughby

Background:

Plastics are pervasive human-derived contaminants that pollute natural wildlife habitats. Ringtails (Bassariscus atustus), an opportunistic omnivore species living in Grand Canyon National Park, have been documented consuming human foods and their plastic packaging in human-dominated environments. To better understand the frequency of plastic ingestion by ringtails in this protected park, we collected 83 scat samples in 2021 from sites of high vs. low human use in Grand Canyon National Park. The areas of lower tourism included wilderness areas along trails and areas of higher tourism were sites with vehicle access such as the historic El Tovar Hotel. We visually examined scat samples to identify the anthropogenic components and quantify the proportion of plastic by weight. We performed a hot needle test to validate the presence of plastics and burn and odor tests to determine, if possible, the type of plastic. We commonly found polyethylene plastics which are used for films and packaging on many food products like granola bars and sandwich bags. We examined whether the frequency of scat containing plastic materials depended on tourism habitat type. Researchers over the past two decades have studied the frequency and health consequences of plastic ingestion by sea turtles and other aquatic wildlife but this issue is not restricted to waterways. This study provides insight into the scavenging habits of terrestrial omnivores in human-manipulated environments. In the future, this study can be used to better understand the effects of humans and their indirect interactions with wildlife in protected areas.

Research Questions:
1. Do ringtails consume plastics?
  A. If so, to what degree (frequency of occurrence, relative frequency of occurrence, and frequency by weight) and what types of plastics? 
2. What are the drivers and physiological consequences of plastic consumption?
   A. Are scats that contain plastics morphologically different (diameter, weight) than plastic-free scats?
   B. How does type of tourism (vehicle access vs. backcountry) influence plastic presences and intensity? 


Study design:
[Experimental design as narrative or list of steps]


### Listing of files in this repository

```
├── README.md                                        | this file in .md format
├── data/                                            | folder with datafiles from Owens et al.
│        ├── 1_site_data.csv                         | datafile with site information
│        ├── 2_scat_data.csv                         | datafile with scat segment information
│        ├── 3_fragment_data.csv                     | datafile with plastic-present subset fragment information
│        ├── 4_plastic_data.csv                      | datafile with anthropogenic items tested for plastic content
│        └── 5_contamination_control.csv             | datafile with tape test information per laboratory day 
├── scripts/                                               
│     ├── 0_in_line_calculations.R                   | script to calculate any values present in the text
│     ├── 1_map_site_locations.R                     | script to map the study locations
│     └── 2_test-plastic-by-site.R                   | script to calculate scat-level analysis
├── figures/
│     ├── figure1.R                                  | figure 1
│     ├── figure2.R                                  | figure 2
└     └── igure1.R                                   | figure 3
                     
```
        

Checklist:
[bullet list of tasks, include dates, assignment, and initial complete, possibilities include]
[obtaining, quality check, formatting, and merging datasets]
[writing auxiliary codes for data preprocessing, support functions, postprocessing/summary/visualization]
[main program]
[final report]

Protocol changes:
[bullet list of changes to protocols with reasons for each change]
