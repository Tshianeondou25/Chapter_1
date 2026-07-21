#Slurry dna results
#These results only represent HOME VS HOME identified fungi
#I will use this data as supplementary data for fungi identified on ESA poster
#This data was extracted using zymo fungal kit and identified using sanger sequencing 


#Here iI will start by cleaning the data and the making figures
#i will remove unnecessary col that i dont need for the figure

#Packages
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)


#Data
#importing raw dta
 
Slurry_DNAresults_HomevsHome <- read_excel("ESA/Slurry_DNAresults_HomevsHome.xlsx")
View(Slurry_DNAresults_HomevsHome)

#rENAMING
dna_raw <- Slurry_DNAresults_HomevsHome


## Examine original structure
names(dna_raw)
dim(dna_raw)
head(dna_raw)

#Renaming and cleaning dta

# Rename col and cleaning variables
dna_CLEANED <- dna_raw %>%
  rename(
    extraction_id = `Extraction ID`,
    experiment_id = `Experiment ID`,
    temperature_treatment = `Experimental treatment`,
    plate_id = PlateID,
    seed_id = SeedID,
    extraction_date = `Extraction date`,
    extracted_by = `Extraction by`,
    dna_concentration = `DNA concentration (ng/uL)`,
    quantification_plate = `Quantification/Purification plate data`,
    dna_reference_id = `DNA Reference ID`,
    sequence_identification = `...11`,
    fungal_genus = `Fungi Genus`,
    fungal_species = `Fungi species`
  ) %>%
  mutate(
    experiment_id = str_squish(experiment_id),
    temperature_treatment = str_squish(temperature_treatment),
    plate_id = str_squish(as.character(plate_id)),
    seed_id = str_squish(toupper(seed_id)),
    fungal_genus = str_squish(fungal_genus),
    fungal_species = str_squish(fungal_species),
    
    experiment_id = recode(
      experiment_id,
      "Seed slury" = "Seed slurry"
    ),
    
    temperature = case_when(
      temperature_treatment == "Warm freezer" ~ "Warm",
      temperature_treatment == "Cool freezer" ~ "Cool",
      TRUE ~ temperature_treatment
    ),
    
    temperature = factor(
      temperature,
      levels = c("Cool", "Warm")
    ),
    
    identified = if_else(
      !is.na(fungal_genus) & fungal_genus != "",
      "Identified",
      "Not identified"
    ),
    
    identified_num = if_else(
      identified == "Identified",
      1L,
      0L
    ),
    
    fungal_taxon = case_when(
      !is.na(fungal_genus) &
        fungal_genus != "" &
        !is.na(fungal_species) &
        fungal_species != "" ~
        paste(fungal_genus, fungal_species),
      
      !is.na(fungal_genus) & fungal_genus != "" ~
        fungal_genus,
      
      TRUE ~ "Not identified"
    )
  )

#checking if it worked 
names(dna_CLEANED)
glimpse(dna_CLEANED)

table(dna_CLEANED$temperature, useNA = "ifany")
table(dna_CLEANED$identified, useNA = "ifany")
table(dna_CLEANED$fungal_genus, useNA = "ifany")
table(dna_CLEANED$fungal_taxon, useNA = "ifany")


##MAKING PLOTS

#Counting genus 
# Countig how many times each fungal genus was identified
genus_counts <- dna_CLEANED %>%
  filter(identified == "Identified") %>%
  count(fungal_genus, sort = TRUE)

genus_counts

#Result
#Penicillium     23
#Alternaria       1
#Fusarium         1

###PLOT FIGURE 
 
fungal_genus_plot <- ggplot(
  genus_counts,
  aes(
    x = reorder(fungal_genus, n),
    y = n,
    fill = fungal_genus
  )
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  geom_text(
    aes(label = n),
    hjust = -0.25,
    size = 5
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    x = NULL,
    y = "Number of identified isolates"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(
      face = "italic",
      size = 14
    ),
    axis.text.x = element_text(size = 13)
  )

fungal_genus_plot

##########plot with bars without numbers!!
#just removing geom text here

fungal_genus_plot <- ggplot(
  genus_counts,
  aes(
    x = reorder(fungal_genus, n),
    y = n,
    fill = fungal_genus
  )
) +
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Number of identified isolates"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(
      face = "italic",
      size = 14
    ),
    axis.text.x = element_text(size = 13)
  )

fungal_genus_plot




#save this plot????????
ggsave(
  "Figures/fungal_genus_counts.png",
  plot = fungal_genus_plot,
  width = 7,
  height = 5,
  dpi = 300
)

 #Caption 
#Figure xxX. Frequency of fungal genera identified from fungi isolated from home-seed × home-soil samples. Bars represent the number of successfully identified isolates assigned to each genus. Taxonomic identifications are based on Sanger sequencing.


################################################################

##############################################################

#Now i am just making creating a figure to represent everything that was found from sanger sequencing.
#using isolates identified from only slurry is not enough since i only have 3 from that exp

#data
Raw_SangerSeq_data <- read_excel("ESA/Raw_SangerSeq_data.xlsx")
View(Raw_SangerSeq_data)
 
#rename
raw_sanger <-Raw_SangerSeq_data
 

names(raw_sanger)
head(raw_sanger)

###########################
# Count each fungal genus across all identified isolates

# Count the number of isolates belonging to each fungal genus
genus_counts <- raw_sanger %>%
  filter(!is.na(Genus), Genus != "") %>%
  count(Genus, sort = TRUE)

genus_counts


#the plot


fungal_genus_plot <- ggplot(
  genus_counts,
  aes(
    x = reorder(Genus, n),
    y = n
  )
) +
  geom_col(
    fill = "steelblue",
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Number of identified isolates"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(
      face = "italic",
      size = 14
    ),
    axis.text.x = element_text(size = 13)
  )

fungal_genus_plot

####plot with different colours
fungal_genus_plot <- ggplot(
  genus_counts,
  aes(
    x = reorder(Genus, -n),
    y = n,
    fill = Genus
  )
) +
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Number of identified isolates"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "italic", size = 14),
    axis.text.x = element_text(size = 13)
  )

fungal_genus_plot


#removing unidentified isolates
# Count only successfully identified fungal genera
genus_counts <- raw_sanger %>%
  filter(
    !is.na(Genus),
    Genus != "-"
  ) %>%
  count(Genus, sort = TRUE)


#plot
# Plot fungal genera
fungal_genus_plot <- ggplot(
  genus_counts,
  aes(
    x = reorder(Genus, n),
    y = n,
    fill = Genus
  )
) +
  geom_col(
    width = 0.75,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Number of fungal isolates"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold", size = 15),
    axis.text.y = element_text(
      face = "italic",
      size = 14
    ),
    axis.text.x = element_text(size = 13)
  )

# Display figure
fungal_genus_plot



#labellibf=g the axis 
fungal_genus_plot <- ggplot(
  genus_counts,
  aes(
    x = reorder(Genus, n),
    y = n,
    fill = Genus
  )
) +
  geom_col(
    width = 0.75,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Fungal genus",
    y = "Number of fungal isolates"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.title.x = element_text(
      face = "plain",
      size = 15
    ),
    axis.title.y = element_text(
      face = "plain",
      size = 15
    ),
    axis.text.y = element_text(
      face = "italic",
      size = 14
    ),
    axis.text.x = element_text(
      size = 13
    )
  )

# Display figure
fungal_genus_plot

# Save figure? yes going to use this for poster
ggsave(
  filename = "Figures/DNAFungal_Genera.png",
  plot = fungal_genus_plot,
  width = 7,
  height = 5,
  dpi = 300
)

#Capption 
#figure Xxxx fequency of fungal genera identified from cultured fungal isolates recovered from big sagebrush (Artemisia tridentata) seeds. Penicillium and Aspergillus were the most frequently identified genera, while Alternaria, Candida, Cladosporium, Fusarium, and Purpureocillium were recovered less frequently. Identifications are based on preliminary Sanger sequencing.