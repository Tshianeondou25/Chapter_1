#Analysis: ESA CONFERENCE: Fungal colonization in sagebrush seeds
# Description:
# This will include a join to include treament on the fungal data sheet. i will Join ESA germination data with treatment sheet, create

#packages and data
library(readxl)
library(tidyverse)
library(janitor)
library(lme4)

 
esa <- read_excel("esa_merged_clean.xlsx") %>%
  clean_names()

# Checking  
view(esa)

#making the variables i will need into factors
esa <- esa %>%
  mutate(
    # outcomes
    germinated = if_else(!is.na(germination_date), 1, 0),
    fungi_present = if_else(!is.na(fungi_date_type), 1, 0),
    
    # factors
    seed_id = factor(seed_id),
    treat = factor(treat),
    garden = factor(garden),
    meso_id = factor(meso_id)
  )

#double checking for my sanity 
table(esa$treat, useNA = "ifany")
table(esa$fungi_present)
table(esa$germinated)
#here germination and fungi presence = 0/1


#Modeling for fungi presence
mod_fungi <- glmer(
  fungi_present ~ treat + garden + (1 | meso_id),
  data = esa,
  family = binomial
)

summary(mod_fungi)

#chcking: does fungi really affect germinatioon?
germ_mod <- glmer(
  germinated ~ fungi_present + garden + (1 | meso_id),
  data = esa,
  family = binomial,
  na.action = na.omit
)

summary(germ_mod)



#Results


#germination check in
esa %>%
  group_by(fungi_present) %>%
  summarise(
    germ_rate = mean(germinated, na.rm = TRUE),
    n = n()
  )

#Resuults
#Germination was lower in seeds with fungal infection (4.4%) than in uninfected seeds (9.0%).

#Fungal infection by garden
esa %>%
  group_by(garden) %>%
  summarise(
    infection_rate = mean(fungi_present, na.rm = TRUE)
  )

#Results

esa %>%
  group_by(fungi_present) %>%
  summarise(
    germ_rate = mean(germinated),
    n = n()
  )



#Overal germinatio rate
mean(esa$germinated, na.rm = TRUE)
