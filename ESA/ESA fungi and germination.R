#Rough draft for ESA conference, will be using this data for results in finding out what colonizes seeds
 #Mercging the treatment col

library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(lme4)
install.packages("writexl")
library(writexl)

# -------------------------
# 1. Load data
# -------------------------
esa <- read_excel("ESA data.xlsx") %>%
  clean_names()

treat <- read_excel("New ComGarden Intactseedcode_rawdata.xlsx", sheet = 1) %>%
  clean_names()

# -------------------------
# 2. Clean key columns (important for matching)
# -------------------------
esa <- esa %>%
  mutate(
    garden = str_trim(toupper(as.character(garden))),
    meso_id = str_trim(as.character(meso_id)),
    soil = str_trim(toupper(as.character(soil))),
    seed_id = str_trim(toupper(as.character(seed_id)))
  )

treat <- treat %>%
  mutate(
    garden = str_trim(toupper(as.character(garden))),
    meso_id = str_trim(as.character(meso_id)),
    soil = str_trim(toupper(as.character(soil))),
    seeds = str_trim(toupper(as.character(seeds))),
    treat = str_trim(toupper(as.character(treat)))
  )

# -------------------------
# 3. Keep ONLY what we need from treat file
# -------------------------
treat_small <- treat %>%
  select(garden, meso_id, soil, seeds, treat) %>%
  distinct()

# -------------------------
# 4. Merge into ESA
# -------------------------
esa_merged <- esa %>%
  left_join(
    treat_small,
    by = c(
      "garden" = "garden",
      "meso_id" = "meso_id",
      "soil" = "soil",
      "seed_id" = "seeds"
    )
  )

# -------------------------
# 5. Check merge worked
# -------------------------
table(esa_merged$treat, useNA = "ifany")

# Look at unmatched rows (if any)
esa_merged %>%
  filter(is.na(treat)) %>%
  select(garden, meso_id, soil, seed_id) %>%
  distinct()
View(esa_merged) #Merging woreked nad i now have a treat col for treatment/ inoculation for live/dead


table(esa_merged$treat, useNA = "ifany")


#saving this new merged data
write_xlsx(unmatched, "unmatched_rows.xlsx")
write_xlsx(esa_merged, "esa_merged_clean.xlsx")


#For germination 

esa_final <- esa_merged %>%
  mutate(
    germinated_num = if_else(!is.na(germination_date), 1, 0),
    seed_id = factor(seed_id),
    treat = factor(treat),
    soil = factor(soil),
    garden = factor(garden),
    meso_id = factor(meso_id)
  )

#Running the germination model

mod_germ_pop <- glmer(
  germinated_num ~ seed_id * treat + soil + garden + (1 | meso_id),
  data = esa_final,
  family = binomial,
  na.action = na.omit
)

summary(mod_germ_pop)


germinated_num ~ seed_id + treat + garden + (1 | meso_id)


#Results interpretation


##For germiantion 2
esa_final <- esa2 %>%
  mutate(
    germinated_num = if_else(!is.na(germination_date), 1, 0),
    seed_id = factor(seed_id),
    treat = factor(treat),
    garden = factor(garden),
    meso_id = factor(meso_id)
  )


#Checkkng models
esa_final <- esa_merged %>%
  mutate(
    germinated_num = if_else(!is.na(germination_date), 1, 0),
    symptoms_num = if_else(!is.na(fungi_date_type), 1, 0),
    seed_id = factor(seed_id),
    treat = factor(treat),
    garden = factor(garden),
    meso_id = factor(meso_id)
  )

#check for my sanity
table(esa_final$treat, useNA = "ifany")
table(esa_final$seed_id, useNA = "ifany")
table(esa_final$garden, useNA = "ifany")


#Runinning the germination model again with mesocosm as a random effect
library(lme4)

mod_germ_simple <- glmer(
  germinated_num ~ seed_id + treat + garden + (1 | meso_id),
  data = esa_final,
  family = binomial,
  na.action = na.omit
)

#view model output 
summary(mod_germ_simple)

#Results interpretation

#Germination rates were low across all treatments. 
#There was no significant effect of inoculation treatment or garden environment on germination, and no strong evidence that germination differed among seed populations.
#Although some populations showed slight variation in germination rates, these differences were not statistically significant. These results suggest that germination is not directly driven by soil microbial inoculation or environmental conditions in this experiment. 
#Instead, earlier analyses indicate that fungal colonization, which varies across environments, may play a more important role in shaping seed fate.

#Some populations showed variation in germination, with CR exhibiting relatively higher germination rates, while HR showed little to no germination.



##############For fungi present as 0/1 by MesoID

esa_final <- esa_merged %>%
  mutate(
    fungi_present = if_else(!is.na(fungi_date_type), 1, 0)
  )

 #model
mod_fungi_simple <- glmer(
  fungi_present ~ treat + garden + (1 | meso_id),
  data = esa_final,
  family = binomial,
  na.action = na.omit
)

summary(mod_fungi_simple)

#results interpretation
#However, colonization varied significantly between common garden environments,
#with higher colonization observed in the WIL site compared to SUM.

