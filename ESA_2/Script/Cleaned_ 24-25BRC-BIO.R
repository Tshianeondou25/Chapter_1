#Second data clean up for 24-25 brc-bio SEEDS
#Here i will make the original data sets long, splitting it from the grouped data so that each seed bag and seed ID is a different item
#i will use the original grouped data, the first batch of plating and second batch of plaiting
#I will also add columns such as seed fate, symptoms (for ranking infected seeds as 0/1)
# my final cleaned data will have the long data


#Packages
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(stringr)

BRC_BIO24_25_RAWgrouped <- read_excel(
  "ESA_2/Raw data/BRC-BIO24-25_RAWgrouped.xlsx",
  skip = 1
)

View(BRC_BIO24_25_RAWgrouped)

#Checking col names
names(BRC_BIO24_25_RAWgrouped) 

#Renaming for consistency

BRC_BIO24_25_RAWgrouped <- BRC_BIO24_25_RAWgrouped %>%
  rename(
    garden            = Garden,
    meso_id           = MesoID,
    soil_id              = Soil,
    treat             = Treat,
    seed_id   = Seeds,
    n_bags            = `# bags`,
    
    bag1_seed_id      = `Bag 1 Seed ID`,
    bag1_intact       = `# Intact...8`,
    bag1_germinated   = `# Germ...9`,
    bag1_burst        = `# Burst...10`,
    bag1_missing      = `#Missing...11`,
    bag1_obs          = `Obs...12`,
    bag1_date         = `Date...13`,
    bag1_notes        = `NOTES...14`,
    
    bag2_seed_id      = `Bag 2 Seed ID`,
    bag2_intact       = `# Intact...16`,
    bag2_germinated   = `# Germ...17`,
    bag2_burst        = `# Burst...18`,
    bag2_missing      = `#Missing...19`,
    bag2_obs          = `Obs...20`,
    bag2_date         = `Date...21`,
    bag2_notes        = `NOTES...22`,
    
    bag3_seed_id      = `Bag 3 Seed ID`,
    bag3_intact       = `# Intact...24`,
    bag3_germinated   = `# Germ...25`,
    bag3_burst        = `# Burst...26`,
    bag3_missing      = `#Missing...27`,
    bag3_obs          = `Obs...28`,
    bag3_date         = date,
    bag3_notes        = `NOTES...30`,
    
    entered_by        = `Entered by`
  )

#Checking
names(BRC_BIO24_25_RAWgrouped)
View(BRC_BIO24_25_RAWgrouped)


#Checking and changing the structure of the data
unique(BRC_BIO24_25_RAWgrouped$bag3_intact)

BRC_BIO24_25_RAWgrouped %>%
  select(contains("intact"), contains("germinated"),
         contains("burst"), contains("missing")) %>%
  glimpse()

#converting data to numeric
BRC_BIO24_25_RAWgrouped <- BRC_BIO24_25_RAWgrouped %>%
  mutate(
    across(
      matches("^bag[123]_(intact|germinated|burst|missing)$"),
      ~ parse_number(as.character(.x))
    )
  )

#col 140 had a note, so here I am getting rid of it because it gives an error
BRC_BIO24_25_RAWgrouped$bag3_missing[140] <- NA

#removing repetition of seed_id labels
BRC_BIO24_25_RAWgrouped <- BRC_BIO24_25_RAWgrouped %>%
  select(-seed_id)

#Making it long
 
RAW_long <- BRC_BIO24_25_RAWgrouped %>%
  pivot_longer(
    cols = starts_with("bag"),
    names_to = c("bag", ".value"),
    names_pattern = "bag(\\d+)_(.*)"
  ) %>%
  mutate(
    bag = as.integer(bag)
  ) %>%
  arrange(meso_id, bag)

View(RAW_long)


#Adding a new col for seed fate which will describe missing, burst or germinated after bag retrival

RAW_fate_long <- RAW_long %>%
  pivot_longer(
    cols = c(germinated, burst, missing),
    names_to = "seed_fate",
    values_to = "n_seeds"
  )

View(RAW_fate_long)

 #########################################
 #Joining new data with seed fate 
#joining it with the data from the first and second batch of the experiment

RAW_fate_long <- RAW_long %>%
  pivot_longer(
    cols = c(germinated, burst, missing),
    names_to = "seed_fate",
    values_to = "n_seeds"
  )

#Saving the new data with Fate seed col name
write.csv(
  RAW_fate_long,
  "RAW_fate_long.csv",
  row.names = FALSE
)


#Importing the combined exp 1and  data
Combined_exp1and2 <- read_excel("ESA_2/Raw data/BRBIO-CombinedExp1and2_batch.xlsx")
View(Combined_exp1and2)

#checking if the col names match
names(Combined_exp1and2)

#checking data format
head(Combined_exp1and2$seed_code, 20)
unique(Combined_exp1and2$seed_code)[1:20]
 
 
#######
#doing this because the seed fate col data was still not joining because of the seedbags
fate_summary <- RAW_fate_long %>%
  group_by(garden, meso_id, soil_id, seed_id, seed_fate) %>%
  summarise(
    n_seeds = sum(n_seeds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = seed_fate,
    values_from = n_seeds,
    values_fill = 0
  )

fate_summary %>%
  count(garden, meso_id, soil_id, seed_id) %>%
  filter(n > 1)


combined_summary <- Combined_exp1and2 %>%
  group_by(garden, meso_id, soil_id, seed_id) %>%
  summarise(
    n_plated = n(),
    intact_seeds_total = sum(intact_seeds, na.rm = TRUE),
    .groups = "drop"
  )

combined_summary %>%
  count(garden, meso_id, soil_id, seed_id) %>%
  filter(n > 1)

#combining fate and summary
analysis_data <- fate_summary %>%
  left_join(
    combined_summary,
    by = c(
      "garden",
      "meso_id",
      "soil_id",
      "seed_id"
    )
  )

#checking if this is working
#if it works, everything should be the same
nrow(fate_summary)




fate_summary %>%
  filter(
    burst < 0 |
      germinated < 0 |
      missing < 0
  )

#inspecting other rows
#found some typos on col names
sort(unique(fate_summary$seed_id))
sort(unique(combined_summary$seed_id))


#this allows me to rename and correct the typos
fate_summary <- fate_summary %>%
  mutate(
    seed_id = recode(
      seed_id,
      "HR- A and B" = "HR",
      "OIH" = "OH",
      "TO" = "TI"
    )
  )

#CHECKING IF IT WORKED
sort(unique(combined_summary$seed_id))

analysis_data <- fate_summary %>%
  left_join(
    combined_summary,
    by = c(
      "garden",
      "meso_id",
      "soil_id",
      "seed_id"
    )
  )

analysis_data %>%
  filter(is.na(n_plated))

#CHECKING THE UNMATCHED ROWWS AGAIN!!
sort(unique(fate_summary$seed_id))

analysis_data <- fate_summary %>%
  left_join(
    combined_summary,
    by = c(
      "garden",
      "meso_id",
      "soil_id",
      "seed_id"
    )
  )

analysis_data %>%
  filter(is.na(n_plated))

#CHECKING WHY I STILL HAVE Some unmatched col
unmatched_keys <- fate_summary %>%
  anti_join(
    combined_summary,
    by = c(
      "garden",
      "meso_id",
      "soil_id",
      "seed_id"
    )
  )

print(unmatched_keys, n = Inf)

#comparing the above with meso id 
unmatched_keys %>%
  select(garden, meso_id, soil_id, seed_id) %>%
  left_join(
    Combined_exp1and2 %>%
      distinct(garden, meso_id, soil_id, seed_id),
    by = c("garden", "meso_id"),
    suffix = c("_fate", "_combined")
  ) %>%
  print(n = Inf)
#join still not working! 

#creating a data set of what is missing and giving error when i join
#i will check this in excel and compare with raw data
mismatch_check <- unmatched_keys %>%
  select(garden, meso_id, soil_id, seed_id) %>%
  left_join(
    Combined_exp1and2 %>%
      distinct(garden, meso_id, soil_id, seed_id),
    by = c("garden", "meso_id"),
    suffix = c("_fate", "_combined")
  )

#save
write.csv(
  mismatch_check,
  "mismatch_check.csv",
  row.names = FALSE
)

missing_mesocosms <- unmatched_keys %>%
  anti_join(
    Combined_exp1and2 %>%
      distinct(garden, meso_id),
    by = c("garden", "meso_id")
  )

View(missing_mesocosms) #seedid that did not join correctly

#some seedid with the fate id are still not joining 
#I am starting a join without the seed fate col 

########################################################

##Joining long raw data without the seed fate col. i will be joining this with the combined ex1 and 2
#this is what i will use to do the analysis

RAW_long <- BRC_BIO24_25_RAWgrouped %>%
  pivot_longer(
    cols = starts_with("bag"),
    names_to = c("bag", ".value"),
    names_pattern = "bag(\\d+)_(.*)"
    
  ) %>%
  mutate(
    bag = as.integer(bag)
  ) %>%
  arrange(meso_id, bag)

View(RAW_long)

#saving raw data
write.csv(
  RAW_long,
  "RAW_long.csv",
  row.names = FALSE
)
