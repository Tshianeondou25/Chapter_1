
#Trying to clean and expand the seed fate here

#Packages and data 
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(openxlsx)
 
RAW_long <- read_csv("ESA_2/Raw data/RAW_long.csv")
View(RAW_long)

#FOR OVERWRITING EACH STEPS FOR CLEAN UP DTA
RAW_long <- RAW_long %>%
  mutate(
    garden = str_to_upper(str_trim(garden)),
    soil_id = str_to_upper(str_trim(soil_id)),
    treat = str_to_upper(str_trim(treat)),
    seed_id = str_to_upper(str_trim(seed_id))
  )

#rEMOVE EMPTY ROWS 
RAW_long <- RAW_long %>%
  filter(
    !is.na(garden),
    !is.na(meso_id),
    !is.na(seed_id),
    seed_id != ""
  )

#cHECKING NGEATIVE VALUES
RAW_long %>%
  filter(
    intact < 0 |
      germinated < 0 |
      burst < 0 |
      missing < 0
  )

#rEMOVING THE NGE VALUE
# Remove the invalid correction row
RAW_long <- RAW_long %>%
  filter(
    !(
      garden == "SUM" &
        meso_id == 67 &
        bag == 3 &
        seed_id == "HR- A AND B"
    )
  )

#DOUBLE CHECKING 
# Confirm there are no negative counts
RAW_long %>%
  filter(
    intact < 0 |
      germinated < 0 |
      burst < 0 |
      missing < 0
  )
#TOTAL SEED BAGS
# Calculate total seeds recorded for each bag
RAW_long <- RAW_long %>%
  mutate(
    total_seeds = intact + germinated + burst + missing
  )
#CHECKING BAGS ARE THERE
# Check how many bags have each total
RAW_long %>%
  count(total_seeds)

#INspecting bags that had >10 seeds
RAW_long %>%
  filter(
    total_seeds != 10 |
      is.na(total_seeds)
  ) %>%
  arrange(total_seeds, garden, meso_id, bag) %>%
  select(
    garden,
    meso_id,
    soil_id,
    treat,
    bag,
    seed_id,
    intact,
    germinated,
    burst,
    missing,
    total_seeds,
    notes
  ) %>%
  print(n = Inf, width = Inf)
#From the checks above, some bags contained more or fewer
# seeds than the expected 10.One invalid correction row (SUM, Meso 67,
# Bag 3) containing negative counts was removed. The remaining
# records were retained because they represent observations rcorded during the experiment (e.g., missing bags, open
# bags, extra seeds, or other documented field notes).

#expanid bag row to seed row AND CRATIGN UNIQUES ID
RAW_long <- RAW_long %>%
  mutate(
    bag_id = paste(
      garden,
      meso_id,
      bag,
      sep = "_"
    )
  )

#CHECKING UNIQU ID
RAW_long %>%
  count(bag_id) %>%
  filter(n > 1)

#checking long faormat

# Convert bag-level fate columns into long format
RAW_fate_long <- RAW_long %>%
  pivot_longer(
    cols = c(
      intact,
      germinated,
      burst,
      missing
    ),
    names_to = "seed_fate",
    values_to = "n_seeds"
  )

#Checking
dim(RAW_fate_long)
RAW_fate_long %>%
  count(seed_fate)

RAW_fate_long %>%
  select(
    bag_id,
    garden,
    meso_id,
    bag,
    seed_id,
    seed_fate,
    n_seeds
  ) %>%
  print(n = 20)

#checking originals after long foramt
RAW_fate_long %>%
  group_by(bag_id) %>%
  summarise(
    total_from_long = sum(n_seeds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    RAW_long %>%
      select(
        bag_id,
        total_seeds
      ),
    by = "bag_id"
  ) %>%
  filter(total_from_long != total_seeds)

#removing rows with zero before expanding 
RAW_fate_long <- RAW_fate_long %>%
  filter(
    !is.na(n_seeds),
    n_seeds > 0
  )
#now expanding seed row
RAW_seed_6000 <- RAW_fate_long %>%
  uncount(
    weights = n_seeds,
    .id = "seed_number_within_fate"
  )

#Checking if it worked dim(RAW_seed_level)

RAW_seed_6000 %>%
  count(seed_fate)

#verifying seed counts
nrow(RAW_seed_6000)
sum(RAW_long$total_seeds, na.rm = TRUE)
#The numbeers are the same i have 5785!!!

# Numbering seeds within each fate and bag
RAW_seed_6000 <- RAW_seed_6000 %>%
  group_by(bag_id, seed_fate) %>%
  mutate(
    seed_number = row_number()
  ) %>%
  ungroup()

# Assign plating letters only to intact seeds
#doing this to avoid mxing intact seeds before plating and intact seedes after plaiting 
RAW_seed_6000 <- RAW_seed_6000 %>%
  mutate(
    plate_seed_letter = if_else(
      seed_fate == "intact",
      LETTERS[seed_number],
      NA_character_
    )
  )

# Check intact seeds and assigned plating letters
RAW_seed_6000 %>%
  filter(seed_fate == "intact") %>%
  select(
    bag_id,
    garden,
    meso_id,
    bag,
    seed_id,
    seed_number,
    plate_seed_letter
  ) %>%
  print(n = 30)
#The above code assigned plated letters to track the seeds.
#I will match this later with the seed code form the plaited experemint 

#Since i have close to 6000 observations, i am, now going to load the data frmo the germination exp
#This data is combined from experiment 1and2 batch combined

CombinedExp1and2 <- read_excel("ESA_2/Raw data/BRBIO-CombinedExp1and2_batch.xlsx")
#col names
names(CombinedExp1and2)

#checking row name
CombinedExp1and2 %>%
  select(everything()) %>%
  print(n = 20, width = Inf)

# Inspect the identifiers in the plating dataset
CombinedExp1and2 %>%
  select(
    plate_id,
    garden,
    meso_id,
    soil_id,
    seed_id,
    intact_seeds,
    seed_code
  ) %>%
  print(n = 30, width = Inf)

#removing duplicates
#will had plates that appears to be plaited twice 
#WIL 36 HR and WIL 116 BB/RC were plated more than once.
#SUM 67 HR was split between two plates
# plate 771 contained four seeds and plate 772 contained two.
CombinedExp1and2_clean <- CombinedExp1and2

CombinedExp1and2_clean %>%
  filter(
    (garden == "SUM" & meso_id == 67 & seed_id == "HR") |
      (garden == "WIL" & meso_id == 36 & seed_id == "HR") |
      (garden == "WIL" & meso_id == 116)
  ) %>%
  select(
    plate_id,
    garden,
    meso_id,
    soil_id,
    seed_id,
    intact_seeds,
    any_of(c("plate_seed_letter", "seed_code")),
    plating_date,
    notes
  ) %>%
  arrange(
    garden,
    meso_id,
    seed_id
  ) %>%
  print(n = Inf)
#
CombinedExp1and2 %>%
  group_by(
    garden,
    meso_id,
    seed_id
  ) %>%
  summarise(
    n_rows = n(),
    n_plate_ids = n_distinct(plate_id),
    plate_ids = paste(unique(plate_id), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_plate_ids > 1) %>%
  print(n = Inf)
##

# Remove the earlier repeated plating records
CombinedExp1and2_clean <- CombinedExp1and2_clean %>%  
filter(
    !plate_id %in% c(
      437,  # Earlier WIL 36 HR plate
      490,  # Earlier WIL 116 BB plate
      491   # Earlier WIL 116 RC plate
    )
  ) %>%
  
  # Relabel the second portion of SUM 67 HR
  mutate(
    seed_code = case_when(
      plate_id == 772 & seed_code == "A" ~ "E",
      plate_id == 772 & seed_code == "B" ~ "F",
      TRUE ~ seed_code
    )
  )

#cheking if i successfully removed duplictes
CombinedExp1and2_clean %>%
  count(
    garden,
    meso_id,
    seed_id,
    seed_code
  ) %>%
  filter(n > 1)

#renqming the plate letter to match the seed code frmo the combined experiment 
CombinedExp1and2_clean <- CombinedExp1and2_clean %>%
  rename(
    plate_seed_letter = seed_code
  )

#Joining the dataset
ESA_final <- RAW_seed_6000 %>%
  left_join(
    CombinedExp1and2_clean,
    by = c(
      "garden",
      "meso_id",
      "seed_id",
      "plate_seed_letter"
    )
  )

#did it work?
nrow(ESA_final)#yes i hvae 5785 obs, close enough to 6000
View(ESA_final)

 #fINAL CHECK
if ("seed_code" %in% names(CombinedExp1and2_clean)) {
  CombinedExp1and2_clean <- CombinedExp1and2_clean %>%
    rename(plate_seed_letter = seed_code)
}
ESA_final <- RAW_seed_6000 %>%
  left_join(
    CombinedExp1and2_clean,
    by = c(
      "garden",
      "meso_id",
      "seed_id",
      "plate_seed_letter"
    )
  )

nrow(ESA_final)

ESA_final %>%
  count(
    bag_id,
    seed_fate,
    seed_number_within_fate
  ) %>%
  filter(n > 1)

#I WANT O RENAME THE COL NAMES FOR CONSISTENCY HERE
names(ESA_final)
#CLEANIG COL NAMES
ESA_final <- ESA_final %>%
  rename(
    soil_id = soil_id.x,
    bag_obs = obs.x,
    bag_notes = notes.x,
    lab_obs = obs.y,
    lab_notes = notes.y
  ) %>%
  select(
    -soil_id.y,
    -`...6`
  )
#DOUBLE CHECKING
names(ESA_final)

#Adding a col name for colonization and ranking as 0/1 
ESA_final <- ESA_final %>%
  mutate(
    colonized = if_else(is.na(fungi_type), 0, 1)
  )

View(ESA_final) #it worked 

#aDDING A HOMEVS AWAY COL NAME
ESA_final <- ESA_final %>%
  mutate(
    home_away = if_else(seed_id == soil_id, "Home", "Away")
  )

ESA_final <- ESA_final %>%
  mutate(
    home_away = factor(home_away,
                       levels = c("Away", "Home"))
  )

View(ESA_final)#it worked 

#Saving on cleaned data folder
write.xlsx(
  ESA_final,
  file = "ESA_2/Cleaned data/ESA_final6000.xlsx",
  overwrite = TRUE
)

