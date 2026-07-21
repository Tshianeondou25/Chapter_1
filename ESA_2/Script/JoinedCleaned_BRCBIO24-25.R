
#this is for joined long data and combined batch for exp 1 and 2
#The final data does not have the joined col name (seed Fate)

#packages  and data
library(tidyverse)
library(dplyr)
library(readxl)

#for combined exp
 
CombinedExp1and2 <- read_excel("ESA_2/Raw data/BRBIO-CombinedExp1and2_batch.xlsx")
View(BRBIO_CombinedExp1and2_batch)

#for long pivot data from raw data
RAW_long <- read_csv("RAW_long.csv")
View(RAW_long)


#Creating a lookup table 
RAW_lookup <- RAW_long %>%
  select(
    garden,
    meso_id,
    soil_id,
    seed_id,
    treat
  ) %>%
  distinct()

#checking if it exist and working okay 
View(RAW_lookup)

#Joining to make 1 datatse
Joined_Cleaned <- CombinedExp1and2 %>%
  left_join(
    RAW_lookup,
    by = c(
      "garden",
      "meso_id",
      "soil_id",
      "seed_id"
    )
  )


 view(Joined_Cleaned ) 
 
 #now i want to assign a 0/1 for the presence of symptomps or colonization on seeds
 #I WILL create and add A NEW COl name  called colonization for ranking 0/1
 
 BRCBIO_JOINEDCLEANED_data <- CommonGarden_master %>%
   mutate(
     colonization = if_else(
       !is.na(fungi_type) & trimws(fungi_type) != "",
       1L,
       0L
     )
   )
 
 view( BRCBIO_JOINEDCLEANED_data) 
 #the joining seems to have worked here. I have the colonization as added col name and it is ranked as 0/1 for the p[resence and absence of infection 
 
 
 ##ADDING A HOME AND AWAY COL 
 library(dplyr)
 
 BRCBIO_JOINEDCLEANED_data <- BRCBIO_JOINEDCLEANED_data %>%
   mutate(
     home_away = if_else(seed_id == soil_id, "Home", "Away")
   )
 
 #mAKING IT A FACTOR
 BRCBIO_JOINEDCLEANED_data <- BRCBIO_JOINEDCLEANED_data %>%
   mutate(
     home_away = factor(home_away, levels = c("Away", "Home"))
   )
 
 #cHECKING IF IT WORKED
 table(BRCBIO_JOINEDCLEANED_data$home_away)
 
 View(BRCBIO_JOINEDCLEANED_data)
 
 
 #Saving this cleaned data in cleaned data folder
 #will be using this for analysis
 write.csv(
   BRCBIO_JOINEDCLEANED_data,
   "ESA_2/Cleaned Data/BRCBIO_JOINEDCLEANED_data.csv",
   row.names = FALSE
 )

 