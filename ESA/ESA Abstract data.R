#ESA ROUGH DATA

# Analysis: ESA CONFERENCE: Fungal colonization in sagebrush seeds
# Description:
# This will include a join to include treatment on the fungal data sheet. i will Join ESA germination data with treatment sheet, create

 # 1. Load packages
 
library(readxl)
library(tidyverse)
library(janitor)
library(lme4)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggeffects)
library(scales)
 



# 2. Reading in the two files
#this one has 

ESA_data <- read_excel("ESA/ESA data.xlsx")
View(ESA_data)

#will use this one to join the innoc treatments column

New_ComGarden_Intactseedcode_rawdata <- read_excel("ESA/New ComGarden Intactseedcode_rawdata.xlsx")
View(New_ComGarden_Intactseedcode_rawdata)

#same as above
treat <- read_excel(
  "ESA/New ComGarden Intactseedcode_rawdata.xlsx",
  sheet = 1
)
View(treat)

# -------------------------
# 3. Check column names
# -------------------------
names(esa) #ignore this one
names(treat)

colnames(treat)

#Renaming here because the previous script had a different naming 

treat <- treat %>%
  rename(
    garden = Garden,
    meso_id = MesoID,
    soil = Soil,
    treatment = Treat,
    rep_id = RepID,
    seeds = Seeds,
    plate_id = plateID,
    seed_label = Seed_Label,
    intact_seeds = Intact.seeds
  )

#Checking and making sure columns looks great
treat <- treat %>%
  mutate(
    Garden = str_trim(toupper(Garden)),
    MesoID = str_trim(as.character(MesoID)),
    Soil = str_trim(toupper(Soil)),
    Seeds = str_trim(toupper(Seeds)),
    Treat = str_trim(toupper(Treat))
  )

# 5. Keep only columns needed for joining
# -------------------------

treat_innoc <- treat %>%
  select(Garden, MesoID, Soil, Seeds, Treat) %>%
  distinct()


##############Checking  ESA data before joining 
names(ESA_data)
head(ESA_data)

esa <- ESA_data %>%
  rename(
    plate_id = PlateID,
    garden = Garden,
    meso_id = MesoID,
    soil = Soil,
    seed_id = SeedID,
    intact_seeds = `Intact seeds`,
    seed_code = `Seed Code`,
    plating_date = `Plating Date`,
    germination_date = `Germination Date`,
    fungi_date_type = `Fungi Date + Type`,
    death_date = `Death Date`,
    obs = Obs,
    notes = Notes
  ) %>%
  mutate(
    garden = str_trim(toupper(garden)),
    meso_id = str_trim(as.character(meso_id)),
    soil = str_trim(toupper(soil)),
    seed_id = str_trim(toupper(seed_id))
  )

#cHECKING 
names(esa)
head(esa)


########nnow doing a left join!!

esa2 <- esa %>%
  left_join(
    treat %>%
      select(Garden, MesoID, Soil, Seeds, Treat) %>%
      distinct(),
    by = c(
      "garden" = "Garden",
      "meso_id" = "MesoID",
      "soil" = "Soil",
      "seed_id" = "Seeds"
    )
  )

##Making variables forb  left join results and ##checking if home vs away worked
esa2 <- esa2 %>%
  mutate(
    symptoms_num = if_else(!is.na(fungi_date_type), 1, 0),
    germinated_num = if_else(!is.na(germination_date), 1, 0),
    home_away = if_else(seed_id == soil, "home", "away")
  )

#Checking if it worked 
table(esa2$Treat, useNA = "ifany")
table(esa2$home_away, useNA = "ifany")
table(esa2$garden, useNA = "ifany")

#Making factors
esa2 <- esa2 %>%
  mutate(
    Treat = factor(Treat),
    home_away = factor(home_away),
    garden = factor(garden),
    meso_id = factor(meso_id)
  )

#nb: ESA2 DATA SET REPRESENT A JOINT VERSION OF  DEAD, LIVE,  AND HOMEVS AWAY
View(esa2)

#Saving this data
write.csv(esa2, "Full.CommonGarden_data", row.names = FALSE)

#dOING THIS bwcuase i cant fint the file 
class(Full.CommonGarden_data)

write.csv(
  esa2,
  file = "Full.CommonGarden_data.csv",
  row.names = FALSE
)

list.files()





###Now starting model here###
#I will be making 3 models
#1. 1. Does fungal infection reduce germination?
#Modelling symptoms 
#renaming the treatment col first 
treatment <- treat
rm(treat)

mod_fungi <- glmer(
  symptoms_num ~ home_away * Treat + garden + (1 | meso_id),
  data = esa2,
  family = binomial
)

 summary(mod_fungi)

#Results:
#There is no evidence that seeds placed in their home soils experienced different fungal infection rates than seeds placed in away soils.
#The effect of live soil inocula was similar for home and away soils
#Seeds deployed in the Wilson common garden had a significantly higher probability of fungal infection than those deployed in the Summit common garden.

 
 ###PLOT FIGURE

 
 ggplot(esa2, aes(x = garden, y = symptoms_num)) +
   stat_summary(fun = mean, geom = "bar", fill = "steelblue", width = 0.6) +
   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   labs(
     x = "Common garden",
     y = "Proportion infected",
     title = "Fungal infection difference between common gardens"
   ) +
   theme_classic(base_size = 14)
 
 ##Second plot with different colours key for each garden
 #DONT LIKE THE 1ST ONE
 
 
 ######FIGURE222
 
 # Get predicted probabilities from Model 1
 pred_fungi_garden <- ggpredict(
   mod_fungi,
   terms = "garden"
 )
 
 # Check the predictions
 pred_fungi_garden
 ##
 
 #THE PLOT
 figure1 <- ggplot(
   pred_fungi_garden,
   aes(x = x, y = predicted)
 ) +
   geom_point(size = 4) +
   geom_errorbar(
     aes(ymin = conf.low, ymax = conf.high),
     width = 0.08,
     linewidth = 1
   ) +
   scale_x_discrete(
     labels = c(
       "SUM" = "Summit",
       "WIL" = "Wilson"
     )
   ) +
   scale_y_continuous(
     labels = percent_format(accuracy = 1),
     limits = c(0, NA),
     expand = expansion(mult = c(0, 0.08))
   ) +
   labs(
     x = "Common garden",
     y = "Probability of fungal infection"
   ) +
   theme_classic(base_size = 16) +
   theme(
     axis.title = element_text(face = "plain"),
     axis.text = element_text(size = 14)
   )
 
 figure1
 
 
 
 ###############
 
 #using colors here 
 figure1 <- ggplot(
   pred_fungi_garden,
   aes(
     x = x,
     y = predicted,
     color = x
   )
 ) +
   geom_point(size = 4) +
   geom_errorbar(
     aes(
       ymin = conf.low,
       ymax = conf.high
     ),
     width = 0.08,
     linewidth = 1
   ) +
   scale_color_manual(
     values = c(
       "SUM" = "#009E73",   # Summit
       "WIL" = "#0072B2"    # Wilson
     ),
     guide = "none"
   ) +
   scale_x_discrete(
     labels = c(
       "SUM" = "Summit",
       "WIL" = "Wilson"
     )
   ) +
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1),
     limits = c(0, NA),
     expand = expansion(mult = c(0, 0.08))
   ) +
   labs(
     x = "Common garden",
     y = "Probability of fungal infection"
   ) +
   theme_classic(base_size = 16) +
   theme(
     axis.title = element_text(face = "plain"),
     axis.text = element_text(size = 14)
   )
 
 figure1
 
 
 #fIGURE CAPTION
 
 #Figure 1. Model-predicted probability of fungal infection in big sagebrush seeds deployed at the Summit and Wilson common gardens. Points represent estimated marginal means and error bars represent 95% confidence intervals. Fungal infection was significantly higher at Wilson than at Summit (GLMM, p = 0.011).
 
 #sAVING THE PLOT 
 ggsave(
   "Figures/Figure1_fungal_infection_by_garden.png",
   plot = figure1,
   width = 6,
   height = 5,
   dpi = 300
 ) 
 


 

#2.Does fungal infection reduce the probability of germination after accounting for treatment and common garden?

 mod_germ <- glmer(
   germinated_num ~ symptoms_num + home_away + Treat + garden +
     (1 | meso_id),
   data = esa2,
   family = binomial,
   na.action = na.omit
 )
 
 summary(mod_germ)
 
 #results 
 #Seeds with fungal infection were less likely to germinate than seeds without fungal infection.
 #Observed germination was approximately 52% lower in infected seeds, and fungal infection showed a marginally non-significant negative effect on germination (GLMM, P = 0.051).
 #can also say it this way:: Germination was approximately 50% lower in infected seeds, although the relationship was only marginally significant in the GLMM (P = 0.051).
 
 esa2 %>%
   group_by(symptoms_num) %>%
   summarise(
     germination_rate = mean(germinated_num),
     n = n()
   )
 
 #Results
# Out of 788 seeds without fungal infection, 9.01% germinated.
 #Out of 206 seeds with fungal infection, only 4.37% germinated
 #Observed germination was approximately 52% lower in seeds with fungal infection than in seeds without fungal infection.
 #After accounting for garden, treatment, home–away status, and variation among seed populations, infected seeds still tended to germinate less, but the evidence was marginally significant (P = 0.051).
 
 
 #another way of interpreting the dat
 #Seeds with visible fungal infection had lower germination rates than seeds without fungal infection (4.4% vs. 9.0%), representing an approximately 50% reduction in germination. This negative relationship was marginally significant after accounting for garden, treatment, home–away status, and seed population in a GLMM (β = −0.74 ± 0.38 SE, P = 0.051).
 #######PLOT@########
 
 
 ggplot(esa2, aes(x = factor(symptoms_num), y = germinated_num)) +
   stat_summary(fun = mean, geom = "bar",
                fill = "forestgreen", width = 0.6) +
   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
   scale_x_discrete(labels = c("No fungi", "Fungi")) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   labs(
     x = "Fungal infection",
     y = "Germination rate",
   ) +
   theme_classic(base_size = 14)
 
 
 #changing colours
 scale_fill_manual(values = c(
   "0" = "#009E73",   # No fungi (green)
   "1" = "#D55E00"    # Fungi (rust orange)
 ))
 
 
 ggplot(esa2, aes(x = factor(symptoms_num),
                  y = germinated_num,
                  fill = factor(symptoms_num))) +
   stat_summary(
     fun = mean,
     geom = "bar",
     width = 0.6
   ) +
   stat_summary(
     fun.data = mean_se,
     geom = "errorbar",
     width = 0.2
   ) +
   scale_fill_manual(
     values = c(
       "0" = "#009E73",
       "1" = "#D55E00"
     ),
     guide = "none"
   ) +
   scale_x_discrete(labels = c("No fungi", "Fungi")) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   labs(
     x = "Fungal infection",
     y = "Germination rate"
   ) +
   theme_classic(base_size = 14)
 
##caption:
 #Figure xxxxx. Observed germination rates of big sagebrush (Artemisia tridentata) seeds with and without visible fungal infection following overwinter field exposure. Seeds with visible fungal infection germinated approximately 50% less frequently (4.4%) than seeds without fungal infection (9.0%). Error bars represent ± SE.
 
 
 
 ##Model 2 second plot
 #here i will use emmeans- this helps to compare the gardens while holding everything else in the model constant
 
 infection_garden <- esa2 %>%
   group_by(garden) %>%
   summarise(
     infection_rate = mean(symptoms_num),
     n = n(),
     se = sd(symptoms_num)/sqrt(n)
   )
 
 infection_garden
 
 
 ###the plot
 
 infection_garden_plot <- ggplot(
   infection_garden,
   aes(x = garden, y = infection_rate)
 ) +
   
   geom_point(size = 5) +
   
   geom_errorbar(
     aes(
       ymin = infection_rate - se,
       ymax = infection_rate + se
     ),
     width = 0.08,
     linewidth = 1
   ) +
   
   geom_text(
     aes(label = scales::percent(infection_rate, accuracy = 0.1)),
     vjust = -1,
     size = 5
   ) +
   
   scale_x_discrete(
     labels = c(
       SUM = "Summit",
       WIL = "Wilson"
     )
   ) +
   
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1),
     limits = c(0, NA)
   ) +
   
   labs(
     x = "Common garden",
     y = "Observed fungal infection (%)"
   ) +
   
   theme_classic(base_size = 16)
 
 infection_garden_plot
 
 #didnt save the second plot
 
 
 
 
#3. Does the effect of fungal infection on germination differ between the two common gardens?
 mod_germ_garden <- glmer(
   germinated_num ~ symptoms_num * garden +
     home_away + Treat +
     (1 | meso_id),
   data = esa2,
   family = binomial,
   na.action = na.omit
 )

 summary(mod_germ_garden)

#Results:
#Germination did not differ between the two common gardens after accounting for fungal infection.
#Results with output:
#Fungal infection was associated with a significant reduction in seed germination (GLMM, β = −1.58, P = 0.036). While fungal infection differed between common gardens, the effect of fungal infection on germination did not vary significantly between gardens (P = 0.135).


 
 #GROUPED OUTPUT
 esa2 %>%
   group_by(symptoms_num) %>%
   summarise(
     germination_rate = mean(germinated_num),
     n = n()
   )

 # INTERPRETING THE ABOVE RESULTS FOR OBSEVED GERMIATION RATES
 
 #Observed germination was approximately 52% lower in infected seeds than in uninfected seeds.
 #A change from 9% germination to 4.4% germination is  a 50% change (4.4% is half of 9%). 4.4/9 = 0.49
 
 #NOW THWET CAPTION FOR THE ABVE FIGURE
 
#Figure XXXX. Observed germination of big sagebrush (Artemisia tridentata) seeds with and without fungal infection in the Summit and Wilson common gardens. Germination decreased from 9.0% in uninfected seeds to 4.4% in infected seeds (approximately a 50% reduction). Error bars represent ±1 SE. Fungal infection significantly reduced the probability of germination (GLMM, β = −1.576, P = 0.036).
 
 
 
 
# second pLOT using prediction instead of observed##########
 ggplot(esa2,
        aes(x = factor(symptoms_num),
            y = germinated_num,
            fill = garden)) +
   stat_summary(fun = mean,
                geom = "bar",
                position = position_dodge(0.8),
                width = 0.7) +
   stat_summary(fun.data = mean_se,
                geom = "errorbar",
                position = position_dodge(0.8),
                width = 0.2) +
   scale_x_discrete(labels = c("No fungi", "Fungi")) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   labs(
     x = "Fungal infection",
     y = "Germination rate",
     fill = "Garden",
   ) +
   theme_classic(base_size = 14)


 
###second plot 
 
 germination_garden <- esa2 %>%
   group_by(garden, symptoms_num) %>%
   summarise(
     germination_rate = mean(germinated_num),
     n = n(),
     se = sd(germinated_num) / sqrt(n),
     .groups = "drop"
   ) %>%
   mutate(
     infection_status = factor(
       symptoms_num,
       levels = c(0, 1),
       labels = c("No fungi", "Fungi")
     )
   )
 
 germination_garden

##The plot
 
 germination_garden_plot <- ggplot(
   germination_garden,
   aes(
     x = infection_status,
     y = germination_rate,
     group = garden,
     shape = garden,
     linetype = garden
   )
 ) +
   geom_line(
     position = position_dodge(width = 0.15),
     linewidth = 0.9
   ) +
   geom_point(
     position = position_dodge(width = 0.15),
     size = 4
   ) +
   geom_errorbar(
     aes(
       ymin = pmax(0, germination_rate - se),
       ymax = germination_rate + se
     ),
     position = position_dodge(width = 0.15),
     width = 0.08,
     linewidth = 0.8
   ) +
   scale_y_continuous(
     labels = percent_format(accuracy = 1),
     limits = c(0, NA),
     expand = expansion(mult = c(0, 0.08))
   ) +
   labs(
     x = "Fungal infection",
     y = "Observed germination rate",
     shape = "Common garden",
     linetype = "Common garden"
   ) +
   theme_classic(base_size = 16) +
   theme(
     axis.title = element_text(face = "bold"),
     legend.position = "right"
   )
 
 
 germination_garden_plot









###################################################################
###Joining two files
esa2 <- esa %>%
  left_join(
    treat %>% select(garden, meso_id, soil, seeds, treat) %>% distinct(),
    by = c(
      "garden" = "garden",
      "meso_id" = "meso_id",
      "soil" = "soil",
      "seed_id" = "seeds"
    )
  )

#Fixng home vs away
esa2 <- esa2 %>%
  mutate(
    symptoms_num = if_else(!is.na(fungi_date_type), 1, 0),
    germinated_num = if_else(!is.na(germination_date), 1, 0),
    home_away = if_else(seed_id == soil, "home", "away")
  )

#Checking if it worked 
table(esa2$treat, useNA = "ifany")
table(esa2$home_away, useNA = "ifany")
table(esa2$garden, useNA = "ifany")

#Making factors
esa2 <- esa2 %>%
  mutate(
    treat = factor(treat),
    home_away = factor(home_away),
    garden = factor(garden),
    meso_id = factor(meso_id)
  )

########################################################


###Overall results interpretation


#Fungal colonization was modeled using a binomial generalized linear mixed-effects model with mesocosm as a random effect. 
#Colonization did not differ between home and away soils or between live and dead soil treatments, and there was no interaction between these factors. However, colonization varied significantly between common garden environments, with higher colonization observed in the WIL site compared to the SUM site. 
#These results suggest that environmental conditions, rather than plant–soil feedbacks or inoculation treatments, are the primary drivers of fungal colonization at the seed stage.

###############################################################################################################################################################################################################################################################################################


##New models after ESA meeting##

#Notes
#Need to change questions and analysis
#Warm = Wilson
#Cool = Summit

#New questions:
#1: 

#Live Microbial inoculation
##How does inoculation reduces germination/ influences infection? =?treatment reduces germination?

#How does inoculation (live/dead) influence infection rate?
#How does translocation affect or influence infection 




