#
#Starting a new script after pre ESA poster meeting

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
###########

#Does live versus sterilized soil inoculum affect the probability of fungal infection?
 # Does home versus away soil affect the probability of fungal infection?
 # Does the effect of live inoculum differ between home and away soils?


####sTARING HERE


#packeGEs
 
library(tidyverse)
library(lme4)
install.packages("lmerTest")
library(lmerTest)
library(emmeans)
library(performance)
library(ggplot2)


#data
# Import dataset
esa2 <- read.csv("Full.CommonGarden_data.csv")


library(readr)
Full_CommonGarden_data <- read_csv("ESA/Full.CommonGarden_data.csv")
View(Full_CommonGarden_data)

#renaming
esa2 <- Full_CommonGarden_data

##qUESTION 1 : Does innoculation (live versus sterilized autoclaved soil inoculum) affect the probability of fungal infection?

#Response variable
#symptoms_num
#0 = No fungal infection
#1 = Fungal infection

#Because the response is binary, we will use a binomial generalized linear mixed-effects model (GLMM).
#Predictor
#Treat : LIVE / Dead (sterilized)
#Random effect: meso_id
#Accounts for non-independence of seeds from the same mesocosm.


#starting to fit models here
mod_inoculation <- glmer(
  symptoms_num ~ Treat +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = esa2
)

summary(mod_inoculation)


#making sure i hvae innoculation treat as dead or live
esa2$Treat <- factor(
  +     esa2$Treat,
  +     levels = c("DEAD", "LIVE")
  + )

#refitting the model
mod_inoculation <- glmer(
  symptoms_num ~ Treat +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = esa2
)

summary(mod_inoculation)

###Calculating the coeffecient 
exp(fixef(mod_inoculation))

####RESULTS INTERPRETATION
#Seeds exposed to live soil inoculum had an estimated 1.35-fold increase in the odds of fungal infection relative to seeds exposed to sterilized soil inoculum. However, this effect was not statistically significant (β = 0.299, SE = 0.361, z = 0.827, P = 0.408).

##now calculating the proablity of infection 
pred_inoculation <- emmeans(
  mod_inoculation,
  ~ Treat,
  type = "response"
)

pred_inoculation


 
pred_inoculation <- emmeans(
  mod_inoculation,
  ~ Treat,
  type = "response"
)

pred_inoculation
##RESULTS ##REPORT THIS ONE
#Seeds exposed to sterilized soil had about a 13.8% probability of fungal infection.
#Seeds exposed to live soil had about a 17.8% probability of fungal infection

##EVEN BETTER
#The predicted probability of fungal infection was 13.8% (95% CI: 8.1–22.6%) in sterilized soil and 17.8% (95% CI: 12.6–24.6%) in live soil. Although infection probability was estimated to be slightly higher in live soil, the effect of inoculation was not statistically significant (β = 0.299, SE = 0.361, z = 0.827, P = 0.408).

      #OR
#The predicted probability of fungal infection was 13.8% (95% CI: 8.1–22.6%) for seeds exposed to sterilized soil inoculum and 17.8% (95% CI: 12.6–24.6%) for seeds exposed to live soil inoculum. Although infection probability was estimated to be slightly higher in live soil, inoculation treatment did not significantly affect fungal infection (β = 0.299, SE = 0.361, z = 0.827, P = 0.408).

#bIOLOGICAL Interpretation 
#There is no evidence that live soil microbial communities increase fungal infection compared with sterilized soil.

##Plot for Model 1

##Makaing a data frame for the innocuclation model
pred_inoculation_df <- as.data.frame(pred_inoculation)

pred_inoculation_df

##the plot
ggplot(
  pred_inoculation_df,
  aes(x = Treat, y = prob)
) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.12,
    linewidth = 0.8
  ) +
  scale_y_continuous(
    limits = c(0, 0.30),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Soil inoculation treatment",
    y = "Probability of fungal infection"
  ) +
  theme_classic(base_size = 15)


#####
ggplot(
  pred_inoculation_df,
  aes(x = Treat, y = prob)
) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.12,
    linewidth = 0.8
  ) +
  scale_x_discrete(
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.30),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Soil inoculation treatment",
    y = "Probability of fungal infection"
  ) +
  theme_classic(base_size = 15)

#Figure caption
#Figure xxx. Predicted probability of fungal infection in seeds inoculated with sterilized (autoclaved) and live soil inoculum. Points represent model-estimated probabilities and error bars represent 95% confidence intervals. Soil inoculation treatment did not significantly affect fungal infection probability (P = 0.408).


####################################################################


##MODEL 2
#Does planting seeds in home versus away soil affect the probability of fungal infection?
##OR Effect of home vs away soil on fungal infection

#CHEKING IF I HAVE THE HOMEVS AWAY
class(esa2$home_away)
unique(esa2$home_away)

#CONVERTING TO FACTORS
esa2$home_away <- factor(
  esa2$home_away,
  levels = c("home", "away")
)

#CHECKING IF IT WORKED 
levels(esa2$home_away)

#fITTING MODEL
mod_homeaway <- glmer(
  symptoms_num ~ home_away +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = esa2
)

summary(mod_homeaway)


#USING PREDICITONGS HERE 
pred_homeaway <- emmeans(
  mod_homeaway,
  ~ home_away,
  type = "response"
)

pred_homeaway

#rESULTS
#The predicted probability of fungal infection was 13.9% (95% CI: 8.6–21.6%) in home soil and 17.2% (95% CI: 12.6–23.0%) in away soil. Although fungal infection was estimated to be slightly higher in away soil, home versus away soil did not significantly affect the probability of fungal infection (β = 0.255, SE = 0.265, z = 0.962, P = 0.336).

#nb: here results looks teh sdame as the opne frmo mod 1?

pred_homeaway_df <- as.data.frame(pred_homeaway)

ggplot(
  pred_homeaway_df,
  aes(x = home_away, y = prob)
) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.12,
    linewidth = 0.8
  ) +
  scale_x_discrete(
    labels = c(
      "home" = "Home soil",
      "away" = "Away soil"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.30),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Soil origin",
    y = "Predicted probability of fungal infection"
  ) +
  theme_classic(base_size = 15)



#############################

#moDEL 3
#Question 3 Does the effect of live soil inoculation depend on whether seeds are planted in home or away soil?

mod_interaction <- glmer(
  symptoms_num ~ Treat * home_away +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = esa2
)


summary(mod_interaction)


pred_interaction <- emmeans(
  mod_interaction,
  ~ Treat * home_away,
  type = "response"
)

pred_interaction


#RESULTS
#Live inoculum does not consistently increase fungal infection.
#Away soils do not consistently increase fungal infection.
#There is no evidence that the effect of live inoculum depends on whether the soil is home or away.
#OR
##The probability of fungal infection was not significantly affected by soil inoculation treatment (β = 0.057, SE = 0.576, P = 0.922), soil origin (β = 0.018, SE = 0.514, P = 0.972), or their interaction (β = 0.306, SE = 0.598, P = 0.609). Predicted infection probabilities ranged from 13.7% in sterilized home soil to 18.8% in live away soil, but confidence intervals overlapped broadly, indicating no evidence that the effect of live soil inoculum differed between home and away soils.

##THE PLOT

pred_interaction_df <- as.data.frame(pred_interaction)

ggplot(
  pred_interaction_df,
  aes(
    x = home_away,
    y = prob,
    color = Treat,
    group = Treat
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.08,
    linewidth = 0.7
  ) +
  scale_x_discrete(
    labels = c(
      "home" = "Home soil",
      "away" = "Away soil"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.30),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c(
      "DEAD" = "#4D4D4D",
      "LIVE" = "#2C7FB8"
    ),
    labels = c(
      "Sterilized",
      "Live"
    )
  ) +
  labs(
    x = "Soil origin",
    y = "Predicted probability of fungal infection",
    color = "Soil inoculation"
  ) +
  theme_classic(base_size = 15)
##

pred_interaction_df <- as.data.frame(pred_interaction)

ggplot(
  pred_interaction_df,
  aes(
    x = home_away,
    y = prob,
    shape = Treat,
    color = Treat
  )
) +
  geom_point(
    position = position_dodge(width = 0.25),
    size = 3.5
  ) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    position = position_dodge(width = 0.25),
    width = 0.08,
    linewidth = 0.8
  ) +
  scale_x_discrete(
    labels = c(
      "home" = "Home soil",
      "away" = "Away soil"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.30),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c(
      "DEAD" = "#4D4D4D",
      "LIVE" = "#2C7FB8"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  scale_shape_manual(
    values = c(
      "DEAD" = 16,
      "LIVE" = 17
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  labs(
    x = "Soil origin",
    y = "Predicted probability of fungal infection",
    color = "Soil inoculation",
    shape = "Soil inoculation"
  ) +
  theme_classic(base_size = 15)


##RESULTS

#CAPTION
###Figure X. Predicted probability of fungal infection (estimated marginal means ± 95% confidence intervals) for seeds grown in home and away soils under sterilized and live soil inoculation treatments. Although infection probability was highest in the live inoculum–away soil treatment, the generalized linear mixed model detected no significant effects of soil inoculation (β = 0.057, P = 0.922), soil origin (β = 0.018, P = 0.972), or their interaction (β = 0.306, P = 0.609).





########mAKING A SECOND PLOT TO REPRESWENT THE ABOVE
#THE SECOND WILL  HAVE CALCULATIONS PER MEsoID FIRST
infection_meso <- esa2 %>%
  group_by(
    meso_id,
    home_away,
    Treat
  ) %>%
  summarise(
    infection_proportion = mean(symptoms_num, na.rm = TRUE),
    seeds_observed = sum(!is.na(symptoms_num)),
    .groups = "drop"
  )

#CHECKING MESOID
head(infection_meso)

#cREATING A PLOT 
ggplot(
  infection_meso,
  aes(
    x = home_away,
    y = infection_proportion,
    fill = Treat,
    color = Treat
  )
) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.65,
    outlier.shape = NA,
    alpha = 0.75
  ) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.12,
      dodge.width = 0.75
    ),
    size = 2.5,
    alpha = 0.75
  ) +
  scale_x_discrete(
    labels = c(
      "home" = "Home soil",
      "away" = "Away soil"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = c(
      "DEAD" = "#F28E8B",
      "LIVE" = "#39C3C8"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  scale_color_manual(
    values = c(
      "DEAD" = "#F28E8B",
      "LIVE" = "#39C3C8"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  labs(
    x = "Soil origin",
    y = "Fungal infection proportion",
    fill = "Soil inoculation",
    color = "Soil inoculation"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "right"
  )

#INTERPRETATION
#In home soil, live inoculation did not noticeably increase fungal infection compared with sterilized soil.
#In away soil, fungal infection tended to be higher under live inoculum, but the variation among mesocosms was large.

#RESULTS
#Fungal infection proportions were generally low across treatments, with substantial variation among mesocosms. Infection tended to be higher in live away soil than in the other treatment combinations, but the distributions overlapped extensively. This pattern is consistent with the generalized linear mixed model, which found no significant effects of soil inoculation, soil origin, or their interaction on fungal infection probability.

##CAPTION
#Figure X. Proportion of fungal-infected seeds in home and away soils under sterilized and live soil inoculation treatments. Each point represents the infection proportion for an individual mesocosm. Infection proportions were generally similar among treatments, although the live inoculum in away soil showed a tendency toward higher infection



##cHECKING THE DF BETWEEN WILL AND SUMMIT
##Does fungal infection differ between the two common gardens (WIL vs SUM)?
#Does the common garden (WIL (warm)vs. SUM (cool)) influence the probability of fungal infection?

#CGECKING GARDEING VARIBALE SO THAT I CAN CHANGE IT TO FACTOR
names(esa2)
class(esa2$garden)
unique(esa2$garden)

#CONVERTING TO FACTOR
esa2$garden <- factor(
  esa2$garden,
  levels = c("WIL", "SUM")
)

#MODEL
mod_garden <- glmer(
  symptoms_num ~ garden +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = esa2
)
 
summary(mod_garden)

#PREDICTIONS OF THE MODEL
pred_garden <- emmeans(
  mod_garden,
  ~ garden,
  type = "response"
)

pred_garden

#interpretation
#Figure xx. Predicted probability of fungal infection (estimated marginal means ± 95% confidence intervals) for big sagebrush seeds grown in the WIL (warm garden) and SUM  (cool garden) common gardens. Seeds grown in the SUM common garden had a significantly lower probability of fungal infection than those grown in the WIL common garden (β = −0.440, SE = 0.171, P = 0.010).

#plot



##ModelS from ASW
intraspp <- glm(
  symptoms_num ~ Treat *seed_id + Treat *soil,
  family = binomial(link = "logit"),
  data = subset(esa2, seed_id!="HR")
)

summary(intraspp)
####Plots code from ASW####################
library(marginaleffects)
plot_comparisons(intraspp, variable=c("Treat"), by=c("seed_id"))
plot_comparisons(intraspp, variable=c("Treat"), by=c("soil"))
plot_predictions(intraspp, condition=c("seed_id"))

#from here, cleanup up the original brc-bio24 and join it with the infection data 
# double check ids and seeds numbers and rerun the models and plots

###################################################################


pred_full <- emmeans(
  mod_full,
  ~ Treat * home_away | garden,
  type = "response"
)

pred_full

###interpretaion
#Seeds grown in the warm common garden had a significantly higher probability of fungal infection than seeds grown in the cool common garden (β = −0.438, SE = 0.171, z = −2.557, P = 0.011). Predicted infection probability was 19.4% (95% CI: 14.1–26.1%) in the warm common garden and 13.4% (95% CI: 9.4–18.8%) in the cool common garden

#makning the plot
pred_full_df <- as.data.frame(pred_full)

ggplot(
  pred_full_df,
  aes(
    x = home_away,
    y = prob,
    color = Treat,
    group = Treat
  )
) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.08,
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ garden,
    labeller = labeller(
      garden = c(
        "WIL" = "Warm common garden",
        "SUM" = "Cool common garden"
      )
    )
  ) +
  scale_x_discrete(
    labels = c(
      "home" = "Home soil",
      "away" = "Away soil"
    )
  ) +
  scale_color_manual(
    values = c(
      "DEAD" = "#4D4D4D",
      "LIVE" = "#2C7FB8"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 0.35),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Soil origin",
    y = "Predicted probability of fungal infection",
    color = "Soil inoculation"
  ) +
  theme_classic(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "top"
  )

###results
#Seeds grown in the warm common garden consistently exhibited a higher predicted probability of fungal infection than those grown in the cool common garden, regardless of soil inoculation or soil origin.

#figure caption
#The probability of fungal infection was consistently higher in the warm common garden (16.5–22.1%) than in the cool common garden (11.3–15.5%). This difference remained significant after accounting for soil inoculation and soil origin (β = −0.438, SE = 0.171, P = 0.011).