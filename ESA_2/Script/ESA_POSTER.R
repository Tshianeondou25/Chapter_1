#Sstarting a third analysis for ESA HERE
#I will be using marginal effects instead of emmeans to model and plot figures
#i WILL USE these results for the poster

#Objectives from proposal
#Objective 1.1: Quantify intraspecific variation in the impacts of soil pathogens on overwinter survival and colonization of sagebrush seeds.
#Objective 1.2: Identify whether population-level variation impacts of pathogens are consistent with negative feedbacks (thus, “enemy release” when populations move to new areas) or positive population feedbacks, which will slow range shifts and favor “local” seeds, relative to the soil microbial community.


#questions for this poster 
#1. Do sagebrush seed populations differ in their probability of fungal colonization?

#Data and packages 

library(readr)
library(ggeffects)
library(lme4)
library(marginaleffects)
library(dplyr)

ESA_Poster <- read_csv("ESA_2/Cleaned data/BRCBIO_JOINEDCLEANED_data.csv")
View(ESA_Poster)


#Model 1
intraspp_colonization <- glm(
  colonization ~ treat * seed_id + treat * soil_id,
  family = binomial(link = "logit"),
  data = ESA_Poster
)

summary(intraspp_colonization)

###Results:
#results demonstrate that fungal colonization varied among sagebrush populations and that the effects of the soil microbial community were population dependent.

#Plot figures
#plot 1
plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat")
)


#Same figure with re-labelled axes

plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat")
) +
  labs(
    x = "Sagebrush seed population",
    y = "Predicted probability of fungal colonization",
    color = "Soil treatment"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
##
#Figure Caption:
#Figure XXXX. Predicted probability of fungal colonization for each sagebrush seed population in live and dead soil treatments. Points represent model predictions from a binomial generalized linear model, and error bars represent 95% confidence intervals.

#Save plot

ggsave(
  "ESA_2/Figures/SeedID_Colonization.png",
  width = 8,
  height = 5,
  dpi = 300
)

#MODEL 2:DOES COLONIZATION DIFFER IN HOME AND AWAY SOIL
homeaway_colonization <- glm(
  colonization ~ treat * home_away,
  family = binomial(link = "logit"),
  data = ESA_Poster
)

summary(homeaway_colonization)
#RESULTS:


#PLOT
plot_predictions(
  homeaway_colonization,
  condition = c("home_away", "treat")
) +
  labs(
    x = "Soil origin",
    y = "Predicted probability of fungal colonization",
    color = "Soil treatment"
  ) +
  theme_classic()

#CAPTION

#Saving the plot
ggsave(
  filename = "ESA_2/Figures/HomeAway_Colonization.png",
  width = 8,
  height = 5,
  dpi = 300
)


#Model 3
#cHECKING THE EFFECT OF GARDEN WARM(WILLSON) VS COOL (SUMMIT)
homeaway_garden <- glm(
  colonization ~ treat * home_away * garden,
  family = binomial(link = "logit"),
  data = ESA_Poster
)
summary(homeaway_garden)

#Results
#fungal colonization was higher in the warm garden than in the cool garden
#These findings are consistent with the prediction that warming climates could increase pathogen pressure on sagebrush seeds???

#pLOT
plot_predictions(
  homeaway_garden,
  condition = c("garden", "home_away", "treat")
)


#renaming the gardens from the plot
plot_predictions(
  homeaway_garden,
  condition = c("garden", "home_away", "treat")
) +
  scale_x_discrete(
    labels = c(
      "SUM" = "Warm",
      "WIL" = "Cool"
    )
  ) +
  labs(
    x = "Experimental garden",
    y = "Predicted probability of fungal colonization",
    color = "Soil origin"
  ) +
  theme_classic()

#Caption
#Figure xxX. Predicted probability of fungal colonization of big sagebrush  seeds in warm and cool experimental gardens for home and away soils under live and sterilized (dead) soil treatments. 

#save plot
ggsave(
  "ESA_2/Figures/Figure_HomeAway_Garden_Colonization.png",
  width = 8,
  height = 5,
  dpi = 300
)

