
#this will use the final data that contains 6000 obs 
library(readr)
library(readxl)
library(ggeffects)
library(lme4)
library(marginaleffects)
library(dplyr)
library(ggplot2)

ESA_final6000 <- read_excel("ESA_2/Cleaned data/ESA_final6000.xlsx")
View(ESA_final6000)

#noticed tupo for seedid
#some were TO and OIH
ESA_final6000 <- ESA_final6000 %>%
  mutate(
    seed_id = recode(
      seed_id,
      "OIH" = "OH",
      "TO"  = "TI"
    )
  )

#Model 1
intraspp_colonization <- glmer(
  colonized ~ treat * seed_id + treat * soil_id +
    (1 | meso_id),
  family = binomial,
  data = ESA_final6000
)
summary(intraspp_colonization)

#PLOT
plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat")
)

##MODEL 2:DOES COLONIZATION DIFFER IN HOME AND AWAY SOIL
homeaway_colonization <- glmer(
  colonized ~ treat * home_away +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = ESA_final6000
)

summary(homeaway_colonization)

#plot
plot_predictions(
  homeaway_colonization,
  condition = c("home_away", "treat")
) +
  labs(
    x = "Soil origin",
    y = "Predicted probability of fungal colonization",
    color = "Soil treatment"
  ) +
  scale_color_manual(
    values = c("DEAD" = "#999999", "LIVE" = "#1B9E77"),
    labels = c("Dead (sterilized)", "Live")
  ) +
  scale_x_discrete(labels = c("Away", "Home")) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

#Model 3 Does the home vs. away effect differ between gardens?
homeaway_garden <- glmer(
  colonized ~ treat * home_away * garden +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = ESA_final6000
)

summary(homeaway_garden)

#plot
plot_predictions(
  homeaway_garden,
  condition = c("home_away", "treat", "garden")
)
