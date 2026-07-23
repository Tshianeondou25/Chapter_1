
#this will use the final data that contains 6000 obs 
library(readr)
library(readxl)
library(ggeffects)
library(lme4)
library(marginaleffects)
library(dplyr)
library(ggplot2)
library(ggh4x)

ESA_final6000 <- read_excel("ESA_2/Cleaned data/ESA_final6000.xlsx")
View(ESA_final6000)

#noticed typo for seedid
#some were TO and OIH
ESA_final6000 <- ESA_final6000 %>%
  mutate(
    seed_id = recode(
      seed_id,
      "OIH" = "OH",
      "TO"  = "TI"
    )
  )

#Model 1: Does fungal colonization vary among sagebrush seed populations and soil populations under live and sterilized soil microbial communities?
intraspp_colonization <- glmer(
  colonized ~ treat * seed_id + treat * soil_id +
    (1 | meso_id),
  family = binomial,
  data = ESA_final6000
)
summary(intraspp_colonization)
#Results:

#PLOT
plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat")
)

#PLot 1 for model 1---- modifying pplot text size here
intraspp_col_plot <- plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat")
) +
  labs(
    x = "Seed population",
    y = "Probability of fungal colonization",
    colour = "Soil treatment"
  ) +
  scale_colour_manual(
    values = c(
      "DEAD" = "#D55E00",
      "LIVE" = "#0072B2"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  coord_cartesian(ylim = c(0, 0.32)) +
  theme_bw(base_size = 22) +
  theme(
    axis.title.x = element_text(
      size = 24,
      face = "bold",
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 22,
      face = "bold",
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
    legend.title = element_text(
      size = 22,
      face = "bold"
    ),
    legend.text = element_text(
      size = 20,
      face = "bold"
    ),
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      linewidth = 1.2,
      fill = NA
    ),
    axis.line = element_line(
      colour = "black",
      linewidth = 1
    ),
    axis.ticks = element_line(
      colour = "black",
      linewidth = 1
    ),
    axis.ticks.length = unit(0.25, "cm")
  )
#view plot
intraspp_col_plot

#Caption:


##MODEL 2: Does fungal colonization differ between home and away soiltreatment ?
homeaway_colonization <- glmer(
  colonized ~ treat * home_away +
    (1 | meso_id),
  family = binomial(link = "logit"),
  data = ESA_final6000
)

summary(homeaway_colonization)

#Results

#plot with modified axes
homeaway_col_plot <- plot_predictions(
  homeaway_colonization,
  condition = c("home_away", "treat")
) +
  labs(
    x = "Soil source",
    y = "Probability of fungal colonization",
    colour = "Soil treatment"
  ) +
  scale_colour_manual(
    values = c(
      "DEAD" = "#D55E00",
      "LIVE" = "#0072B2"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  scale_x_discrete(labels = c("Away", "Home")) +
  coord_cartesian(ylim = c(0.05, 0.21)) +
  theme_bw(base_size = 22) +
  theme(
    axis.title.x = element_text(
      size = 24,
      face = "bold",
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 22,
      face = "bold",
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
    legend.title = element_text(
      size = 22,
      face = "bold"
    ),
    legend.text = element_text(
      size = 20,
      face = "bold"
    ),
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      linewidth = 1.2,
      fill = NA
    ),
    axis.line = element_line(
      colour = "black",
      linewidth = 1
    ),
    axis.ticks = element_line(
      colour = "black",
      linewidth = 1
    ),
    axis.ticks.length = unit(0.25, "cm")
  )
#view
homeaway_col_plot
#####

#Model 3: Does the effect of home versus away soils on fungal colonization differ between comon gardens ( warm and cool garden)
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

#plot with modified axes
homeaway_garden_plot <- plot_predictions(
  homeaway_garden,
  condition = c("home_away", "treat", "garden")
) +
  labs(
    x = "Soil source",
    y = "Probability of fungal colonization",
    colour = "Soil treatment"
  ) +
  scale_colour_manual(
    values = c(
      "DEAD" = "#D55E00",
      "LIVE" = "#0072B2"
    ),
    labels = c(
      "DEAD" = "Sterilized",
      "LIVE" = "Live"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Away" = "Away",
      "Home" = "Home"
    )
  ) +
  facet_wrap2(
    ~ garden,
    labeller = as_labeller(
      c(
        "SUM" = "Cool garden",
        "WIL" = "Warm garden"
      )
    ),
    strip = strip_themed(
      background_x = elem_list_rect(
        fill = c(
          "#DCEAF7",  # pale blue for the cool garden
          "#F9E2CC"   # pale warm orange for the warm garden
        ),
        colour = "black",
        linewidth = 1
      ),
      text_x = elem_list_text(
        colour = "black",
        face = "bold",
        size = 22
      )
    )
  ) +
  theme_bw(base_size = 22) +
  theme(
    axis.title.x = element_text(
      size = 24,
      face = "bold",
      margin = margin(t = 12)
    ),
    axis.title.y = element_text(
      size = 22,
      face = "bold",
      margin = margin(r = 15)
    ),
    axis.text = element_text(
      size = 20,
      face = "bold",
      colour = "black"
    ),
    legend.title = element_text(
      size = 22,
      face = "bold"
    ),
    legend.text = element_text(
      size = 20,
      face = "bold"
    ),
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      linewidth = 1.2,
      fill = NA
    ),
    axis.line = element_line(
      colour = "black",
      linewidth = 1
    ),
    axis.ticks = element_line(
      colour = "black",
      linewidth = 1
    ),
    axis.ticks.length = unit(0.25, "cm"),
    panel.spacing = unit(1.2, "cm"),
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 20
    )
  )
#view
homeaway_garden_plot

###################code from ASW############
plot_comparisons(intraspp, variable=c("Treat"), by=c("seed_id"))
plot_comparisons(intraspp, variable=c("Treat"), by=c("soil"))
plot_predictions(intraspp, condition=c("seed_id"))
