
#this will use the final data that contains 6000 obs 
library(readr)
library(readxl)
library(ggeffects)
library(lme4)
library(marginaleffects)
library(dplyr)
library(ggplot2)
library(ggh4x)
library(brms)
library(bayestestR)

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

ESA_final6000 <- ESA_final6000 %>% mutate(germ_plate = ifelse(is.na(germination_date), 0, 1),
                                          germinated = ifelse((seed_fate=="germinated" |
                                                             germ_plate ==1), 1, 0),
                                          viable = ifelse((seed_fate=="germinated" |
                                                              germ_plate ==1), 1, 0),
                                          uniquemesoid = paste(garden, meso_id, sep="-"))




#Model 1: Does fungal colonization vary among sagebrush seed populations and soil populations under live and sterilized soil microbial communities?
intraspp_colonization<- brm(
  colonized ~ treat * seed_id + treat * soil_id +
    (1 | uniquemesoid),
  family = bernoulli, chains=2,
  data = subset(ESA_final6000)
)
summary(intraspp_colonization)
mcmc_plot(intraspp_colonization)

intraspp_germ <- brm(
  germinated  ~ treat * seed_id + treat * soil_id +
    (1 | uniquemesoid),
  family = bernoulli, chains=2,
  data = subset(ESA_final6000)
)
summary(intraspp_germ)
mcmc_plot(intraspp_germ)
#Results:

#PLOT
plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat"), re_formula=NA
)

plot_comparisons(
  intraspp_colonization, variable=c("treat"),
  condition = c("soil_id"), re_formula=NA
) + ylab("Effect of inoculation on probability of\nseed colonization by culturable fungi") + xlab("Soil Origin ID")

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



#Model 3: Does the effect of home versus away soils on fungal colonization differ between comon gardens ( warm and cool garden)
homeaway_garden_germ <- brm(
  germinated |trials(1) ~ treat * home_away * garden + 
    (1|soil_id) + (1|seed_id) + 
    (1 | uniquemesoid),
  family = binomial(link = "logit"), chains=2,
  data = subset(ESA_final6000)
)
p_direction(homeaway_garden_germ)
mcmc_plot(homeaway_garden_germ, prob_outer=0.9)


homeaway_garden <- brm(
  colonized |trials(1) ~ treat * home_away * garden + 
    (1|soil_id) + 
    (1 | uniquemesoid),
  family = binomial(link = "logit"), chains=2,
  data = ESA_final6000
)
p_direction(homeaway_garden)
mcmc_plot(homeaway_garden, prob_outer=0.9)
summary(homeaway_garden)

#plot
plot_predictions(
  homeaway_garden, re_formula=NA,
  condition = c("home_away", "treat", "garden")
) + ylab("Effect of soil microbial inoculation\non probability of seed colonization by culturable fungi")

plot_comparisons(
  homeaway_garden, re_formula=NA, variable=c("treat"),
  condition = c("home_away", "garden")
) + xlab("Local adaptation treatment") +
  ylab("Effect of soil microbial inoculation\non probability of seed colonization by culturable fungi") +
  geom_hline(yintercept=0, linetype="dashed") + theme_bw()

plot_comparisons(
  homeaway_garden_germ, re_formula=NA, variable=c("treat"),
  condition = c("home_away", "garden")
) + xlab("Local adaptation treatment") +
  ylab("Effect of soil microbial inoculation\non probability of seed germination") +
  geom_hline(yintercept=0, linetype="dashed") + theme_bw()



#plot with modified axes
homeaway_garden_plot <- plot_predictions(
  homeaway_garden,
  condition = c("home_away", "treat", "garden"), re.form=NA
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
