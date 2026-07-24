#Using the edits from ASW
#Changing from glmm to brms

#Packages
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

#noticed A typo for seedid
#some were TO and OIH
ESA_final6000 <- ESA_final6000 %>%
  mutate(
    seed_id = recode(
      seed_id,
      "OIH" = "OH",
      "TO"  = "TI"
    )
  )

ESA_final6000 <- ESA_final6000 %>% mutate(germ_plate = ifelse(is.na(germination_date), 0, 1),                                         germinated = ifelse((seed_fate=="germinated" |                                                               germ_plate ==1), 1, 0),                                         viable = ifelse((seed_fate=="germinated" |                                                            germ_plate ==1), 1, 0),                                         uniquemesoid = paste(garden, meso_id, sep="-"))

#Model 1: Does fungal colonization vary among sagebrush seed populations and soil populations under live and sterilized soil microbial communities?
intraspp_colonization <- brm(
  colonized ~ treat * seed_id + treat * soil_id +
    (1 | uniquemesoid),
  family = bernoulli,
  chains = 4,
  iter = 4000, #Increased iterations to improve the Effective Sample Size (ESS)
  # after the initial model produced a low Bulk ESS warning.
  warmup = 2000,   
  data = subset(ESA_final6000)
)

summary(intraspp_colonization)
mcmc_plot(intraspp_colonization)

#checking results table
p_direction(intraspp_colonization)

#PLOT
plot_predictions(
  intraspp_colonization,
  condition = c("seed_id", "treat"), re_formula=NA
)

plot_comparisons(
  intraspp_colonization, variable=c("treat"),
  condition = c("soil_id"), re_formula=NA
) + ylab("Effect of inoculation on probability of\nseed colonization by culturable fungi") + xlab("Soil Origin ID")

#Modifying the axes

plot_comparisons(
  intraspp_colonization,
  variable = "treat",
  condition = "soil_id",
  re_formula = NA
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ylab(
    "Effect of inoculation on probability of\nseed colonization by culturable fungi)"
  ) +
  xlab("Soil origin ID")

#Saving the plot
ggsave("Figures/BRMS_Intraspecific_Colonization.png",
       width = 7, height = 5, dpi = 600)
 
#Model 2: Do soil microbial treatment influence germination?
intraspp_germ <- brm(
  germinated ~ treat * seed_id + treat * soil_id +
    (1 | uniquemesoid),
  family = bernoulli,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  data = subset(ESA_final6000)
)
summary(intraspp_germ)
mcmc_plot(intraspp_germ)
#Saving model here incase the laptop crash
saveRDS(intraspp_germ,"intraspp_germ.rds")

#Results TABLE
p_direction(intraspp_germ)
 
#Plot
Germination <- plot_comparisons(
  intraspp_germ,
  variable = "treat",
  condition = "soil_id",
  re_formula = NA
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  ylab("Effect of inoculation on\nprobability of seed germination") +
  xlab("Soil origin") +
  theme_bw(base_size = 16)

Germination

#####no grid
Germination <- plot_comparisons(
  intraspp_germ,
  variable = "treat",
  condition = "soil_id",
  re_formula = NA
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.8
  ) +
  ylab("Effect of inoculation on\nprobability of seed germination") +
  xlab("Soil origin") +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 18, colour = "black")
  )
Germination

#####@@@
Germination <- plot_comparisons(
  intraspp_germ,
  variable = "treat",
  condition = "soil_id",
  re_formula = NA
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.8
  ) +
  labs(
    x = "Soil origin",
    y = "Inoculation effect\non germination"
  ) +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.title.x = element_text(
      face = "bold",
      size = 20,
      margin = margin(t = 8)
    ),
    
    axis.title.y = element_text(
      face = "bold",
      size = 20,
      margin = margin(r = 12)
    ),
    
    axis.text.x = element_text(
      size = 18,
      colour = "black"
    ),
    
    axis.text.y = element_text(
      size = 18,
      colour = "black"
    ),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 35
    )
  )

Germination


###

#save plot
ggsave("Figures/germination_plot.png",
       width = 7, height = 5, dpi = 600)

#Model 3.1: germination in home vs away
#
homeaway_garden_germ <- brm(
  germinated | trials(1) ~ treat * home_away * garden +
    (1 | soil_id) +
    (1 | seed_id) +
    (1 | uniquemesoid),
  family = binomial(link = "logit"),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  data = subset(ESA_final6000)
)

mcmc_plot(homeaway_garden_germ, prob_outer=0.9)
summary(homeaway_garden_germ)
#save model

saveRDS(intraspp_germ,"homeaway_garden_germ.rds")

#results table 
p_direction(homeaway_garden_germ)


#Plot
plot_predictions(
  homeaway_garden_germ,
  re_formula = NA,
  condition = c("home_away", "treat", "garden")
) +
  ylab("Predicted probability of\nseed germination") +
  xlab("Local adaptation treatment") +
  theme_bw(base_size = 16)

plot_comparisons(
  homeaway_garden_germ,
  re_formula = NA,
  variable = "treat",
  condition = c("home_away", "garden")
) +
  xlab("Soil origin") +
  ylab("Effect of soil microbial inoculation\non probability of seed germination") +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  scale_colour_discrete(
    name = "Garden",
    labels = c("SUM" = "Cool garden", "WIL" = "Warm garden")
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


########Making a second plot to show local adaptation with lines

predictions(
  homeaway_garden_germ,
  newdata = datagrid(
    home_away = c("Home","Away"),
    garden = c("SUM","WIL"),
    treat = c("DEAD","LIVE")
  ),
  type = "response",
  re_formula = NA
) %>%
  
  mutate(
    
    home_away = factor(
      home_away,
      levels = c("Home","Away"),
      labels = c("Home soil","Away soil")
    ),
    garden = factor(
      garden,
      levels = c("SUM","WIL"),
      labels = c("Cool","Warm")
    ),
    treat = factor(
      treat,
      levels = c("DEAD","LIVE"),
      labels = c("Sterilized","Live")
    )
  ) %>%
  ggplot(
    aes(
      x = home_away,
      y = estimate,
      colour = garden,
      group = garden
    )
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    colour = "grey55"
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = .05,
    linewidth = .8
  ) +
  facet_wrap(
    ~treat,
    nrow = 1
  ) +
  scale_colour_manual(
    values = c(
      "Cool" = "#0072B2",
      "Warm" = "#E69F00"
    )
  ) +
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0,1,.2)
  ) +
  labs(
    x = "Soil source",
    y = "Probability of\nseed germination",
    colour = "Garden"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = .5
    ),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(
      face = "bold",
      size = 16
    ),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

#Save plot:
ggsave("Figures/CommonGarden_Germ.png",
       width = 7, height = 5, dpi = 600)

#############################################################################
#3.2 colonization home vs away

homeaway_garden_colonization <- brm(
  colonized | trials(1) ~ treat * home_away * garden +
    (1 | soil_id) +
    (1 | seed_id) +
    (1 | uniquemesoid),
  family = binomial(link = "logit"),
  chains = 4,
  iter = 4000,
  warmup = 2000,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 15
  ),
  data = ESA_final6000
)

summary(homeaway_garden_colonization)
mcmc_plot(homeaway_garden_colonization, prob_outer=0.9)
p_direction(homeaway_garden_colonization)
saveRDS(intraspp_germ,"homeaway_garden_colonization")

 
#plot
plot_comparisons(
  homeaway_garden_colonization, re_formula=NA, variable=c("treat"),
  condition = c("home_away", "garden")
) + xlab("Local adaptation treatment") +
  ylab("Effect of soil microbial inoculation\non probability of seed colonization by culturable fungi") +
  geom_hline(yintercept=0, linetype="dashed") + theme_bw()

#plotig for local adaptation
predictions(
  homeaway_garden_colonization,
  newdata = datagrid(
    home_away = c("Home","Away"),
    garden = c("SUM","WIL"),
    treat = c("DEAD","LIVE")
  ),
  type = "response",
  re_formula = NA
) %>%
  
  mutate(
    
    home_away = factor(
      home_away,
      levels = c("Home","Away"),
      labels = c("Home soil","Away soil")
    ),
    
    garden = factor(
      garden,
      levels = c("SUM","WIL"),
      labels = c("Cool","Warm")
    ),
    
    treat = factor(
      treat,
      levels = c("DEAD","LIVE"),
      labels = c("Sterilized","Live")
    )
    
  ) %>%
  
  ggplot(
    aes(
      x = home_away,
      y = estimate,
      colour = garden,
      group = garden
    )
  ) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 3) +
  
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = .05,
    linewidth = .8
  ) +
  
  facet_wrap(~treat, nrow = 1) +
  
  scale_colour_manual(
    values = c(
      "Cool" = "#0072B2",
      "Warm" = "#E69F00"
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 0.20),
    breaks = seq(0, 0.20, 0.05)
  ) +
  
  labs(
    x = "Soil source",
    y = "Predicted probability of\nseed colonization",
    colour = "Garden"
  ) +
  
  theme_bw(base_size = 16) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(
      face = "bold",
      size = 16
    ),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

#save the ploty

ggsave("Figures/CommonGarden_colonization.png",
       width = 7, height = 5, dpi = 600)















 