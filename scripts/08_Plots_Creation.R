# ==============================================================================
# 08_Plot_Creation.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
# Generate the main and supplementary figures used in the manuscript from the
# fertilizer-use, crop-use allocation, trade, population, biofuel, and
# decomposition datasets generated in previous scripts.
#
# Analysis:
# 1. Load processed fertilizer-use and auxiliary datasets.
# 2. Define functions used to summarize uncertainty and generate plots.
# 3. Calculate global fertilizer-use trends and feed-to-food ratios.
# 4. Quantify country-level changes in fertilizer allocation over time.
# 5. Generate manuscript and supplementary figures.
# 6. Export figures and supporting datasets.
# ==============================================================================


# Loading libraries -------------------------------------------------------

library(tidyverse)
library(readxl)
library(cowplot)
library(ggforce)
library(fmsb)
library(weights)
library(coin)
library(reshape2)
library(circlize)
library(flowmapblue)



# Loading data ------------------------------------------------------------

## Fertilizer-use estimates generated in Script 06 -------------------------
#
# Files ending in "_all" use the uncertainty estimates calculated from all
# available observations.
#
# Country-level datasets contain the 500 simulated fertilizer-use estimates
# allocated among food, feed, and other uses.

# Nitrogen
N_wheat_uses <- read_csv("results/N_wheat_uses_all.csv") %>%
  rename(N_food = 4, N_feed = 5, N_others = 6)

N_maize_uses <- read_csv("results/N_maize_uses_all.csv") %>%
  rename(N_food = 4, N_feed = 5, N_others = 6)

N_rice_uses <- read_csv("results/N_rice_uses_all.csv") %>%
  rename(N_food = 4, N_feed = 5, N_others = 6)

N_soybean_uses <- read_csv("results/N_soybean_uses_all.csv") %>%
  rename(N_food = 4, N_feed = 5, N_others = 6)


# Phosphorus
P_wheat_uses <- read_csv("results/P_wheat_uses_all.csv") %>%
  rename(P_food = 4, P_feed = 5, P_others = 6)

P_maize_uses <- read_csv("results/P_maize_uses_all.csv") %>%
  rename(P_food = 4, P_feed = 5, P_others = 6)

P_rice_uses <- read_csv("results/P_rice_uses_all.csv") %>%
  rename(P_food = 4, P_feed = 5, P_others = 6)

P_soybean_uses <- read_csv("results/P_soybean_uses_all.csv") %>%
  rename(P_food = 4, P_feed = 5, P_others = 6)


# Potassium
K_wheat_uses <- read_csv("results/K_wheat_uses_all.csv") %>%
  rename(K_food = 4, K_feed = 5, K_others = 6)

K_maize_uses <- read_csv("results/K_maize_uses_all.csv") %>%
  rename(K_food = 4, K_feed = 5, K_others = 6)

K_rice_uses <- read_csv("results/K_rice_uses_all.csv") %>%
  rename(K_food = 4, K_feed = 5, K_others = 6)

K_soybean_uses <- read_csv("results/K_soybean_uses_all.csv") %>%
  rename(K_food = 4, K_feed = 5, K_others = 6)


## Global Monte Carlo fertilizer-use estimates -----------------------------
#
# Global results retain one estimate per year and Monte Carlo iteration.
# Restrict the analysis to the study period ending in 2019.

# Wheat
N_wheat_global_uses <- read_csv("results/Wheat_uses_N_global_all.csv") %>%
  filter(Year < 2020)

P_wheat_global_uses <- read_csv("results/Wheat_uses_P_global_all.csv") %>%
  filter(Year < 2020)

K_wheat_global_uses <- read_csv("results/Wheat_uses_K_global_all.csv") %>%
  filter(Year < 2020)


# Maize
N_maize_global_uses <- read_csv("results/maize_uses_N_global_all.csv") %>%
  filter(Year < 2020)

P_maize_global_uses <- read_csv("results/maize_uses_P_global_all.csv") %>%
  filter(Year < 2020)

K_maize_global_uses <- read_csv("results/maize_uses_K_global_all.csv") %>%
  filter(Year < 2020)


# Rice
N_rice_global_uses <- read_csv("results/rice_uses_N_global_all.csv") %>%
  filter(Year < 2020)

P_rice_global_uses <- read_csv("results/rice_uses_P_global_all.csv") %>%
  filter(Year < 2020)

K_rice_global_uses <- read_csv("results/rice_uses_K_global_all.csv") %>%
  filter(Year < 2020)


# Soybean
N_soybean_global_uses <- read_csv("results/soybean_uses_N_global_all.csv") %>%
  filter(Year < 2020)

P_soybean_global_uses <- read_csv("results/soybean_uses_P_global_all.csv") %>%
  filter(Year < 2020)

K_soybean_global_uses <- read_csv("results/soybean_uses_K_global_all.csv") %>%
  filter(Year < 2020)


## Country and regional information ----------------------------------------

# FAOSTAT country codes, ISO3 codes, and regional classification.
FAOSTAT_regions <- read_csv("data/FAOSTAT_regions.csv") %>%
  select(2, 3, 7) %>%
  rename(
    Region = 1,
    FAOStat_area_code = 2,
    ISO3 = 3
  )

## UN population data.
PopulationUN_db <- read_csv("data/PopulationUN_db.csv")


## Crop-use allocation datasets --------------------------------------------
#
# Crop-use fractions generated in Script 04.

Wheat_uses <- read_csv("results/Wheat_uses.csv")
Maize_uses <- read_csv("results/maize_uses.csv")
Rice_uses <- read_csv("results/rice_uses.csv")
Soybean_uses <- read_csv("results/soybean_uses.csv")


## Bilateral trade information ---------------------------------------------
#
# Trade-allocation matrices generated using the Kastner-style approach in script 02.

Maize_import <- read_csv("results/Maize_Kastner_df_result.csv")
Wheat_import <- read_csv("results/Wheat_Kastner_df_result.csv")
Rice_import <- read_csv("results/Rice_Kastner_df_result.csv")


## Soybean processed-product exports ---------------------------------------
#
# Export fractions for soybean cake and soybean oil.

soybean_cake_exp <- read_csv("results/Soybean_cake_Exp_FB.csv")
soybean_oil_exp <- read_csv("results/Soybean_oil_Exp_FB.csv")


## Regional classification and colors --------------------------------------
#
# Define broader regional groups and colors used in regional figures.

region_colors <- data.frame(
  Region = c(
    "Northern Africa", "Middle Africa", "Western Africa",
    "Eastern Africa", "Southern Africa",
    "Western Asia", "Central Asia", "Eastern Asia",
    "Southern Asia", "South-eastern Asia",
    "Micronesia", "Polynesia", "Melanesia",
    "Australia and New Zealand",
    "Northern America", "Caribbean", "Central America",
    "South America",
    "Northern Europe", "Western Europe", "Eastern Europe",
    "Southern Europe"
  ),
  Color = c(
    "#BF9001", "#BF9001", "#BF9001", "#BF9001", "#BF9001",
    "#51CA63", "#51CA63", "#51CA63", "#51CA63", "#51CA63",
    "#ADADAD", "#ADADAD", "#ADADAD", "#ADADAD",
    "#DA0000", "#FFBF77", "#FFBF77", "#FFBF77",
    "#107FE0", "#107FE0", "#107FE0", "#107FE0"
  ),
  Region_g = c(
    "Africa", "Africa", "Africa", "Africa", "Africa",
    "Asia", "Asia", "Asia", "Asia", "Asia",
    "Oceania", "Oceania", "Oceania", "Oceania",
    "Northern America", "Other America", "Other America",
    "Other America",
    "Europe", "Europe", "Europe", "Europe"
  )
)


## Biofuel production -------------------------------------------------------

Biofuel_world_production <- read_excel(
  "data/Biofuel/Biofuel_world_production.xlsx"
)


## Additive decomposition ---------------------------------------------------
#
# Load global additive decomposition results generated in Script 07 for
# N, P, and K; food, feed, and other uses; and all comparison periods.


## Function to load one decomposition file ---------------------------------

load_decomposition <- function(
    nutrient,
    use,
    year_s,
    year_e
) {
  
  file <- paste0(
    "results/Decomposition_Additive/Dec_Ad_",
    nutrient, "_",
    use, "_Global_",
    year_s, "_",
    year_e,
    ".csv"
  )
  
  read_csv(
    file,
    show_col_types = FALSE
  )
}


## Comparison periods ------------------------------------------------------

periods <- tribble(
  ~period,        ~year_s, ~year_e,
  "2010_2019",       2010,    2019,
  "2006_2019",       2006,    2019,
  "1965_2015",       1965,    2015,
  "1989_2005",       1989,    2005,
  "1975_1989",       1975,    1989,
  "1961_1975",       1961,    1975
)


## Nutrients and fertilizer-use categories --------------------------------

nutrients <- c(
  "N",
  "P",
  "K"
)

uses <- c(
  "Food",
  "Feed",
  "Others"
)


## Load all global decomposition results -----------------------------------

Decomposition <- list()

for (nutrient_i in nutrients) {
  
  for (use_i in uses) {
    
    for (period_i in seq_len(nrow(periods))) {
      
      object_name <- paste(
        nutrient_i,
        use_i,
        periods$period[period_i],
        sep = "_"
      )
      
      Decomposition[[object_name]] <- load_decomposition(
        nutrient = nutrient_i,
        use = use_i,
        year_s = periods$year_s[period_i],
        year_e = periods$year_e[period_i]
      )
    }
  }
}

# Figure 1 ----------------------------------------------------------------

## Functions for Figure 1 --------------------------------------------------

# Summarize annual nitrogen fertilizer use across the 500 Monte Carlo
# iterations for each crop.
Global_trend_N <- function(Nutrient_Uses){
  
  Crop_N_Use <- Nutrient_Uses %>%
    group_by(Year) %>%
    summarise(
      N_use_food = mean(N_food, na.rm = TRUE),
      N_use_food_q025 = quantile(N_food, 0.025, na.rm = TRUE),
      N_use_food_q975 = quantile(N_food, 0.975, na.rm = TRUE),
      N_use_food_sd = sd(N_food, na.rm = TRUE),
      N_use_food_se = sd(N_food, na.rm = TRUE) / 500^(1/2),
      
      N_use_feed = mean(N_feed, na.rm = TRUE),
      N_use_feed_q025 = quantile(N_feed, 0.025, na.rm = TRUE),
      N_use_feed_q975 = quantile(N_feed, 0.975, na.rm = TRUE),
      N_use_feed_sd = sd(N_feed, na.rm = TRUE),
      N_use_feed_se = sd(N_feed, na.rm = TRUE) / 500^(1/2),
      
      N_use_others = mean(N_others, na.rm = TRUE),
      N_use_others_q025 = quantile(N_others, 0.025, na.rm = TRUE),
      N_use_others_q975 = quantile(N_others, 0.975, na.rm = TRUE),
      N_use_others_sd = sd(N_others, na.rm = TRUE),
      N_use_others_se = sd(N_others, na.rm = TRUE) / 500^(1/2)
    )
  
  return(Crop_N_Use)
}


# Summarize annual phosphorus fertilizer use across the 500 Monte Carlo
# iterations for each crop.
Global_trend_P <- function(Nutrient_Uses){
  
  Crop_P_Use <- Nutrient_Uses %>%
    group_by(Year) %>%
    summarise(
      P_use_food = mean(P_food, na.rm = TRUE),
      P_use_food_q025 = quantile(P_food, 0.025, na.rm = TRUE),
      P_use_food_q975 = quantile(P_food, 0.975, na.rm = TRUE),
      P_use_food_sd = sd(P_food, na.rm = TRUE),
      P_use_food_se = sd(P_food, na.rm = TRUE) / 500^(1/2),
      
      P_use_feed = mean(P_feed, na.rm = TRUE),
      P_use_feed_q025 = quantile(P_feed, 0.025, na.rm = TRUE),
      P_use_feed_q975 = quantile(P_feed, 0.975, na.rm = TRUE),
      P_use_feed_sd = sd(P_feed, na.rm = TRUE),
      P_use_feed_se = sd(P_feed, na.rm = TRUE) / 500^(1/2),
      
      P_use_others = mean(P_others, na.rm = TRUE),
      P_use_others_q025 = quantile(P_others, 0.025, na.rm = TRUE),
      P_use_others_q975 = quantile(P_others, 0.975, na.rm = TRUE),
      P_use_others_sd = sd(P_others, na.rm = TRUE),
      P_use_others_se = sd(P_others, na.rm = TRUE) / 500^(1/2)
    )
  
  return(Crop_P_Use)
}


# Summarize annual potassium fertilizer use across the 500 Monte Carlo
# iterations for each crop.
Global_trend_K <- function(Nutrient_Uses){
  
  Crop_K_Use <- Nutrient_Uses %>%
    group_by(Year) %>%
    summarise(
      K_use_food = mean(K_food, na.rm = TRUE),
      K_use_food_q025 = quantile(K_food, 0.025, na.rm = TRUE),
      K_use_food_q975 = quantile(K_food, 0.975, na.rm = TRUE),
      K_use_food_sd = sd(K_food, na.rm = TRUE),
      K_use_food_se = sd(K_food, na.rm = TRUE) / 500^(1/2),
      
      K_use_feed = mean(K_feed, na.rm = TRUE),
      K_use_feed_q025 = quantile(K_feed, 0.025, na.rm = TRUE),
      K_use_feed_q975 = quantile(K_feed, 0.975, na.rm = TRUE),
      K_use_feed_sd = sd(K_feed, na.rm = TRUE),
      K_use_feed_se = sd(K_feed, na.rm = TRUE) / 500^(1/2),
      
      K_use_others = mean(K_others, na.rm = TRUE),
      K_use_others_q025 = quantile(K_others, 0.025, na.rm = TRUE),
      K_use_others_q975 = quantile(K_others, 0.975, na.rm = TRUE),
      K_use_others_sd = sd(K_others, na.rm = TRUE),
      K_use_others_se = sd(K_others, na.rm = TRUE) / 500^(1/2)
    )
  
  return(Crop_K_Use)
}


## FAOSTAT Food Balance methodology transition -----------------------------

FB_change_start <- 2010
FB_change_end <- 2013


# Plot annual nitrogen fertilizer use allocated to food, feed, and other uses.
Plot_global_trend_N <- function(Nutrient_Uses){
  
  N_plot <- ggplot(Nutrient_Uses, aes(x = Year)) +
    
    # Highlight the 2010-2013 Food Balance methodology transition.
    annotate(
      "rect",
      xmin = FB_change_start,
      xmax = FB_change_end,
      ymin = -Inf,
      ymax = Inf,
      fill = "coral",
      alpha = 0.20
    ) +
    
    # Standard deviation.
    geom_ribbon(
      aes(
        ymin = N_use_food - N_use_food_sd,
        ymax = N_use_food + N_use_food_sd
      ),
      alpha = 0.25,
      fill = "#FF9E00"
    ) +
    geom_ribbon(
      aes(
        ymin = N_use_feed - N_use_feed_sd,
        ymax = N_use_feed + N_use_feed_sd
      ),
      alpha = 0.25,
      fill = "#458472"
    ) +
    geom_ribbon(
      aes(
        ymin = N_use_others - N_use_others_sd,
        ymax = N_use_others + N_use_others_sd
      ),
      alpha = 0.25,
      fill = "black"
    ) +
    
    # Standard error.
    geom_ribbon(
      aes(
        ymin = N_use_food - N_use_food_se,
        ymax = N_use_food + N_use_food_se
      ),
      alpha = 0.5,
      fill = "#FF9E00"
    ) +
    geom_ribbon(
      aes(
        ymin = N_use_feed - N_use_feed_se,
        ymax = N_use_feed + N_use_feed_se
      ),
      alpha = 0.5,
      fill = "#458472"
    ) +
    geom_ribbon(
      aes(
        ymin = N_use_others - N_use_others_se,
        ymax = N_use_others + N_use_others_se
      ),
      alpha = 0.5,
      fill = "black"
    ) +
    
    # Mean annual fertilizer use.
    geom_line(
      aes(y = N_use_food),
      color = "#FF9E00",
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = N_use_feed),
      color = "#458472",
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = N_use_others),
      color = "black",
      linewidth = 0.5
    ) +
    
    labs(
      x = "Year",
      y = "Tg"
    ) +
    
    theme_bw() +
    theme(
      text = element_text(
        family = "Arial",
        size = 7
      ),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  return(N_plot)
}


# Plot annual phosphorus fertilizer use allocated to food, feed, and other uses.
Plot_global_trend_P <- function(Nutrient_Uses){
  
  P_plot <- ggplot(Nutrient_Uses, aes(x = Year)) +
    
    # Highlight the 2010-2013 Food Balance methodology transition.
    annotate(
      "rect",
      xmin = FB_change_start,
      xmax = FB_change_end,
      ymin = -Inf,
      ymax = Inf,
      fill = "coral",
      alpha = 0.20
    ) +
    
    # Standard deviation.
    geom_ribbon(
      aes(
        ymin = P_use_food - P_use_food_sd,
        ymax = P_use_food + P_use_food_sd
      ),
      alpha = 0.25,
      fill = "#FF9E00"
    ) +
    geom_ribbon(
      aes(
        ymin = P_use_feed - P_use_feed_sd,
        ymax = P_use_feed + P_use_feed_sd
      ),
      alpha = 0.25,
      fill = "#458472"
    ) +
    geom_ribbon(
      aes(
        ymin = P_use_others - P_use_others_sd,
        ymax = P_use_others + P_use_others_sd
      ),
      alpha = 0.25,
      fill = "black"
    ) +
    
    # Standard error.
    geom_ribbon(
      aes(
        ymin = P_use_food - P_use_food_se,
        ymax = P_use_food + P_use_food_se
      ),
      alpha = 0.5,
      fill = "#FF9E00"
    ) +
    geom_ribbon(
      aes(
        ymin = P_use_feed - P_use_feed_se,
        ymax = P_use_feed + P_use_feed_se
      ),
      alpha = 0.5,
      fill = "#458472"
    ) +
    geom_ribbon(
      aes(
        ymin = P_use_others - P_use_others_se,
        ymax = P_use_others + P_use_others_se
      ),
      alpha = 0.5,
      fill = "black"
    ) +
    
    # Mean annual fertilizer use.
    geom_line(
      aes(y = P_use_food),
      color = "#FF9E00",
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = P_use_feed),
      color = "#458472",
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = P_use_others),
      color = "black",
      linewidth = 0.5
    ) +
    
    labs(
      x = "Year",
      y = "Tg"
    ) +
    
    theme_bw() +
    theme(
      text = element_text(
        family = "Arial",
        size = 7
      ),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  return(P_plot)
}


# Plot annual potassium fertilizer use allocated to food, feed, and other uses.
Plot_global_trend_K <- function(Nutrient_Uses){
  
  K_plot <- ggplot(Nutrient_Uses, aes(x = Year)) +
    
    # Highlight the 2010-2013 Food Balance methodology transition.
    annotate(
      "rect",
      xmin = FB_change_start,
      xmax = FB_change_end,
      ymin = -Inf,
      ymax = Inf,
      fill = "coral",
      alpha = 0.20
    ) +
    
    # Standard deviation.
    geom_ribbon(
      aes(
        ymin = K_use_food - K_use_food_sd,
        ymax = K_use_food + K_use_food_sd
      ),
      alpha = 0.25,
      fill = "#FF9E00"
    ) +
    geom_ribbon(
      aes(
        ymin = K_use_feed - K_use_feed_sd,
        ymax = K_use_feed + K_use_feed_sd
      ),
      alpha = 0.25,
      fill = "#458472"
    ) +
    geom_ribbon(
      aes(
        ymin = K_use_others - K_use_others_sd,
        ymax = K_use_others + K_use_others_sd
      ),
      alpha = 0.25,
      fill = "black"
    ) +
    
    # Standard error.
    geom_ribbon(
      aes(
        ymin = K_use_food - K_use_food_se,
        ymax = K_use_food + K_use_food_se
      ),
      alpha = 0.5,
      fill = "#FF9E00"
    ) +
    geom_ribbon(
      aes(
        ymin = K_use_feed - K_use_feed_se,
        ymax = K_use_feed + K_use_feed_se
      ),
      alpha = 0.5,
      fill = "#458472"
    ) +
    geom_ribbon(
      aes(
        ymin = K_use_others - K_use_others_se,
        ymax = K_use_others + K_use_others_se
      ),
      alpha = 0.5,
      fill = "black"
    ) +
    
    # Mean annual fertilizer use.
    geom_line(
      aes(y = K_use_food),
      color = "#FF9E00",
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = K_use_feed),
      color = "#458472",
      linewidth = 0.5
    ) +
    geom_line(
      aes(y = K_use_others),
      color = "black",
      linewidth = 0.5
    ) +
    
    labs(
      x = "Year",
      y = "Tg"
    ) +
    
    theme_bw() +
    theme(
      text = element_text(
        family = "Arial",
        size = 7
      ),
      axis.title = element_text(size = 7),
      axis.text = element_text(size = 6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  return(K_plot)
}


## Figure 1: Global fertilizer-use trends ----------------------------------
#
# Calculate crop-specific annual fertilizer-use estimates and combine them
# to obtain global N, P, and K fertilizer use for food, feed, and other uses.
#
# Crop-specific SD and SE estimates are combined using root-sum-of-squares,
# assuming independence among crop estimates.
#
# Values are converted from tonnes to Tg (1 Tg = 1e6 tonnes).


## Nitrogen ----------------------------------------------------------------

Wheat_N_global <- Global_trend_N(N_wheat_global_uses)
Maize_N_global <- Global_trend_N(N_maize_global_uses)
Rice_N_global <- Global_trend_N(N_rice_global_uses)
Soybean_N_global <- Global_trend_N(N_soybean_global_uses)

World_N_global <- bind_rows(
  Wheat_N_global,
  Maize_N_global,
  Rice_N_global,
  Soybean_N_global
) %>%
  group_by(Year) %>%
  summarise(
    
    # Sum central estimates.
    across(
      c(
        N_use_food,
        N_use_feed,
        N_use_others
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # Sum crop-specific uncertainty quantiles.
    across(
      ends_with("_q025"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    across(
      ends_with("_q975"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # Combine standard deviations using root-sum-of-squares.
    across(
      ends_with("_sd"),
      ~ sqrt(sum((.x)^2, na.rm = TRUE))
    ),
    
    # Combine standard errors using root-sum-of-squares.
    across(
      ends_with("_se"),
      ~ sqrt(sum((.x)^2, na.rm = TRUE))
    )
  ) %>%
  
  # Convert tonnes to Tg.
  mutate(
    across(
      -Year,
      ~ .x / 1e6
    )
  )


## Phosphorus --------------------------------------------------------------

Wheat_P_global <- Global_trend_P(P_wheat_global_uses)
Maize_P_global <- Global_trend_P(P_maize_global_uses)
Rice_P_global <- Global_trend_P(P_rice_global_uses)
Soybean_P_global <- Global_trend_P(P_soybean_global_uses)

World_P_global <- bind_rows(
  Wheat_P_global,
  Maize_P_global,
  Rice_P_global,
  Soybean_P_global
) %>%
  group_by(Year) %>%
  summarise(
    
    # Sum central estimates.
    across(
      c(
        P_use_food,
        P_use_feed,
        P_use_others
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # Sum crop-specific uncertainty quantiles.
    across(
      ends_with("_q025"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    across(
      ends_with("_q975"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # Combine standard deviations using root-sum-of-squares.
    across(
      ends_with("_sd"),
      ~ sqrt(sum((.x)^2, na.rm = TRUE))
    ),
    
    # Combine standard errors using root-sum-of-squares.
    across(
      ends_with("_se"),
      ~ sqrt(sum((.x)^2, na.rm = TRUE))
    )
  ) %>%
  
  # Convert tonnes to Tg.
  mutate(
    across(
      -Year,
      ~ .x / 1e6
    )
  )


## Potassium ---------------------------------------------------------------

Wheat_K_global <- Global_trend_K(K_wheat_global_uses)
Maize_K_global <- Global_trend_K(K_maize_global_uses)
Rice_K_global <- Global_trend_K(K_rice_global_uses)
Soybean_K_global <- Global_trend_K(K_soybean_global_uses)

World_K_global <- bind_rows(
  Wheat_K_global,
  Maize_K_global,
  Rice_K_global,
  Soybean_K_global
) %>%
  group_by(Year) %>%
  summarise(
    
    # Sum central estimates.
    across(
      c(
        K_use_food,
        K_use_feed,
        K_use_others
      ),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # Sum crop-specific uncertainty quantiles.
    across(
      ends_with("_q025"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    across(
      ends_with("_q975"),
      ~ sum(.x, na.rm = TRUE)
    ),
    
    # Combine standard deviations using root-sum-of-squares.
    across(
      ends_with("_sd"),
      ~ sqrt(sum((.x)^2, na.rm = TRUE))
    ),
    
    # Combine standard errors using root-sum-of-squares.
    across(
      ends_with("_se"),
      ~ sqrt(sum((.x)^2, na.rm = TRUE))
    )
  ) %>%
  
  # Convert tonnes to Tg.
  mutate(
    across(
      -Year,
      ~ .x / 1e6
    )
  )



## Global fertilizer-use plots ---------------------------------------------

p1_N <- Plot_global_trend_N(World_N_global)

p1_P <- Plot_global_trend_P(World_P_global)

p1_K <- Plot_global_trend_K(World_K_global)


## Feed-to-food ratios ------------------------------------------------------
#
# Calculate the global feed-to-food fertilizer-use ratio for each nutrient
# and Monte Carlo iteration, then estimate the annual mean, SD, and SE.

N_Ratios <- bind_rows(
  N_wheat_global_uses,
  N_maize_global_uses,
  N_rice_global_uses,
  N_soybean_global_uses
) %>%
  group_by(
    Year,
    Iter
  ) %>%
  summarise(
    N_food = sum(N_food, na.rm = TRUE),
    N_feed = sum(N_feed, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Rat = N_feed / N_food,
    Nutrient = "N"
  ) %>%
  select(
    Year,
    Iter,
    Rat,
    Nutrient
  )


P_Ratios <- bind_rows(
  P_wheat_global_uses,
  P_maize_global_uses,
  P_rice_global_uses,
  P_soybean_global_uses
) %>%
  group_by(
    Year,
    Iter
  ) %>%
  summarise(
    P_food = sum(P_food, na.rm = TRUE),
    P_feed = sum(P_feed, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Rat = P_feed / P_food,
    Nutrient = "P"
  ) %>%
  select(
    Year,
    Iter,
    Rat,
    Nutrient
  )


K_Ratios <- bind_rows(
  K_wheat_global_uses,
  K_maize_global_uses,
  K_rice_global_uses,
  K_soybean_global_uses
) %>%
  group_by(
    Year,
    Iter
  ) %>%
  summarise(
    K_food = sum(K_food, na.rm = TRUE),
    K_feed = sum(K_feed, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Rat = K_feed / K_food,
    Nutrient = "K"
  ) %>%
  select(
    Year,
    Iter,
    Rat,
    Nutrient
  )


G_Ratios <- bind_rows(
  N_Ratios,
  P_Ratios,
  K_Ratios
) %>%
  group_by(
    Year,
    Nutrient
  ) %>%
  summarise(
    Rat_mean = mean(Rat, na.rm = TRUE),
    Rat_sd = sd(Rat, na.rm = TRUE),
    Rat_q025 = quantile(
      Rat,
      0.025,
      na.rm = TRUE
    ),
    Rat_q975 = quantile(
      Rat,
      0.975,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_se = Rat_sd / sqrt(500)
  ) %>%
  rename(
    Rat = Rat_mean
  )


## Feed-to-food ratio plot --------------------------------------------------

p1_Ratios <- ggplot(
  G_Ratios,
  aes(
    x = Year,
    y = Rat,
    fill = Nutrient,
    color = Nutrient
  )
) +
  
  # Highlight the 2010-2013 Food Balance methodology transition.
  annotate(
    "rect",
    xmin = FB_change_start,
    xmax = FB_change_end,
    ymin = -Inf,
    ymax = Inf,
    fill = "coral",
    alpha = 0.20
  ) +
  
  # Standard deviation.
  geom_ribbon(
    aes(
      ymin = Rat - Rat_sd,
      ymax = Rat + Rat_sd
    ),
    alpha = 0.25,
    color = "transparent"
  ) +
  
  # Standard error.
  geom_ribbon(
    aes(
      ymin = Rat - Rat_se,
      ymax = Rat + Rat_se
    ),
    alpha = 0.5,
    color = "transparent"
  ) +
  
  geom_line(
    linewidth = 0.5
  ) +
  
  scale_fill_manual(
    values = c(
      "N" = "darkred",
      "P" = "darkblue",
      "K" = "darkgreen"
    )
  ) +
  
  scale_color_manual(
    values = c(
      "N" = "darkred",
      "P" = "darkblue",
      "K" = "darkgreen"
    )
  ) +
  
  ylim(
    0,
    1.6
  ) +
  
  labs(
    x = "Year",
    y = expression(
      paste(
        "FFR (Feed ",
        Food^{-1},
        ")"
      )
    )
  ) +
  
  theme_bw() +
  
  theme(
    text = element_text(
      family = "Arial",
      size = 7
    ),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


### Combine Figure 1 panels --------------------------------------------------

p1 <- plot_grid(
  p1_N,
  p1_P,
  p1_K,
  p1_Ratios,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c",
    "d"
  ),
  label_size = 7,
  label_fontface = "bold"
)


### Export Figure 1 ----------------------------------------------------------
#
# Export as a vector PDF at the final two-column width recommended for
# Nature figures.

ggsave(
  filename = "plots/Figure1_Global_Trend_publication.pdf",
  plot = p1,
  device = cairo_pdf,
  width = 180,
  height = 62,
  units = "mm"
)

### Export data underlying Figure 1 -----------------------------------------
#### Nitrogen ----------------------------------------------------------------
Figure1_N_data <- World_N_global %>%
  transmute(
    Year,
    
    N_Food_mean = N_use_food,
    N_Food_sd = N_use_food_sd,
    N_Food_se = N_use_food_se,
    N_Food_q025 = N_use_food_q025,
    N_Food_q975 = N_use_food_q975,
    
    N_Feed_mean = N_use_feed,
    N_Feed_sd = N_use_feed_sd,
    N_Feed_se = N_use_feed_se,
    N_Feed_q025 = N_use_feed_q025,
    N_Feed_q975 = N_use_feed_q975,
    
    N_Others_mean = N_use_others,
    N_Others_sd = N_use_others_sd,
    N_Others_se = N_use_others_se,
    N_Others_q025 = N_use_others_q025,
    N_Others_q975 = N_use_others_q975
  ) %>%
  pivot_longer(
    cols = -Year,
    names_to = c("Nutrient", "Use", ".value"),
    names_pattern = "([^_]+)_([^_]+)_(.+)"
  ) %>%
  mutate(
    Panel = "Fertilizer use"
  )

#### Phosphorus ---------------------------------------------------------------
Figure1_P_data <- World_P_global %>%
  transmute(
    Year,
    
    P_Food_mean = P_use_food,
    P_Food_sd = P_use_food_sd,
    P_Food_se = P_use_food_se,
    P_Food_q025 = P_use_food_q025,
    P_Food_q975 = P_use_food_q975,
    
    P_Feed_mean = P_use_feed,
    P_Feed_sd = P_use_feed_sd,
    P_Feed_se = P_use_feed_se,
    P_Feed_q025 = P_use_feed_q025,
    P_Feed_q975 = P_use_feed_q975,
    
    P_Others_mean = P_use_others,
    P_Others_sd = P_use_others_sd,
    P_Others_se = P_use_others_se,
    P_Others_q025 = P_use_others_q025,
    P_Others_q975 = P_use_others_q975
  ) %>%
  pivot_longer(
    cols = -Year,
    names_to = c("Nutrient", "Use", ".value"),
    names_pattern = "([^_]+)_([^_]+)_(.+)"
  ) %>%
  mutate(
    Panel = "Fertilizer use"
  )


#### Potassium ----------------------------------------------------------------
Figure1_K_data <- World_K_global %>%
  transmute(
    Year,
    
    K_Food_mean = K_use_food,
    K_Food_sd = K_use_food_sd,
    K_Food_se = K_use_food_se,
    K_Food_q025 = K_use_food_q025,
    K_Food_q975 = K_use_food_q975,
    
    K_Feed_mean = K_use_feed,
    K_Feed_sd = K_use_feed_sd,
    K_Feed_se = K_use_feed_se,
    K_Feed_q025 = K_use_feed_q025,
    K_Feed_q975 = K_use_feed_q975,
    
    K_Others_mean = K_use_others,
    K_Others_sd = K_use_others_sd,
    K_Others_se = K_use_others_se,
    K_Others_q025 = K_use_others_q025,
    K_Others_q975 = K_use_others_q975
  ) %>%
  pivot_longer(
    cols = -Year,
    names_to = c("Nutrient", "Use", ".value"),
    names_pattern = "([^_]+)_([^_]+)_(.+)"
  ) %>%
  mutate(
    Panel = "Fertilizer use"
  )

#### Feed-to-food ratio -------------------------------------------------------
Figure1_ratio_data <- G_Ratios %>%
  transmute(
    Year,
    Nutrient,
    Use = "FFR",
    mean = Rat,
    sd = Rat_sd,
    se = Rat_se,
    q025 = NA_real_,
    q975 = NA_real_,
    Panel = "Feed-to-food ratio"
  )



#### Combine Figure 1 data ----------------------------------------------------
Figure1_data <- bind_rows(
  Figure1_N_data,
  Figure1_P_data,
  Figure1_K_data,
  Figure1_ratio_data
) %>%
  select(
    Year,
    Panel,
    Nutrient,
    Use,
    mean,
    sd,
    se
  ) %>%
  arrange(
    Panel,
    Nutrient,
    Use,
    Year
  )

# Export csv
write_csv(
  Figure1_data,
  "results/Figure1_Global_Trend_data.csv"
)

# Figure 2: Country changes in feed-to-food ratio -------------------------
#
# For each nutrient, fertilizer use is summed across wheat, maize, rice,
# and soybean for each country, year, and Monte Carlo simulation. The
# feed-to-food ratio (FFR) is then calculated and summarized as the median
# across all year × simulation observations within each decade.
#
# Figure 2 compares the earliest and latest available decade for the
# 50 largest fertilizer-consuming countries. Increasing FFR is shown in
# red and decreasing FFR in blue. Values are displayed within a 0-4 range.


region_levels <- c(
  "Northern Africa",
  "Middle Africa",
  "Western Africa",
  "Eastern Africa",
  "Southern Africa",
  "Western Asia",
  "Central Asia",
  "Eastern Asia",
  "Southern Asia",
  "South-eastern Asia",
  "Micronesia",
  "Polynesia",
  "Melanesia",
  "Australia and New Zealand",
  "Northern America",
  "Caribbean",
  "Central America",
  "South America",
  "Northern Europe",
  "Western Europe",
  "Eastern Europe",
  "Southern Europe"
)


## Nitrogen ----------------------------------------------------------------

Wheat_N_Uses <- N_wheat_uses %>%
  filter(FAOStat_area_code != 351)

Maize_N_Uses <- N_maize_uses %>%
  filter(FAOStat_area_code != 351)

Rice_N_Uses <- N_rice_uses %>%
  filter(FAOStat_area_code != 351)

Soybean_N_Uses <- N_soybean_uses %>%
  filter(FAOStat_area_code != 351)


Country_N_Uses <- bind_rows(
  Wheat_N_Uses,
  Maize_N_Uses,
  Rice_N_Uses,
  Soybean_N_Uses
) %>%
  group_by(
    FAOStat_area_code,
    Year,
    Sim
  ) %>%
  summarise(
    N_food = sum(N_food, na.rm = TRUE),
    N_feed = sum(N_feed, na.rm = TRUE),
    N_others = sum(N_others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_FeedFood = N_feed / N_food,
    Year = as.numeric(Year),
    decade = case_when(
      Year < 1970 ~ "1960s",
      Year < 1980 ~ "1970s",
      Year < 1990 ~ "1980s",
      Year < 2000 ~ "1990s",
      Year < 2010 ~ "2000s",
      TRUE        ~ "2010s"
    )
  )


Country_N_Uses_plot <- Country_N_Uses %>%
  group_by(
    FAOStat_area_code,
    decade
  ) %>%
  summarise(
    Rat_FeedFood = median(
      Rat_FeedFood,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = decade,
    values_from = Rat_FeedFood
  ) %>%
  rename(
    decade_1960 = `1960s`,
    decade_1970 = `1970s`,
    decade_1980 = `1980s`,
    decade_1990 = `1990s`,
    decade_2000 = `2000s`,
    decade_2010 = `2010s`
  ) %>%
  left_join(
    FAOSTAT_regions,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    ISO3 = ifelse(
      FAOStat_area_code == 41,
      "CHN",
      ISO3
    ),
    
    first_decade = case_when(
      !is.na(decade_1960) ~ decade_1960,
      !is.na(decade_1970) ~ decade_1970,
      !is.na(decade_1980) ~ decade_1980,
      !is.na(decade_1990) ~ decade_1990,
      !is.na(decade_2000) ~ decade_2000,
      TRUE                ~ NA_real_
    ),
    
    first_decade_years = case_when(
      !is.na(decade_1960) ~ "1960s",
      !is.na(decade_1970) ~ "1970s",
      !is.na(decade_1980) ~ "1980s",
      !is.na(decade_1990) ~ "1990s",
      !is.na(decade_2000) ~ "2000s",
      TRUE                ~ NA_character_
    ),
    
    last_decade = case_when(
      !is.na(decade_2010) ~ decade_2010,
      !is.na(decade_2000) ~ decade_2000,
      !is.na(decade_1990) ~ decade_1990,
      TRUE                ~ NA_real_
    ),
    
    last_decade_years = case_when(
      !is.na(decade_2010) ~ "2010s",
      !is.na(decade_2000) ~ "2000s",
      !is.na(decade_1990) ~ "1990s",
      TRUE                ~ NA_character_
    )
  ) %>%
  drop_na(
    ISO3,
    first_decade,
    last_decade
  ) %>%
  mutate(
    first_decade = ifelse(
      first_decade < 0,
      0,
      first_decade
    ),
    last_decade = ifelse(
      last_decade > 4,
      4,
      last_decade
    ),
    direction = ifelse(
      last_decade > first_decade,
      "Increase",
      "Decrease"
    ),
    Region = factor(
      Region,
      levels = region_levels
    )
  ) %>%
  arrange(
    Region,
    ISO3
  )


## Phosphorus ---------------------------------------------------------------

Country_P_Uses <- bind_rows(
  P_wheat_uses,
  P_maize_uses,
  P_rice_uses,
  P_soybean_uses
) %>%
  filter(
    FAOStat_area_code != 351
  ) %>%
  group_by(
    FAOStat_area_code,
    Year,
    Sim
  ) %>%
  summarise(
    P_food = sum(P_food, na.rm = TRUE),
    P_feed = sum(P_feed, na.rm = TRUE),
    P_others = sum(P_others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_FeedFood = P_feed / P_food,
    Year = as.numeric(Year),
    decade = case_when(
      Year < 1970 ~ "1960s",
      Year < 1980 ~ "1970s",
      Year < 1990 ~ "1980s",
      Year < 2000 ~ "1990s",
      Year < 2010 ~ "2000s",
      TRUE        ~ "2010s"
    )
  )


Country_P_Uses_plot <- Country_P_Uses %>%
  group_by(
    FAOStat_area_code,
    decade
  ) %>%
  summarise(
    Rat_FeedFood = median(
      Rat_FeedFood,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = decade,
    values_from = Rat_FeedFood
  ) %>%
  rename(
    decade_1960 = `1960s`,
    decade_1970 = `1970s`,
    decade_1980 = `1980s`,
    decade_1990 = `1990s`,
    decade_2000 = `2000s`,
    decade_2010 = `2010s`
  ) %>%
  left_join(
    FAOSTAT_regions,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    ISO3 = ifelse(
      FAOStat_area_code == 41,
      "CHN",
      ISO3
    ),
    
    first_decade = case_when(
      !is.na(decade_1960) ~ decade_1960,
      !is.na(decade_1970) ~ decade_1970,
      !is.na(decade_1980) ~ decade_1980,
      !is.na(decade_1990) ~ decade_1990,
      !is.na(decade_2000) ~ decade_2000,
      TRUE                ~ NA_real_
    ),
    
    first_decade_years = case_when(
      !is.na(decade_1960) ~ "1960s",
      !is.na(decade_1970) ~ "1970s",
      !is.na(decade_1980) ~ "1980s",
      !is.na(decade_1990) ~ "1990s",
      !is.na(decade_2000) ~ "2000s",
      TRUE                ~ NA_character_
    ),
    
    last_decade = case_when(
      !is.na(decade_2010) ~ decade_2010,
      !is.na(decade_2000) ~ decade_2000,
      !is.na(decade_1990) ~ decade_1990,
      TRUE                ~ NA_real_
    ),
    
    last_decade_years = case_when(
      !is.na(decade_2010) ~ "2010s",
      !is.na(decade_2000) ~ "2000s",
      !is.na(decade_1990) ~ "1990s",
      TRUE                ~ NA_character_
    )
  ) %>%
  drop_na(
    ISO3,
    first_decade,
    last_decade
  ) %>%
  mutate(
    first_decade = ifelse(
      first_decade < 0,
      0,
      first_decade
    ),
    last_decade = ifelse(
      last_decade > 4,
      4,
      last_decade
    ),
    direction = ifelse(
      last_decade > first_decade,
      "Increase",
      "Decrease"
    ),
    Region = factor(
      Region,
      levels = region_levels
    )
  ) %>%
  arrange(
    Region,
    ISO3
  )


## Potassium ----------------------------------------------------------------

Country_K_Uses <- bind_rows(
  K_wheat_uses,
  K_maize_uses,
  K_rice_uses,
  K_soybean_uses
) %>%
  filter(
    FAOStat_area_code != 351
  ) %>%
  group_by(
    FAOStat_area_code,
    Year,
    Sim
  ) %>%
  summarise(
    K_food = sum(K_food, na.rm = TRUE),
    K_feed = sum(K_feed, na.rm = TRUE),
    K_others = sum(K_others, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_FeedFood = K_feed / K_food,
    Year = as.numeric(Year),
    decade = case_when(
      Year < 1970 ~ "1960s",
      Year < 1980 ~ "1970s",
      Year < 1990 ~ "1980s",
      Year < 2000 ~ "1990s",
      Year < 2010 ~ "2000s",
      TRUE        ~ "2010s"
    )
  )


Country_K_Uses_plot <- Country_K_Uses %>%
  group_by(
    FAOStat_area_code,
    decade
  ) %>%
  summarise(
    Rat_FeedFood = median(
      Rat_FeedFood,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = decade,
    values_from = Rat_FeedFood
  ) %>%
  rename(
    decade_1960 = `1960s`,
    decade_1970 = `1970s`,
    decade_1980 = `1980s`,
    decade_1990 = `1990s`,
    decade_2000 = `2000s`,
    decade_2010 = `2010s`
  ) %>%
  left_join(
    FAOSTAT_regions,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    ISO3 = ifelse(
      FAOStat_area_code == 41,
      "CHN",
      ISO3
    ),
    
    first_decade = case_when(
      !is.na(decade_1960) ~ decade_1960,
      !is.na(decade_1970) ~ decade_1970,
      !is.na(decade_1980) ~ decade_1980,
      !is.na(decade_1990) ~ decade_1990,
      !is.na(decade_2000) ~ decade_2000,
      TRUE                ~ NA_real_
    ),
    
    first_decade_years = case_when(
      !is.na(decade_1960) ~ "1960s",
      !is.na(decade_1970) ~ "1970s",
      !is.na(decade_1980) ~ "1980s",
      !is.na(decade_1990) ~ "1990s",
      !is.na(decade_2000) ~ "2000s",
      TRUE                ~ NA_character_
    ),
    
    last_decade = case_when(
      !is.na(decade_2010) ~ decade_2010,
      !is.na(decade_2000) ~ decade_2000,
      !is.na(decade_1990) ~ decade_1990,
      TRUE                ~ NA_real_
    ),
    
    last_decade_years = case_when(
      !is.na(decade_2010) ~ "2010s",
      !is.na(decade_2000) ~ "2000s",
      !is.na(decade_1990) ~ "1990s",
      TRUE                ~ NA_character_
    )
  ) %>%
  drop_na(
    ISO3,
    first_decade,
    last_decade
  ) %>%
  mutate(
    first_decade = ifelse(
      first_decade < 0,
      0,
      first_decade
    ),
    last_decade = ifelse(
      last_decade > 4,
      4,
      last_decade
    ),
    direction = ifelse(
      last_decade > first_decade,
      "Increase",
      "Decrease"
    ),
    Region = factor(
      Region,
      levels = region_levels
    )
  ) %>%
  arrange(
    Region,
    ISO3
  )


## Global reference change in feed-to-food ratio ---------------------------

med_rat_1960_N <- World_N_global %>%
  filter(Year < 1970) %>%
  transmute(Rat = N_use_feed / N_use_food) %>%
  pull(Rat) %>%
  median(na.rm = TRUE)

med_rat_2010_N <- World_N_global %>%
  filter(Year > 2010) %>%
  transmute(Rat = N_use_feed / N_use_food) %>%
  pull(Rat) %>%
  median(na.rm = TRUE)


med_rat_1960_P <- World_P_global %>%
  filter(Year < 1970) %>%
  transmute(Rat = P_use_feed / P_use_food) %>%
  pull(Rat) %>%
  median(na.rm = TRUE)

med_rat_2010_P <- World_P_global %>%
  filter(Year > 2010) %>%
  transmute(Rat = P_use_feed / P_use_food) %>%
  pull(Rat) %>%
  median(na.rm = TRUE)


med_rat_1960_K <- World_K_global %>%
  filter(Year < 1970) %>%
  transmute(Rat = K_use_feed / K_use_food) %>%
  pull(Rat) %>%
  median(na.rm = TRUE)

med_rat_2010_K <- World_K_global %>%
  filter(Year > 2010) %>%
  transmute(Rat = K_use_feed / K_use_food) %>%
  pull(Rat) %>%
  median(na.rm = TRUE)


## Identify the 50 largest fertilizer-consuming countries ------------------

Country_N_tot <- Country_N_Uses %>%
  filter(
    Year > 1999
  ) %>%
  mutate(
    N_tot = N_food + N_feed + N_others
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    N_tot = median(
      N_tot,
      na.rm = TRUE
    ) / 1e6,
    .groups = "drop"
  )


Country_P_tot <- Country_P_Uses %>%
  filter(
    Year > 1999
  ) %>%
  mutate(
    P_tot = P_food + P_feed + P_others
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    P_tot = median(
      P_tot,
      na.rm = TRUE
    ) / 1e6,
    .groups = "drop"
  )


Country_K_tot <- Country_K_Uses %>%
  filter(
    Year > 1999
  ) %>%
  mutate(
    K_tot = K_food + K_feed + K_others
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    K_tot = median(
      K_tot,
      na.rm = TRUE
    ) / 1e6,
    .groups = "drop"
  )


Top50_Country_NPK_tot <- full_join(
  Country_N_tot,
  Country_P_tot,
  by = "FAOStat_area_code"
) %>%
  full_join(
    Country_K_tot,
    by = "FAOStat_area_code"
  ) %>%
  rowwise() %>%
  mutate(
    NPK_tot = sum(
      N_tot,
      P_tot,
      K_tot,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    FAOStat_area_code != 351,
    FAOStat_area_code != 186
  ) %>%
  arrange(
    desc(NPK_tot)
  ) %>%
  slice_head(
    n = 50
  )


Country_N_Uses_plot_50 <- Country_N_Uses_plot %>%
  filter(
    FAOStat_area_code %in%
      Top50_Country_NPK_tot$FAOStat_area_code
  )

Country_P_Uses_plot_50 <- Country_P_Uses_plot %>%
  filter(
    FAOStat_area_code %in%
      Top50_Country_NPK_tot$FAOStat_area_code
  )

Country_K_Uses_plot_50 <- Country_K_Uses_plot %>%
  filter(
    FAOStat_area_code %in%
      Top50_Country_NPK_tot$FAOStat_area_code
  )


## Figure panels ------------------------------------------------------------

Country_change_N <- ggplot(
  Country_N_Uses_plot_50,
  aes(y = ISO3)
) +
  geom_segment(
    aes(
      x = first_decade,
      xend = last_decade,
      yend = ISO3,
      color = direction
    ),
    arrow = arrow(
      length = unit(0.1, "cm"),
      type = "closed"
    ),
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = med_rat_1960_N,
      xend = med_rat_2010_N,
      yend = ISO3
    ),
    color = "grey30",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_grid(
    Region ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_color_manual(
    values = c(
      "Increase" = "darkred",
      "Decrease" = "lightblue3"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 4)
  ) + labs(
    x = expression(
      paste(
        "FFR (",
        N[feed],
        " ",
        N[food],
        ")"
      )
    ),
    y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 6.5),
    axis.text.x = element_text(size = 6.5),
    legend.position = "none"
  )


Country_change_P <- ggplot(
  Country_P_Uses_plot_50,
  aes(y = ISO3)
) +
  geom_segment(
    aes(
      x = first_decade,
      xend = last_decade,
      yend = ISO3,
      color = direction
    ),
    arrow = arrow(
      length = unit(0.1, "cm"),
      type = "closed"
    ),
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = med_rat_1960_P,
      xend = med_rat_2010_P,
      yend = ISO3
    ),
    color = "grey30",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_grid(
    Region ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_color_manual(
    values = c(
      "Increase" = "darkred",
      "Decrease" = "lightblue3"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 4)
  ) + labs(
    x = expression(
      paste(
        "FFR (",
        P[feed],
        " ",
        P[food],
        ")"
      )
    ),
    y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 6.5),
    axis.text.x = element_text(size = 6.5),
    legend.position = "none"
  )


Country_change_K <- ggplot(
  Country_K_Uses_plot_50,
  aes(y = ISO3)
) +
  geom_segment(
    aes(
      x = first_decade,
      xend = last_decade,
      yend = ISO3,
      color = direction
    ),
    arrow = arrow(
      length = unit(0.1, "cm"),
      type = "closed"
    ),
    linewidth = 0.7
  ) +
  geom_segment(
    aes(
      x = med_rat_1960_K,
      xend = med_rat_2010_K,
      yend = ISO3
    ),
    color = "grey30",
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_grid(
    Region ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_color_manual(
    values = c(
      "Increase" = "darkred",
      "Decrease" = "lightblue3"
    )
  ) + labs(
    x = expression(
      paste(
        "FFR (",
        K[feed],
        " ",
        K[food],
        ")"
      )
    ),
    y = NULL
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 6.5),
    axis.text.x = element_text(size = 6.5),
    legend.position = "none"
  )



## Combine and export Figure 2 ---------------------------------------------

Figure2 <- plot_grid(
  Country_change_N,
  Country_change_P,
  Country_change_K,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold"
)

# Export Figure 2 

ggsave(
  filename = "plots/Figure2_Country_change_publication.pdf",
  plot = Figure2,
  device = cairo_pdf,
  width = 180,
  height = 170,
  units = "mm"
)
## Export data underlying Figure 2 -----------------------------------------
#
# Create a single source-data table containing the country-level FFR
# values used in Figure 2 for N, P, and K.
#
# The table includes:
#   - country ISO3 code
#   - geographic region
#   - median FFR in the 1960s and 2010s
#   - earliest and latest available decade used in the figure
#   - direction of change
#   - a global reference row for each nutrient
#
# Cliff's delta can be added later as an additional column.


### Nitrogen ----------------------------------------------------------------

Figure2_N_data <- Country_N_Uses_plot_50 %>%
  transmute(
    Nutrient = "N",
    ISO3,
    Region = as.character(Region),
    
    FFR_1960s = decade_1960,
    FFR_2010s = decade_2010,
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR = first_decade,
    End_FFR = last_decade,
    
    Direction = ifelse(
      End_FFR > Start_FFR,
      "Increase",
      "Decrease"
    )
  )


# Phosphorus 

Figure2_P_data <- Country_P_Uses_plot_50 %>%
  transmute(
    Nutrient = "P",
    ISO3,
    Region = as.character(Region),
    
    FFR_1960s = decade_1960,
    FFR_2010s = decade_2010,
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR = first_decade,
    End_FFR = last_decade,
    
    Direction = ifelse(
      End_FFR > Start_FFR,
      "Increase",
      "Decrease"
    )
  )


# Potassium 

Figure2_K_data <- Country_K_Uses_plot_50 %>%
  transmute(
    Nutrient = "K",
    ISO3,
    Region = as.character(Region),
    
    FFR_1960s = decade_1960,
    FFR_2010s = decade_2010,
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR = first_decade,
    End_FFR = last_decade,
    
    Direction = ifelse(
      End_FFR > Start_FFR,
      "Increase",
      "Decrease"
    )
  )


# Global reference rows

Figure2_Global_data <- tibble(
  Nutrient = c(
    "N",
    "P",
    "K"
  ),
  
  ISO3 = "GLOBAL",
  
  Region = "Global",
  
  FFR_1960s = c(
    med_rat_1960_N,
    med_rat_1960_P,
    med_rat_1960_K
  ),
  
  FFR_2010s = c(
    med_rat_2010_N,
    med_rat_2010_P,
    med_rat_2010_K
  ),
  
  Start_decade = "1960s",
  End_decade = "2010s",
  
  Start_FFR = c(
    med_rat_1960_N,
    med_rat_1960_P,
    med_rat_1960_K
  ),
  
  End_FFR = c(
    med_rat_2010_N,
    med_rat_2010_P,
    med_rat_2010_K
  )
) %>%
  mutate(
    Direction = ifelse(
      End_FFR > Start_FFR,
      "Increase",
      "Decrease"
    )
  )


# Combine country and global data 

Figure2_data <- bind_rows(
  Figure2_N_data,
  Figure2_P_data,
  Figure2_K_data,
  Figure2_Global_data
) %>%
  arrange(
    Nutrient,
    Region,
    ISO3
  )

### Cliff's delta for Figure 2 ----------------------------------------------
#
# Cliff's delta compares the distribution of FFR values in the latest
# decade used in the figure with the distribution in the earliest decade.
#
# Positive values indicate generally higher FFR in the latest decade,
# whereas negative values indicate generally lower FFR.
#
# The calculation uses the original FFR values, before applying the
# 0-4 visualization limits.


# Calculate Cliff's delta without requiring an additional package.

cliffs_delta <- function(x_end, x_start) {
  
  x_end <- x_end[
    is.finite(x_end)
  ]
  
  x_start <- x_start[
    is.finite(x_start)
  ]
  
  if (
    length(x_end) == 0 ||
    length(x_start) == 0
  ) {
    return(NA_real_)
  }
  
  n_end <- length(x_end)
  n_start <- length(x_start)
  
  ranks <- rank(
    c(
      x_end,
      x_start
    ),
    ties.method = "average"
  )
  
  U <- sum(
    ranks[seq_len(n_end)]
  ) -
    n_end * (n_end + 1) / 2
  
  delta <- 2 * U /
    (n_end * n_start) - 1
  
  return(delta)
}


# Calculate country-specific Cliff's delta using the actual earliest and
# latest decades selected for each country in Figure 2.

calculate_country_cliff <- function(
    country_data,
    plot_data
) {
  
  results <- lapply(
    seq_len(nrow(plot_data)),
    function(i) {
      
      country_i <- plot_data$FAOStat_area_code[i]
      
      start_decade_i <-
        plot_data$first_decade_years[i]
      
      end_decade_i <-
        plot_data$last_decade_years[i]
      
      
      start_values <- country_data %>%
        filter(
          FAOStat_area_code == country_i,
          decade == start_decade_i
        ) %>%
        pull(
          Rat_FeedFood
        )
      
      
      end_values <- country_data %>%
        filter(
          FAOStat_area_code == country_i,
          decade == end_decade_i
        ) %>%
        pull(
          Rat_FeedFood
        )
      
      
      tibble(
        FAOStat_area_code = country_i,
        
        ISO3 = as.character(
          plot_data$ISO3[i]
        ),
        
        Start_decade = start_decade_i,
        
        End_decade = end_decade_i,
        
        Cliff_delta = cliffs_delta(
          x_end = end_values,
          x_start = start_values
        ),
        
        N_start = sum(
          is.finite(start_values)
        ),
        
        N_end = sum(
          is.finite(end_values)
        )
      )
    }
  )
  
  bind_rows(
    results
  )
}


# Calculate Cliff's delta for N, P, and K

Cliff_N <- calculate_country_cliff(
  country_data = Country_N_Uses,
  plot_data = Country_N_Uses_plot_50
) %>%
  mutate(
    Nutrient = "N"
  )


Cliff_P <- calculate_country_cliff(
  country_data = Country_P_Uses,
  plot_data = Country_P_Uses_plot_50
) %>%
  mutate(
    Nutrient = "P"
  )


Cliff_K <- calculate_country_cliff(
  country_data = Country_K_Uses,
  plot_data = Country_K_Uses_plot_50
) %>%
  mutate(
    Nutrient = "K"
  )


Country_Cliff <- bind_rows(
  Cliff_N,
  Cliff_P,
  Cliff_K
)


# Global Cliff's delta

Global_Cliff_N <- World_N_global %>%
  mutate(
    Rat = N_use_feed / N_use_food
  )

Global_Cliff_P <- World_P_global %>%
  mutate(
    Rat = P_use_feed / P_use_food
  )

Global_Cliff_K <- World_K_global %>%
  mutate(
    Rat = K_use_feed / K_use_food
  )


Global_Cliff <- tibble(
  Nutrient = c(
    "N",
    "P",
    "K"
  ),
  
  Cliff_delta = c(
    
    cliffs_delta(
      Global_Cliff_N %>%
        filter(Year > 2010) %>%
        pull(Rat),
      
      Global_Cliff_N %>%
        filter(Year < 1970) %>%
        pull(Rat)
    ),
    
    cliffs_delta(
      Global_Cliff_P %>%
        filter(Year > 2010) %>%
        pull(Rat),
      
      Global_Cliff_P %>%
        filter(Year < 1970) %>%
        pull(Rat)
    ),
    
    cliffs_delta(
      Global_Cliff_K %>%
        filter(Year > 2010) %>%
        pull(Rat),
      
      Global_Cliff_K %>%
        filter(Year < 1970) %>%
        pull(Rat)
    )
  )
)


Figure2_N_data <- Country_N_Uses_plot_50 %>%
  transmute(
    FAOStat_area_code,
    Nutrient = "N",
    ISO3 = as.character(ISO3),
    Region = as.character(Region),
    
    FFR_1960s = decade_1960,
    FFR_2010s = decade_2010,
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR = first_decade,
    End_FFR = last_decade,
    
    Direction = direction
  ) %>%
  left_join(
    Cliff_N %>%
      select(
        FAOStat_area_code,
        Cliff_delta,
        N_start,
        N_end
      ),
    by = "FAOStat_area_code"
  )


Figure2_P_data <- Country_P_Uses_plot_50 %>%
  transmute(
    FAOStat_area_code,
    Nutrient = "P",
    ISO3 = as.character(ISO3),
    Region = as.character(Region),
    
    FFR_1960s = decade_1960,
    FFR_2010s = decade_2010,
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR = first_decade,
    End_FFR = last_decade,
    
    Direction = direction
  ) %>%
  left_join(
    Cliff_P %>%
      select(
        FAOStat_area_code,
        Cliff_delta,
        N_start,
        N_end
      ),
    by = "FAOStat_area_code"
  )


Figure2_K_data <- Country_K_Uses_plot_50 %>%
  transmute(
    FAOStat_area_code,
    Nutrient = "K",
    ISO3 = as.character(ISO3),
    Region = as.character(Region),
    
    FFR_1960s = decade_1960,
    FFR_2010s = decade_2010,
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR = first_decade,
    End_FFR = last_decade,
    
    Direction = direction
  ) %>%
  left_join(
    Cliff_K %>%
      select(
        FAOStat_area_code,
        Cliff_delta,
        N_start,
        N_end
      ),
    by = "FAOStat_area_code"
  )

Figure2_Global_data <- tibble(
  FAOStat_area_code = NA_real_,
  
  Nutrient = c(
    "N",
    "P",
    "K"
  ),
  
  ISO3 = "GLOBAL",
  
  Region = "Global",
  
  FFR_1960s = c(
    med_rat_1960_N,
    med_rat_1960_P,
    med_rat_1960_K
  ),
  
  FFR_2010s = c(
    med_rat_2010_N,
    med_rat_2010_P,
    med_rat_2010_K
  ),
  
  Start_decade = "1960s",
  End_decade = "2010s",
  
  Start_FFR = c(
    med_rat_1960_N,
    med_rat_1960_P,
    med_rat_1960_K
  ),
  
  End_FFR = c(
    med_rat_2010_N,
    med_rat_2010_P,
    med_rat_2010_K
  )
) %>%
  mutate(
    Direction = ifelse(
      End_FFR > Start_FFR,
      "Increase",
      "Decrease"
    )
  ) %>%
  left_join(
    Global_Cliff,
    by = "Nutrient"
  ) %>%
  mutate(
    N_start = NA_integer_,
    N_end = NA_integer_
  )

Figure2_data <- bind_rows(
  Figure2_N_data,
  Figure2_P_data,
  Figure2_K_data,
  Figure2_Global_data
) %>%
  arrange(
    Nutrient,
    Region,
    ISO3
  )


write_csv(
  Figure2_data,
  "results/Figure2_Country_FFR_data.csv"
)





## Supplementary figures: Country FFR changes by continent -----------------
#
# These figures show all available countries, grouped into four broad
# geographical areas. Within each figure, countries remain separated by
# FAOSTAT subregion using facet_grid().
#
# The analytical values are the same as those used for Figure 2:
# earliest vs latest available decade, with displayed FFR values limited
# to the 0-4 range.


# Define subregions included in each supplementary figure

regions_americas <- c(
  "Northern America",
  "Caribbean",
  "Central America",
  "South America"
)

regions_asia_oceania <- c(
  "Western Asia",
  "Central Asia",
  "Eastern Asia",
  "Southern Asia",
  "South-eastern Asia",
  "Micronesia",
  "Polynesia",
  "Melanesia",
  "Australia and New Zealand"
)

regions_africa <- c(
  "Northern Africa",
  "Middle Africa",
  "Western Africa",
  "Eastern Africa",
  "Southern Africa"
)

regions_europe <- c(
  "Northern Europe",
  "Western Europe",
  "Eastern Europe",
  "Southern Europe"
)


# Function to produce one nutrient panel

plot_country_FFR <- function(
    data,
    regions,
    global_start,
    global_end,
    x_label
) {
  
  data_plot <- data %>%
    filter(
      Region %in% regions
    ) %>%
    droplevels()
  
  ggplot(
    data_plot,
    aes(y = ISO3)
  ) +
    
    # Country-level change in FFR.
    geom_segment(
      aes(
        x = first_decade,
        xend = last_decade,
        yend = ISO3,
        color = direction
      ),
      arrow = arrow(
        length = unit(0.1, "cm"),
        type = "closed"
      ),
      linewidth = 0.7
    ) +
    
    # Global FFR change shown for reference.
    geom_segment(
      aes(
        x = global_start,
        xend = global_end,
        yend = ISO3
      ),
      color = "grey30",
      linewidth = 1,
      alpha = 0.5
    ) +
    
    # Retain the original FAOSTAT subregions within each broad region.
    facet_grid(
      Region ~ .,
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    
    scale_color_manual(
      values = c(
        "Increase" = "darkred",
        "Decrease" = "lightblue3"
      )
    ) +
    
    scale_x_continuous(
      limits = c(0, 4)
    ) +
    
    labs(
      x = x_label,
      y = NULL
    ) +
    
    theme_bw() +
    
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(
        size = 6.5
      ),axis.text.x = element_text(
        size = 6.5
      ),
      legend.position = "none"
    )
}


# Americas

Americas_N <- plot_country_FFR(
  data = Country_N_Uses_plot,
  regions = regions_americas,
  global_start = med_rat_1960_N,
  global_end = med_rat_2010_N,
  x_label = expression(
    paste(
      "FFR (",
      N[feed],
      " ",
      N[food],
      ")"
    )
  )
)

Americas_P <- plot_country_FFR(
  data = Country_P_Uses_plot,
  regions = regions_americas,
  global_start = med_rat_1960_P,
  global_end = med_rat_2010_P,
  x_label = expression(
    paste(
      "FFR (",
      P[feed],
      " ",
      P[food],
      ")"
    )
  )
)

Americas_K <- plot_country_FFR(
  data = Country_K_Uses_plot,
  regions = regions_americas,
  global_start = med_rat_1960_K,
  global_end = med_rat_2010_K,
  x_label = expression(
    paste(
      "FFR (",
      K[feed],
      " ",
      K[food],
      ")"
    )
  )
)

FigureS_Americas <- plot_grid(
  Americas_N,
  Americas_P,
  Americas_K,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold"
)


# Asia and Oceania

AsiaOceania_N <- plot_country_FFR(
  data = Country_N_Uses_plot,
  regions = regions_asia_oceania,
  global_start = med_rat_1960_N,
  global_end = med_rat_2010_N,
  x_label = expression(
    paste(
      "FFR (",
      N[feed],
      " ",
      N[food],
      ")"
    )
  )
)

AsiaOceania_P <- plot_country_FFR(
  data = Country_P_Uses_plot,
  regions = regions_asia_oceania,
  global_start = med_rat_1960_P,
  global_end = med_rat_2010_P,
  x_label = expression(
    paste(
      "FFR (",
      P[feed],
      " ",
      P[food],
      ")"
    )
  )
)

AsiaOceania_K <- plot_country_FFR(
  data = Country_K_Uses_plot,
  regions = regions_asia_oceania,
  global_start = med_rat_1960_K,
  global_end = med_rat_2010_K,
  x_label = expression(
    paste(
      "FFR (",
      K[feed],
      " ",
      K[food],
      ")"
    )
  )
)

FigureS_AsiaOceania <- plot_grid(
  AsiaOceania_N,
  AsiaOceania_P,
  AsiaOceania_K,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold"
)


# Africa

Africa_N <- plot_country_FFR(
  data = Country_N_Uses_plot,
  regions = regions_africa,
  global_start = med_rat_1960_N,
  global_end = med_rat_2010_N,
  x_label = expression(
    paste(
      "FFR (",
      N[feed],
      " ",
      N[food],
      ")"
    )
  )
)

Africa_P <- plot_country_FFR(
  data = Country_P_Uses_plot,
  regions = regions_africa,
  global_start = med_rat_1960_P,
  global_end = med_rat_2010_P,
  x_label = expression(
    paste(
      "FFR (",
      P[feed],
      " ",
      P[food],
      ")"
    )
  )
)

Africa_K <- plot_country_FFR(
  data = Country_K_Uses_plot,
  regions = regions_africa,
  global_start = med_rat_1960_K,
  global_end = med_rat_2010_K,
  x_label = expression(
    paste(
      "FFR (",
      K[feed],
      " ",
      K[food],
      ")"
    )
  )
)

FigureS_Africa <- plot_grid(
  Africa_N,
  Africa_P,
  Africa_K,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold"
)


# Europe

Europe_N <- plot_country_FFR(
  data = Country_N_Uses_plot,
  regions = regions_europe,
  global_start = med_rat_1960_N,
  global_end = med_rat_2010_N,
  x_label = expression(
    paste(
      "FFR (",
      N[feed],
      " ",
      N[food],
      ")"
    )
  )
)

Europe_P <- plot_country_FFR(
  data = Country_P_Uses_plot,
  regions = regions_europe,
  global_start = med_rat_1960_P,
  global_end = med_rat_2010_P,
  x_label = expression(
    paste(
      "FFR (",
      P[feed],
      " ",
      P[food],
      ")"
    )
  )
)

Europe_K <- plot_country_FFR(
  data = Country_K_Uses_plot,
  regions = regions_europe,
  global_start = med_rat_1960_K,
  global_end = med_rat_2010_K,
  x_label = expression(
    paste(
      "FFR (",
      K[feed],
      " ",
      K[food]^{-1},
      ")"
    )
  )
)

FigureS_Europe <- plot_grid(
  Europe_N,
  Europe_P,
  Europe_K,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold"
)


### Export supplementary figures --------------------------------------------

ggsave(
  "plots/FigureS_Country_change_Americas.pdf",
  FigureS_Americas,
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

ggsave(
  "plots/FigureS_Country_change_Asia_Oceania.pdf",
  FigureS_AsiaOceania,
  device = cairo_pdf,
  width = 180,
  height = 150,
  units = "mm"
)

ggsave(
  "plots/FigureS_Country_change_Africa.pdf",
  FigureS_Africa,
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

ggsave(
  "plots/FigureS_Country_change_Europe.pdf",
  FigureS_Europe,
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

### Supplementary tables: Country FFR change and Cliff's delta ---------------
#
# Cliff's delta is calculated using the same earliest and latest decades
# selected for each country in the plots.
#
# The effect size is calculated from the original year × simulation FFR
# distributions, before applying the 0-4 visualization limits.


# Calculate Cliff's delta for all countries 

Cliff_N_all <- calculate_country_cliff(
  country_data = Country_N_Uses,
  plot_data = Country_N_Uses_plot
) %>%
  mutate(
    Nutrient = "N"
  )

Cliff_P_all <- calculate_country_cliff(
  country_data = Country_P_Uses,
  plot_data = Country_P_Uses_plot
) %>%
  mutate(
    Nutrient = "P"
  )

Cliff_K_all <- calculate_country_cliff(
  country_data = Country_K_Uses,
  plot_data = Country_K_Uses_plot
) %>%
  mutate(
    Nutrient = "K"
  )


# Create country-level source tables 

Table_N <- Country_N_Uses_plot %>%
  transmute(
    FAOStat_area_code,
    Nutrient = "N",
    ISO3 = as.character(ISO3),
    Region = as.character(Region),
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    # Original decade medians, before the plotting limits are applied.
    Start_FFR_median = case_when(
      Start_decade == "1960s" ~ decade_1960,
      Start_decade == "1970s" ~ decade_1970,
      Start_decade == "1980s" ~ decade_1980,
      Start_decade == "1990s" ~ decade_1990,
      Start_decade == "2000s" ~ decade_2000,
      TRUE ~ NA_real_
    ),
    
    End_FFR_median = case_when(
      End_decade == "2010s" ~ decade_2010,
      End_decade == "2000s" ~ decade_2000,
      End_decade == "1990s" ~ decade_1990,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    Direction = ifelse(
      End_FFR_median > Start_FFR_median,
      "Increase",
      "Decrease"
    )
  ) %>%
  left_join(
    Cliff_N_all %>%
      select(
        FAOStat_area_code,
        Cliff_delta,
        N_start,
        N_end
      ),
    by = "FAOStat_area_code"
  )


Table_P <- Country_P_Uses_plot %>%
  transmute(
    FAOStat_area_code,
    Nutrient = "P",
    ISO3 = as.character(ISO3),
    Region = as.character(Region),
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR_median = case_when(
      Start_decade == "1960s" ~ decade_1960,
      Start_decade == "1970s" ~ decade_1970,
      Start_decade == "1980s" ~ decade_1980,
      Start_decade == "1990s" ~ decade_1990,
      Start_decade == "2000s" ~ decade_2000,
      TRUE ~ NA_real_
    ),
    
    End_FFR_median = case_when(
      End_decade == "2010s" ~ decade_2010,
      End_decade == "2000s" ~ decade_2000,
      End_decade == "1990s" ~ decade_1990,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    Direction = ifelse(
      End_FFR_median > Start_FFR_median,
      "Increase",
      "Decrease"
    )
  ) %>%
  left_join(
    Cliff_P_all %>%
      select(
        FAOStat_area_code,
        Cliff_delta,
        N_start,
        N_end
      ),
    by = "FAOStat_area_code"
  )


Table_K <- Country_K_Uses_plot %>%
  transmute(
    FAOStat_area_code,
    Nutrient = "K",
    ISO3 = as.character(ISO3),
    Region = as.character(Region),
    
    Start_decade = first_decade_years,
    End_decade = last_decade_years,
    
    Start_FFR_median = case_when(
      Start_decade == "1960s" ~ decade_1960,
      Start_decade == "1970s" ~ decade_1970,
      Start_decade == "1980s" ~ decade_1980,
      Start_decade == "1990s" ~ decade_1990,
      Start_decade == "2000s" ~ decade_2000,
      TRUE ~ NA_real_
    ),
    
    End_FFR_median = case_when(
      End_decade == "2010s" ~ decade_2010,
      End_decade == "2000s" ~ decade_2000,
      End_decade == "1990s" ~ decade_1990,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    Direction = ifelse(
      End_FFR_median > Start_FFR_median,
      "Increase",
      "Decrease"
    )
  ) %>%
  left_join(
    Cliff_K_all %>%
      select(
        FAOStat_area_code,
        Cliff_delta,
        N_start,
        N_end
      ),
    by = "FAOStat_area_code"
  )


# Combine nutrients

Country_FFR_Cliff_table <- bind_rows(
  Table_N,
  Table_P,
  Table_K
) %>%
  arrange(
    Region,
    ISO3,
    Nutrient
  )


# Add broad geographical group 

Country_FFR_Cliff_table <- Country_FFR_Cliff_table %>%
  mutate(
    Continent_group = case_when(
      
      Region %in% regions_americas ~
        "Americas",
      
      Region %in% regions_asia_oceania ~
        "Asia and Oceania",
      
      Region %in% regions_africa ~
        "Africa",
      
      Region %in% regions_europe ~
        "Europe",
      
      TRUE ~ NA_character_
    )
  )


# Create separate supplementary tables

Table_Americas <- Country_FFR_Cliff_table %>%
  filter(
    Continent_group == "Americas"
  )

Table_Asia_Oceania <- Country_FFR_Cliff_table %>%
  filter(
    Continent_group == "Asia and Oceania"
  )

Table_Africa <- Country_FFR_Cliff_table %>%
  filter(
    Continent_group == "Africa"
  )

Table_Europe <- Country_FFR_Cliff_table %>%
  filter(
    Continent_group == "Europe"
  )


# Export

write_csv(
  Country_FFR_Cliff_table,
  "results/Supplementary_Country_FFR_Cliff_all.csv"
)

write_csv(
  Table_Americas,
  "results/Supplementary_Country_FFR_Cliff_Americas.csv"
)

write_csv(
  Table_Asia_Oceania,
  "results/Supplementary_Country_FFR_Cliff_Asia_Oceania.csv"
)

write_csv(
  Table_Africa,
  "results/Supplementary_Country_FFR_Cliff_Africa.csv"
)

write_csv(
  Table_Europe,
  "results/Supplementary_Country_FFR_Cliff_Europe.csv"
)

# Figure 3: Crop-specific feed-to-food fertilizer ratios ------------------
#
# Figure 3 shows the evolution of the global feed-to-food fertilizer-use
# ratio (FFR) for wheat, maize, rice, and soybean.
#
# For each crop, nutrient, year, and Monte Carlo simulation:
#   1. Fertilizer use is summed across countries.
#   2. FFR is calculated as feed fertilizer use / food fertilizer use.
#   3. Annual means, SDs, and 95% Monte Carlo quantiles are calculated.
#
# The figure displays the annual mean FFR ± 1 SD. Point size represents
# total fertilizer use allocated to food + feed for each crop.
# The shaded interval (2010-2013) indicates the Food Balance methodology
# transition period.


## Nitrogen ----------------------------------------------------------------

N_wheat_ratio <- N_wheat_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    N_food = sum(N_food, na.rm = TRUE),
    N_feed = sum(N_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = N_feed / N_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    N_food_mean = mean(N_food, na.rm = TRUE),
    N_food_q025 = quantile(N_food, 0.025, na.rm = TRUE),
    N_food_q975 = quantile(N_food, 0.975, na.rm = TRUE),
    
    N_feed_mean = mean(N_feed, na.rm = TRUE),
    N_feed_q025 = quantile(N_feed, 0.025, na.rm = TRUE),
    N_feed_q975 = quantile(N_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Wheat"
  )


N_maize_ratio <- N_maize_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    N_food = sum(N_food, na.rm = TRUE),
    N_feed = sum(N_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = N_feed / N_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    N_food_mean = mean(N_food, na.rm = TRUE),
    N_food_q025 = quantile(N_food, 0.025, na.rm = TRUE),
    N_food_q975 = quantile(N_food, 0.975, na.rm = TRUE),
    
    N_feed_mean = mean(N_feed, na.rm = TRUE),
    N_feed_q025 = quantile(N_feed, 0.025, na.rm = TRUE),
    N_feed_q975 = quantile(N_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Maize"
  )


N_rice_ratio <- N_rice_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    N_food = sum(N_food, na.rm = TRUE),
    N_feed = sum(N_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = N_feed / N_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    N_food_mean = mean(N_food, na.rm = TRUE),
    N_food_q025 = quantile(N_food, 0.025, na.rm = TRUE),
    N_food_q975 = quantile(N_food, 0.975, na.rm = TRUE),
    
    N_feed_mean = mean(N_feed, na.rm = TRUE),
    N_feed_q025 = quantile(N_feed, 0.025, na.rm = TRUE),
    N_feed_q975 = quantile(N_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Rice"
  )


N_soybean_ratio <- N_soybean_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    N_food = sum(N_food, na.rm = TRUE),
    N_feed = sum(N_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = N_feed / N_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    N_food_mean = mean(N_food, na.rm = TRUE),
    N_food_q025 = quantile(N_food, 0.025, na.rm = TRUE),
    N_food_q975 = quantile(N_food, 0.975, na.rm = TRUE),
    
    N_feed_mean = mean(N_feed, na.rm = TRUE),
    N_feed_q025 = quantile(N_feed, 0.025, na.rm = TRUE),
    N_feed_q975 = quantile(N_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Soybean"
  )


N_ratio <- bind_rows(
  N_wheat_ratio,
  N_maize_ratio,
  N_rice_ratio,
  N_soybean_ratio
) %>%
  mutate(
    Nutrient = "N",
    Crop = factor(
      Crop,
      levels = c(
        "Wheat",
        "Maize",
        "Rice",
        "Soybean"
      )
    ),
    Food_feed_use_Tg = (
      N_food_mean +
        N_feed_mean
    ) / 1e6
  )


## Phosphorus ---------------------------------------------------------------

P_wheat_ratio <- P_wheat_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    P_food = sum(P_food, na.rm = TRUE),
    P_feed = sum(P_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = P_feed / P_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    P_food_mean = mean(P_food, na.rm = TRUE),
    P_food_q025 = quantile(P_food, 0.025, na.rm = TRUE),
    P_food_q975 = quantile(P_food, 0.975, na.rm = TRUE),
    
    P_feed_mean = mean(P_feed, na.rm = TRUE),
    P_feed_q025 = quantile(P_feed, 0.025, na.rm = TRUE),
    P_feed_q975 = quantile(P_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Wheat"
  )


P_maize_ratio <- P_maize_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    P_food = sum(P_food, na.rm = TRUE),
    P_feed = sum(P_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = P_feed / P_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    P_food_mean = mean(P_food, na.rm = TRUE),
    P_food_q025 = quantile(P_food, 0.025, na.rm = TRUE),
    P_food_q975 = quantile(P_food, 0.975, na.rm = TRUE),
    
    P_feed_mean = mean(P_feed, na.rm = TRUE),
    P_feed_q025 = quantile(P_feed, 0.025, na.rm = TRUE),
    P_feed_q975 = quantile(P_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Maize"
  )


P_rice_ratio <- P_rice_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    P_food = sum(P_food, na.rm = TRUE),
    P_feed = sum(P_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = P_feed / P_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    P_food_mean = mean(P_food, na.rm = TRUE),
    P_food_q025 = quantile(P_food, 0.025, na.rm = TRUE),
    P_food_q975 = quantile(P_food, 0.975, na.rm = TRUE),
    
    P_feed_mean = mean(P_feed, na.rm = TRUE),
    P_feed_q025 = quantile(P_feed, 0.025, na.rm = TRUE),
    P_feed_q975 = quantile(P_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Rice"
  )


P_soybean_ratio <- P_soybean_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    P_food = sum(P_food, na.rm = TRUE),
    P_feed = sum(P_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = P_feed / P_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    P_food_mean = mean(P_food, na.rm = TRUE),
    P_food_q025 = quantile(P_food, 0.025, na.rm = TRUE),
    P_food_q975 = quantile(P_food, 0.975, na.rm = TRUE),
    
    P_feed_mean = mean(P_feed, na.rm = TRUE),
    P_feed_q025 = quantile(P_feed, 0.025, na.rm = TRUE),
    P_feed_q975 = quantile(P_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Soybean"
  )


P_ratio <- bind_rows(
  P_wheat_ratio,
  P_maize_ratio,
  P_rice_ratio,
  P_soybean_ratio
) %>%
  mutate(
    Nutrient = "P",
    Crop = factor(
      Crop,
      levels = c(
        "Wheat",
        "Maize",
        "Rice",
        "Soybean"
      )
    ),
    Food_feed_use_Tg = (
      P_food_mean +
        P_feed_mean
    ) / 1e6
  )


## Potassium ----------------------------------------------------------------

K_wheat_ratio <- K_wheat_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    K_food = sum(K_food, na.rm = TRUE),
    K_feed = sum(K_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = K_feed / K_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    K_food_mean = mean(K_food, na.rm = TRUE),
    K_food_q025 = quantile(K_food, 0.025, na.rm = TRUE),
    K_food_q975 = quantile(K_food, 0.975, na.rm = TRUE),
    
    K_feed_mean = mean(K_feed, na.rm = TRUE),
    K_feed_q025 = quantile(K_feed, 0.025, na.rm = TRUE),
    K_feed_q975 = quantile(K_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Wheat"
  )


K_maize_ratio <- K_maize_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    K_food = sum(K_food, na.rm = TRUE),
    K_feed = sum(K_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = K_feed / K_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    K_food_mean = mean(K_food, na.rm = TRUE),
    K_food_q025 = quantile(K_food, 0.025, na.rm = TRUE),
    K_food_q975 = quantile(K_food, 0.975, na.rm = TRUE),
    
    K_feed_mean = mean(K_feed, na.rm = TRUE),
    K_feed_q025 = quantile(K_feed, 0.025, na.rm = TRUE),
    K_feed_q975 = quantile(K_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Maize"
  )


K_rice_ratio <- K_rice_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    K_food = sum(K_food, na.rm = TRUE),
    K_feed = sum(K_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = K_feed / K_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    K_food_mean = mean(K_food, na.rm = TRUE),
    K_food_q025 = quantile(K_food, 0.025, na.rm = TRUE),
    K_food_q975 = quantile(K_food, 0.975, na.rm = TRUE),
    
    K_feed_mean = mean(K_feed, na.rm = TRUE),
    K_feed_q025 = quantile(K_feed, 0.025, na.rm = TRUE),
    K_feed_q975 = quantile(K_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Rice"
  )


K_soybean_ratio <- K_soybean_uses %>%
  filter(FAOStat_area_code != 351) %>%
  ungroup() %>%
  group_by(Sim, Year) %>%
  summarise(
    K_food = sum(K_food, na.rm = TRUE),
    K_feed = sum(K_feed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Rat_feed_food = K_feed / K_food
  ) %>%
  group_by(Year) %>%
  summarise(
    Rat_feed_food_mean = mean(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_sd = sd(Rat_feed_food, na.rm = TRUE),
    Rat_feed_food_q025 = quantile(Rat_feed_food, 0.025, na.rm = TRUE),
    Rat_feed_food_q975 = quantile(Rat_feed_food, 0.975, na.rm = TRUE),
    
    K_food_mean = mean(K_food, na.rm = TRUE),
    K_food_q025 = quantile(K_food, 0.025, na.rm = TRUE),
    K_food_q975 = quantile(K_food, 0.975, na.rm = TRUE),
    
    K_feed_mean = mean(K_feed, na.rm = TRUE),
    K_feed_q025 = quantile(K_feed, 0.025, na.rm = TRUE),
    K_feed_q975 = quantile(K_feed, 0.975, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Crop = "Soybean"
  )


K_ratio <- bind_rows(
  K_wheat_ratio,
  K_maize_ratio,
  K_rice_ratio,
  K_soybean_ratio
) %>%
  mutate(
    Nutrient = "K",
    Crop = factor(
      Crop,
      levels = c(
        "Wheat",
        "Maize",
        "Rice",
        "Soybean"
      )
    ),
    Food_feed_use_Tg = (
      K_food_mean +
        K_feed_mean
    ) / 1e6
  )


## Figure aesthetics --------------------------------------------------------

crop_colors <- c(
  "Wheat" = "#0000ff",
  "Maize" = "#ff0000",
  "Rice" = "#ba55d3",
  "Soybean" = "#d4a900"
)


Figure3_N <- ggplot(
  N_ratio,
  aes(
    x = Year,
    color = Crop
  )
) +
  
  # Food Balance methodology transition.
  annotate(
    "rect",
    xmin = 2010,
    xmax = 2013,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.25
  ) +
  
  # Monte Carlo uncertainty shown as ±1 SD.
  geom_ribbon(
    aes(
      ymin = Rat_feed_food_mean - Rat_feed_food_sd,
      ymax = Rat_feed_food_mean + Rat_feed_food_sd,
      fill = Crop
    ),
    color = NA,
    alpha = 0.25
  ) +
  
  geom_line(
    aes(
      y = Rat_feed_food_mean
    ),
    linewidth = 0.6
  ) +
  
  # Point size represents food + feed fertilizer use.
  geom_point(
    aes(
      y = Rat_feed_food_mean,
      size = Food_feed_use_Tg
    ),
    color = "black",
    shape = 1,
    stroke = 0.4
  ) +
  
  # FFR = 1 indicates equal fertilizer use for feed and food.
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  scale_color_manual(
    name = "Crop",
    values = crop_colors
  ) +
  
  scale_fill_manual(
    name = "Crop",
    values = crop_colors
  ) +
  
  scale_size_continuous(
    name = "Food + feed use (Tg)",
    range = c(0.2, 1)
  ) +
  
  labs(
    x = "Year",
    y = expression(
      paste(
        "FFR (",
        N[feed],
        " ",
        N[food],
        ")"
      )
    )
  ) +
  
  theme_bw(
    base_family = "Arial",
    base_size = 7
  ) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6.5)
  )


Figure3_P <- ggplot(
  P_ratio,
  aes(
    x = Year,
    color = Crop
  )
) +
  annotate(
    "rect",
    xmin = 2010,
    xmax = 2013,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.25
  ) +
  geom_ribbon(
    aes(
      ymin = Rat_feed_food_mean - Rat_feed_food_sd,
      ymax = Rat_feed_food_mean + Rat_feed_food_sd,
      fill = Crop
    ),
    color = NA,
    alpha = 0.25
  ) +
  geom_line(
    aes(
      y = Rat_feed_food_mean
    ),
    linewidth = 0.6
  ) +
  geom_point(
    aes(
      y = Rat_feed_food_mean,
      size = Food_feed_use_Tg
    ),
    color = "black",
    shape = 1,
    stroke = 0.4
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = "Crop",
    values = crop_colors
  ) +
  scale_fill_manual(
    name = "Crop",
    values = crop_colors
  ) +
  scale_size_continuous(
    name = "Food + feed use (Tg)",
    range = c(0.2, 1)
  ) +
  labs(
    x = "Year",
    y = expression(
      paste(
        "FFR (",
        P[feed],
        " ",
        P[food],
        ")"
      )
    )
  ) +
  theme_bw(
    base_family = "Arial",
    base_size = 7
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6.5)
  )


Figure3_K <- ggplot(
  K_ratio,
  aes(
    x = Year,
    color = Crop
  )
) +
  annotate(
    "rect",
    xmin = 2010,
    xmax = 2013,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey80",
    alpha = 0.25
  ) +
  geom_ribbon(
    aes(
      ymin = Rat_feed_food_mean - Rat_feed_food_sd,
      ymax = Rat_feed_food_mean + Rat_feed_food_sd,
      fill = Crop
    ),
    color = NA,
    alpha = 0.25
  ) +
  geom_line(
    aes(
      y = Rat_feed_food_mean
    ),
    linewidth = 0.6
  ) +
  geom_point(
    aes(
      y = Rat_feed_food_mean,
      size = Food_feed_use_Tg
    ),
    color = "black",
    shape = 1,
    stroke = 0.4
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_color_manual(
    name = "Crop",
    values = crop_colors
  ) +
  scale_fill_manual(
    name = "Crop",
    values = crop_colors
  ) +
  scale_size_continuous(
    name = "Food + feed use (Tg)",
    range = c(0.2, 1)
  ) +
  labs(
    x = "Year",
    y = expression(
      paste(
        "FFR (",
        K[feed],
        " ",
        K[food],
        ")"
      )
    )
  ) +
  theme_bw(
    base_family = "Arial",
    base_size = 7
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6.5)
  )


## Combine panels -----------------------------------------------------------

Figure3 <- plot_grid(
  Figure3_N,
  Figure3_P,
  Figure3_K,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold",
  label_size = 8
)


## Export source data -------------------------------------------------------

Figure3_data <- bind_rows(
  N_ratio %>%
    transmute(
      Year,
      Nutrient,
      Crop,
      FFR_mean = Rat_feed_food_mean,
      FFR_sd = Rat_feed_food_sd,
      FFR_q025 = Rat_feed_food_q025,
      FFR_q975 = Rat_feed_food_q975,
      Food_mean = N_food_mean,
      Feed_mean = N_feed_mean,
      Food_feed_use_Tg
    ),
  
  P_ratio %>%
    transmute(
      Year,
      Nutrient,
      Crop,
      FFR_mean = Rat_feed_food_mean,
      FFR_sd = Rat_feed_food_sd,
      FFR_q025 = Rat_feed_food_q025,
      FFR_q975 = Rat_feed_food_q975,
      Food_mean = P_food_mean,
      Feed_mean = P_feed_mean,
      Food_feed_use_Tg
    ),
  
  K_ratio %>%
    transmute(
      Year,
      Nutrient,
      Crop,
      FFR_mean = Rat_feed_food_mean,
      FFR_sd = Rat_feed_food_sd,
      FFR_q025 = Rat_feed_food_q025,
      FFR_q975 = Rat_feed_food_q975,
      Food_mean = K_food_mean,
      Feed_mean = K_feed_mean,
      Food_feed_use_Tg
    )
) %>%
  arrange(
    Nutrient,
    Crop,
    Year
  )


write_csv(
  Figure3_data,
  "results/Figure3_Crop_FFR_data.csv"
)


## Export publication figure -----------------------------------------------
#
# 180 mm corresponds approximately to a full two-column Nature figure.
# Vector PDF preserves lines and text without rasterization.

ggsave(
  filename = "plots/Figure3_Crop_FFR.pdf",
  plot = Figure3,
  device = cairo_pdf,
  width = 180,
  height = 65,
  units = "mm"
)



# Figure 4: Additive decomposition of fertilizer-use change --------------
#
# Figure 4 summarizes the additive decomposition of fertilizer-use change
# across the available comparison periods.
#
# The upper row shows contributions from the three decomposition
# components (dU, dF, and dA), while the lower row shows contributions
# from the four crops (maize, rice, soybean, and wheat).
#
# Food, feed, and other uses are shown as separate facets.
# Bars represent the mean contribution across Monte Carlo simulations
# and error bars represent ±1 SD.
#
# Contributions are expressed in Tg of elemental N, P, or K.


# Locate and load additive decomposition results

base_dir <- "results/Decomposition_Additive"

files_N <- list.files(
  base_dir,
  pattern = "^Dec_Ad_N_.*_Global_\\d{4}_\\d{4}\\.csv$",
  full.names = TRUE
)

files_P <- list.files(
  base_dir,
  pattern = "^Dec_Ad_P_.*_Global_\\d{4}_\\d{4}\\.csv$",
  full.names = TRUE
)

files_K <- list.files(
  base_dir,
  pattern = "^Dec_Ad_K_.*_Global_\\d{4}_\\d{4}\\.csv$",
  full.names = TRUE
)


# Extract fertilizer use and comparison period from each filename.

load_decomposition_file <- function(file) {
  
  fname <- basename(file)
  
  parts <- str_match(
    fname,
    "^Dec_Ad_[NPK]_(.*?)_Global_(\\d{4})_(\\d{4})\\.csv$"
  )
  
  read_csv(
    file,
    show_col_types = FALSE
  ) %>%
    mutate(
      Use_df = parts[2],
      year0 = as.numeric(parts[3]),
      year1 = as.numeric(parts[4]),
      file = fname
    )
}


Dec_N_All <- map_df(
  files_N,
  load_decomposition_file
)

Dec_P_All <- map_df(
  files_P,
  load_decomposition_file
)

Dec_K_All <- map_df(
  files_K,
  load_decomposition_file
)


# Period colors
#
# The long-term comparison is shown in grey. Shorter comparison periods
# retain distinct colors to facilitate comparisons within each panel.

period_colors <- c(
  "1961–1975" = "#8C510A",
  "1965–2015" = "grey40",
  "1975–1989" = "#D8B365",
  "1989–2005" = "#F9A825",
  "2006–2019" = "#2166AC",
  "2010–2019" = "#01665E"
)

period_order <- c(
  "1965–2015",
  "1961–1975",
  "1975–1989",
  "1989–2005",
  "2006–2019",
  "2010–2019"
)

use_order <- c(
  "Food",
  "Feed",
  "Others"
)


## Summarize decomposition components --------------------------------------

summarise_components <- function(df) {
  
  df %>%
    mutate(
      Period = sprintf(
        "%d–%d",
        year0,
        year1
      )
    ) %>%
    select(
      Use_df,
      Period,
      dU_global,
      dF_global,
      dA_global
    ) %>%
    pivot_longer(
      cols = c(
        dU_global,
        dF_global,
        dA_global
      ),
      names_to = "Component",
      values_to = "Contribution"
    ) %>%
    mutate(
      Component = recode(
        Component,
        dU_global = "ΔU",
        dF_global = "ΔF",
        dA_global = "ΔA"
      ),
      
      Component = factor(
        Component,
        levels = c(
          "ΔU",
          "ΔF",
          "ΔA"
        )
      ),
      
      Period = factor(
        Period,
        levels = period_order
      ),
      
      Use_df = factor(
        Use_df,
        levels = use_order
      )
    ) %>%
    group_by(
      Use_df,
      Period,
      Component
    ) %>%
    summarise(
      Mean = mean(
        Contribution,
        na.rm = TRUE
      ),
      
      SD = sd(
        Contribution,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      Mean_Tg = Mean / 1e6,
      SD_Tg = SD / 1e6
    )
}


## Summarize crop contributions --------------------------------------------

summarise_crops <- function(df) {
  
  df %>%
    mutate(
      Period = sprintf(
        "%d–%d",
        year0,
        year1
      )
    ) %>%
    select(
      Use_df,
      Period,
      Delta_maize,
      Delta_rice,
      Delta_soybean,
      Delta_wheat
    ) %>%
    pivot_longer(
      cols = starts_with("Delta_"),
      names_to = "Crop",
      values_to = "Contribution"
    ) %>%
    mutate(
      Crop = recode(
        Crop,
        Delta_maize = "Maize",
        Delta_rice = "Rice",
        Delta_soybean = "Soybean",
        Delta_wheat = "Wheat"
      ),
      
      Crop = factor(
        Crop,
        levels = c(
          "Maize",
          "Rice",
          "Soybean",
          "Wheat"
        )
      ),
      
      Period = factor(
        Period,
        levels = period_order
      ),
      
      Use_df = factor(
        Use_df,
        levels = use_order
      )
    ) %>%
    group_by(
      Use_df,
      Period,
      Crop
    ) %>%
    summarise(
      Mean = mean(
        Contribution,
        na.rm = TRUE
      ),
      
      SD = sd(
        Contribution,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      Mean_Tg = Mean / 1e6,
      SD_Tg = SD / 1e6
    )
}


## Prepare N, P, and K data -------------------------------------------------

N_components <- summarise_components(
  Dec_N_All
)

P_components <- summarise_components(
  Dec_P_All
)

K_components <- summarise_components(
  Dec_K_All
)


N_crops <- summarise_crops(
  Dec_N_All
)

P_crops <- summarise_crops(
  Dec_P_All
)

K_crops <- summarise_crops(
  Dec_K_All
)


## Plotting functions -------------------------------------------------------

plot_components <- function(
    data,
    nutrient,
    show_legend = FALSE
) {
  
  ggplot(
    data,
    aes(
      x = Component,
      y = Mean_Tg,
      fill = Period
    )
  ) +
    
    geom_hline(
      yintercept = 0,
      color = "grey50",
      linewidth = 0.3
    ) +
    
    geom_col(
      position = position_dodge(
        width = 0.75
      ),
      width = 0.68,
      color = "grey25",
      linewidth = 0.25
    ) +
    
    geom_errorbar(
      aes(
        ymin = Mean_Tg - SD_Tg,
        ymax = Mean_Tg + SD_Tg
      ),
      position = position_dodge(
        width = 0.75
      ),
      width = 0.20,
      linewidth = 0.4
    ) +
    
    facet_wrap(
      ~ Use_df,
      ncol = 1,
      strip.position = "left"
    ) +
    
    scale_fill_manual(
      values = period_colors,
      drop = FALSE,
      name = "Period"
    ) +
    
    labs(
      title = nutrient,
      x = NULL,
      y = "Contribution to fertilizer-use change (Tg)"
    ) +
    
    theme_bw(
      base_family = "Arial",
      base_size = 7
    ) +
    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      strip.background = element_blank(),
      strip.placement = "outside",
      
      strip.text.y.left = element_text(
        angle = 0,
        face = "bold",
        size = 7
      ),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 8
      ),
      
      axis.title = element_text(
        size = 7
      ),
      
      axis.text = element_text(
        size = 6.5
      ),
      
      legend.position = if (
        show_legend
      ) {
        "bottom"
      } else {
        "none"
      },
      
      legend.title = element_text(
        size = 7
      ),
      
      legend.text = element_text(
        size = 6.5
      )
    )
}


plot_crops <- function(
    data,
    show_legend = FALSE
) {
  
  ggplot(
    data,
    aes(
      x = Crop,
      y = Mean_Tg,
      fill = Period
    )
  ) +
    
    geom_hline(
      yintercept = 0,
      color = "grey50",
      linewidth = 0.3
    ) +
    
    geom_col(
      position = position_dodge(
        width = 0.75
      ),
      width = 0.68,
      color = "grey25",
      linewidth = 0.25
    ) +
    
    geom_errorbar(
      aes(
        ymin = Mean_Tg - SD_Tg,
        ymax = Mean_Tg + SD_Tg
      ),
      position = position_dodge(
        width = 0.75
      ),
      width = 0.20,
      linewidth = 0.4
    ) +
    
    facet_wrap(
      ~ Use_df,
      ncol = 1,
      strip.position = "left"
    ) +
    
    scale_fill_manual(
      values = period_colors,
      drop = FALSE,
      name = "Period"
    ) +
    
    labs(
      x = NULL,
      y = "Contribution to fertilizer-use change (Tg)"
    ) +
    
    theme_bw(
      base_family = "Arial",
      base_size = 7
    ) +
    
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      strip.background = element_blank(),
      strip.placement = "outside",
      
      strip.text.y.left = element_text(
        angle = 0,
        face = "bold",
        size = 7
      ),
      
      axis.title = element_text(
        size = 7
      ),
      
      axis.text = element_text(
        size = 6.5
      ),
      
      legend.position = if (
        show_legend
      ) {
        "bottom"
      } else {
        "none"
      },
      
      legend.title = element_text(
        size = 7
      ),
      
      legend.text = element_text(
        size = 6.5
      )
    )
}


## Create panels ------------------------------------------------------------

pN <- plot_components(
  N_components,
  nutrient = "N"
)

pP <- plot_components(
  P_components,
  nutrient = "P"
)

pK <- plot_components(
  K_components,
  nutrient = "K"
)


pN_crop <- plot_crops(
  N_crops
)

pP_crop <- plot_crops(
  P_crops
)

pK_crop <- plot_crops(
  K_crops
)


## Extract one common period legend ----------------------------------------

legend_plot <- plot_components(
  N_components,
  nutrient = "N",
  show_legend = TRUE
) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  )

Figure4_legend <- get_legend(
  legend_plot
)


## Combine Figure 4 ---------------------------------------------------------
#
# Panels a-c: contributions of the three decomposition components.
# Panels d-f: contributions attributable to individual crops.

Figure4_components <- plot_grid(
  pN,
  pP,
  pK,
  nrow = 1,
  labels = c(
    "a",
    "b",
    "c"
  ),
  label_fontface = "bold",
  label_size = 8
)


Figure4_crops <- plot_grid(
  pN_crop,
  pP_crop,
  pK_crop,
  nrow = 1,
  labels = c(
    "d",
    "e",
    "f"
  ),
  label_fontface = "bold",
  label_size = 8
)


Figure4_body <- plot_grid(
  Figure4_components,
  Figure4_crops,
  ncol = 1,
  rel_heights = c(
    1,
    1
  )
)


Figure4 <- plot_grid(
  Figure4_body,
  Figure4_legend,
  ncol = 1,
  rel_heights = c(
    1,
    0.07
  )
)


## Export source data -------------------------------------------------------
#
# The exported table contains exactly the mean and SD values represented
# in Figure 4, expressed in Tg.

Figure4_data <- bind_rows(
  
  N_components %>%
    transmute(
      Nutrient = "N",
      Section = "Decomposition component",
      Use = as.character(Use_df),
      Period = as.character(Period),
      Variable = as.character(Component),
      Mean_Tg,
      SD_Tg
    ),
  
  P_components %>%
    transmute(
      Nutrient = "P",
      Section = "Decomposition component",
      Use = as.character(Use_df),
      Period = as.character(Period),
      Variable = as.character(Component),
      Mean_Tg,
      SD_Tg
    ),
  
  K_components %>%
    transmute(
      Nutrient = "K",
      Section = "Decomposition component",
      Use = as.character(Use_df),
      Period = as.character(Period),
      Variable = as.character(Component),
      Mean_Tg,
      SD_Tg
    ),
  
  N_crops %>%
    transmute(
      Nutrient = "N",
      Section = "Crop contribution",
      Use = as.character(Use_df),
      Period = as.character(Period),
      Variable = as.character(Crop),
      Mean_Tg,
      SD_Tg
    ),
  
  P_crops %>%
    transmute(
      Nutrient = "P",
      Section = "Crop contribution",
      Use = as.character(Use_df),
      Period = as.character(Period),
      Variable = as.character(Crop),
      Mean_Tg,
      SD_Tg
    ),
  
  K_crops %>%
    transmute(
      Nutrient = "K",
      Section = "Crop contribution",
      Use = as.character(Use_df),
      Period = as.character(Period),
      Variable = as.character(Crop),
      Mean_Tg,
      SD_Tg
    )
) %>%
  arrange(
    Section,
    Nutrient,
    Use,
    Period,
    Variable
  )


write_csv(
  Figure4_data,
  "results/Figure4_Decomposition_data.csv"
)


## Export Figure 4 ----------------------------------------------------------

ggsave(
  filename = "plots/Figure4_Decomposition.pdf",
  plot = Figure4,
  device = cairo_pdf,
  width = 180,
  height = 190,
  units = "mm"
)


# Trade data preparation ---------------------------------------------------
#
# This script creates the bilateral virtual fertilizer-use datasets required
# for the trade figure.
#
# It combines:
#   - bilateral crop trade estimated using the Kastner approach;
#   - fertilizer use associated with crop production;
#   - exported fractions of production;
#   - bilateral import shares;
#   - final allocation of imported crops to food and feed.
#
# The resulting objects are:
#
#   Maize_bil_trade_uses_N
#   Wheat_bil_trade_uses_N
#   Rice_bil_trade_uses_N
#   Soybean_bil_trade_uses_N
#   Soybean_bil_oil_trade_uses_N
#   Soybean_bil_cake_trade_uses_N
#
# and the equivalent six objects for P and K.
#
# These objects can then be used directly by the Figure 5 trade-map script.


## Load bilateral trade estimates ------------------------------------------

Maize_bt <- read_csv(
  "results/Maize_Kastner_df_result.csv",
  show_col_types = FALSE
)

Wheat_bt <- read_csv(
  "results/Wheat_Kastner_df_result.csv",
  show_col_types = FALSE
)

Rice_bt <- read_csv(
  "results/Rice_Kastner_df_result.csv",
  show_col_types = FALSE
)

Soybean_bt <- read_csv(
  "results/Soybean_Kastner_df_result.csv",
  show_col_types = FALSE
)

Soybean_oil_bt <- read_csv(
  "results/Soybean_oil_Kastner_df_result.csv",
  show_col_types = FALSE
)

Soybean_cake_bt <- read_csv(
  "results/Soybean_cake_Kastner_df_result.csv",
  show_col_types = FALSE
)


## Country correspondence table --------------------------------------------

Countries_DB <- read_excel(
  "data/Countries_DB.xlsx"
) %>%
  select(
    ISO3 = 2,
    FAOStat_area_code = 3
  ) %>%
  distinct()


## Check that upstream fertilizer-use and crop-use objects exist -----------
#
# These objects are generated in the preceding fertilizer allocation and
# crop-use scripts. soybean_oil_exp and soybean_cake_exp are also required
# for processed soybean exports.

required_objects <- c(
  "N_wheat_uses",
  "N_maize_uses",
  "N_rice_uses",
  "N_soybean_uses",
  
  "P_wheat_uses",
  "P_maize_uses",
  "P_rice_uses",
  "P_soybean_uses",
  
  "K_wheat_uses",
  "K_maize_uses",
  "K_rice_uses",
  "K_soybean_uses",
  
  "Wheat_uses",
  "Maize_uses",
  "Rice_uses",
  "Soybean_uses",
  
  "soybean_oil_exp",
  "soybean_cake_exp"
)



## Prepare bilateral trade tables ------------------------------------------
#
# In the Kastner outputs, the first country column identifies the importer
# and the second identifies the exporter. ISO3 codes are added explicitly
# here so that the following calculations do not depend on column positions.

prepare_bilateral_trade <- function(data) {
  
  data %>%
    
    rename(
      FAOStat_area_code_exp = FAOStat_area_code_Exp,
      FAOStat_area_code_imp = FAOStat_area_code_Imp
    ) %>%
    
    left_join(
      Countries_DB %>%
        transmute(
          FAOStat_area_code_exp = FAOStat_area_code,
          ISO3_exp = ISO3
        ),
      by = "FAOStat_area_code_exp"
    ) %>%
    
    left_join(
      Countries_DB %>%
        transmute(
          FAOStat_area_code_imp = FAOStat_area_code,
          ISO3_imp = ISO3
        ),
      by = "FAOStat_area_code_imp"
    )
}

Wheat_bil_trade <- prepare_bilateral_trade(
  Wheat_bt
)

Maize_bil_trade <- prepare_bilateral_trade(
  Maize_bt
)

Rice_bil_trade <- prepare_bilateral_trade(
  Rice_bt
)

Soybean_bil_trade <- prepare_bilateral_trade(
  Soybean_bt
)

Soybean_oil_bil_trade <- prepare_bilateral_trade(
  Soybean_oil_bt
)

Soybean_cake_bil_trade <- prepare_bilateral_trade(
  Soybean_cake_bt
)


## Function for wheat, maize, rice, and unprocessed soybean -----------------
#
# Fertilizer use is first summed across food, feed, and other uses for each
# Monte Carlo simulation. The mean fertilizer use across simulations is then
# calculated for each country and year.
#
# Exported fertilizer use is estimated from the fraction of crop production
# exported (per_Exp). It is subsequently distributed among importing
# countries according to bilateral import shares (Perc_Imp).
#
# Finally, the fertilizer embodied in imports is assigned to food and feed
# according to its use in the importing country.
#
# For wheat, maize, and rice:
#   processing -> food
#
# For soybean:
#   19% of processing -> food
#   81% of processing -> feed

make_primary_trade_uses <- function(
    fertilizer_data,
    crop_uses,
    bilateral_trade,
    nutrient,
    processing_food_share = 1,
    processing_feed_share = 0
) {
  
  food_column <- paste0(
    nutrient,
    "_food"
  )
  
  feed_column <- paste0(
    nutrient,
    "_feed"
  )
  
  others_column <- paste0(
    nutrient,
    "_others"
  )
  
  output_food <- paste0(
    nutrient,
    "_exp_food"
  )
  
  output_feed <- paste0(
    nutrient,
    "_exp_feed"
  )
  
  
  # Mean total fertilizer use across Monte Carlo simulations.
  
  fertilizer_exports <- fertilizer_data %>%
    
    rowwise() %>%
    
    mutate(
      nutrient_total =
        sum(
          c_across(
            all_of(
              c(
                food_column,
                feed_column,
                others_column
              )
            )
          ),
          na.rm = TRUE
        ) / 1e6
    ) %>%
    
    ungroup() %>%
    
    group_by(
      Year,
      FAOStat_area_code
    ) %>%
    
    summarise(
      nutrient_total =
        mean(
          nutrient_total,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    
    left_join(
      crop_uses %>%
        select(
          Year,
          FAOStat_area_code,
          per_Exp
        ) %>%
        distinct(),
      by = c(
        "Year",
        "FAOStat_area_code"
      )
    ) %>%
    
    mutate(
      nutrient_total_exp =
        nutrient_total *
        per_Exp
    ) %>%
    
    # China aggregate code is excluded to avoid double counting.
    filter(
      FAOStat_area_code != 351
    )
  
  
  # Final-use fractions in importing countries.
  
  destination_uses <- crop_uses %>%
    
    # Apply the Belgium-Luxembourg harmonization used throughout the analysis.
    filter(
      Region != "Western Europe" |
        FAOStat_area_code != 255
    ) %>%
    
    select(
      FAOStat_area_code,
      Year,
      per_Food_d,
      per_Processing_d,
      per_Feed_d,
      per_Losses_d,
      per_Seed_d,
      per_Other_Uses_nf_d
    ) %>%
    
    distinct()
  
  
  # Allocate exported fertilizer use to bilateral destinations.
  
  fertilizer_exports %>%
    
    left_join(
      bilateral_trade,
      by = c(
        "Year",
        "FAOStat_area_code" =
          "FAOStat_area_code_exp"
      )
    ) %>%
    
    rename(
      FAOStat_area_code_exp =
        FAOStat_area_code,
      
      FAOStat_area_code =
        FAOStat_area_code_imp
    ) %>%
    
    left_join(
      destination_uses,
      by = c(
        "Year",
        "FAOStat_area_code"
      )
    ) %>%
    
    mutate(
      
      !!output_food :=
        nutrient_total_exp *
        Perc_Imp *
        (
          per_Food_d +
            per_Processing_d *
            processing_food_share
        ),
      
      !!output_feed :=
        nutrient_total_exp *
        Perc_Imp *
        (
          per_Feed_d +
            per_Processing_d *
            processing_feed_share
        )
    ) %>%
    
    select(
      Year,
      FAOStat_area_code_exp,
      FAOStat_area_code_imp =
        FAOStat_area_code,
      per_Exp,
      nutrient_total_exp,
      Perc_Imp,
      ISO3_exp,
      ISO3_imp,
      all_of(output_food),
      all_of(output_feed)
    )
}


## Function for processed soybean products ---------------------------------
#
# Only domestically processed soybean is considered here:
#
#   soybean oil  = 19% of processed soybean -> food
#   soybean cake = 81% of processed soybean -> feed
#
# P_exp_FB gives the exported proportion of the corresponding processed
# soybean product.

make_processed_soy_trade_uses <- function(
    fertilizer_data,
    product_exports,
    bilateral_trade,
    nutrient,
    processing_share,
    allocation
) {
  
  # Check that allocation is either "food" or "feed"
  allocation <- match.arg(
    allocation,
    choices = c("food", "feed")
  )
  
  
  # Define nutrient-specific column names
  food_column <- paste0(
    nutrient,
    "_food"
  )
  
  feed_column <- paste0(
    nutrient,
    "_feed"
  )
  
  others_column <- paste0(
    nutrient,
    "_others"
  )
  
  output_food <- paste0(
    nutrient,
    "_exp_food"
  )
  
  output_feed <- paste0(
    nutrient,
    "_exp_feed"
  )
  
  
  # Estimate fertilizer associated with exported processed soybean product
  soybean_exported_fertilizer <- fertilizer_data %>%
    
    # Sum food, feed and other fertilizer uses for each MC realization
    rowwise() %>%
    
    mutate(
      nutrient_total =
        sum(
          c_across(
            all_of(
              c(
                food_column,
                feed_column,
                others_column
              )
            )
          ),
          na.rm = TRUE
        ) / 1e6
    ) %>%
    
    ungroup() %>%
    
    # Average across Monte Carlo simulations
    group_by(
      Year,
      FAOStat_area_code
    ) %>%
    
    summarise(
      nutrient_total =
        mean(
          nutrient_total,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    
    # Add soybean export and processing fractions
    left_join(
      Soybean_uses %>%
        select(
          Year,
          FAOStat_area_code,
          per_Exp,
          per_Processing_d
        ) %>%
        distinct(),
      by = c(
        "Year",
        "FAOStat_area_code"
      )
    ) %>%
    
    # Add export fraction of soybean oil or cake
    left_join(
      product_exports,
      by = c(
        "Year",
        "FAOStat_area_code"
      )
    ) %>%
    
    # Fertilizer embodied in the exported processed product
    mutate(
      nutrient_total_exp =
        nutrient_total *
        P_exp_FB *
        per_Processing_d *
        (1 - per_Exp) *
        processing_share
    ) %>%
    
    # Remove aggregate China entry
    filter(
      FAOStat_area_code != 351
    )
  
  
  # Allocate exported fertilizer among bilateral destinations
  soybean_exported_fertilizer %>%
    
    left_join(
      bilateral_trade,
      by = c(
        "Year",
        "FAOStat_area_code" =
          "FAOStat_area_code_exp"
      )
    ) %>%
    
    rename(
      FAOStat_area_code_exp =
        FAOStat_area_code
    ) %>%
    
    # Allocate processed soybean products to their final use:
    # soybean oil  -> food
    # soybean cake -> feed
    mutate(
      
      !!output_food :=
        ifelse(
          .env$allocation == "food",
          nutrient_total_exp *
            Perc_Imp,
          0
        ),
      
      !!output_feed :=
        ifelse(
          .env$allocation == "feed",
          nutrient_total_exp *
            Perc_Imp,
          0
        ),
      
      # Keep the processed-product export fraction
      per_Exp = P_exp_FB
    ) %>%
    
    select(
      Year,
      FAOStat_area_code_exp,
      FAOStat_area_code_imp,
      per_Exp,
      nutrient_total_exp,
      Perc_Imp,
      ISO3_exp,
      ISO3_imp,
      all_of(output_food),
      all_of(output_feed)
    )
}

## Nitrogen bilateral virtual fertilizer trade -----------------------------

Wheat_bil_trade_uses_N <- make_primary_trade_uses(
  fertilizer_data = N_wheat_uses,
  crop_uses = Wheat_uses,
  bilateral_trade = Wheat_bil_trade,
  nutrient = "N"
)


Maize_bil_trade_uses_N <- make_primary_trade_uses(
  fertilizer_data = N_maize_uses,
  crop_uses = Maize_uses,
  bilateral_trade = Maize_bil_trade,
  nutrient = "N"
)


Rice_bil_trade_uses_N <- make_primary_trade_uses(
  fertilizer_data = N_rice_uses,
  crop_uses = Rice_uses,
  bilateral_trade = Rice_bil_trade,
  nutrient = "N"
)


Soybean_bil_trade_uses_N <- make_primary_trade_uses(
  fertilizer_data = N_soybean_uses,
  crop_uses = Soybean_uses,
  bilateral_trade = Soybean_bil_trade,
  nutrient = "N",
  
  processing_food_share = 0.19,
  processing_feed_share = 0.81
)


Soybean_bil_oil_trade_uses_N <- make_processed_soy_trade_uses(
  fertilizer_data = N_soybean_uses,
  product_exports = soybean_oil_exp,
  bilateral_trade = Soybean_oil_bil_trade,
  nutrient = "N",
  
  processing_share = 0.19,
  allocation = "food"
)

Soybean_bil_oil_trade_uses_N <- Soybean_bil_oil_trade_uses_N %>%
  mutate(
    N_exp_food = nutrient_total_exp * Perc_Imp,
    N_exp_feed = 0
  )

Soybean_bil_cake_trade_uses_N <- make_processed_soy_trade_uses(
  fertilizer_data = N_soybean_uses,
  product_exports = soybean_cake_exp,
  bilateral_trade = Soybean_cake_bil_trade,
  nutrient = "N",
  
  processing_share = 0.81,
  allocation = "feed"
)

Soybean_bil_cake_trade_uses_N <- Soybean_bil_cake_trade_uses_N %>%
  mutate(
    N_exp_food = 0,
    N_exp_feed = nutrient_total_exp * Perc_Imp
  )

## Phosphorus bilateral virtual fertilizer trade ---------------------------

Wheat_bil_trade_uses_P <- make_primary_trade_uses(
  fertilizer_data = P_wheat_uses,
  crop_uses = Wheat_uses,
  bilateral_trade = Wheat_bil_trade,
  nutrient = "P"
)


Maize_bil_trade_uses_P <- make_primary_trade_uses(
  fertilizer_data = P_maize_uses,
  crop_uses = Maize_uses,
  bilateral_trade = Maize_bil_trade,
  nutrient = "P"
)


Rice_bil_trade_uses_P <- make_primary_trade_uses(
  fertilizer_data = P_rice_uses,
  crop_uses = Rice_uses,
  bilateral_trade = Rice_bil_trade,
  nutrient = "P"
)


Soybean_bil_trade_uses_P <- make_primary_trade_uses(
  fertilizer_data = P_soybean_uses,
  crop_uses = Soybean_uses,
  bilateral_trade = Soybean_bil_trade,
  nutrient = "P",
  
  processing_food_share = 0.19,
  processing_feed_share = 0.81
)


Soybean_bil_oil_trade_uses_P <- make_processed_soy_trade_uses(
  fertilizer_data = P_soybean_uses,
  product_exports = soybean_oil_exp,
  bilateral_trade = Soybean_oil_bil_trade,
  nutrient = "P",
  
  processing_share = 0.19,
  allocation = "food"
)

Soybean_bil_oil_trade_uses_P <- Soybean_bil_oil_trade_uses_P %>%
  mutate(
    P_exp_food = nutrient_total_exp * Perc_Imp,
    P_exp_feed = 0
  )

Soybean_bil_cake_trade_uses_P <- make_processed_soy_trade_uses(
  fertilizer_data = P_soybean_uses,
  product_exports = soybean_cake_exp,
  bilateral_trade = Soybean_cake_bil_trade,
  nutrient = "P",
  
  processing_share = 0.81,
  allocation = "feed"
)

Soybean_bil_cake_trade_uses_P <- Soybean_bil_cake_trade_uses_P %>%
  mutate(
    P_exp_food = 0,
    P_exp_feed = nutrient_total_exp * Perc_Imp
  )

## Potassium bilateral virtual fertilizer trade ----------------------------

Wheat_bil_trade_uses_K <- make_primary_trade_uses(
  fertilizer_data = K_wheat_uses,
  crop_uses = Wheat_uses,
  bilateral_trade = Wheat_bil_trade,
  nutrient = "K"
)


Maize_bil_trade_uses_K <- make_primary_trade_uses(
  fertilizer_data = K_maize_uses,
  crop_uses = Maize_uses,
  bilateral_trade = Maize_bil_trade,
  nutrient = "K"
)


Rice_bil_trade_uses_K <- make_primary_trade_uses(
  fertilizer_data = K_rice_uses,
  crop_uses = Rice_uses,
  bilateral_trade = Rice_bil_trade,
  nutrient = "K"
)


Soybean_bil_trade_uses_K <- make_primary_trade_uses(
  fertilizer_data = K_soybean_uses,
  crop_uses = Soybean_uses,
  bilateral_trade = Soybean_bil_trade,
  nutrient = "K",
  
  processing_food_share = 0.19,
  processing_feed_share = 0.81
)


Soybean_bil_oil_trade_uses_K <- make_processed_soy_trade_uses(
  fertilizer_data = K_soybean_uses,
  product_exports = soybean_oil_exp,
  bilateral_trade = Soybean_oil_bil_trade,
  nutrient = "K",
  
  processing_share = 0.19,
  allocation = "food"
)

Soybean_bil_oil_trade_uses_K <- Soybean_bil_oil_trade_uses_K %>%
  mutate(
    K_exp_food = nutrient_total_exp * Perc_Imp,
    K_exp_feed = 0
  )

Soybean_bil_cake_trade_uses_K <- make_processed_soy_trade_uses(
  fertilizer_data = K_soybean_uses,
  product_exports = soybean_cake_exp,
  bilateral_trade = Soybean_cake_bil_trade,
  nutrient = "K",
  
  processing_share = 0.81,
  allocation = "feed"
)

Soybean_bil_cake_trade_uses_K <- Soybean_bil_cake_trade_uses_K %>%
  mutate(
    K_exp_food = 0,
    K_exp_feed = nutrient_total_exp * Perc_Imp
  )

## Combine into one tidy bilateral-trade database --------------------------
#
# This table is useful as a reproducible source-data file, while the
# individual objects above are retained for compatibility with the
# Figure 5 plotting code.

Trade_bilateral_N <- bind_rows(
  
  Wheat_bil_trade_uses_N %>%
    mutate(
      Nutrient = "N",
      Product = "Wheat"
    ),
  
  Maize_bil_trade_uses_N %>%
    mutate(
      Nutrient = "N",
      Product = "Maize"
    ),
  
  Rice_bil_trade_uses_N %>%
    mutate(
      Nutrient = "N",
      Product = "Rice"
    ),
  
  Soybean_bil_trade_uses_N %>%
    mutate(
      Nutrient = "N",
      Product = "Soybean"
    ),
  
  Soybean_bil_oil_trade_uses_N %>%
    mutate(
      Nutrient = "N",
      Product = "Soybean oil"
    ),
  
  Soybean_bil_cake_trade_uses_N %>%
    mutate(
      Nutrient = "N",
      Product = "Soybean cake"
    )
)


Trade_bilateral_P <- bind_rows(
  
  Wheat_bil_trade_uses_P %>%
    mutate(
      Nutrient = "P",
      Product = "Wheat"
    ),
  
  Maize_bil_trade_uses_P %>%
    mutate(
      Nutrient = "P",
      Product = "Maize"
    ),
  
  Rice_bil_trade_uses_P %>%
    mutate(
      Nutrient = "P",
      Product = "Rice"
    ),
  
  Soybean_bil_trade_uses_P %>%
    mutate(
      Nutrient = "P",
      Product = "Soybean"
    ),
  
  Soybean_bil_oil_trade_uses_P %>%
    mutate(
      Nutrient = "P",
      Product = "Soybean oil"
    ),
  
  Soybean_bil_cake_trade_uses_P %>%
    mutate(
      Nutrient = "P",
      Product = "Soybean cake"
    )
)


Trade_bilateral_K <- bind_rows(
  
  Wheat_bil_trade_uses_K %>%
    mutate(
      Nutrient = "K",
      Product = "Wheat"
    ),
  
  Maize_bil_trade_uses_K %>%
    mutate(
      Nutrient = "K",
      Product = "Maize"
    ),
  
  Rice_bil_trade_uses_K %>%
    mutate(
      Nutrient = "K",
      Product = "Rice"
    ),
  
  Soybean_bil_trade_uses_K %>%
    mutate(
      Nutrient = "K",
      Product = "Soybean"
    ),
  
  Soybean_bil_oil_trade_uses_K %>%
    mutate(
      Nutrient = "K",
      Product = "Soybean oil"
    ),
  
  Soybean_bil_cake_trade_uses_K %>%
    mutate(
      Nutrient = "K",
      Product = "Soybean cake"
    )
)


Trade_bilateral_all <- bind_rows(
  Trade_bilateral_N,
  Trade_bilateral_P,
  Trade_bilateral_K
) %>%
  arrange(
    Nutrient,
    Product,
    Year,
    ISO3_exp,
    ISO3_imp
  )


## Export prepared trade data ----------------------------------------------

dir.create(
  "results/Trade",
  showWarnings = FALSE,
  recursive = TRUE
)


write_csv(
  Trade_bilateral_N,
  "results/Trade/Bilateral_virtual_fertilizer_N.csv"
)

write_csv(
  Trade_bilateral_P,
  "results/Trade/Bilateral_virtual_fertilizer_P.csv"
)

write_csv(
  Trade_bilateral_K,
  "results/Trade/Bilateral_virtual_fertilizer_K.csv"
)

write_csv(
  Trade_bilateral_all,
  "results/Trade/Bilateral_virtual_fertilizer_all.csv"
)


# Figure 5: Average bilateral flows over 2001-2019 ------------------------
# Load prepared bilateral virtual fertilizer trade data

Bilateral_virtual_fertilizer_N <- read_csv(
  "results/Trade/Bilateral_virtual_fertilizer_N.csv"
)

Bilateral_virtual_fertilizer_P <- read_csv(
  "results/Trade/Bilateral_virtual_fertilizer_P.csv"
)

Bilateral_virtual_fertilizer_K <- read_csv(
  "results/Trade/Bilateral_virtual_fertilizer_K.csv"
)

Bilateral_virtual_fertilizer_all <- read_csv(
  "results/Trade/Bilateral_virtual_fertilizer_all.csv"
)

Countries_DB_locations <- read_excel(
  "data/Countries_DB_locations.xlsx"
) %>%
  select(
    2,
    11,
    12,
    13
  ) %>%
  rename(
    ISO3 = 1
  ) %>%
  drop_na()

## Helper ------------------------------------------------------------------

summarise_trade_product <- function(
    data,
    food_variable,
    feed_variable
) {
  
  data %>%
    ungroup() %>%
    
    filter(
      Year > 2000
    ) %>%
    
    distinct() %>%
    
    # Keep only variables relevant to this calculation before removing NAs.
    select(
      ISO3_exp,
      ISO3_imp,
      all_of(food_variable),
      all_of(feed_variable)
    ) %>%
    
    drop_na() %>%
    
    group_by(
      ISO3_exp,
      ISO3_imp
    ) %>%
    
    summarise(
      food = mean(
        .data[[food_variable]],
        na.rm = TRUE
      ),
      
      feed = mean(
        .data[[feed_variable]],
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    
    filter(
      food > 0 |
        feed > 0
    )
}


# Nitrogen

Maize_trade_N <- summarise_trade_product(
  Bilateral_virtual_fertilizer_N %>% filter(Product == "Maize"),
  "N_exp_food",
  "N_exp_feed"
)

Wheat_trade_N <- summarise_trade_product(
  Bilateral_virtual_fertilizer_N %>% filter(Product == "Wheat"),
  "N_exp_food",
  "N_exp_feed"
)

Rice_trade_N <- summarise_trade_product(
  Bilateral_virtual_fertilizer_N %>% filter(Product == "Rice"),
  "N_exp_food",
  "N_exp_feed"
)

Soybean_trade_N <- summarise_trade_product(
  Bilateral_virtual_fertilizer_N %>% filter(Product == "Soybean"),
  "N_exp_food",
  "N_exp_feed"
)

Soybean_oil_trade_N <- summarise_trade_product(
  Bilateral_virtual_fertilizer_N %>% filter(Product == "Soybean oil"),
  "N_exp_food",
  "N_exp_feed"
)

Soybean_cake_trade_N <- summarise_trade_product(
  Bilateral_virtual_fertilizer_N %>% filter(Product == "Soybean cake"),
  "N_exp_food",
  "N_exp_feed"
)


Map_N_bil_trade_uses <- bind_rows(
  Maize_trade_N,
  Wheat_trade_N,
  Rice_trade_N,
  Soybean_trade_N,
  Soybean_oil_trade_N,
  Soybean_cake_trade_N
) %>%
  
  group_by(
    ISO3_exp,
    ISO3_imp
  ) %>%
  
  summarise(
    N_exp_food = sum(
      food,
      na.rm = TRUE
    ),
    
    N_exp_feed = sum(
      feed,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Phosphorus

Maize_trade_P <- summarise_trade_product(
  Bilateral_virtual_fertilizer_P %>% filter(Product == "Maize"),
  "P_exp_food",
  "P_exp_feed"
)

Wheat_trade_P <- summarise_trade_product(
  Bilateral_virtual_fertilizer_P %>% filter(Product == "Wheat"),
  "P_exp_food",
  "P_exp_feed"
)

Rice_trade_P <- summarise_trade_product(
  Bilateral_virtual_fertilizer_P %>% filter(Product == "Rice"),
  "P_exp_food",
  "P_exp_feed"
)

Soybean_trade_P <- summarise_trade_product(
  Bilateral_virtual_fertilizer_P %>% filter(Product == "Soybean"),
  "P_exp_food",
  "P_exp_feed"
)

Soybean_oil_trade_P <- summarise_trade_product(
  Bilateral_virtual_fertilizer_P %>% filter(Product == "Soybean oil"),
  "P_exp_food",
  "P_exp_feed"
)

Soybean_cake_trade_P <- summarise_trade_product(
  Bilateral_virtual_fertilizer_P %>% filter(Product == "Soybean cake"),
  "P_exp_food",
  "P_exp_feed"
)


Map_P_bil_trade_uses <- bind_rows(
  Maize_trade_P,
  Wheat_trade_P,
  Rice_trade_P,
  Soybean_trade_P,
  Soybean_oil_trade_P,
  Soybean_cake_trade_P
) %>%
  
  group_by(
    ISO3_exp,
    ISO3_imp
  ) %>%
  
  summarise(
    P_exp_food = sum(
      food,
      na.rm = TRUE
    ),
    
    P_exp_feed = sum(
      feed,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Potassium

Maize_trade_K <- summarise_trade_product(
  Bilateral_virtual_fertilizer_K %>% filter(Product == "Maize"),
  "K_exp_food",
  "K_exp_feed"
)

Wheat_trade_K <- summarise_trade_product(
  Bilateral_virtual_fertilizer_K %>% filter(Product == "Wheat"),
  "K_exp_food",
  "K_exp_feed"
)

Rice_trade_K <- summarise_trade_product(
  Bilateral_virtual_fertilizer_K %>% filter(Product == "Rice"),
  "K_exp_food",
  "K_exp_feed"
)

Soybean_trade_K <- summarise_trade_product(
  Bilateral_virtual_fertilizer_K %>% filter(Product == "Soybean"),
  "K_exp_food",
  "K_exp_feed"
)

Soybean_oil_trade_K <- summarise_trade_product(
  Bilateral_virtual_fertilizer_K %>% filter(Product == "Soybean oil"),
  "K_exp_food",
  "K_exp_feed"
)

Soybean_cake_trade_K <- summarise_trade_product(
  Bilateral_virtual_fertilizer_K %>% filter(Product == "Soybean cake"),
  "K_exp_food",
  "K_exp_feed"
)


Map_K_bil_trade_uses <- bind_rows(
  Maize_trade_K,
  Wheat_trade_K,
  Rice_trade_K,
  Soybean_trade_K,
  Soybean_oil_trade_K,
  Soybean_cake_trade_K
) %>%
  
  group_by(
    ISO3_exp,
    ISO3_imp
  ) %>%
  
  summarise(
    K_exp_food = sum(
      food,
      na.rm = TRUE
    ),
    
    K_exp_feed = sum(
      feed,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# Combine nutrients

Map_NPK_bil_trade_uses <- full_join(
  Map_N_bil_trade_uses,
  Map_P_bil_trade_uses,
  by = c(
    "ISO3_exp",
    "ISO3_imp"
  )
) %>%
  
  full_join(
    Map_K_bil_trade_uses,
    by = c(
      "ISO3_exp",
      "ISO3_imp"
    )
  ) %>%
  
  mutate(
    across(
      c(
        N_exp_food,
        N_exp_feed,
        P_exp_food,
        P_exp_feed,
        K_exp_food,
        K_exp_feed
      ),
      ~ replace_na(.x, 0)
    ),
    
    NPK_exp_food =
      N_exp_food +
      P_exp_food +
      K_exp_food,
    
    NPK_exp_feed =
      N_exp_feed +
      P_exp_feed +
      K_exp_feed
  )

## Country trade balances ---------------------------------------------------

NPK_exports <- Map_NPK_bil_trade_uses %>%
  
  group_by(
    ISO3_exp
  ) %>%
  
  summarise(
    NPK_exp_food = sum(
      NPK_exp_food,
      na.rm = TRUE
    ),
    
    NPK_exp_feed = sum(
      NPK_exp_feed,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  rename(
    ISO3 = ISO3_exp
  )


NPK_imports <- Map_NPK_bil_trade_uses %>%
  
  group_by(
    ISO3_imp
  ) %>%
  
  summarise(
    NPK_imp_food = sum(
      NPK_exp_food,
      na.rm = TRUE
    ),
    
    NPK_imp_feed = sum(
      NPK_exp_feed,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  rename(
    ISO3 = ISO3_imp
  )


NPK_trade_balance <- full_join(
  NPK_exports,
  NPK_imports,
  by = "ISO3"
) %>%
  
  replace_na(
    list(
      NPK_exp_food = 0,
      NPK_exp_feed = 0,
      NPK_imp_food = 0,
      NPK_imp_feed = 0
    )
  ) %>%
  
  mutate(
    Balance_food =
      NPK_exp_food -
      NPK_imp_food,
    
    Balance_feed =
      NPK_exp_feed -
      NPK_imp_feed,
    
    Trade_class = case_when(
      
      Balance_food >= 0 &
        Balance_feed >= 0 ~
        "Food & feed exporter",
      
      Balance_food < 0 &
        Balance_feed >= 0 ~
        "Food importer / feed exporter",
      
      Balance_food < 0 &
        Balance_feed < 0 ~
        "Food & feed importer",
      
      Balance_food >= 0 &
        Balance_feed < 0 ~
        "Food exporter / feed importer"
    )
  ) %>%
  
  left_join(
    Countries_DB_locations %>%
      select(
        ISO3,
        map_names
      ),
    by = "ISO3"
  )


# Bilateral flows for map
#
# Retain bilateral flows > 5 for map visualization.

Food_trade_flows <- Map_NPK_bil_trade_uses %>%
  
  select(
    ISO3_exp,
    ISO3_imp,
    NPK_exp_food
  ) %>%
  
  left_join(
    Countries_DB_locations %>%
      transmute(
        ISO3_exp = ISO3,
        origin_lon = long,
        origin_lat = lat
      ),
    by = "ISO3_exp"
  ) %>%
  
  left_join(
    Countries_DB_locations %>%
      transmute(
        ISO3_imp = ISO3,
        dest_lon = long,
        dest_lat = lat
      ),
    by = "ISO3_imp"
  ) %>%
  
  filter(
    NPK_exp_food > 5
  ) %>%
  
  drop_na(
    origin_lon,
    origin_lat,
    dest_lon,
    dest_lat
  ) %>%
  
  filter(
    !(
      origin_lon == dest_lon &
        origin_lat == dest_lat
    )
  )


Feed_trade_flows <- Map_NPK_bil_trade_uses %>%
  
  select(
    ISO3_exp,
    ISO3_imp,
    NPK_exp_feed
  ) %>%
  
  left_join(
    Countries_DB_locations %>%
      transmute(
        ISO3_exp = ISO3,
        origin_lon = long,
        origin_lat = lat
      ),
    by = "ISO3_exp"
  ) %>%
  
  left_join(
    Countries_DB_locations %>%
      transmute(
        ISO3_imp = ISO3,
        dest_lon = long,
        dest_lat = lat
      ),
    by = "ISO3_imp"
  ) %>%
  
  filter(
    NPK_exp_feed > 5
  ) %>%
  
  drop_na(
    origin_lon,
    origin_lat,
    dest_lon,
    dest_lat
  ) %>%
  
  filter(
    !(
      origin_lon == dest_lon &
        origin_lat == dest_lat
    )
  )


# World map

world_map <- map_data(
  "world"
)

world_map_joined <- world_map %>%
  
  left_join(
    NPK_trade_balance,
    by = c(
      "region" = "map_names"
    )
  )


# Colors

trade_class_colors <- c(
  
  "Food & feed exporter" =
    "darkblue",
  
  "Food importer / feed exporter" =
    "lightblue",
  
  "Food & feed importer" =
    "pink",
  
  "Food exporter / feed importer" =
    "red"
)


# Balance inset

highlight_ISO3 <- c(
  "USA",
  "BRA",
  "CHN",
  "IND",
  "RUS",
  "DEU",
  "ARG",
  "NGA",
  "KOR",
  "MEX",
  "VNM"
)


NPK_trade_balance <- NPK_trade_balance %>%
  
  mutate(
    Highlight = ifelse(
      ISO3 %in% highlight_ISO3,
      "Highlighted",
      "Other"
    )
  )


labeled_countries <- NPK_trade_balance %>%
  
  filter(
    Highlight == "Highlighted"
  )


Figure5_inset <- ggplot(
  NPK_trade_balance,
  aes(
    x = Balance_food,
    y = Balance_feed
  )
) +
  
  annotate(
    "rect",
    xmin = 0,
    xmax = Inf,
    ymin = 0,
    ymax = Inf,
    fill = "darkblue",
    alpha = 0.4
  ) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = 0,
    ymin = 0,
    ymax = Inf,
    fill = "lightblue",
    alpha = 0.4
  ) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = 0,
    ymin = -Inf,
    ymax = 0,
    fill = "pink",
    alpha = 0.4
  ) +
  
  annotate(
    "rect",
    xmin = 0,
    xmax = Inf,
    ymin = -Inf,
    ymax = 0,
    fill = "red",
    alpha = 0.4
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.35
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.35
  ) +
  
  geom_point(
    aes(
      color = Highlight
    ),
    size = 1.2
  ) +
  
  ggrepel::geom_text_repel(
    data = labeled_countries,
    aes(
      label = ISO3
    ),
    size = 2.2,
    family = "Arial",
    min.segment.length = 0,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(
    values = c(
      "Other" = "grey60",
      "Highlighted" = "black"
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(-2000, 2000, by = 500)
  ) +
  
  coord_cartesian(
    ylim = c(
      min(NPK_trade_balance$Balance_feed, na.rm = TRUE),
      2000
    )
  ) +
  
  labs(
    x = "Virtual NPK balance – food (Gg)",
    y = "Virtual NPK balance – feed (Gg)"
  ) +
  
  theme_bw(
    base_family = "Arial",
    base_size = 7
  ) +
  
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6)
  )


## Main trade map -----------------------------------------------------------

flow_limits <- range(
  c(
    Food_trade_flows$NPK_exp_food,
    Feed_trade_flows$NPK_exp_feed
  ),
  na.rm = TRUE
)


Figure5_map <- ggplot() +
  
  geom_polygon(
    data = world_map_joined,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = Trade_class
    ),
    color = "grey20",
    alpha = 0.4,
    linewidth = 0.08
  ) +
  
  scale_fill_manual(
    values = trade_class_colors,
    na.value = "grey92",
    name = "Trade balance"
  ) +
  
  geom_curve(
    data = Food_trade_flows,
    aes(
      x = origin_lon,
      y = origin_lat,
      xend = dest_lon,
      yend = dest_lat,
      linewidth = NPK_exp_food,
      alpha = NPK_exp_food
    ),
    curvature = 0.20,
    color = "#FF9E00",
    arrow = arrow(
      length = unit(
        0.10,
        "cm"
      )
    )
  ) +
  
  geom_curve(
    data = Feed_trade_flows,
    aes(
      x = origin_lon,
      y = origin_lat,
      xend = dest_lon,
      yend = dest_lat,
      linewidth = NPK_exp_feed,
      alpha = NPK_exp_feed
    ),
    curvature = 0.35,
    color = "#458472",
    arrow = arrow(
      length = unit(
        0.10,
        "cm"
      )
    )
  ) +
  
  scale_linewidth_continuous(
    range = c(
      0.15,
      1.5
    ),
    limits = flow_limits,
    name = "Mean bilateral NPK flow (Gg)"
  ) +
  
  scale_alpha_continuous(
    range = c(
      0.15,
      0.75
    ),
    limits = flow_limits,
    guide = "none"
  ) +
  
  coord_quickmap() +
  
  theme_void(
    base_family = "Arial",
    base_size = 7
  ) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(
      size = 7
    ),
    legend.text = element_text(
      size = 6.5
    )
  )


## Combine main map and inset -----------------------------------------------

Figure5 <- cowplot::ggdraw() +
  
  cowplot::draw_plot(
    Figure5_map,
    x = 0,
    y = 0,
    width = 1,
    height = 1
  ) +
  
  cowplot::draw_plot(
    Figure5_inset,
    x = 0.01,
    y = 0.10,
    width = 0.28,
    height = 0.32
  ) +
  
  cowplot::draw_label(
    "a",
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    fontfamily = "Arial",
    fontface = "bold",
    size = 8
  ) +
  
  cowplot::draw_label(
    "b",
    x = 0.66,
    y = 0.47,
    hjust = 0,
    vjust = 1,
    fontfamily = "Arial",
    fontface = "bold",
    size = 8
  )


## Source data --------------------------------------------------------------

Figure5_country_data <- NPK_trade_balance %>%
  
  select(
    ISO3,
    NPK_exp_food,
    NPK_imp_food,
    Balance_food,
    NPK_exp_feed,
    NPK_imp_feed,
    Balance_feed,
    Trade_class
  ) %>%
  
  arrange(
    ISO3
  )


Figure5_flow_data <- Map_NPK_bil_trade_uses %>%
  
  transmute(
    Exporter = ISO3_exp,
    Importer = ISO3_imp,
    
    N_food = N_exp_food,
    P_food = P_exp_food,
    K_food = K_exp_food,
    NPK_food = NPK_exp_food,
    
    N_feed = N_exp_feed,
    P_feed = P_exp_feed,
    K_feed = K_exp_feed,
    NPK_feed = NPK_exp_feed
  ) %>%
  
  arrange(
    Exporter,
    Importer
  )


write_csv(
  Figure5_country_data,
  "results/Figure5_Trade_country_balances.csv"
)

write_csv(
  Figure5_flow_data,
  "results/Figure5_Trade_bilateral_flows.csv"
)


## Nature-format export -----------------------------------------------------

ggsave(
  filename = "plots/Figure5_Virtual_NPK_Trade.pdf",
  plot = Figure5,
  device = cairo_pdf,
  width = 180,
  height = 115,
  units = "mm"
)


# Supplementary trade chord diagrams --------------------------------------
#
# Uses the corrected bilateral trade datasets:
#
#   Maize_bil_trade_uses_N/P/K
#   Wheat_bil_trade_uses_N/P/K
#   Rice_bil_trade_uses_N/P/K
#   Soybean_bil_trade_uses_N/P/K
#   Soybean_bil_oil_trade_uses_N/P/K
#   Soybean_bil_cake_trade_uses_N/P/K
#
# Chord diagrams are produced for 1965, 1990 and 2015.
#
# CHN, USA, BRA and IND are shown individually.
# All other countries are aggregated into FAO regions.


library(dplyr)
library(tidyr)
library(circlize)


## Region information -------------------------------------------------------

Region <- readr::read_csv(
  "data/FAOSTAT_regions.csv"
) %>%
  select(
    Region = 2,
    ISO3 = 7
  ) %>%
  distinct()

# Assign Taiwan to Eastern Asia.
Region <- Region %>%
  mutate(
    Region = ifelse(
      ISO3 == "TWN",
      "Eastern Asia",
      Region
    )
  )


## Region colours -----------------------------------------------------------

region_colors <- c(
  "Western Asia" = "#51CA63",
  "Southern Asia" = "#51CA63",
  "South-eastern Asia" = "#51CA63",
  "Eastern Asia" = "#51CA63",
  "Central Asia" = "#51CA63",
  
  "Southern Europe" = "#107FE0",
  "Western Europe" = "#107FE0",
  "Eastern Europe" = "#107FE0",
  "Northern Europe" = "#107FE0",
  
  "Northern Africa" = "#BF9001",
  "Middle Africa" = "#BF9001",
  "Eastern Africa" = "#BF9001",
  "Western Africa" = "#BF9001",
  "Southern Africa" = "#BF9001",
  
  "Polynesia" = "#ADADAD",
  "Australia and New Zealand" = "#ADADAD",
  "Melanesia" = "#ADADAD",
  "Micronesia" = "#ADADAD",
  
  "Caribbean" = "#FFBF77",
  "South America" = "#FFBF77",
  "Central America" = "#FFBF77",
  
  "Northern America" = "#DA0000"
)


## Countries retained individually -----------------------------------------

individual_countries <- c(
  "CHN",
  "USA",
  "BRA",
  "IND"
)

# Load corrected bilateral virtual fertilizer datasets
Bilateral_virtual_fertilizer_N <- read_csv(
  "results/trade/Bilateral_virtual_fertilizer_N.csv"
)

Bilateral_virtual_fertilizer_P <- read_csv(
  "results/trade/Bilateral_virtual_fertilizer_P.csv"
)

Bilateral_virtual_fertilizer_K <- read_csv(
  "results/trade/Bilateral_virtual_fertilizer_K.csv"
)


## Function to generate chord diagrams -------------------------------------

make_trade_chord <- function(
    nutrient,
    year_sel
) {
  
  # Define nutrient-specific column names
  food_var <- paste0(
    nutrient,
    "_exp_food"
  )
  
  feed_var <- paste0(
    nutrient,
    "_exp_feed"
  )
  
  
  # Select corrected bilateral dataset ------------------------------------
  
  if (nutrient == "N") {
    
    trade_data <- Bilateral_virtual_fertilizer_N
    
  } else if (nutrient == "P") {
    
    trade_data <- Bilateral_virtual_fertilizer_P
    
  } else if (nutrient == "K") {
    
    trade_data <- Bilateral_virtual_fertilizer_K
    
  } else {
    
    stop(
      "nutrient must be 'N', 'P', or 'K'"
    )
  }
  
  
  # Bilateral fertilizer flows for selected year --------------------------
  
  Year_bil_trade_uses <- trade_data %>%
    
    filter(
      Year == year_sel
    ) %>%
    
    select(
      ISO3_exp,
      ISO3_imp,
      all_of(food_var),
      all_of(feed_var)
    ) %>%
    
    # Only exporter/importer identifiers are required to be non-missing.
    # Missing fertilizer values can still be handled by sum(..., na.rm = TRUE).
    drop_na(
      ISO3_exp,
      ISO3_imp
    ) %>%
    
    group_by(
      ISO3_exp,
      ISO3_imp
    ) %>%
    
    summarise(
      
      food = sum(
        .data[[food_var]],
        na.rm = TRUE
      ),
      
      feed = sum(
        .data[[feed_var]],
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    
    filter(
      food > 0 |
        feed > 0
    )
  
  # FOOD ------------------------------------------------------------------
  
  Year_bil_trade_uses_food <- Year_bil_trade_uses %>%
    
    select(
      ISO3_exp,
      ISO3_imp,
      food
    ) %>%
    
    filter(
      food > 0
    ) %>%
    
    left_join(
      Region %>%
        rename(
          Region_imp = Region
        ),
      by = c(
        "ISO3_imp" = "ISO3"
      )
    ) %>%
    
    left_join(
      Region %>%
        rename(
          Region_exp = Region
        ),
      by = c(
        "ISO3_exp" = "ISO3"
      )
    ) %>%
    
    mutate(
      
      ISO3_imp_group =
        ifelse(
          ISO3_imp %in% individual_countries,
          ISO3_imp,
          Region_imp
        ),
      
      ISO3_exp_group =
        ifelse(
          ISO3_exp %in% individual_countries,
          ISO3_exp,
          Region_exp
        )
    ) %>%
    
    group_by(
      ISO3_exp_group,
      ISO3_imp_group
    ) %>%
    
    summarise(
      food = sum(
        food,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    
    drop_na()
  
  
  # FEED ------------------------------------------------------------------
  
  Year_bil_trade_uses_feed <- Year_bil_trade_uses %>%
    
    select(
      ISO3_exp,
      ISO3_imp,
      feed
    ) %>%
    
    filter(
      feed > 0
    ) %>%
    
    left_join(
      Region %>%
        rename(
          Region_imp = Region
        ),
      by = c(
        "ISO3_imp" = "ISO3"
      )
    ) %>%
    
    left_join(
      Region %>%
        rename(
          Region_exp = Region
        ),
      by = c(
        "ISO3_exp" = "ISO3"
      )
    ) %>%
    
    mutate(
      
      ISO3_imp_group =
        ifelse(
          ISO3_imp %in% individual_countries,
          ISO3_imp,
          Region_imp
        ),
      
      ISO3_exp_group =
        ifelse(
          ISO3_exp %in% individual_countries,
          ISO3_exp,
          Region_exp
        )
    ) %>%
    
    group_by(
      ISO3_exp_group,
      ISO3_imp_group
    ) %>%
    
    summarise(
      feed = sum(
        feed,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    
    drop_na()
  
  
  # Total traded amounts ---------------------------------------------------
  
  total_traded_food <- round(
    sum(
      Year_bil_trade_uses_food$food,
      na.rm = TRUE
    ),
    2
  )
  
  total_traded_feed <- round(
    sum(
      Year_bil_trade_uses_feed$feed,
      na.rm = TRUE
    ),
    2
  )
  
  
  # Convert to matrices ----------------------------------------------------
  
  trade_matrix_food <- Year_bil_trade_uses_food %>%
    
    pivot_wider(
      names_from = ISO3_imp_group,
      values_from = food,
      values_fill = 0
    ) %>%
    
    as.data.frame()
  
  rownames(
    trade_matrix_food
  ) <- trade_matrix_food$ISO3_exp_group
  
  trade_matrix_food <-
    as.matrix(
      trade_matrix_food[, -1]
    )
  
  
  trade_matrix_feed <- Year_bil_trade_uses_feed %>%
    
    pivot_wider(
      names_from = ISO3_imp_group,
      values_from = feed,
      values_fill = 0
    ) %>%
    
    as.data.frame()
  
  rownames(
    trade_matrix_feed
  ) <- trade_matrix_feed$ISO3_exp_group
  
  trade_matrix_feed <-
    as.matrix(
      trade_matrix_feed[, -1]
    )
  
  
  # Colours ---------------------------------------------------------------
  
  country_colors <- Region %>%
    
    left_join(
      tibble(
        Region = names(
          region_colors
        ),
        region_color =
          unname(
            region_colors
          )
      ),
      by = "Region"
    ) %>%
    
    select(
      ISO3,
      region_color
    )
  
  
  region_color_df <- tibble(
    ISO3 = names(
      region_colors
    ),
    region_color =
      unname(
        region_colors
      )
  )
  
  
  # Retain original regional colours for individual countries
  country_colors <- bind_rows(
    country_colors,
    region_color_df
  )
  
  
  food_sectors <- union(
    rownames(
      trade_matrix_food
    ),
    colnames(
      trade_matrix_food
    )
  )
  
  country_colors_food <- country_colors %>%
    
    filter(
      ISO3 %in%
        food_sectors
    )
  
  country_colors_v_food <-
    country_colors_food$region_color
  
  names(
    country_colors_v_food
  ) <- country_colors_food$ISO3
  
  
  feed_sectors <- union(
    rownames(
      trade_matrix_feed
    ),
    colnames(
      trade_matrix_feed
    )
  )
  
  country_colors_feed <- country_colors %>%
    
    filter(
      ISO3 %in%
        feed_sectors
    )
  
  country_colors_v_feed <-
    country_colors_feed$region_color
  
  names(
    country_colors_v_feed
  ) <- country_colors_feed$ISO3
  
  
  # Export PDF -------------------------------------------------------------
  
  pdf(
    paste0(
      "plots/Sup/Trade_diagrams_",
      nutrient,
      "_",
      year_sel,
      ".pdf"
    ),
    width = 35,
    height = 20
  )
  
  
  layout(
    matrix(
      c(
        1, 2,
        3, 4
      ),
      2,
      2,
      byrow = TRUE
    ),
    heights = c(
      0.1,
      0.9
    )
  )
  
  
  # Food title -------------------------------------------------------------
  
  plot.new()
  
  text(
    0.5,
    0.5,
    paste0(
      nutrient,
      " for traded production used for food in ",
      year_sel,
      ": ",
      total_traded_food,
      " Gg"
    ),
    cex = 3,
    font = 2
  )
  
  
  # Feed title -------------------------------------------------------------
  
  plot.new()
  
  text(
    0.5,
    0.5,
    paste0(
      nutrient,
      " for traded production used for feed in ",
      year_sel,
      ": ",
      total_traded_feed,
      " Gg"
    ),
    cex = 3,
    font = 2
  )
  
  
  # FOOD chord diagram -----------------------------------------------------
  
  circos.clear()
  
  circos.par(
    start.degree = 90,
    gap.degree = 5,
    track.margin = c(
      0.02,
      0.02
    ),
    canvas.xlim = c(
      -1.4,
      1.4
    ),
    canvas.ylim = c(
      -1.4,
      1.4
    )
  )
  
  
  chordDiagram(
    trade_matrix_food,
    grid.col =
      country_colors_v_food,
    directional = 1,
    link.lty =
      ifelse(
        trade_matrix_food > 0,
        1,
        2
      ),
    annotationTrack = "grid"
  )
  
  
  for (
    si in
    get.all.sector.index()
  ) {
    
    xlim <-
      get.cell.meta.data(
        "xlim",
        sector.index = si,
        track.index = 1
      )
    
    ylim <-
      get.cell.meta.data(
        "ylim",
        sector.index = si,
        track.index = 1
      )
    
    
    circos.text(
      mean(
        xlim
      ),
      ylim[2] +
        mm_y(
          5
        ),
      si,
      sector.index = si,
      track.index = 1,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(
        -0.1,
        -0.1
      ),
      cex = 1.6,
      col = "black"
    )
    
    
    circos.axis(
      sector.index = si,
      labels.cex = 1.6,
      direction = "outside"
    )
  }
  
  
  circos.clear()
  
  
  # FEED chord diagram -----------------------------------------------------
  
  circos.par(
    start.degree = 90,
    gap.degree = 5,
    track.margin = c(
      0.02,
      0.02
    ),
    canvas.xlim = c(
      -1.4,
      1.4
    ),
    canvas.ylim = c(
      -1.4,
      1.4
    )
  )
  
  
  chordDiagram(
    trade_matrix_feed,
    grid.col =
      country_colors_v_feed,
    directional = 1,
    link.lty =
      ifelse(
        trade_matrix_feed > 0,
        1,
        2
      ),
    annotationTrack = "grid"
  )
  
  
  for (
    si in
    get.all.sector.index()
  ) {
    
    xlim <-
      get.cell.meta.data(
        "xlim",
        sector.index = si,
        track.index = 1
      )
    
    ylim <-
      get.cell.meta.data(
        "ylim",
        sector.index = si,
        track.index = 1
      )
    
    
    circos.text(
      mean(
        xlim
      ),
      ylim[2] +
        mm_y(
          5
        ),
      si,
      sector.index = si,
      track.index = 1,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(
        -0.1,
        -0.1
      ),
      cex = 1.6,
      col = "black"
    )
    
    
    circos.axis(
      sector.index = si,
      labels.cex = 1.6,
      direction = "outside"
    )
  }
  
  
  circos.clear()
  
  dev.off()
  
  
  # Return data used in the figure ----------------------------------------
  
  invisible(
    list(
      bilateral =
        Year_bil_trade_uses,
      
      food =
        Year_bil_trade_uses_food,
      
      feed =
        Year_bil_trade_uses_feed,
      
      food_matrix =
        trade_matrix_food,
      
      feed_matrix =
        trade_matrix_feed,
      
      total_food =
        total_traded_food,
      
      total_feed =
        total_traded_feed
    )
  )
}

# Nitrogen
N_1965 <- make_trade_chord(
  "N",
  1965
)

N_1990 <- make_trade_chord(
  "N",
  1990
)

N_2015 <- make_trade_chord(
  "N",
  2015
)


# Phosphorus
P_1965 <- make_trade_chord(
  "P",
  1965
)

P_1990 <- make_trade_chord(
  "P",
  1990
)

P_2015 <- make_trade_chord(
  "P",
  2015
)


# Potassium
K_1965 <- make_trade_chord(
  "K",
  1965
)

K_1990 <- make_trade_chord(
  "K",
  1990
)

K_2015 <- make_trade_chord(
  "K",
  2015
)


# Extended Data Figure 2 ---------------------------------------------------
#
# Global temporal trends in virtual fertilizer associated with traded
# production allocated to food, feed, and other uses.
#
# Fertilizer-use uncertainty is propagated through the bilateral trade
# structure using the 500 Monte Carlo fertilizer-use estimates.
#
# "Others" is calculated as the residual bilateral fertilizer flow:
#
#   total bilateral flow - food flow - feed flow
#
# This corresponds to Seed + Losses + Other_Uses_nf for the primary crops.
# Soybean oil and soybean cake are already entirely allocated to food and
# feed, respectively, and therefore have zero "Others".
#
# Fertilizer values are handled in Gg and converted to Tg for plotting:
# 1 Tg = 1000 Gg.


## Prepare fertilizer use for every Monte Carlo realization -----------------

prepare_trade_mc <- function(
    data,
    nutrient,
    crop
) {
  
  food_var <- paste0(
    nutrient,
    "_food"
  )
  
  feed_var <- paste0(
    nutrient,
    "_feed"
  )
  
  others_var <- paste0(
    nutrient,
    "_others"
  )
  
  
  data %>%
    
    filter(
      FAOStat_area_code != 351
    ) %>%
    
    rowwise() %>%
    
    mutate(
      
      nutrient_total_sim_Gg =
        sum(
          c_across(
            all_of(
              c(
                food_var,
                feed_var,
                others_var
              )
            )
          ),
          na.rm = TRUE
        ) / 1e6
    ) %>%
    
    ungroup() %>%
    
    transmute(
      Year,
      FAOStat_area_code_exp =
        FAOStat_area_code,
      Sim,
      Crop = crop,
      nutrient_total_sim_Gg
    )
}


# Nitrogen

N_trade_mc <- bind_rows(
  
  prepare_trade_mc(
    N_wheat_uses,
    "N",
    "Wheat"
  ),
  
  prepare_trade_mc(
    N_maize_uses,
    "N",
    "Maize"
  ),
  
  prepare_trade_mc(
    N_rice_uses,
    "N",
    "Rice"
  ),
  
  prepare_trade_mc(
    N_soybean_uses,
    "N",
    "Soybean"
  )
)


# Phosphorus

P_trade_mc <- bind_rows(
  
  prepare_trade_mc(
    P_wheat_uses,
    "P",
    "Wheat"
  ),
  
  prepare_trade_mc(
    P_maize_uses,
    "P",
    "Maize"
  ),
  
  prepare_trade_mc(
    P_rice_uses,
    "P",
    "Rice"
  ),
  
  prepare_trade_mc(
    P_soybean_uses,
    "P",
    "Soybean"
  )
)


# Potassium

K_trade_mc <- bind_rows(
  
  prepare_trade_mc(
    K_wheat_uses,
    "K",
    "Wheat"
  ),
  
  prepare_trade_mc(
    K_maize_uses,
    "K",
    "Maize"
  ),
  
  prepare_trade_mc(
    K_rice_uses,
    "K",
    "Rice"
  ),
  
  prepare_trade_mc(
    K_soybean_uses,
    "K",
    "Soybean"
  )
)


## Derive trade coefficients -----------------------------------------------
#
# The bilateral datasets already contain:
#
#   nutrient_total_exp
#   Perc_Imp
#   N/P/K_exp_food
#   N/P/K_exp_feed
#
# or, depending on the object construction:
#
#   N_total_exp / P_total_exp / K_total_exp
#
# The total fertilizer embodied in each bilateral flow is:
#
#   total exported fertilizer * Perc_Imp
#
# "Others" is therefore obtained as the residual after subtracting the
# fertilizer assigned to food and feed.


prepare_trade_coefficients <- function(
    bilateral_data,
    mc_data,
    nutrient
) {
  
  food_var <- paste0(
    nutrient,
    "_exp_food"
  )
  
  feed_var <- paste0(
    nutrient,
    "_exp_feed"
  )
  
  
  # Identify the total exported-fertilizer variable
  
  total_candidates <- c(
    "nutrient_total_exp",
    paste0(
      nutrient,
      "_total_exp"
    )
  )
  
  total_var <- total_candidates[
    total_candidates %in%
      names(
        bilateral_data
      )
  ][1]
  
  
  if (
    length(total_var) == 0 ||
    is.na(total_var)
  ) {
    
    stop(
      paste0(
        "No total exported fertilizer variable found for ",
        nutrient,
        ". Expected either 'nutrient_total_exp' or '",
        nutrient,
        "_total_exp'."
      )
    )
  }
  
  
  if (
    !"Perc_Imp" %in%
    names(
      bilateral_data
    )
  ) {
    
    stop(
      "The bilateral dataset must contain 'Perc_Imp'."
    )
  }
  
  
  # Prepare bilateral flows -----------------------------------------------
  
  bilateral_prepared <- bilateral_data %>%
    
    mutate(
      
      Crop = case_when(
        
        str_detect(
          str_to_lower(Product),
          "wheat"
        ) ~ "Wheat",
        
        str_detect(
          str_to_lower(Product),
          "maize|corn"
        ) ~ "Maize",
        
        str_detect(
          str_to_lower(Product),
          "rice"
        ) ~ "Rice",
        
        str_detect(
          str_to_lower(Product),
          "soy"
        ) ~ "Soybean",
        
        TRUE ~ NA_character_
      ),
      
      
      # Total fertilizer embodied in this bilateral flow.
      
      total_bilateral_Gg =
        .data[[total_var]] *
        Perc_Imp,
      
      
      # Existing food and feed allocation.
      
      trade_food_Gg =
        .data[[food_var]],
      
      trade_feed_Gg =
        .data[[feed_var]],
      
      
      # Remaining traded fertilizer is assigned to "Others".
      
      trade_others_Gg_raw =
        total_bilateral_Gg -
        trade_food_Gg -
        trade_feed_Gg
    )
  
  
  # Check for meaningful negative residuals
  
  negative_residuals <- bilateral_prepared %>%
    
    filter(
      trade_others_Gg_raw <
        -1e-8
    )
  
  
  if (
    nrow(
      negative_residuals
    ) > 0
  ) {
    
    warning(
      paste0(
        nrow(
          negative_residuals
        ),
        " bilateral rows have negative 'Others' residuals. ",
        "Inspect negative_residuals before interpreting the results."
      )
    )
  }
  
  
  # Remove only numerical floating-point negatives.
  
  bilateral_prepared <- bilateral_prepared %>%
    
    mutate(
      
      trade_others_Gg =
        if_else(
          trade_others_Gg_raw < 0 &
            abs(
              trade_others_Gg_raw
            ) < 1e-8,
          0,
          trade_others_Gg_raw
        )
    )
  
  
  # Aggregate all bilateral destinations and products
  #
  # Soybean, soybean oil, and soybean cake are all assigned to the
  # fertilizer used in soybean production.
  
  trade_central <- bilateral_prepared %>%
    
    filter(
      !is.na(Crop),
      FAOStat_area_code_exp != 351
    ) %>%
    
    group_by(
      Year,
      FAOStat_area_code_exp,
      Crop
    ) %>%
    
    summarise(
      
      trade_food_Gg =
        sum(
          trade_food_Gg,
          na.rm = TRUE
        ),
      
      trade_feed_Gg =
        sum(
          trade_feed_Gg,
          na.rm = TRUE
        ),
      
      trade_others_Gg =
        sum(
          trade_others_Gg,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
  
  
  # Mean source fertilizer use
  #
  # This is used only to express the central bilateral flows as fractions
  # of the fertilizer associated with crop production.
  
  source_central <- mc_data %>%
    
    group_by(
      Year,
      FAOStat_area_code_exp,
      Crop
    ) %>%
    
    summarise(
      
      source_total_Gg =
        mean(
          nutrient_total_sim_Gg,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
  
  
  # Trade coefficients
  
  trade_coef <- trade_central %>%
    
    left_join(
      source_central,
      by = c(
        "Year",
        "FAOStat_area_code_exp",
        "Crop"
      )
    ) %>%
    
    mutate(
      
      coef_food =
        if_else(
          source_total_Gg > 0,
          trade_food_Gg /
            source_total_Gg,
          0
        ),
      
      coef_feed =
        if_else(
          source_total_Gg > 0,
          trade_feed_Gg /
            source_total_Gg,
          0
        ),
      
      coef_others =
        if_else(
          source_total_Gg > 0,
          trade_others_Gg /
            source_total_Gg,
          0
        ),
      
      coef_total =
        coef_food +
        coef_feed +
        coef_others
    )
  
  
  # Check that traded fertilizer does not exceed source fertilizer
  
  coefficient_check <- trade_coef %>%
    
    filter(
      coef_total >
        1.0001
    )
  
  
  if (
    nrow(
      coefficient_check
    ) > 0
  ) {
    
    warning(
      paste0(
        nrow(
          coefficient_check
        ),
        " exporter-year-crop combinations have total trade coefficients > 1."
      )
    )
  }
  
  
  trade_coef %>%
    
    select(
      Year,
      FAOStat_area_code_exp,
      Crop,
      coef_food,
      coef_feed,
      coef_others
    )
}


# Calculate N, P, and K trade coefficients

N_trade_coef <- prepare_trade_coefficients(
  Bilateral_virtual_fertilizer_N,
  N_trade_mc,
  "N"
)


P_trade_coef <- prepare_trade_coefficients(
  Bilateral_virtual_fertilizer_P,
  P_trade_mc,
  "P"
)


K_trade_coef <- prepare_trade_coefficients(
  Bilateral_virtual_fertilizer_K,
  K_trade_mc,
  "K"
)


# Check trade-allocation coefficients
trade_coef_check <- bind_rows(
  
  N_trade_coef %>%
    mutate(
      Nutrient = "N"
    ),
  
  P_trade_coef %>%
    mutate(
      Nutrient = "P"
    ),
  
  K_trade_coef %>%
    mutate(
      Nutrient = "K"
    )
) %>%
  
  mutate(
    coef_total =
      coef_food +
      coef_feed +
      coef_others
  )


summary(
  trade_coef_check$coef_total
)


trade_coef_check %>%
  filter(
    coef_total >
      1.0001 |
      coef_food < 0 |
      coef_feed < 0 |
      coef_others < 0
  )


## Propagate fertilizer-use uncertainty through trade ----------------------

propagate_trade_mc <- function(
    mc_data,
    coef_data,
    nutrient
) {
  
  trade_sim <- mc_data %>%
    
    left_join(
      coef_data,
      by = c(
        "Year",
        "FAOStat_area_code_exp",
        "Crop"
      )
    ) %>%
    
    mutate(
      
      coef_food =
        replace_na(
          coef_food,
          0
        ),
      
      coef_feed =
        replace_na(
          coef_feed,
          0
        ),
      
      coef_others =
        replace_na(
          coef_others,
          0
        ),
      
      
      trade_food_Gg =
        nutrient_total_sim_Gg *
        coef_food,
      
      trade_feed_Gg =
        nutrient_total_sim_Gg *
        coef_feed,
      
      trade_others_Gg =
        nutrient_total_sim_Gg *
        coef_others
    ) %>%
    
    group_by(
      Year,
      Sim
    ) %>%
    
    summarise(
      
      food_Gg =
        sum(
          trade_food_Gg,
          na.rm = TRUE
        ),
      
      feed_Gg =
        sum(
          trade_feed_Gg,
          na.rm = TRUE
        ),
      
      others_Gg =
        sum(
          trade_others_Gg,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      Nutrient = nutrient
    )
  
  
  # Annual fertilizer-trade trends ----------------------------------------
  
  trade_summary <- trade_sim %>%
    
    group_by(
      Year,
      Nutrient
    ) %>%
    
    summarise(
      
      n_sim =
        n_distinct(
          Sim
        ),
      
      food_mean =
        mean(
          food_Gg,
          na.rm = TRUE
        ) / 1000,
      
      food_sd =
        sd(
          food_Gg,
          na.rm = TRUE
        ) / 1000,
      
      feed_mean =
        mean(
          feed_Gg,
          na.rm = TRUE
        ) / 1000,
      
      feed_sd =
        sd(
          feed_Gg,
          na.rm = TRUE
        ) / 1000,
      
      others_mean =
        mean(
          others_Gg,
          na.rm = TRUE
        ) / 1000,
      
      others_sd =
        sd(
          others_Gg,
          na.rm = TRUE
        ) / 1000,
      
      .groups = "drop"
    ) %>%
    
    mutate(
      
      food_se =
        food_sd /
        sqrt(
          n_sim
        ),
      
      feed_se =
        feed_sd /
        sqrt(
          n_sim
        ),
      
      others_se =
        others_sd /
        sqrt(
          n_sim
        )
    )
  
  
  # Feed-to-food ratio -----------------------------------------------------
  
  trade_ratio <- trade_sim %>%
    
    mutate(
      
      Rat =
        if_else(
          food_Gg > 0,
          feed_Gg /
            food_Gg,
          NA_real_
        )
    ) %>%
    
    group_by(
      Year,
      Nutrient
    ) %>%
    
    summarise(
      
      n_sim =
        sum(
          is.finite(
            Rat
          )
        ),
      
      Rat =
        mean(
          Rat,
          na.rm = TRUE
        ),
      
      Rat_sd =
        sd(
          Rat,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      
      Rat_se =
        Rat_sd /
        sqrt(
          n_sim
        )
    )
  
  
  list(
    sim = trade_sim,
    trend = trade_summary,
    ratio = trade_ratio
  )
}


# Apply to N, P, and K

N_trade_results <- propagate_trade_mc(
  N_trade_mc,
  N_trade_coef,
  "N"
)


P_trade_results <- propagate_trade_mc(
  P_trade_mc,
  P_trade_coef,
  "P"
)


K_trade_results <- propagate_trade_mc(
  K_trade_mc,
  K_trade_coef,
  "K"
)


# Combine nutrients


Global_trade_trend <- bind_rows(
  N_trade_results$trend,
  P_trade_results$trend,
  K_trade_results$trend
)


Global_trade_ratio <- bind_rows(
  N_trade_results$ratio,
  P_trade_results$ratio,
  K_trade_results$ratio
)



Global_trade_trend


Global_trade_ratio


## Plot global traded fertilizer trends ------------------------------------

Plot_trade_trend <- function(
    data,
    nutrient
) {
  
  plot_data <- data %>%
    
    filter(
      Nutrient == nutrient
    )
  
  
  ggplot(
    plot_data,
    aes(
      x = Year
    )
  ) +
    
    
    # Standard deviation ---------------------------------------------------
  
  geom_ribbon(
    aes(
      ymin =
        food_mean -
        food_sd,
      ymax =
        food_mean +
        food_sd
    ),
    alpha = 0.25,
    fill = "#FF9E00"
  ) +
    
    geom_ribbon(
      aes(
        ymin =
          feed_mean -
          feed_sd,
        ymax =
          feed_mean +
          feed_sd
      ),
      alpha = 0.25,
      fill = "#458472"
    ) +
    
    geom_ribbon(
      aes(
        ymin =
          others_mean -
          others_sd,
        ymax =
          others_mean +
          others_sd
      ),
      alpha = 0.25,
      fill = "black"
    ) +
    
    
    # Standard error -------------------------------------------------------
  
  geom_ribbon(
    aes(
      ymin =
        food_mean -
        food_se,
      ymax =
        food_mean +
        food_se
    ),
    alpha = 0.5,
    fill = "#FF9E00"
  ) +
    
    geom_ribbon(
      aes(
        ymin =
          feed_mean -
          feed_se,
        ymax =
          feed_mean +
          feed_se
      ),
      alpha = 0.5,
      fill = "#458472"
    ) +
    
    geom_ribbon(
      aes(
        ymin =
          others_mean -
          others_se,
        ymax =
          others_mean +
          others_se
      ),
      alpha = 0.5,
      fill = "black"
    ) +
    
    
    # Mean annual traded fertilizer ---------------------------------------
  
  geom_line(
    aes(
      y = food_mean
    ),
    colour = "#FF9E00",
    linewidth = 0.5
  ) +
    
    geom_line(
      aes(
        y = feed_mean
      ),
      colour = "#458472",
      linewidth = 0.5
    ) +
    
    geom_line(
      aes(
        y = others_mean
      ),
      colour = "black",
      linewidth = 0.5
    ) +
    
    
    labs(
      x = "Year",
      y = "Tg"
    ) +
    
    theme_bw() +
    
    theme(
      
      text =
        element_text(
          family = "Arial",
          size = 7
        ),
      
      axis.title =
        element_text(
          size = 7
        ),
      
      axis.text =
        element_text(
          size = 6
        ),
      
      panel.grid.major =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      legend.position =
        "none"
    )
}


## Individual nutrient panels ----------------------------------------------

pED2_N <- Plot_trade_trend(
  Global_trade_trend,
  "N"
)


pED2_P <- Plot_trade_trend(
  Global_trade_trend,
  "P"
)


pED2_K <- Plot_trade_trend(
  Global_trade_trend,
  "K"
)


# Combine Extended Data Figure 2

pED2 <- plot_grid(
  pED2_N,
  pED2_P,
  pED2_K,
  nrow = 1
)


pED2


## Export ------------------------------------------------------------------

ggsave(
  filename =
    "plots/Extended_Data_Figure_2.pdf",
  plot =
    pED2,
  width =
    180,
  height =
    60,
  units =
    "mm",
  device =
    cairo_pdf
)

## Export source data for Extended Data Figure 2 ----------------------------
#
# Create a single tidy dataset containing the values represented in
# Extended Data Figure 2.
#
# Panels a-c: virtual fertilizer associated with traded production
#             allocated to Food, Feed, and Others.
#
# Panel d:   feed-to-food ratio of virtual fertilizer trade.


## Panels a-c ---------------------------------------------------------------

ED_Figure2_trade_data <- Global_trade_trend %>%
  
  select(
    Year,
    Nutrient,
    food_mean,
    food_sd,
    food_se,
    feed_mean,
    feed_sd,
    feed_se,
    others_mean,
    others_sd,
    others_se
  ) %>%
  
  pivot_longer(
    cols = -c(
      Year,
      Nutrient
    ),
    names_to = c(
      "Use",
      "Statistic"
    ),
    names_pattern =
      "(food|feed|others)_(mean|sd|se)",
    values_to = "Value"
  ) %>%
  
  mutate(
    
    Use = recode(
      Use,
      "food" = "Food",
      "feed" = "Feed",
      "others" = "Others"
    ),
    
    Statistic = recode(
      Statistic,
      "mean" = "Mean",
      "sd" = "SD",
      "se" = "SE"
    ),
    
    Panel = case_when(
      Nutrient == "N" ~ "a",
      Nutrient == "P" ~ "b",
      Nutrient == "K" ~ "c"
    ),
    
    Measure =
      "Virtual fertilizer associated with traded production",
    
    Unit =
      "Tg"
  ) %>%
  
  select(
    Panel,
    Year,
    Nutrient,
    Measure,
    Use,
    Statistic,
    Value,
    Unit
  )





## Export source data for Extended Data Figure 2 ----------------------------
#
# Create a single tidy dataset containing the values represented in
# Extended Data Figure 2.
#
# Panels a-c: virtual fertilizer associated with traded production
#             allocated to Food, Feed, and Others.
#

ED_Figure2_trade_data <- Global_trade_trend %>%
  
  select(
    Year,
    Nutrient,
    food_mean,
    food_sd,
    food_se,
    feed_mean,
    feed_sd,
    feed_se,
    others_mean,
    others_sd,
    others_se
  ) %>%
  
  pivot_longer(
    cols = -c(
      Year,
      Nutrient
    ),
    names_to = c(
      "Use",
      "Statistic"
    ),
    names_pattern =
      "(food|feed|others)_(mean|sd|se)",
    values_to = "Value"
  ) %>%
  
  mutate(
    
    Use = recode(
      Use,
      "food" = "Food",
      "feed" = "Feed",
      "others" = "Others"
    ),
    
    Statistic = recode(
      Statistic,
      "mean" = "Mean",
      "sd" = "SD",
      "se" = "SE"
    ),
    
    Panel = case_when(
      Nutrient == "N" ~ "a",
      Nutrient == "P" ~ "b",
      Nutrient == "K" ~ "c"
    ),
    
    Measure =
      "Virtual fertilizer associated with traded production",
    
    Unit =
      "Tg"
  ) %>%
  
  select(
    Panel,
    Year,
    Nutrient,
    Measure,
    Use,
    Statistic,
    Value,
    Unit
  )



# Combine all panels

Extended_Data_Figure_2_source_data <- bind_rows(
  ED_Figure2_trade_data
)



Extended_Data_Figure_2_source_data


# Export

write_csv(
  Extended_Data_Figure_2_source_data,
  "results/Extended_Data_Figure_2_source_data.csv"
)

# Extended Data Figure 3: Fertilizer use for other maize uses --------------
#
# Compare temporal changes in N, P, and K fertilizer use associated with
# "other" maize uses with the expansion of fuel-ethanol production.
#
# The "others" category corresponds to maize production allocated to uses
# other than food and feed, and includes the biofuel-related component of
# maize use.
#
# The analysis is shown globally and for major fuel-ethanol-producing
# countries.


# Global maize fertilizer use
#
# Estimate annual global N, P, and K fertilizer use associated with each
# maize-use category from the Monte Carlo fertilizer-use estimates.
#
# The Global_trend_* functions return annual means and uncertainty estimates
# across Monte Carlo realizations.

Maize_N_global <- Global_trend_N(
  N_maize_global_uses
)

Maize_P_global <- Global_trend_P(
  P_maize_global_uses
)

Maize_K_global <- Global_trend_K(
  K_maize_global_uses
)


# Country-level fertilizer use associated with other maize uses
#
# For a selected country, summarize annual N, P, and K fertilizer use
# allocated to the "others" category across Monte Carlo realizations.
#
# For each nutrient and year, retain:
#   - mean fertilizer use
#   - 2.5th percentile
#   - 97.5th percentile
#   - standard deviation
#
# The percentile estimates are retained in the output although the figure
# below represents mean ± 1 SD.

Country_NPK_maize_others <- function(
    N_maize = N_maize_uses,
    P_maize = P_maize_uses,
    K_maize = K_maize_uses,
    area_code
) {
  
  # Nitrogen
  
  N_maize_other_uses_country <- N_maize_uses %>%
    
    filter(
      FAOStat_area_code == area_code
    ) %>%
    
    group_by(
      Year
    ) %>%
    
    summarise(
      
      N_use_others =
        mean(
          N_others,
          na.rm = TRUE
        ),
      
      N_use_others_q025 =
        quantile(
          N_others,
          0.025,
          na.rm = TRUE
        ),
      
      N_use_others_q975 =
        quantile(
          N_others,
          0.975,
          na.rm = TRUE
        ),
      
      N_use_others_sd =
        sd(
          N_others,
          na.rm = TRUE
        )
    )
  
  
  # Phosphorus -------------------------------------------------------------
  
  P_maize_other_uses_country <- P_maize_uses %>%
    
    filter(
      FAOStat_area_code == area_code
    ) %>%
    
    group_by(
      Year
    ) %>%
    
    summarise(
      
      P_use_others =
        mean(
          P_others,
          na.rm = TRUE
        ),
      
      P_use_others_q025 =
        quantile(
          P_others,
          0.025,
          na.rm = TRUE
        ),
      
      P_use_others_q975 =
        quantile(
          P_others,
          0.975,
          na.rm = TRUE
        ),
      
      P_use_others_sd =
        sd(
          P_others,
          na.rm = TRUE
        )
    )
  
  
  # Potassium --------------------------------------------------------------
  
  K_maize_other_uses_country <- K_maize_uses %>%
    
    filter(
      FAOStat_area_code == area_code
    ) %>%
    
    group_by(
      Year
    ) %>%
    
    summarise(
      
      K_use_others =
        mean(
          K_others,
          na.rm = TRUE
        ),
      
      K_use_others_q025 =
        quantile(
          K_others,
          0.025,
          na.rm = TRUE
        ),
      
      K_use_others_q975 =
        quantile(
          K_others,
          0.975,
          na.rm = TRUE
        ),
      
      K_use_others_sd =
        sd(
          K_others,
          na.rm = TRUE
        )
    )
  
  
  # Combine nutrients into one annual country-level dataset ----------------
  
  NPK_maize_other_uses_country <-
    N_maize_other_uses_country %>%
    
    left_join(
      P_maize_other_uses_country,
      by = "Year"
    ) %>%
    
    left_join(
      K_maize_other_uses_country,
      by = "Year"
    )
  
  
  return(
    NPK_maize_other_uses_country
  )
}


# Selected major fuel-ethanol-producing countries
#
# FAOSTAT area codes:
#   United States = 231
#   Brazil        = 21
#   China         = 41
#   Canada        = 33
#   Thailand      = 216

US_NPK_maize <- Country_NPK_maize_others(
  area_code = 231
)

Brazil_NPK_maize <- Country_NPK_maize_others(
  area_code = 21
)

China_NPK_maize <- Country_NPK_maize_others(
  area_code = 41
)

Canada_NPK_maize <- Country_NPK_maize_others(
  area_code = 33
)

Thailand_NPK_maize <- Country_NPK_maize_others(
  area_code = 216
)


# Fuel-ethanol production
#
# Reshape the country-level fuel-ethanol production dataset from wide to
# long format.
#
# Production is expressed in million tonnes of oil equivalent (MMTOE).

Bioethanol <- Biofuel_world_production %>%
  
  select(
    1:42
  ) %>%
  
  filter(
    Product == "Fuel ethanol (MMTOE)"
  ) %>%
  
  pivot_longer(
    cols = !c(
      "Country",
      "Product"
    ),
    names_to = "Year",
    values_to = "Production"
  ) %>%
  
  mutate(
    Production =
      as.numeric(
        Production
      )
  ) %>%
  
  filter(
    Production > 0,
    !is.na(
      Production
    )
  )


unique(
  Bioethanol$Country
)


# Global annual fuel-ethanol production
#
# Sum production across all countries for each year.

Bioethanol_global <- Bioethanol %>%
  
  group_by(
    Year
  ) %>%
  
  summarise(
    Production =
      sum(
        Production,
        na.rm = TRUE
      )
  ) %>%
  
  mutate(
    Year =
      as.numeric(
        Year
      )
  )


# Identify major fuel-ethanol producers
#
# Calculate mean production after 2010 for each country. This was used to
# identify the countries with the highest recent fuel-ethanol production.

Bioethanol_2010 <- Bioethanol %>%
  
  filter(
    Year > 2010
  ) %>%
  
  group_by(
    Country
  ) %>%
  
  summarise(
    Production_10s =
      mean(
        Production,
        na.rm = TRUE
      )
  ) %>%
  
  mutate(
    x = seq(
      nrow(.)
    ),
    y = seq(
      nrow(.)
    )
  )


# The five major producers identified were:
# United States, Brazil, China, Canada, and Thailand.

United_States_bioethanol <- Bioethanol %>%
  
  filter(
    Country == "United States"
  ) %>%
  
  mutate(
    Year =
      as.numeric(
        Year
      )
  )


Brazil_bioethanol <- Bioethanol %>%
  
  filter(
    Country == "Brazil"
  ) %>%
  
  mutate(
    Year =
      as.numeric(
        Year
      )
  )


China_bioethanol <- Bioethanol %>%
  
  filter(
    Country == "China"
  ) %>%
  
  mutate(
    Year =
      as.numeric(
        Year
      )
  )


Canada_bioethanol <- Bioethanol %>%
  
  filter(
    Country == "Canada"
  ) %>%
  
  mutate(
    Year =
      as.numeric(
        Year
      )
  )


Thailand_bioethanol <- Bioethanol %>%
  
  filter(
    Country == "Thailand"
  ) %>%
  
  mutate(
    Year =
      as.numeric(
        Year
      )
  )




# Extended Data Figure 3
#
# Temporal association between fertilizer use allocated to other maize uses
# and fuel-ethanol production.
#
# Fertilizer lines represent the annual mean across Monte Carlo estimates.
# Shaded areas represent mean ± 1 SD.
#
# Fuel-ethanol production is shown on the secondary y-axis in MMTOE.


# Plot settings

ETHANOL_SCALE <- 10

# Colour-blind-accessible colours.
COL_N <- "darkred"
COL_P <- "darkblue"
COL_K <- "darkgreen"
COL_ETHANOL <- "black"


## Common Nature-style theme ------------------------------------------------

theme_nature <- theme_classic(
  base_family = "Arial",
  base_size = 6
) +
  theme(
    plot.title = element_text(
      size = 7,
      face = "plain",
      hjust = 0
    ),
    axis.title = element_text(
      size = 6
    ),
    axis.text = element_text(
      size = 5.5,
      colour = "black"
    ),
    axis.line = element_line(
      linewidth = 0.25
    ),
    axis.ticks = element_line(
      linewidth = 0.25
    ),
    axis.ticks.length = unit(
      1.5,
      "mm"
    ),
    legend.position = "none",
    plot.margin = margin(
      3,
      3,
      3,
      3,
      unit = "mm"
    )
  )


# Plot function
#
# fertilizer_divisor converts the fertilizer values contained in the input
# datasets to Tg.
#
# IMPORTANT:
#   Global data currently use 1e6.
#   Country data currently use 1e9.
#
# These conversions reproduce the existing analysis and should be checked
# against the units of the corresponding input objects before publication.

plot_bioethanol_nature <- function(
    dataset_N,
    dataset_P,
    dataset_K,
    dataset_bioethanol,
    fertilizer_divisor,
    panel_title
) {
  
  ggplot() +
    
    # Nitrogen
  
  geom_ribbon(
    data = dataset_N,
    aes(
      x = Year,
      ymin =
        (N_use_others - N_use_others_sd) /
        fertilizer_divisor,
      ymax =
        (N_use_others + N_use_others_sd) /
        fertilizer_divisor
    ),
    fill = COL_N,
    alpha = 0.20
  ) +
    
    geom_line(
      data = dataset_N,
      aes(
        x = Year,
        y = N_use_others /
          fertilizer_divisor
      ),
      colour = COL_N,
      linewidth = 0.30
    ) +
    
    
    # Phosphorus
  
  geom_ribbon(
    data = dataset_P,
    aes(
      x = Year,
      ymin =
        (P_use_others - P_use_others_sd) /
        fertilizer_divisor,
      ymax =
        (P_use_others + P_use_others_sd) /
        fertilizer_divisor
    ),
    fill = COL_P,
    alpha = 0.20
  ) +
    
    geom_line(
      data = dataset_P,
      aes(
        x = Year,
        y = P_use_others /
          fertilizer_divisor
      ),
      colour = COL_P,
      linewidth = 0.30
    ) +
    
    
    # Potassium
  
  geom_ribbon(
    data = dataset_K,
    aes(
      x = Year,
      ymin =
        (K_use_others - K_use_others_sd) /
        fertilizer_divisor,
      ymax =
        (K_use_others + K_use_others_sd) /
        fertilizer_divisor
    ),
    fill = COL_K,
    alpha = 0.20
  ) +
    
    geom_line(
      data = dataset_K,
      aes(
        x = Year,
        y = K_use_others /
          fertilizer_divisor
      ),
      colour = COL_K,
      linewidth = 0.30
    ) +
    
    
    # Fuel-ethanol production
  
  geom_line(
    data = dataset_bioethanol,
    aes(
      x = Year,
      y = Production /
        ETHANOL_SCALE
    ),
    colour = COL_ETHANOL,
    linewidth = 0.45
  ) +
    
    
    # Axes
  
  scale_y_continuous(
    name = "Fertilizer use (Tg)",
    sec.axis = sec_axis(
      ~ . * ETHANOL_SCALE,
      name = "Fuel ethanol production (MMTOE)"
    ),
    expand = expansion(
      mult = c(
        0.03,
        0.05
      )
    )
  ) +
    
    scale_x_continuous(
      breaks = scales::pretty_breaks(
        n = 5
      )
    ) +
    
    labs(
      x = "Year",
      title = panel_title
    ) +
    
    theme_nature
}

# Global

ED3_global <- plot_bioethanol_nature(
  dataset_N = Maize_N_global,
  dataset_P = Maize_P_global,
  dataset_K = Maize_K_global,
  dataset_bioethanol = Bioethanol_global,
  fertilizer_divisor = 1e6,
  panel_title = "Global"
)


# United States

ED3_USA <- plot_bioethanol_nature(
  dataset_N = US_NPK_maize,
  dataset_P = US_NPK_maize,
  dataset_K = US_NPK_maize,
  dataset_bioethanol = United_States_bioethanol,
  fertilizer_divisor = 1e9,
  panel_title = "United States"
)


# Brazil

ED3_Brazil <- plot_bioethanol_nature(
  dataset_N = Brazil_NPK_maize,
  dataset_P = Brazil_NPK_maize,
  dataset_K = Brazil_NPK_maize,
  dataset_bioethanol = Brazil_bioethanol,
  fertilizer_divisor = 1e9,
  panel_title = "Brazil"
)

# Combine panels

ED3_country_panels <- plot_grid(
  ED3_USA,
  ED3_Brazil,
  ncol = 1,
  labels = c(
    "b",
    "c"
  ),
  label_fontfamily = "Arial",
  label_fontface = "bold",
  label_size = 7,
  align = "v"
)


Extended_Data_Figure_3 <- plot_grid(
  ED3_global,
  ED3_country_panels,
  nrow = 1,
  rel_widths = c(
    1.35,
    1
  ),
  labels = c(
    "a",
    ""
  ),
  label_fontfamily = "Arial",
  label_fontface = "bold",
  label_size = 7
)


Extended_Data_Figure_3

# Vector PDF for repository / archive

ggsave(
  filename =
    "plots/Extended_Data_Figure_3_OtherUses.pdf",
  plot =
    Extended_Data_Figure_3,
  device =
    cairo_pdf,
  width =
    180,
  height =
    90,
  units =
    "mm",
  bg =
    "white"
)

# Association between fertilizer use and fuel-ethanol production
#
# Combine fuel-ethanol production with N, P, and K fertilizer use associated
# with other maize uses for overlapping years.
#
# Total NPK fertilizer use is calculated as the sum of N, P, and K by mass.
# NPK_Fuel_rat expresses fertilizer use relative to fuel-ethanol production.


# Global.
r2_global <- Bioethanol_global %>%
  
  left_join(
    Maize_N_global,
    by = "Year"
  ) %>%
  
  left_join(
    Maize_P_global,
    by = "Year"
  ) %>%
  
  left_join(
    Maize_K_global,
    by = "Year"
  ) %>%
  
  drop_na() %>%
  
  rowwise() %>%
  
  mutate(
    
    NPK_others =
      sum(
        N_use_others,
        P_use_others,
        K_use_others
      ),
    
    NPK_Fuel_rat =
      NPK_others /
      (
        Production *
          1000000000
      )
  )


# Correlation and linear association at the global level.
cor.test(
  r2_global$Production,
  r2_global$N_use_others
)

t.test(
  r2_global$Production,
  r2_global$N_use_others
)

summary(
  lm(
    data = r2_global,
    NPK_others / 1000000 ~ Production
  )
)


# United States.
r2_USA <- United_States_bioethanol %>%
  
  left_join(
    US_NPK_maize,
    by = "Year"
  ) %>%
  
  drop_na() %>%
  
  rowwise() %>%
  
  mutate(
    
    NPK_others =
      sum(
        N_use_others,
        P_use_others,
        K_use_others
      ),
    
    NPK_Fuel_rat =
      NPK_others /
      (
        Production *
          1000000000
      )
  )


cor.test(
  r2_USA$Production,
  r2_USA$N_use_others
)

summary(
  lm(
    data = r2_USA,
    NPK_others / 1000000000 ~ Production
  )
)


# Brazil.
r2_Brazil <- Brazil_bioethanol %>%
  
  left_join(
    Brazil_NPK_maize,
    by = "Year"
  ) %>%
  
  drop_na() %>%
  
  rowwise() %>%
  
  mutate(
    
    NPK_others =
      sum(
        N_use_others,
        P_use_others,
        K_use_others
      ),
    
    NPK_Fuel_rat =
      NPK_others /
      (
        Production *
          1000000000
      )
  )


cor.test(
  r2_Brazil$Production,
  r2_Brazil$N_use_others
)

summary(
  lm(
    data = r2_Brazil,
    NPK_others / 1000000000 ~ Production
  )
)


# Extended Data Figure 3 source data
# Global source data

ED3_source_Global <- Maize_N_global %>%
  
  select(
    Year,
    N_use_others,
    N_use_others_sd
  ) %>%
  
  full_join(
    Maize_P_global %>%
      select(
        Year,
        P_use_others,
        P_use_others_sd
      ),
    by = "Year"
  ) %>%
  
  full_join(
    Maize_K_global %>%
      select(
        Year,
        K_use_others,
        K_use_others_sd
      ),
    by = "Year"
  ) %>%
  
  full_join(
    Bioethanol_global %>%
      select(
        Year,
        Bioethanol_MMTOE =
          Production
      ),
    by = "Year"
  ) %>%
  
  transmute(
    Year,
    
    N_others_mean_Tg =
      N_use_others /
      1e6,
    
    N_others_SD_Tg =
      N_use_others_sd /
      1e6,
    
    P_others_mean_Tg =
      P_use_others /
      1e6,
    
    P_others_SD_Tg =
      P_use_others_sd /
      1e6,
    
    K_others_mean_Tg =
      K_use_others /
      1e6,
    
    K_others_SD_Tg =
      K_use_others_sd /
      1e6,
    
    Bioethanol_MMTOE
  ) %>%
  
  arrange(
    Year
  )

ED3_source_USA <- US_NPK_maize %>%
  
  select(
    Year,
    N_use_others,
    N_use_others_sd,
    P_use_others,
    P_use_others_sd,
    K_use_others,
    K_use_others_sd
  ) %>%
  
  full_join(
    United_States_bioethanol %>%
      select(
        Year,
        Bioethanol_MMTOE =
          Production
      ),
    by = "Year"
  ) %>%
  
  transmute(
    Year,
    
    N_others_mean_Tg =
      N_use_others /
      1e9,
    
    N_others_SD_Tg =
      N_use_others_sd /
      1e9,
    
    P_others_mean_Tg =
      P_use_others /
      1e9,
    
    P_others_SD_Tg =
      P_use_others_sd /
      1e9,
    
    K_others_mean_Tg =
      K_use_others /
      1e9,
    
    K_others_SD_Tg =
      K_use_others_sd /
      1e9,
    
    Bioethanol_MMTOE
  ) %>%
  
  arrange(
    Year
  )

# Brazil source data

ED3_source_Brazil <- Brazil_NPK_maize %>%
  
  select(
    Year,
    N_use_others,
    N_use_others_sd,
    P_use_others,
    P_use_others_sd,
    K_use_others,
    K_use_others_sd
  ) %>%
  
  full_join(
    Brazil_bioethanol %>%
      select(
        Year,
        Bioethanol_MMTOE =
          Production
      ),
    by = "Year"
  ) %>%
  
  transmute(
    Year,
    
    N_others_mean_Tg =
      N_use_others /
      1e9,
    
    N_others_SD_Tg =
      N_use_others_sd /
      1e9,
    
    P_others_mean_Tg =
      P_use_others /
      1e9,
    
    P_others_SD_Tg =
      P_use_others_sd /
      1e9,
    
    K_others_mean_Tg =
      K_use_others /
      1e9,
    
    K_others_SD_Tg =
      K_use_others_sd /
      1e9,
    
    Bioethanol_MMTOE
  ) %>%
  
  arrange(
    Year
  )

# Extended Data Figure 3 source data

ED3_source_data <- bind_rows(
  
  # Panel a: Global
  ED3_source_Global %>%
    mutate(
      Panel = "a",
      Country = "Global"
    ),
  
  # Panel b: United States
  ED3_source_USA %>%
    mutate(
      Panel = "b",
      Country = "United States"
    ),
  
  # Panel c: Brazil
  ED3_source_Brazil %>%
    mutate(
      Panel = "c",
      Country = "Brazil"
    )
  
) %>%
  
  select(
    Panel,
    Country,
    Year,
    everything()
  ) %>%
  
  arrange(
    Panel,
    Year
  )


ED3_source_data


# Export

write_csv(
  ED3_source_data,
  "results/Extended_Data_Figure_3_source_data.csv"
)
