# ==============================================================================
# 07_additive_decomposition.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
## Quantify the contributions of changes in fertilizer application rates,
## harvested area, and crop-use allocation to changes in N, P, and K fertilizer
## use associated with food, feed, and other uses. The analysis applies an
## additive Logarithmic Mean Divisia Index (LMDI) (Ang, 2005) decomposition across countries
## and crops, while propagating uncertainty through the Monte Carlo simulations
## generated in the preceding analyses.
##
## Ang, B. W. The LMDI approach to decomposition analysis: a practical guide. Energy Policy 33, 867–871 (2005).
#
# Analysis:
# 1. Load crop-use fractions and fertilizer-use simulations.
# 2. Construct country-, crop-, nutrient-, and use-specific fertilizer-use
#    datasets.
# 3. Harmonize historical country boundaries around 1990.
# 4. Apply an additive decomposition to changes in fertilizer use.
# 5. Repeat the decomposition across Monte Carlo simulations.
# 6. Export country-, crop-, and global-level decomposition results.
# ==============================================================================


# Loading libraries -------------------------------------------------------

library(readxl)
library(tidyverse)
library(parallel)
library(doParallel)

# Loading data ------------------------------------------------------------

# Country code correspondence ---------------------------------------------
#
# Used to link the ISO3 country codes contained in the fertilizer-use
# simulations generated in Script 05 with FAOSTAT area codes used throughout
# the crop-use allocation datasets.

Countries_DB <- read_excel("data/Countries_DB.xlsx") %>%
  rename(ISO3 = 2, FAOStat_area_code = 3) %>%
  select(
    ISO3,
    FAOStat_area_code
  )

# Crop-use allocation datasets --------------------------------------------
#
# Crop-use fractions generated in Script 04.
# Historical Belgium-Luxembourg observations are retained under the
# harmonized FAOSTAT area code used in the analysis.

Wheat_uses <- read_csv("results/Wheat_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

maize_uses <- read_csv("results/maize_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

rice_uses <- read_csv("results/rice_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

soybean_uses <- read_csv("results/soybean_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)


# Fertilizer application-rate simulations ---------------------------------
#
# Load the 500 application-rate simulations generated in Script 05.
# Only the Sim_* columns are retained to avoid selecting the additional
# total-use columns contained in the complete simulation files.

# Wheat
Wheat_sim_N <- read_csv("results/Wheat_sim_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Navg,
    Area_FAOStat,
    starts_with("Sim_")
  )

Wheat_sim_P <- read_csv("results/Wheat_sim_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Pavg,
    Area_FAOStat,
    starts_with("Sim_")
  )

Wheat_sim_K <- read_csv("results/Wheat_sim_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Kavg,
    Area_FAOStat,
    starts_with("Sim_")
  )


# Maize
maize_sim_N <- read_csv("results/Maize_sim_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Navg,
    Area_FAOStat,
    starts_with("Sim_")
  )

maize_sim_P <- read_csv("results/Maize_sim_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Pavg,
    Area_FAOStat,
    starts_with("Sim_")
  )

maize_sim_K <- read_csv("results/Maize_sim_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Kavg,
    Area_FAOStat,
    starts_with("Sim_")
  )


# Rice
rice_sim_N <- read_csv("results/Rice_sim_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Navg,
    Area_FAOStat,
    starts_with("Sim_")
  )

rice_sim_P <- read_csv("results/Rice_sim_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Pavg,
    Area_FAOStat,
    starts_with("Sim_")
  )

rice_sim_K <- read_csv("results/Rice_sim_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Kavg,
    Area_FAOStat,
    starts_with("Sim_")
  )


# Soybean
soybean_sim_N <- read_csv("results/Soybean_sim_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Navg,
    Area_FAOStat,
    starts_with("Sim_")
  )

soybean_sim_P <- read_csv("results/Soybean_sim_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Pavg,
    Area_FAOStat,
    starts_with("Sim_")
  )

soybean_sim_K <- read_csv("results/Soybean_sim_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Kavg,
    Area_FAOStat,
    starts_with("Sim_")
  )


# Monte Carlo total fertilizer-use estimates ------------------------------

# Wheat
Wheat_mc_N <- read_csv("results/Wheat_mc_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    N_wheat = Nut_crop
  )

Wheat_mc_P <- read_csv("results/Wheat_mc_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    P_wheat = Nut_crop
  )

Wheat_mc_K <- read_csv("results/Wheat_mc_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    K_wheat = Nut_crop
  )


# Maize
maize_mc_N <- read_csv("results/Maize_mc_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    N_maize = Nut_crop
  )

maize_mc_P <- read_csv("results/Maize_mc_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    P_maize = Nut_crop
  )

maize_mc_K <- read_csv("results/Maize_mc_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    K_maize = Nut_crop
  )


# Rice
rice_mc_N <- read_csv("results/Rice_mc_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    N_rice = Nut_crop
  )

rice_mc_P <- read_csv("results/Rice_mc_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    P_rice = Nut_crop
  )

rice_mc_K <- read_csv("results/Rice_mc_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    K_rice = Nut_crop
  )


# Soybean
soybean_mc_N <- read_csv("results/Soybean_mc_N_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    N_soybean = Nut_crop
  )

soybean_mc_P <- read_csv("results/Soybean_mc_P_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    P_soybean = Nut_crop
  )

soybean_mc_K <- read_csv("results/Soybean_mc_K_all.csv") %>%
  left_join(
    Countries_DB,
    by = c("ISO_3_Code" = "ISO3")
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Nut_crop,
    Iter
  ) %>%
  rename(
    K_soybean = Nut_crop
  )


# FAOSTAT fertilizer-use availability -------------------------------------
#
# National fertilizer-use records are used as an availability mask.
# Historical Belgium-Luxembourg codes are harmonized with the coding used
# throughout the analysis.

N_total <- read_csv("data/N_total_FAOStat.csv") %>%
  filter(
    FAOStat_area_code != 255 |
      Year > 1999
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  )

P2O5_total <- read_csv("data/P2O5_total_FAOStat.csv") %>%
  filter(
    FAOStat_area_code != 255 |
      Year > 1999
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  )

K2O_total <- read_csv("data/K2O_total_FAOStat.csv") %>%
  filter(
    FAOStat_area_code != 255 |
      Year > 1999
  ) %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15 & Year < 2000,
      255,
      FAOStat_area_code
    )
  )


# Creating fertilizer-use databases ---------------------------------------
#
# Allocate each simulated fertilizer application rate among food, feed, and
# other uses according to the crop-use fractions generated in Script 04.
#
# Processing is allocated to food for wheat, maize, and rice. For soybean,
# processing is divided between food (19%) and feed (81%).
#
# FAOSTAT area code 351 (aggregate China) is excluded because China is
# represented through its constituent FAOSTAT territories.
#
# National FAOSTAT fertilizer-use records are used as an availability mask:
# where no corresponding national nutrient-use observation is available,
# fertilizer use for that crop-country-year is set to zero.


## Wheat -------------------------------------------------------------------

## Nitrogen

N_wheat_uses <- Wheat_sim_N %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    Wheat_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    N_use_food = per_Food * Value * Area_FAOStat,
    N_use_feed = per_Feed * Value * Area_FAOStat,
    N_use_processing = per_Processing * Value * Area_FAOStat,
    N_use_seed = per_Seed * Value * Area_FAOStat,
    N_use_losses = per_Losses * Value * Area_FAOStat,
    N_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    N_use_food = sum(
      N_use_food,
      N_use_processing,
      na.rm = TRUE
    ),
    N_use_others = sum(
      N_use_seed,
      N_use_losses,
      N_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_1"
  ) %>%
  ungroup() %>%
  left_join(
    N_total %>%
      filter(Crop_Code == "1_1"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    N_use_food = ifelse(
      is.na(Country_total_N),
      0,
      N_use_food
    ),
    N_use_feed = ifelse(
      is.na(Country_total_N),
      0,
      N_use_feed
    ),
    N_use_others = ifelse(
      is.na(Country_total_N),
      0,
      N_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    N_use_food,
    N_use_feed,
    N_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Phosphorus

P_wheat_uses <- Wheat_sim_P %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    Wheat_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    P_use_food = per_Food * Value * Area_FAOStat,
    P_use_feed = per_Feed * Value * Area_FAOStat,
    P_use_processing = per_Processing * Value * Area_FAOStat,
    P_use_seed = per_Seed * Value * Area_FAOStat,
    P_use_losses = per_Losses * Value * Area_FAOStat,
    P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    P_use_food = sum(
      P_use_food,
      P_use_processing,
      na.rm = TRUE
    ),
    P_use_others = sum(
      P_use_seed,
      P_use_losses,
      P_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_1"
  ) %>%
  ungroup() %>%
  left_join(
    P2O5_total %>%
      filter(Crop_Code == "1_1"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    P_use_food = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_food
    ),
    P_use_feed = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_feed
    ),
    P_use_others = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    P_use_food,
    P_use_feed,
    P_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Potassium

K_wheat_uses <- Wheat_sim_K %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    Wheat_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    K_use_food = per_Food * Value * Area_FAOStat,
    K_use_feed = per_Feed * Value * Area_FAOStat,
    K_use_processing = per_Processing * Value * Area_FAOStat,
    K_use_seed = per_Seed * Value * Area_FAOStat,
    K_use_losses = per_Losses * Value * Area_FAOStat,
    K_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    K_use_food = sum(
      K_use_food,
      K_use_processing,
      na.rm = TRUE
    ),
    K_use_others = sum(
      K_use_seed,
      K_use_losses,
      K_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_1"
  ) %>%
  ungroup() %>%
  left_join(
    K2O_total %>%
      filter(Crop_Code == "1_1"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    K_use_food = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_food
    ),
    K_use_feed = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_feed
    ),
    K_use_others = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    K_use_food,
    K_use_feed,
    K_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Maize -------------------------------------------------------------------

## Nitrogen

N_maize_uses <- maize_sim_N %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    maize_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    N_use_food = per_Food * Value * Area_FAOStat,
    N_use_feed = per_Feed * Value * Area_FAOStat,
    N_use_processing = per_Processing * Value * Area_FAOStat,
    N_use_seed = per_Seed * Value * Area_FAOStat,
    N_use_losses = per_Losses * Value * Area_FAOStat,
    N_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    N_use_food = sum(
      N_use_food,
      N_use_processing,
      na.rm = TRUE
    ),
    N_use_others = sum(
      N_use_seed,
      N_use_losses,
      N_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_2"
  ) %>%
  ungroup() %>%
  left_join(
    N_total %>%
      filter(Crop_Code == "1_2"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    N_use_food = ifelse(
      is.na(Country_total_N),
      0,
      N_use_food
    ),
    N_use_feed = ifelse(
      is.na(Country_total_N),
      0,
      N_use_feed
    ),
    N_use_others = ifelse(
      is.na(Country_total_N),
      0,
      N_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    N_use_food,
    N_use_feed,
    N_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Phosphorus

P_maize_uses <- maize_sim_P %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    maize_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    P_use_food = per_Food * Value * Area_FAOStat,
    P_use_feed = per_Feed * Value * Area_FAOStat,
    P_use_processing = per_Processing * Value * Area_FAOStat,
    P_use_seed = per_Seed * Value * Area_FAOStat,
    P_use_losses = per_Losses * Value * Area_FAOStat,
    P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    P_use_food = sum(
      P_use_food,
      P_use_processing,
      na.rm = TRUE
    ),
    P_use_others = sum(
      P_use_seed,
      P_use_losses,
      P_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_2"
  ) %>%
  ungroup() %>%
  left_join(
    P2O5_total %>%
      filter(Crop_Code == "1_2"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    P_use_food = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_food
    ),
    P_use_feed = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_feed
    ),
    P_use_others = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    P_use_food,
    P_use_feed,
    P_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Potassium

K_maize_uses <- maize_sim_K %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    maize_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    K_use_food = per_Food * Value * Area_FAOStat,
    K_use_feed = per_Feed * Value * Area_FAOStat,
    K_use_processing = per_Processing * Value * Area_FAOStat,
    K_use_seed = per_Seed * Value * Area_FAOStat,
    K_use_losses = per_Losses * Value * Area_FAOStat,
    K_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    K_use_food = sum(
      K_use_food,
      K_use_processing,
      na.rm = TRUE
    ),
    K_use_others = sum(
      K_use_seed,
      K_use_losses,
      K_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_2"
  ) %>%
  ungroup() %>%
  left_join(
    K2O_total %>%
      filter(Crop_Code == "1_2"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    K_use_food = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_food
    ),
    K_use_feed = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_feed
    ),
    K_use_others = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    K_use_food,
    K_use_feed,
    K_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Rice --------------------------------------------------------------------

## Nitrogen

N_rice_uses <- rice_sim_N %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    rice_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    N_use_food = per_Food * Value * Area_FAOStat,
    N_use_feed = per_Feed * Value * Area_FAOStat,
    N_use_processing = per_Processing * Value * Area_FAOStat,
    N_use_seed = per_Seed * Value * Area_FAOStat,
    N_use_losses = per_Losses * Value * Area_FAOStat,
    N_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    N_use_food = sum(
      N_use_food,
      N_use_processing,
      na.rm = TRUE
    ),
    N_use_others = sum(
      N_use_seed,
      N_use_losses,
      N_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_3"
  ) %>%
  ungroup() %>%
  left_join(
    N_total %>%
      filter(Crop_Code == "1_3"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    N_use_food = ifelse(
      is.na(Country_total_N),
      0,
      N_use_food
    ),
    N_use_feed = ifelse(
      is.na(Country_total_N),
      0,
      N_use_feed
    ),
    N_use_others = ifelse(
      is.na(Country_total_N),
      0,
      N_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    N_use_food,
    N_use_feed,
    N_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Phosphorus

P_rice_uses <- rice_sim_P %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    rice_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    P_use_food = per_Food * Value * Area_FAOStat,
    P_use_feed = per_Feed * Value * Area_FAOStat,
    P_use_processing = per_Processing * Value * Area_FAOStat,
    P_use_seed = per_Seed * Value * Area_FAOStat,
    P_use_losses = per_Losses * Value * Area_FAOStat,
    P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    P_use_food = sum(
      P_use_food,
      P_use_processing,
      na.rm = TRUE
    ),
    P_use_others = sum(
      P_use_seed,
      P_use_losses,
      P_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_3"
  ) %>%
  ungroup() %>%
  left_join(
    P2O5_total %>%
      filter(Crop_Code == "1_3"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    P_use_food = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_food
    ),
    P_use_feed = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_feed
    ),
    P_use_others = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    P_use_food,
    P_use_feed,
    P_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Potassium

K_rice_uses <- rice_sim_K %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    rice_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    K_use_food = per_Food * Value * Area_FAOStat,
    K_use_feed = per_Feed * Value * Area_FAOStat,
    K_use_processing = per_Processing * Value * Area_FAOStat,
    K_use_seed = per_Seed * Value * Area_FAOStat,
    K_use_losses = per_Losses * Value * Area_FAOStat,
    K_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    K_use_food = sum(
      K_use_food,
      K_use_processing,
      na.rm = TRUE
    ),
    K_use_others = sum(
      K_use_seed,
      K_use_losses,
      K_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing,
      na.rm = TRUE
    ),
    P_Feed = per_Feed,
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "1_3"
  ) %>%
  ungroup() %>%
  left_join(
    K2O_total %>%
      filter(Crop_Code == "1_3"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    K_use_food = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_food
    ),
    K_use_feed = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_feed
    ),
    K_use_others = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    K_use_food,
    K_use_feed,
    K_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Soybean -----------------------------------------------------------------
#
# Soybean processing is allocated between food (19%) and feed (81%).

## Nitrogen

N_soybean_uses <- soybean_sim_N %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    soybean_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    N_use_food = per_Food * Value * Area_FAOStat,
    N_use_feed = per_Feed * Value * Area_FAOStat,
    N_use_processing = per_Processing * Value * Area_FAOStat,
    N_use_seed = per_Seed * Value * Area_FAOStat,
    N_use_losses = per_Losses * Value * Area_FAOStat,
    N_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    N_use_food = sum(
      N_use_food,
      N_use_processing * 0.19,
      na.rm = TRUE
    ),
    N_use_feed = sum(
      N_use_feed,
      N_use_processing * 0.81,
      na.rm = TRUE
    ),
    N_use_others = sum(
      N_use_seed,
      N_use_losses,
      N_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing * 0.19,
      na.rm = TRUE
    ),
    P_Feed = sum(
      per_Feed,
      per_Processing * 0.81,
      na.rm = TRUE
    ),
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "2_1"
  ) %>%
  ungroup() %>%
  left_join(
    N_total %>%
      filter(Crop_Code == "2_1"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    N_use_food = ifelse(
      is.na(Country_total_N),
      0,
      N_use_food
    ),
    N_use_feed = ifelse(
      is.na(Country_total_N),
      0,
      N_use_feed
    ),
    N_use_others = ifelse(
      is.na(Country_total_N),
      0,
      N_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    N_use_food,
    N_use_feed,
    N_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Phosphorus

P_soybean_uses <- soybean_sim_P %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    soybean_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    P_use_food = per_Food * Value * Area_FAOStat,
    P_use_feed = per_Feed * Value * Area_FAOStat,
    P_use_processing = per_Processing * Value * Area_FAOStat,
    P_use_seed = per_Seed * Value * Area_FAOStat,
    P_use_losses = per_Losses * Value * Area_FAOStat,
    P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    P_use_food = sum(
      P_use_food,
      P_use_processing * 0.19,
      na.rm = TRUE
    ),
    P_use_feed = sum(
      P_use_feed,
      P_use_processing * 0.81,
      na.rm = TRUE
    ),
    P_use_others = sum(
      P_use_seed,
      P_use_losses,
      P_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing * 0.19,
      na.rm = TRUE
    ),
    P_Feed = sum(
      per_Feed,
      per_Processing * 0.81,
      na.rm = TRUE
    ),
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "2_1"
  ) %>%
  ungroup() %>%
  left_join(
    P2O5_total %>%
      filter(Crop_Code == "2_1"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    P_use_food = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_food
    ),
    P_use_feed = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_feed
    ),
    P_use_others = ifelse(
      is.na(Country_total_P2O5),
      0,
      P_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    P_use_food,
    P_use_feed,
    P_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)


## Potassium

K_soybean_uses <- soybean_sim_K %>%
  pivot_longer(
    cols = starts_with("Sim_"),
    names_to = "Sim",
    values_to = "Value"
  ) %>%
  left_join(
    soybean_uses,
    by = c("FAOStat_area_code", "Year")
  ) %>%
  filter(
    Year < 2020,
    FAOStat_area_code != 351
  ) %>%
  mutate(
    K_use_food = per_Food * Value * Area_FAOStat,
    K_use_feed = per_Feed * Value * Area_FAOStat,
    K_use_processing = per_Processing * Value * Area_FAOStat,
    K_use_seed = per_Seed * Value * Area_FAOStat,
    K_use_losses = per_Losses * Value * Area_FAOStat,
    K_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat
  ) %>%
  rowwise() %>%
  mutate(
    K_use_food = sum(
      K_use_food,
      K_use_processing * 0.19,
      na.rm = TRUE
    ),
    K_use_feed = sum(
      K_use_feed,
      K_use_processing * 0.81,
      na.rm = TRUE
    ),
    K_use_others = sum(
      K_use_seed,
      K_use_losses,
      K_use_other_uses_nf,
      na.rm = TRUE
    ),
    P_Food = sum(
      per_Food,
      per_Processing * 0.19,
      na.rm = TRUE
    ),
    P_Feed = sum(
      per_Feed,
      per_Processing * 0.81,
      na.rm = TRUE
    ),
    P_Others = sum(
      per_Losses,
      per_Seed,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    Crop_code = "2_1"
  ) %>%
  ungroup() %>%
  left_join(
    K2O_total %>%
      filter(Crop_Code == "2_1"),
    by = c("FAOStat_area_code", "Year")
  ) %>%
  mutate(
    K_use_food = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_food
    ),
    K_use_feed = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_feed
    ),
    K_use_others = ifelse(
      is.na(Country_total_K2O),
      0,
      K_use_others
    )
  ) %>%
  select(
    Year,
    FAOStat_area_code,
    Crop_code,
    Sim,
    K_use_food,
    K_use_feed,
    K_use_others,
    Area_FAOStat,
    Value,
    P_Food,
    P_Feed,
    P_Others
  ) %>%
  drop_na(Area_FAOStat)

# Harmonize historical country boundaries ---------------------------------
#
# Aggregate successor states to historical country units to ensure
# geographically consistent comparisons across the decomposition period:
#
# USSR          -> FAOSTAT area code 228
# Yugoslavia    -> FAOSTAT area code 248
# Czechoslovakia -> FAOSTAT area code 51
#
# Fertilizer use and harvested area are summed across successor states.
# Crop-use fractions and fertilizer application rates are calculated as
# harvested-area-weighted means.


# Historical country groupings

FAOStat_area_code_USSR <- c(
  1, 52, 57, 63, 73, 108, 113, 119,
  126, 146, 185, 208, 213, 230, 235
)

FAOStat_area_code_Yug <- c(
  80, 98, 154, 186, 198, 272, 273
)

FAOStat_area_code_CSVK <- c(
  167, 199
)


## Function to harmonize country boundaries --------------------------------

harmonize_1990 <- function(data, use_columns) {
  
  data %>%
    
    # Assign successor states to their corresponding historical entity.
    mutate(
      FAOStat_area_code_d1990 = case_when(
        FAOStat_area_code %in% FAOStat_area_code_USSR ~ 228,
        FAOStat_area_code %in% FAOStat_area_code_Yug  ~ 248,
        FAOStat_area_code %in% FAOStat_area_code_CSVK ~ 51,
        TRUE ~ FAOStat_area_code
      ),
      
      # Area-weighted quantities.
      P_Food_a = P_Food * Area_FAOStat,
      P_Feed_a = P_Feed * Area_FAOStat,
      P_Others_a = P_Others * Area_FAOStat,
      Value_a = Value * Area_FAOStat
    ) %>%
    
    group_by(
      FAOStat_area_code_d1990,
      Year,
      Crop_code,
      Sim
    ) %>%
    
    summarise(
      
      # Sum fertilizer use across harmonized territories.
      across(
        all_of(use_columns),
        ~ sum(.x, na.rm = TRUE)
      ),
      
      # Sum harvested area and weighted quantities.
      Area_FAOStat = sum(
        Area_FAOStat,
        na.rm = TRUE
      ),
      
      P_Food_a = sum(
        P_Food_a,
        na.rm = TRUE
      ),
      
      P_Feed_a = sum(
        P_Feed_a,
        na.rm = TRUE
      ),
      
      P_Others_a = sum(
        P_Others_a,
        na.rm = TRUE
      ),
      
      Value_a = sum(
        Value_a,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    
    # Recover area-weighted crop-use fractions and fertilizer rate.
    mutate(
      P_Food = ifelse(
        Area_FAOStat > 0,
        P_Food_a / Area_FAOStat,
        0
      ),
      
      P_Feed = ifelse(
        Area_FAOStat > 0,
        P_Feed_a / Area_FAOStat,
        0
      ),
      
      P_Others = ifelse(
        Area_FAOStat > 0,
        P_Others_a / Area_FAOStat,
        0
      ),
      
      Value = ifelse(
        Area_FAOStat > 0,
        Value_a / Area_FAOStat,
        0
      )
    ) %>%
    
    rename(
      FAOStat_area_code = FAOStat_area_code_d1990
    ) %>%
    
    select(
      Year,
      FAOStat_area_code,
      Crop_code,
      Sim,
      all_of(use_columns),
      Area_FAOStat,
      Value,
      P_Food,
      P_Feed,
      P_Others
    )
}


## Nitrogen ----------------------------------------------------------------

N_wheat_uses_1990 <- harmonize_1990(
  N_wheat_uses,
  c(
    "N_use_food",
    "N_use_feed",
    "N_use_others"
  )
)

N_maize_uses_1990 <- harmonize_1990(
  N_maize_uses,
  c(
    "N_use_food",
    "N_use_feed",
    "N_use_others"
  )
)

N_rice_uses_1990 <- harmonize_1990(
  N_rice_uses,
  c(
    "N_use_food",
    "N_use_feed",
    "N_use_others"
  )
)

N_soybean_uses_1990 <- harmonize_1990(
  N_soybean_uses,
  c(
    "N_use_food",
    "N_use_feed",
    "N_use_others"
  )
)


## Phosphorus ---------------------------------------------------------------

P_wheat_uses_1990 <- harmonize_1990(
  P_wheat_uses,
  c(
    "P_use_food",
    "P_use_feed",
    "P_use_others"
  )
)

P_maize_uses_1990 <- harmonize_1990(
  P_maize_uses,
  c(
    "P_use_food",
    "P_use_feed",
    "P_use_others"
  )
)

P_rice_uses_1990 <- harmonize_1990(
  P_rice_uses,
  c(
    "P_use_food",
    "P_use_feed",
    "P_use_others"
  )
)

P_soybean_uses_1990 <- harmonize_1990(
  P_soybean_uses,
  c(
    "P_use_food",
    "P_use_feed",
    "P_use_others"
  )
)


## Potassium ---------------------------------------------------------------

K_wheat_uses_1990 <- harmonize_1990(
  K_wheat_uses,
  c(
    "K_use_food",
    "K_use_feed",
    "K_use_others"
  )
)

K_maize_uses_1990 <- harmonize_1990(
  K_maize_uses,
  c(
    "K_use_food",
    "K_use_feed",
    "K_use_others"
  )
)

K_rice_uses_1990 <- harmonize_1990(
  K_rice_uses,
  c(
    "K_use_food",
    "K_use_feed",
    "K_use_others"
  )
)

K_soybean_uses_1990 <- harmonize_1990(
  K_soybean_uses,
  c(
    "K_use_food",
    "K_use_feed",
    "K_use_others"
  )
)

# Functions for additive decomposition ------------------------------------
#
# Changes in fertilizer use are decomposed into contributions from:
#
# dU = change in crop-use allocation
# dF = change in fertilizer application rate
# dA = change in harvested area
#
# Endpoint values used in the decomposition are represented by arithmetic
# means across a three-year window centred on the selected start and end
# years (year - 1, year, year + 1).
#
# Geographically harmonized datasets are used when the comparison spans
# the 1990 boundary, preventing changes associated with the dissolution of
# the USSR, Yugoslavia, and Czechoslovakia from being interpreted as
# fertilizer-use changes.

## Helper functions --------------------------------------------------------

# Logarithmic mean used as the additive LMDI weight.
logarithmic_mean <- function(a, b) {
  
  same <- a == b
  
  out <- (a - b) / (log(a) - log(b))
  
  # Define L(a, a) = a.
  out[same] <- a[same]
  
  # Set the logarithmic-mean weight to zero when either value is
  # non-positive or the result is not finite.
  bad <- (a <= 0) | (b <= 0) | !is.finite(out)
  
  out[bad & !same] <- 0
  
  return(out)
}


# Calculate the logarithmic ratio between endpoint values.
# A small epsilon prevents log(0).
log_ratio <- function(xT, x0, eps = 1e-12) {
  
  xT <- pmax(xT, eps)
  x0 <- pmax(x0, eps)
  
  return(
    log(xT / x0)
  )
}


# Extract the endpoint value for one country using a centred temporal
# averaging window. With k = 1, the mean is calculated across year - 1,
# year, and year + 1 when those observations are available.
get_cell <- function(
    df,
    year,
    country,
    use_var,
    k = 1,
    min_years = 1
) {
  
  yrs <- seq.int(
    year - k,
    year + k
  )
  
  x <- df %>%
    filter(
      FAOStat_area_code == country,
      Year %in% yrs
    )
  
  if (nrow(x) == 0L) {
    
    return(
      tibble(
        nutrient_use = 0,
        Area_FAOStat = 0,
        Fert_ha = 0,
        P_Use = 0
      )
    )
  }
  
  # Number of years containing information for at least one component.
  n_obs <- n_distinct(
    x$Year[
      !is.na(x[[use_var]]) |
        !is.na(x$Area_FAOStat) |
        !is.na(x$Fert_ha) |
        !is.na(x$P_Use)
    ]
  )
  
  if (n_obs < min_years) {
    
    return(
      tibble(
        nutrient_use = 0,
        Area_FAOStat = 0,
        Fert_ha = 0,
        P_Use = 0
      )
    )
  }
  
  out <- x %>%
    summarise(
      nutrient_use = mean(
        .data[[use_var]],
        na.rm = TRUE
      ),
      Area_FAOStat = mean(
        Area_FAOStat,
        na.rm = TRUE
      ),
      Fert_ha = mean(
        Fert_ha,
        na.rm = TRUE
      ),
      P_Use = mean(
        P_Use,
        na.rm = TRUE
      )
    )
  
  # When all observations for a variable are missing, mean(..., na.rm = TRUE)
  # returns NaN. Replace these values by zero for decomposition robustness.
  out[is.na(out)] <- 0
  
  return(out)
}


`%notin%` <- function(x, table) {
  !(x %in% table)
}



## Nitrogen decomposition --------------------------------------------------

global_decomposition_N <- function(
    year_s,
    year_e,
    Use,
    Sim_n,
    normalize = c("both", "local", "global")
) {
  
  # Retained for compatibility with previous function calls.
  normalize <- match.arg(
    normalize,
    c("both", "local", "global")
  )
  
  
  # Select geographic representation -------------------------------------
  
  crosses_1990 <- min(year_s, year_e) < 1990 &&
    max(year_s, year_e) >= 1990
  
  if (crosses_1990) {
    
    N_soybean_data <- N_soybean_uses_1990
    N_maize_data   <- N_maize_uses_1990
    N_rice_data    <- N_rice_uses_1990
    N_wheat_data   <- N_wheat_uses_1990
    
  } else {
    
    N_soybean_data <- N_soybean_uses
    N_maize_data   <- N_maize_uses
    N_rice_data    <- N_rice_uses
    N_wheat_data   <- N_wheat_uses
  }
  
  
  # Retain selected Monte Carlo simulation -------------------------------
  
  N_soybean_uses1 <- N_soybean_data %>%
    filter(Sim == Sim_n)
  
  N_maize_uses1 <- N_maize_data %>%
    filter(Sim == Sim_n)
  
  N_rice_uses1 <- N_rice_data %>%
    filter(Sim == Sim_n)
  
  N_wheat_uses1 <- N_wheat_data %>%
    filter(Sim == Sim_n)
  
  
  # Select fertilizer-use category ---------------------------------------
  
  pick <- if (Use == "Food") {
    
    c(
      "Year",
      "FAOStat_area_code",
      "N_use_food",
      "Area_FAOStat",
      "Value",
      "P_Food",
      "Crop_code"
    )
    
  } else if (Use == "Feed") {
    
    c(
      "Year",
      "FAOStat_area_code",
      "N_use_feed",
      "Area_FAOStat",
      "Value",
      "P_Feed",
      "Crop_code"
    )
    
  } else {
    
    c(
      "Year",
      "FAOStat_area_code",
      "N_use_others",
      "Area_FAOStat",
      "Value",
      "P_Others",
      "Crop_code"
    )
  }
  
  
  N_soybean_uses1 <- N_soybean_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      N_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  N_maize_uses1 <- N_maize_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      N_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  N_rice_uses1 <- N_rice_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      N_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  N_wheat_uses1 <- N_wheat_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      N_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  
  # Identify countries present at both endpoints -------------------------
  
  countries_soybean <- inner_join(
    N_soybean_uses1 %>%
      filter(
        Year == year_s,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    N_soybean_uses1 %>%
      filter(
        Year == year_e,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_maize <- inner_join(
    N_maize_uses1 %>%
      filter(
        Year == year_s,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    N_maize_uses1 %>%
      filter(
        Year == year_e,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_rice <- inner_join(
    N_rice_uses1 %>%
      filter(
        Year == year_s,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    N_rice_uses1 %>%
      filter(
        Year == year_e,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_wheat <- inner_join(
    N_wheat_uses1 %>%
      filter(
        Year == year_s,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    N_wheat_uses1 %>%
      filter(
        Year == year_e,
        N_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries <- bind_rows(
    countries_soybean,
    countries_maize,
    countries_rice,
    countries_wheat
  ) %>%
    distinct()
  
  
  # Global endpoint totals ------------------------------------------------
  
  N0_soy_all <- N_soybean_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_soybean$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N0_mai_all <- N_maize_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_maize$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N0_ric_all <- N_rice_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_rice$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N0_whe_all <- N_wheat_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_wheat$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N0_Use_all <- sum(
    N0_soy_all,
    N0_mai_all,
    N0_ric_all,
    N0_whe_all,
    na.rm = TRUE
  )
  
  
  N1_soy_all <- N_soybean_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_soybean$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N1_mai_all <- N_maize_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_maize$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N1_ric_all <- N_rice_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_rice$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N1_whe_all <- N_wheat_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_wheat$FAOStat_area_code
    ) %>%
    summarise(
      N_use = sum(N_use, na.rm = TRUE) / 1000
    ) %>%
    pull(N_use)
  
  N1_Use_all <- sum(
    N1_soy_all,
    N1_mai_all,
    N1_ric_all,
    N1_whe_all,
    na.rm = TRUE
  )
  
  Delta_global <- N1_Use_all - N0_Use_all
  
  
  # Additive decomposition ------------------------------------------------
  
  nC <- nrow(countries)
  
  rows_crop <- vector(
    "list",
    nC
  )
  
  rows_country <- vector(
    "list",
    nC
  )
  
  
  for (idx in seq_len(nC)) {
    
    country <- countries$FAOStat_area_code[idx]
    
    
    # Three-year mean values centred on start and end years.
    
    N0_soy <- get_cell(
      N_soybean_uses1,
      year_s,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    N0_mai <- get_cell(
      N_maize_uses1,
      year_s,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    N0_ric <- get_cell(
      N_rice_uses1,
      year_s,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    N0_whe <- get_cell(
      N_wheat_uses1,
      year_s,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    
    N1_soy <- get_cell(
      N_soybean_uses1,
      year_e,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    N1_mai <- get_cell(
      N_maize_uses1,
      year_e,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    N1_ric <- get_cell(
      N_rice_uses1,
      year_e,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    N1_whe <- get_cell(
      N_wheat_uses1,
      year_e,
      country,
      "N_use"
    ) %>%
      mutate(
        nutrient_use = nutrient_use / 1000
      )
    
    
    # Country totals.
    
    N0_all <- sum(
      N0_soy$nutrient_use,
      N0_mai$nutrient_use,
      N0_ric$nutrient_use,
      N0_whe$nutrient_use,
      na.rm = TRUE
    )
    
    N1_all <- sum(
      N1_soy$nutrient_use,
      N1_mai$nutrient_use,
      N1_ric$nutrient_use,
      N1_whe$nutrient_use,
      na.rm = TRUE
    )
    
    
    # Additive LMDI weights.
    
    Wi_soy <- logarithmic_mean(
      N1_soy$nutrient_use,
      N0_soy$nutrient_use
    )
    
    Wi_maize <- logarithmic_mean(
      N1_mai$nutrient_use,
      N0_mai$nutrient_use
    )
    
    Wi_rice <- logarithmic_mean(
      N1_ric$nutrient_use,
      N0_ric$nutrient_use
    )
    
    Wi_wheat <- logarithmic_mean(
      N1_whe$nutrient_use,
      N0_whe$nutrient_use
    )
    
    
    # Change in crop-use allocation.
    
    dU <- c(
      soybean = Wi_soy *
        log_ratio(
          N1_soy$P_Use,
          N0_soy$P_Use
        ),
      
      maize = Wi_maize *
        log_ratio(
          N1_mai$P_Use,
          N0_mai$P_Use
        ),
      
      rice = Wi_rice *
        log_ratio(
          N1_ric$P_Use,
          N0_ric$P_Use
        ),
      
      wheat = Wi_wheat *
        log_ratio(
          N1_whe$P_Use,
          N0_whe$P_Use
        )
    )
    
    
    # Change in fertilizer application rate.
    
    dF <- c(
      soybean = Wi_soy *
        log_ratio(
          N1_soy$Fert_ha,
          N0_soy$Fert_ha
        ),
      
      maize = Wi_maize *
        log_ratio(
          N1_mai$Fert_ha,
          N0_mai$Fert_ha
        ),
      
      rice = Wi_rice *
        log_ratio(
          N1_ric$Fert_ha,
          N0_ric$Fert_ha
        ),
      
      wheat = Wi_wheat *
        log_ratio(
          N1_whe$Fert_ha,
          N0_whe$Fert_ha
        )
    )
    
    
    # Change in harvested area.
    
    dA <- c(
      soybean = Wi_soy *
        log_ratio(
          N1_soy$Area_FAOStat,
          N0_soy$Area_FAOStat
        ),
      
      maize = Wi_maize *
        log_ratio(
          N1_mai$Area_FAOStat,
          N0_mai$Area_FAOStat
        ),
      
      rice = Wi_rice *
        log_ratio(
          N1_ric$Area_FAOStat,
          N0_ric$Area_FAOStat
        ),
      
      wheat = Wi_wheat *
        log_ratio(
          N1_whe$Area_FAOStat,
          N0_whe$Area_FAOStat
        )
    )
    
    
    # Country × crop contributions.
    
    rows_crop[[idx]] <- tibble(
      Country = country,
      crop = c(
        "soybean",
        "maize",
        "rice",
        "wheat"
      ),
      
      N0 = c(
        N0_soy$nutrient_use,
        N0_mai$nutrient_use,
        N0_ric$nutrient_use,
        N0_whe$nutrient_use
      ),
      
      N1 = c(
        N1_soy$nutrient_use,
        N1_mai$nutrient_use,
        N1_ric$nutrient_use,
        N1_whe$nutrient_use
      ),
      
      dU = as.numeric(dU),
      dF = as.numeric(dF),
      dA = as.numeric(dA)
    ) %>%
      mutate(
        d_total = dU + dF + dA,
        Delta_N = N1 - N0
      )
    
    
    # Country-level contributions.
    
    dU_tot <- sum(
      dU,
      na.rm = TRUE
    )
    
    dF_tot <- sum(
      dF,
      na.rm = TRUE
    )
    
    dA_tot <- sum(
      dA,
      na.rm = TRUE
    )
    
    d_tot <- dU_tot + dF_tot + dA_tot
    
    
    rows_country[[idx]] <- tibble(
      Country = country,
      N0_country = N0_all,
      N1_country = N1_all,
      Delta_country = N1_all - N0_all,
      dU = dU_tot,
      dF = dF_tot,
      dA = dA_tot,
      d_total = d_tot
    )
    
    
    # Check additive decomposition consistency.
    
    if (
      is.finite(N0_all) &&
      abs(d_tot - (N1_all - N0_all)) > 1e-10
    ) {
      
      warning(
        sprintf(
          paste0(
            "Additive exactness fail: country %s; ",
            "d_total = %.12g vs Delta_N = %.12g"
          ),
          country,
          d_tot,
          N1_all - N0_all
        )
      )
    }
  }
  
  
  # Combine country results -----------------------------------------------
  
  contrib_crop <- bind_rows(
    rows_crop
  )
  
  contrib_country <- bind_rows(
    rows_country
  )
  
  
  # Global driver contributions ------------------------------------------
  
  dU_global <- sum(
    contrib_country$dU,
    na.rm = TRUE
  )
  
  dF_global <- sum(
    contrib_country$dF,
    na.rm = TRUE
  )
  
  dA_global <- sum(
    contrib_country$dA,
    na.rm = TRUE
  )
  
  d_tot_global <- dU_global +
    dF_global +
    dA_global
  
  
  # Global crop-specific contributions.
  
  crop_global_add <- contrib_crop %>%
    group_by(crop) %>%
    summarise(
      Delta_crop = sum(
        d_total,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    complete(
      crop = c(
        "soybean",
        "maize",
        "rice",
        "wheat"
      ),
      fill = list(
        Delta_crop = 0
      )
    )
  
  
  crop_wide <- crop_global_add %>%
    pivot_wider(
      names_from = crop,
      values_from = Delta_crop,
      names_prefix = "Delta_"
    )
  
  
  # Global results.
  
  global_results <- tibble(
    Use_df = Use,
    Sim_df = Sim_n,
    year0 = year_s,
    year1 = year_e,
    N0_global = N0_Use_all,
    N1_global = N1_Use_all,
    Delta_global = Delta_global,
    dU_global = dU_global,
    dF_global = dF_global,
    dA_global = dA_global,
    d_total_global = d_tot_global
  ) %>%
    bind_cols(
      crop_wide
    )
  
  
  # Global additive consistency check.
  
  if (
    abs(d_tot_global - Delta_global) > 1e-10
  ) {
    
    warning(
      "Global additive exactness fail: sum of driver contributions != Delta_N_global."
    )
  }
  
  
  metadata <- tibble(
    Use_df = Use,
    Sim_df = Sim_n,
    year0 = year_s,
    year1 = year_e,
    harmonized_geography = crosses_1990
  )
  
  
  return(
    list(
      contrib_country = contrib_country,
      contrib_crop = contrib_crop,
      global_results = global_results,
      crop_global_add = crop_global_add,
      metadata = metadata
    )
  )
}

## Phosphorus decomposition ------------------------------------------------

global_decomposition_P <- function(
    year_s,
    year_e,
    Use,
    Sim_n,
    normalize = c("both", "local", "global")
) {
  
  normalize <- match.arg(
    normalize,
    c("both", "local", "global")
  )
  
  
  # Select geographic representation -------------------------------------
  
  crosses_1990 <- min(year_s, year_e) < 1990 &&
    max(year_s, year_e) >= 1990
  
  if (crosses_1990) {
    
    P_soybean_data <- P_soybean_uses_1990
    P_maize_data   <- P_maize_uses_1990
    P_rice_data    <- P_rice_uses_1990
    P_wheat_data   <- P_wheat_uses_1990
    
  } else {
    
    P_soybean_data <- P_soybean_uses
    P_maize_data   <- P_maize_uses
    P_rice_data    <- P_rice_uses
    P_wheat_data   <- P_wheat_uses
  }
  
  
  # Retain selected Monte Carlo simulation -------------------------------
  
  P_soybean_uses1 <- P_soybean_data %>%
    filter(Sim == Sim_n)
  
  P_maize_uses1 <- P_maize_data %>%
    filter(Sim == Sim_n)
  
  P_rice_uses1 <- P_rice_data %>%
    filter(Sim == Sim_n)
  
  P_wheat_uses1 <- P_wheat_data %>%
    filter(Sim == Sim_n)
  
  
  # Select fertilizer-use category ---------------------------------------
  
  pick <- if (Use == "Food") {
    
    c(
      "Year",
      "FAOStat_area_code",
      "P_use_food",
      "Area_FAOStat",
      "Value",
      "P_Food",
      "Crop_code"
    )
    
  } else if (Use == "Feed") {
    
    c(
      "Year",
      "FAOStat_area_code",
      "P_use_feed",
      "Area_FAOStat",
      "Value",
      "P_Feed",
      "Crop_code"
    )
    
  } else {
    
    c(
      "Year",
      "FAOStat_area_code",
      "P_use_others",
      "Area_FAOStat",
      "Value",
      "P_Others",
      "Crop_code"
    )
  }
  
  
  P_soybean_uses1 <- P_soybean_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      P_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  P_maize_uses1 <- P_maize_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      P_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  P_rice_uses1 <- P_rice_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      P_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  P_wheat_uses1 <- P_wheat_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      P_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  
  # Identify countries present at both endpoints -------------------------
  
  countries_soybean <- inner_join(
    P_soybean_uses1 %>%
      filter(
        Year == year_s,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    P_soybean_uses1 %>%
      filter(
        Year == year_e,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_maize <- inner_join(
    P_maize_uses1 %>%
      filter(
        Year == year_s,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    P_maize_uses1 %>%
      filter(
        Year == year_e,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_rice <- inner_join(
    P_rice_uses1 %>%
      filter(
        Year == year_s,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    P_rice_uses1 %>%
      filter(
        Year == year_e,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_wheat <- inner_join(
    P_wheat_uses1 %>%
      filter(
        Year == year_s,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    P_wheat_uses1 %>%
      filter(
        Year == year_e,
        P_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries <- bind_rows(
    countries_soybean,
    countries_maize,
    countries_rice,
    countries_wheat
  ) %>%
    distinct()
  
  
  # Global endpoint totals ------------------------------------------------
  
  P0_soy_all <- P_soybean_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_soybean$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P0_mai_all <- P_maize_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_maize$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P0_ric_all <- P_rice_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_rice$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P0_whe_all <- P_wheat_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_wheat$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P0_Use_all <- sum(
    P0_soy_all,
    P0_mai_all,
    P0_ric_all,
    P0_whe_all,
    na.rm = TRUE
  )
  
  
  P1_soy_all <- P_soybean_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_soybean$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P1_mai_all <- P_maize_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_maize$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P1_ric_all <- P_rice_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_rice$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P1_whe_all <- P_wheat_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_wheat$FAOStat_area_code
    ) %>%
    summarise(P_use = sum(P_use, na.rm = TRUE) / 1000) %>%
    pull(P_use)
  
  P1_Use_all <- sum(
    P1_soy_all,
    P1_mai_all,
    P1_ric_all,
    P1_whe_all,
    na.rm = TRUE
  )
  
  Delta_global <- P1_Use_all - P0_Use_all
  
  
  # Additive decomposition ------------------------------------------------
  
  nC <- nrow(countries)
  
  rows_crop <- vector("list", nC)
  rows_country <- vector("list", nC)
  
  
  for (idx in seq_len(nC)) {
    
    country <- countries$FAOStat_area_code[idx]
    
    
    # Three-year means centred on start and end years.
    
    P0_soy <- get_cell(
      P_soybean_uses1, year_s, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    P0_mai <- get_cell(
      P_maize_uses1, year_s, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    P0_ric <- get_cell(
      P_rice_uses1, year_s, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    P0_whe <- get_cell(
      P_wheat_uses1, year_s, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    
    P1_soy <- get_cell(
      P_soybean_uses1, year_e, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    P1_mai <- get_cell(
      P_maize_uses1, year_e, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    P1_ric <- get_cell(
      P_rice_uses1, year_e, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    P1_whe <- get_cell(
      P_wheat_uses1, year_e, country, "P_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    
    P0_all <- sum(
      P0_soy$nutrient_use,
      P0_mai$nutrient_use,
      P0_ric$nutrient_use,
      P0_whe$nutrient_use,
      na.rm = TRUE
    )
    
    P1_all <- sum(
      P1_soy$nutrient_use,
      P1_mai$nutrient_use,
      P1_ric$nutrient_use,
      P1_whe$nutrient_use,
      na.rm = TRUE
    )
    
    
    # Additive LMDI weights.
    
    Wi_soy <- logarithmic_mean(
      P1_soy$nutrient_use,
      P0_soy$nutrient_use
    )
    
    Wi_maize <- logarithmic_mean(
      P1_mai$nutrient_use,
      P0_mai$nutrient_use
    )
    
    Wi_rice <- logarithmic_mean(
      P1_ric$nutrient_use,
      P0_ric$nutrient_use
    )
    
    Wi_wheat <- logarithmic_mean(
      P1_whe$nutrient_use,
      P0_whe$nutrient_use
    )
    
    
    # Crop-use allocation effect.
    
    dU <- c(
      soybean = Wi_soy *
        log_ratio(P1_soy$P_Use, P0_soy$P_Use),
      
      maize = Wi_maize *
        log_ratio(P1_mai$P_Use, P0_mai$P_Use),
      
      rice = Wi_rice *
        log_ratio(P1_ric$P_Use, P0_ric$P_Use),
      
      wheat = Wi_wheat *
        log_ratio(P1_whe$P_Use, P0_whe$P_Use)
    )
    
    
    # Fertilizer-rate effect.
    
    dF <- c(
      soybean = Wi_soy *
        log_ratio(P1_soy$Fert_ha, P0_soy$Fert_ha),
      
      maize = Wi_maize *
        log_ratio(P1_mai$Fert_ha, P0_mai$Fert_ha),
      
      rice = Wi_rice *
        log_ratio(P1_ric$Fert_ha, P0_ric$Fert_ha),
      
      wheat = Wi_wheat *
        log_ratio(P1_whe$Fert_ha, P0_whe$Fert_ha)
    )
    
    
    # Harvested-area effect.
    
    dA <- c(
      soybean = Wi_soy *
        log_ratio(P1_soy$Area_FAOStat, P0_soy$Area_FAOStat),
      
      maize = Wi_maize *
        log_ratio(P1_mai$Area_FAOStat, P0_mai$Area_FAOStat),
      
      rice = Wi_rice *
        log_ratio(P1_ric$Area_FAOStat, P0_ric$Area_FAOStat),
      
      wheat = Wi_wheat *
        log_ratio(P1_whe$Area_FAOStat, P0_whe$Area_FAOStat)
    )
    
    
    # Country × crop contributions.
    
    rows_crop[[idx]] <- tibble(
      Country = country,
      crop = c("soybean", "maize", "rice", "wheat"),
      
      P0 = c(
        P0_soy$nutrient_use,
        P0_mai$nutrient_use,
        P0_ric$nutrient_use,
        P0_whe$nutrient_use
      ),
      
      P1 = c(
        P1_soy$nutrient_use,
        P1_mai$nutrient_use,
        P1_ric$nutrient_use,
        P1_whe$nutrient_use
      ),
      
      dU = as.numeric(dU),
      dF = as.numeric(dF),
      dA = as.numeric(dA)
    ) %>%
      mutate(
        d_total = dU + dF + dA,
        Delta_P = P1 - P0
      )
    
    
    # Country-level contributions.
    
    dU_tot <- sum(dU, na.rm = TRUE)
    dF_tot <- sum(dF, na.rm = TRUE)
    dA_tot <- sum(dA, na.rm = TRUE)
    
    d_tot <- dU_tot + dF_tot + dA_tot
    
    
    rows_country[[idx]] <- tibble(
      Country = country,
      P0_country = P0_all,
      P1_country = P1_all,
      Delta_country = P1_all - P0_all,
      dU = dU_tot,
      dF = dF_tot,
      dA = dA_tot,
      d_total = d_tot
    )
    
    
    # Country-level decomposition consistency.
    
    if (
      is.finite(P0_all) &&
      abs(d_tot - (P1_all - P0_all)) > 1e-10
    ) {
      
      warning(
        sprintf(
          paste0(
            "Additive exactness fail: country %s; ",
            "d_total = %.12g vs Delta_P = %.12g"
          ),
          country,
          d_tot,
          P1_all - P0_all
        )
      )
    }
  }
  
  
  # Combine results -------------------------------------------------------
  
  contrib_crop <- bind_rows(rows_crop)
  contrib_country <- bind_rows(rows_country)
  
  
  # Global driver contributions ------------------------------------------
  
  dU_global <- sum(contrib_country$dU, na.rm = TRUE)
  dF_global <- sum(contrib_country$dF, na.rm = TRUE)
  dA_global <- sum(contrib_country$dA, na.rm = TRUE)
  
  d_tot_global <- dU_global +
    dF_global +
    dA_global
  
  
  # Global crop-specific contributions.
  
  crop_global_add <- contrib_crop %>%
    group_by(crop) %>%
    summarise(
      Delta_crop = sum(d_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      crop = c(
        "soybean",
        "maize",
        "rice",
        "wheat"
      ),
      fill = list(
        Delta_crop = 0
      )
    )
  
  crop_wide <- crop_global_add %>%
    pivot_wider(
      names_from = crop,
      values_from = Delta_crop,
      names_prefix = "Delta_"
    )
  
  
  global_results <- tibble(
    Use_df = Use,
    Sim_df = Sim_n,
    year0 = year_s,
    year1 = year_e,
    P0_global = P0_Use_all,
    P1_global = P1_Use_all,
    Delta_global = Delta_global,
    dU_global = dU_global,
    dF_global = dF_global,
    dA_global = dA_global,
    d_total_global = d_tot_global
  ) %>%
    bind_cols(crop_wide)
  
  
  if (
    abs(d_tot_global - Delta_global) > 1e-10
  ) {
    
    warning(
      "Global additive exactness fail: sum of driver contributions != Delta_P_global."
    )
  }
  
  
  metadata <- tibble(
    Use_df = Use,
    Sim_df = Sim_n,
    year0 = year_s,
    year1 = year_e,
    harmonized_geography = crosses_1990
  )
  
  
  return(
    list(
      contrib_country = contrib_country,
      contrib_crop = contrib_crop,
      global_results = global_results,
      crop_global_add = crop_global_add,
      metadata = metadata
    )
  )
}



## Potassium decomposition -------------------------------------------------

global_decomposition_K <- function(
    year_s,
    year_e,
    Use,
    Sim_n,
    normalize = c("both", "local", "global")
) {
  
  normalize <- match.arg(
    normalize,
    c("both", "local", "global")
  )
  
  
  # Select geographic representation -------------------------------------
  
  crosses_1990 <- min(year_s, year_e) < 1990 &&
    max(year_s, year_e) >= 1990
  
  if (crosses_1990) {
    
    K_soybean_data <- K_soybean_uses_1990
    K_maize_data   <- K_maize_uses_1990
    K_rice_data    <- K_rice_uses_1990
    K_wheat_data   <- K_wheat_uses_1990
    
  } else {
    
    K_soybean_data <- K_soybean_uses
    K_maize_data   <- K_maize_uses
    K_rice_data    <- K_rice_uses
    K_wheat_data   <- K_wheat_uses
  }
  
  
  # Retain selected Monte Carlo simulation -------------------------------
  
  K_soybean_uses1 <- K_soybean_data %>%
    filter(Sim == Sim_n)
  
  K_maize_uses1 <- K_maize_data %>%
    filter(Sim == Sim_n)
  
  K_rice_uses1 <- K_rice_data %>%
    filter(Sim == Sim_n)
  
  K_wheat_uses1 <- K_wheat_data %>%
    filter(Sim == Sim_n)
  
  
  # Select fertilizer-use category ---------------------------------------
  
  pick <- if (Use == "Food") {
    
    c(
      "Year",
      "FAOStat_area_code",
      "K_use_food",
      "Area_FAOStat",
      "Value",
      "P_Food",
      "Crop_code"
    )
    
  } else if (Use == "Feed") {
    
    c(
      "Year",
      "FAOStat_area_code",
      "K_use_feed",
      "Area_FAOStat",
      "Value",
      "P_Feed",
      "Crop_code"
    )
    
  } else {
    
    c(
      "Year",
      "FAOStat_area_code",
      "K_use_others",
      "Area_FAOStat",
      "Value",
      "P_Others",
      "Crop_code"
    )
  }
  
  
  K_soybean_uses1 <- K_soybean_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      K_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  K_maize_uses1 <- K_maize_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      K_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  K_rice_uses1 <- K_rice_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      K_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  K_wheat_uses1 <- K_wheat_uses1 %>%
    select(all_of(pick)) %>%
    rename(
      K_use = 3,
      Fert_ha = 5,
      P_Use = 6
    )
  
  
  # Identify countries present at both endpoints -------------------------
  
  countries_soybean <- inner_join(
    K_soybean_uses1 %>%
      filter(
        Year == year_s,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    K_soybean_uses1 %>%
      filter(
        Year == year_e,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_maize <- inner_join(
    K_maize_uses1 %>%
      filter(
        Year == year_s,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    K_maize_uses1 %>%
      filter(
        Year == year_e,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_rice <- inner_join(
    K_rice_uses1 %>%
      filter(
        Year == year_s,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    K_rice_uses1 %>%
      filter(
        Year == year_e,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries_wheat <- inner_join(
    K_wheat_uses1 %>%
      filter(
        Year == year_s,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    K_wheat_uses1 %>%
      filter(
        Year == year_e,
        K_use > 0
      ) %>%
      select(FAOStat_area_code),
    by = "FAOStat_area_code"
  )
  
  countries <- bind_rows(
    countries_soybean,
    countries_maize,
    countries_rice,
    countries_wheat
  ) %>%
    distinct()
  
  
  # Global endpoint totals ------------------------------------------------
  
  K0_soy_all <- K_soybean_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_soybean$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K0_mai_all <- K_maize_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_maize$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K0_ric_all <- K_rice_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_rice$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K0_whe_all <- K_wheat_uses1 %>%
    filter(
      Year == year_s,
      FAOStat_area_code %in% countries_wheat$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K0_Use_all <- sum(
    K0_soy_all,
    K0_mai_all,
    K0_ric_all,
    K0_whe_all,
    na.rm = TRUE
  )
  
  
  K1_soy_all <- K_soybean_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_soybean$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K1_mai_all <- K_maize_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_maize$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K1_ric_all <- K_rice_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_rice$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K1_whe_all <- K_wheat_uses1 %>%
    filter(
      Year == year_e,
      FAOStat_area_code %in% countries_wheat$FAOStat_area_code
    ) %>%
    summarise(K_use = sum(K_use, na.rm = TRUE) / 1000) %>%
    pull(K_use)
  
  K1_Use_all <- sum(
    K1_soy_all,
    K1_mai_all,
    K1_ric_all,
    K1_whe_all,
    na.rm = TRUE
  )
  
  Delta_global <- K1_Use_all - K0_Use_all
  
  
  # Additive decomposition ------------------------------------------------
  
  nC <- nrow(countries)
  
  rows_crop <- vector("list", nC)
  rows_country <- vector("list", nC)
  
  
  for (idx in seq_len(nC)) {
    
    country <- countries$FAOStat_area_code[idx]
    
    
    K0_soy <- get_cell(
      K_soybean_uses1, year_s, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    K0_mai <- get_cell(
      K_maize_uses1, year_s, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    K0_ric <- get_cell(
      K_rice_uses1, year_s, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    K0_whe <- get_cell(
      K_wheat_uses1, year_s, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    
    K1_soy <- get_cell(
      K_soybean_uses1, year_e, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    K1_mai <- get_cell(
      K_maize_uses1, year_e, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    K1_ric <- get_cell(
      K_rice_uses1, year_e, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    K1_whe <- get_cell(
      K_wheat_uses1, year_e, country, "K_use"
    ) %>%
      mutate(nutrient_use = nutrient_use / 1000)
    
    
    K0_all <- sum(
      K0_soy$nutrient_use,
      K0_mai$nutrient_use,
      K0_ric$nutrient_use,
      K0_whe$nutrient_use,
      na.rm = TRUE
    )
    
    K1_all <- sum(
      K1_soy$nutrient_use,
      K1_mai$nutrient_use,
      K1_ric$nutrient_use,
      K1_whe$nutrient_use,
      na.rm = TRUE
    )
    
    
    # LMDI weights.
    
    Wi_soy <- logarithmic_mean(
      K1_soy$nutrient_use,
      K0_soy$nutrient_use
    )
    
    Wi_maize <- logarithmic_mean(
      K1_mai$nutrient_use,
      K0_mai$nutrient_use
    )
    
    Wi_rice <- logarithmic_mean(
      K1_ric$nutrient_use,
      K0_ric$nutrient_use
    )
    
    Wi_wheat <- logarithmic_mean(
      K1_whe$nutrient_use,
      K0_whe$nutrient_use
    )
    
    
    # Crop-use allocation effect.
    
    dU <- c(
      soybean = Wi_soy *
        log_ratio(K1_soy$P_Use, K0_soy$P_Use),
      
      maize = Wi_maize *
        log_ratio(K1_mai$P_Use, K0_mai$P_Use),
      
      rice = Wi_rice *
        log_ratio(K1_ric$P_Use, K0_ric$P_Use),
      
      wheat = Wi_wheat *
        log_ratio(K1_whe$P_Use, K0_whe$P_Use)
    )
    
    
    # Fertilizer-rate effect.
    
    dF <- c(
      soybean = Wi_soy *
        log_ratio(K1_soy$Fert_ha, K0_soy$Fert_ha),
      
      maize = Wi_maize *
        log_ratio(K1_mai$Fert_ha, K0_mai$Fert_ha),
      
      rice = Wi_rice *
        log_ratio(K1_ric$Fert_ha, K0_ric$Fert_ha),
      
      wheat = Wi_wheat *
        log_ratio(K1_whe$Fert_ha, K0_whe$Fert_ha)
    )
    
    
    # Harvested-area effect.
    
    dA <- c(
      soybean = Wi_soy *
        log_ratio(K1_soy$Area_FAOStat, K0_soy$Area_FAOStat),
      
      maize = Wi_maize *
        log_ratio(K1_mai$Area_FAOStat, K0_mai$Area_FAOStat),
      
      rice = Wi_rice *
        log_ratio(K1_ric$Area_FAOStat, K0_ric$Area_FAOStat),
      
      wheat = Wi_wheat *
        log_ratio(K1_whe$Area_FAOStat, K0_whe$Area_FAOStat)
    )
    
    
    rows_crop[[idx]] <- tibble(
      Country = country,
      crop = c("soybean", "maize", "rice", "wheat"),
      
      K0 = c(
        K0_soy$nutrient_use,
        K0_mai$nutrient_use,
        K0_ric$nutrient_use,
        K0_whe$nutrient_use
      ),
      
      K1 = c(
        K1_soy$nutrient_use,
        K1_mai$nutrient_use,
        K1_ric$nutrient_use,
        K1_whe$nutrient_use
      ),
      
      dU = as.numeric(dU),
      dF = as.numeric(dF),
      dA = as.numeric(dA)
    ) %>%
      mutate(
        d_total = dU + dF + dA,
        Delta_K = K1 - K0
      )
    
    
    dU_tot <- sum(dU, na.rm = TRUE)
    dF_tot <- sum(dF, na.rm = TRUE)
    dA_tot <- sum(dA, na.rm = TRUE)
    
    d_tot <- dU_tot + dF_tot + dA_tot
    
    
    rows_country[[idx]] <- tibble(
      Country = country,
      K0_country = K0_all,
      K1_country = K1_all,
      Delta_country = K1_all - K0_all,
      dU = dU_tot,
      dF = dF_tot,
      dA = dA_tot,
      d_total = d_tot
    )
    
    
    if (
      is.finite(K0_all) &&
      abs(d_tot - (K1_all - K0_all)) > 1e-10
    ) {
      
      warning(
        sprintf(
          paste0(
            "Additive exactness fail: country %s; ",
            "d_total = %.12g vs Delta_K = %.12g"
          ),
          country,
          d_tot,
          K1_all - K0_all
        )
      )
    }
  }
  
  
  # Combine results -------------------------------------------------------
  
  contrib_crop <- bind_rows(rows_crop)
  contrib_country <- bind_rows(rows_country)
  
  
  # Global driver contributions ------------------------------------------
  
  dU_global <- sum(contrib_country$dU, na.rm = TRUE)
  dF_global <- sum(contrib_country$dF, na.rm = TRUE)
  dA_global <- sum(contrib_country$dA, na.rm = TRUE)
  
  d_tot_global <- dU_global +
    dF_global +
    dA_global
  
  
  crop_global_add <- contrib_crop %>%
    group_by(crop) %>%
    summarise(
      Delta_crop = sum(d_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      crop = c(
        "soybean",
        "maize",
        "rice",
        "wheat"
      ),
      fill = list(
        Delta_crop = 0
      )
    )
  
  crop_wide <- crop_global_add %>%
    pivot_wider(
      names_from = crop,
      values_from = Delta_crop,
      names_prefix = "Delta_"
    )
  
  
  global_results <- tibble(
    Use_df = Use,
    Sim_df = Sim_n,
    year0 = year_s,
    year1 = year_e,
    K0_global = K0_Use_all,
    K1_global = K1_Use_all,
    Delta_global = Delta_global,
    dU_global = dU_global,
    dF_global = dF_global,
    dA_global = dA_global,
    d_total_global = d_tot_global
  ) %>%
    bind_cols(crop_wide)
  
  
  if (
    abs(d_tot_global - Delta_global) > 1e-10
  ) {
    
    warning(
      "Global additive exactness fail: sum of driver contributions != Delta_K_global."
    )
  }
  
  
  metadata <- tibble(
    Use_df = Use,
    Sim_df = Sim_n,
    year0 = year_s,
    year1 = year_e,
    harmonized_geography = crosses_1990
  )
  
  
  return(
    list(
      contrib_country = contrib_country,
      contrib_crop = contrib_crop,
      global_results = global_results,
      crop_global_add = crop_global_add,
      metadata = metadata
    )
  )
}


# Decomposition -----------------------------------------------------------

# Nitrogen - Food
#

set.seed(123)

sims <- unique(N_soybean_uses$Sim)[1:200]
n <- length(sims)

out_country <- vector("list", n)
out_crop <- vector("list", n)
out_global <- vector("list", n)
out_meta <- vector("list", n)


for (k in seq_along(sims)) {
  
  i <- sims[k]
  t0 <- Sys.time()
  
  gd <- global_decomposition_N(
    year_s = 2006,
    year_e = 2019,
    Use = "Food",
    Sim_n = i,
    normalize = "both"
  )
  
  out_country[[k]] <- gd$contrib_country %>%
    mutate(
      Sim = i,
      norm = "additive"
    )
  
  out_crop[[k]] <- gd$contrib_crop %>%
    mutate(
      Sim = i,
      norm = "additive"
    )
  
  out_global[[k]] <- gd$global_results %>%
    mutate(
      Sim = i
    )
  
  out_meta[[k]] <- gd$metadata %>%
    mutate(
      Sim = i
    )
  
  message(
    sprintf(
      "Simulation %s took %.2f sec",
      i,
      as.numeric(
        Sys.time() - t0,
        units = "secs"
      )
    )
  )
}


# Combine simulation results ---------------------------------------------

Country_Results <- bind_rows(
  out_country
) %>%
  relocate(
    Sim,
    norm,
    everything()
  )

Crop_Results <- bind_rows(
  out_crop
) %>%
  relocate(
    Sim,
    norm,
    everything()
  )

Global_Results <- bind_rows(
  out_global
)

Meta_All <- bind_rows(
  out_meta
)

write.csv(
  Global_Results,
  "results/Decomposition_Additive/Dec_Ad_N_Food_Global_2006_2019.csv",
  row.names = FALSE
)

write.csv(
  Country_Results,
  "results/Decomposition_Additive/Dec_Ad_N_Food_Country_2006_2019.csv",
  row.names = FALSE
)

write.csv(
  Crop_Results,
  "results/Decomposition_Additive/Dec_Ad_N_Food_Crop_2006_2019.csv",
  row.names = FALSE
)

# Phosphorus - Food
#
# Run the selected Monte Carlo simulations and store country-, crop-, and
# global-level additive decomposition results.

set.seed(123)

sims <- unique(P_soybean_uses$Sim)[1:200]
n <- length(sims)

out_country <- vector("list", n)
out_crop <- vector("list", n)
out_global <- vector("list", n)
out_meta <- vector("list", n)


for (k in seq_along(sims)) {
  
  i <- sims[k]
  t0 <- Sys.time()
  
  gd <- global_decomposition_P(
    year_s = 2006,
    year_e = 2019,
    Use = "Food",
    Sim_n = i,
    normalize = "both"
  )
  
  out_country[[k]] <- gd$contrib_country %>%
    mutate(
      Sim = i,
      norm = "additive"
    )
  
  out_crop[[k]] <- gd$contrib_crop %>%
    mutate(
      Sim = i,
      norm = "additive"
    )
  
  out_global[[k]] <- gd$global_results %>%
    mutate(
      Sim = i
    )
  
  out_meta[[k]] <- gd$metadata %>%
    mutate(
      Sim = i
    )
  
  message(
    sprintf(
      "Simulation %s took %.2f sec",
      i,
      as.numeric(
        Sys.time() - t0,
        units = "secs"
      )
    )
  )
}


# Combine simulation results ---------------------------------------------

Country_Results <- bind_rows(
  out_country
) %>%
  relocate(
    Sim,
    norm,
    everything()
  )

Crop_Results <- bind_rows(
  out_crop
) %>%
  relocate(
    Sim,
    norm,
    everything()
  )

Global_Results <- bind_rows(
  out_global
)

Meta_All <- bind_rows(
  out_meta
)


# Export results ----------------------------------------------------------

write.csv(
  Global_Results,
  "results/Decomposition_Additive/Dec_Ad_P_Food_Global_2006_2019.csv",
  row.names = FALSE
)

write.csv(
  Country_Results,
  "results/Decomposition_Additive/Dec_Ad_P_Food_Country_2006_2019.csv",
  row.names = FALSE
)

write.csv(
  Crop_Results,
  "results/Decomposition_Additive/Dec_Ad_P_Food_Crop_2006_2019.csv",
  row.names = FALSE
)


# Potassium - Others
#
# Run the selected Monte Carlo simulations and store country-, crop-, and
# global-level additive decomposition results.

set.seed(123)

sims <- unique(K_soybean_uses$Sim)[1:200]
n <- length(sims)

out_country <- vector("list", n)
out_crop <- vector("list", n)
out_global <- vector("list", n)
out_meta <- vector("list", n)


for (k in seq_along(sims)) {
  
  i <- sims[k]
  t0 <- Sys.time()
  
  gd <- global_decomposition_K(
    year_s = 1975,
    year_e = 1989,
    Use = "Others",
    Sim_n = i,
    normalize = "both"
  )
  
  out_country[[k]] <- gd$contrib_country %>%
    mutate(
      Sim = i,
      norm = "additive"
    )
  
  out_crop[[k]] <- gd$contrib_crop %>%
    mutate(
      Sim = i,
      norm = "additive"
    )
  
  out_global[[k]] <- gd$global_results %>%
    mutate(
      Sim = i
    )
  
  out_meta[[k]] <- gd$metadata %>%
    mutate(
      Sim = i
    )
  
  message(
    sprintf(
      "Simulation %s took %.2f sec",
      i,
      as.numeric(
        Sys.time() - t0,
        units = "secs"
      )
    )
  )
}


# Combine simulation results ---------------------------------------------

Country_Results <- bind_rows(
  out_country
) %>%
  relocate(
    Sim,
    norm,
    everything()
  )

Crop_Results <- bind_rows(
  out_crop
) %>%
  relocate(
    Sim,
    norm,
    everything()
  )

Global_Results <- bind_rows(
  out_global
)

Meta_All <- bind_rows(
  out_meta
)


# Export results ----------------------------------------------------------

write.csv(
  Global_Results,
  "results/Decomposition_Additive/Dec_Ad_K_Others_Global_1975_1989.csv",
  row.names = FALSE
)

write.csv(
  Country_Results,
  "results/Decomposition_Additive/Dec_Ad_K_Others_Country_1975_1989.csv",
  row.names = FALSE
)

write.csv(
  Crop_Results,
  "results/Decomposition_Additive/Dec_Ad_K_Others_Crop_1975_1989.csv",
  row.names = FALSE
)


# Run remaining additive decompositions -----------------------------------
#
# Remaining comparisons:
#
#
# Analyses are repeated for N, P, and K.
#
# Results from each analysis are written to disk immediately so that
# completed analyses are retained if a later run is interrupted.


# Create output directory if necessary ------------------------------------

dir.create(
  "results/Decomposition_Additive",
  showWarnings = FALSE,
  recursive = TRUE
)


# Function to run one decomposition ---------------------------------------

run_decomposition <- function(
    nutrient,
    use,
    year_s,
    year_e,
    n_sims
) {
  
  message(
    "\n============================================================"
  )
  
  message(
    sprintf(
      "Starting %s - %s: %s-%s (%s simulations)",
      nutrient,
      use,
      year_s,
      year_e,
      n_sims
    )
  )
  
  message(
    "============================================================\n"
  )
  
  
  # Select nutrient-specific objects and decomposition function.
  
  if (nutrient == "N") {
    
    sims_available <- unique(N_soybean_uses$Sim)
    decomposition_fun <- global_decomposition_N
    
  } else if (nutrient == "P") {
    
    sims_available <- unique(P_soybean_uses$Sim)
    decomposition_fun <- global_decomposition_P
    
  } else if (nutrient == "K") {
    
    sims_available <- unique(K_soybean_uses$Sim)
    decomposition_fun <- global_decomposition_K
    
  } else {
    
    stop("Nutrient must be 'N', 'P', or 'K'.")
  }
  
  
  # Check that the requested number of simulations is available.
  
  if (length(sims_available) < n_sims) {
    
    stop(
      sprintf(
        "Only %s simulations are available for %s, but %s were requested.",
        length(sims_available),
        nutrient,
        n_sims
      )
    )
  }
  
  
  sims <- sims_available[seq_len(n_sims)]
  n <- length(sims)
  
  
  # Prepare output lists.
  
  out_country <- vector("list", n)
  out_crop <- vector("list", n)
  out_global <- vector("list", n)
  out_meta <- vector("list", n)
  
  
  # Run Monte Carlo simulations ------------------------------------------
  
  for (k in seq_along(sims)) {
    
    i <- sims[k]
    t0 <- Sys.time()
    
    
    gd <- decomposition_fun(
      year_s = year_s,
      year_e = year_e,
      Use = use,
      Sim_n = i,
      normalize = "both"
    )
    
    
    out_country[[k]] <- gd$contrib_country %>%
      mutate(
        Sim = i,
        norm = "additive"
      )
    
    
    out_crop[[k]] <- gd$contrib_crop %>%
      mutate(
        Sim = i,
        norm = "additive"
      )
    
    
    out_global[[k]] <- gd$global_results %>%
      mutate(
        Sim = i
      )
    
    
    out_meta[[k]] <- gd$metadata %>%
      mutate(
        Sim = i
      )
    
    
    message(
      sprintf(
        "%s - %s - %s/%s: simulation %s took %.2f sec",
        nutrient,
        use,
        k,
        n,
        i,
        as.numeric(
          Sys.time() - t0,
          units = "secs"
        )
      )
    )
  }
  
  
  # Combine simulation results -------------------------------------------
  
  Country_Results <- bind_rows(
    out_country
  ) %>%
    relocate(
      Sim,
      norm,
      everything()
    )
  
  
  Crop_Results <- bind_rows(
    out_crop
  ) %>%
    relocate(
      Sim,
      norm,
      everything()
    )
  
  
  Global_Results <- bind_rows(
    out_global
  )
  
  
  Meta_All <- bind_rows(
    out_meta
  )
  
  
  # Output filename -------------------------------------------------------
  
  prefix <- paste0(
    "results/Decomposition_Additive/Dec_Ad_",
    nutrient,
    "_",
    use,
    "_"
  )
  
  period <- paste0(
    year_s,
    "_",
    year_e
  )
  
  
  # Export results --------------------------------------------------------
  
  write.csv(
    Global_Results,
    paste0(
      prefix,
      "Global_",
      period,
      ".csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    Country_Results,
    paste0(
      prefix,
      "Country_",
      period,
      ".csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    Crop_Results,
    paste0(
      prefix,
      "Crop_",
      period,
      ".csv"
    ),
    row.names = FALSE
  )
  
  
  write.csv(
    Meta_All,
    paste0(
      prefix,
      "Metadata_",
      period,
      ".csv"
    ),
    row.names = FALSE
  )
  
  
  message(
    sprintf(
      "\nCompleted %s - %s: %s-%s\n",
      nutrient,
      use,
      year_s,
      year_e
    )
  )
  
  
  # Free memory before starting the next analysis.
  
  rm(
    out_country,
    out_crop,
    out_global,
    out_meta,
    Country_Results,
    Crop_Results,
    Global_Results,
    Meta_All,
    gd
  )
  
  gc()
}
decomposition_jobs <- tribble(
  ~year_s, ~year_e, ~use,     ~n_sims,
  1989,    2005, "Others",     200,
  1989,    2005, "Food",       200,
  1989,    2005, "Feed",       200
)

# Run all remaining decompositions ---------------------------------------

nutrients <- c(
  "K"
)


for (nutrient_i in nutrients) {
  
  for (job_i in seq_len(nrow(decomposition_jobs))) {
    
    run_decomposition(
      nutrient = nutrient_i,
      use = decomposition_jobs$use[job_i],
      year_s = decomposition_jobs$year_s[job_i],
      year_e = decomposition_jobs$year_e[job_i],
      n_sims = decomposition_jobs$n_sims[job_i]
    )
  }
}


message(
  "\nAll decomposition analyses completed successfully."
)