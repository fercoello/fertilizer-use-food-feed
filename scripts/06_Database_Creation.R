# ==============================================================================
# 06_fertilizer_use_allocation.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
# Allocate simulated crop-specific fertilizer use among food, feed, and other
# end uses for wheat, maize, rice, and soybean, while propagating uncertainty
# from the fertilizer-use simulations generated in Script 05.
#
# Analysis:
# 1. Load crop-use allocation fractions generated in Script 04.
# 2. Load the fertilizer application-rate simulations and Monte Carlo outputs
#    generated in Script 05.
# 3. Allocate each simulated N, P, and K fertilizer estimate among food, feed,
#    and other uses according to crop-specific use fractions.
# 4. For soybean, allocate processing uses between food (19%) and feed (81%).
# 5. Use the Monte Carlo realizations to estimate global fertilizer use by
#    end-use category for each year and iteration.
# 6. Export country-level uncertainty estimates and global Monte Carlo results.
# ==============================================================================

# 1: Loading Libraries ------------------------------------------------------------
library(readr)
library(tidyverse)


# 2: Load data ---------------------------------------------------------------

# Country-code correspondence used to recover FAOSTAT area codes from the
# ISO3 codes included in the outputs generated in Script 05.

Countries_DB <- read_excel("data/Countries_DB.xlsx") %>%
  rename(ISO3 = 2, FAOStat_area_code = 3) %>%
  select(
    ISO3,
    FAOStat_area_code
  )

## Wheat ------------------------------------------------------------------------

Wheat_uses <- read_csv("results/Wheat_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

# Load fertilizer application-rate simulations generated in Script 05.

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

# Load Monte Carlo realizations of total fertilizer use generated in Script 05.

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


## Maize ------------------------------------------------------------------------

maize_uses <- read_csv("results/maize_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

# Load fertilizer application-rate simulations generated in Script 05.

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

# Load Monte Carlo realizations of total fertilizer use generated in Script 05.

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

## Rice -------------------------------------------------------------------------

rice_uses <- read_csv("results/rice_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

# Load fertilizer application-rate simulations generated in Script 05.

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

# Load Monte Carlo realizations of total fertilizer use generated in Script 05.

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

## Soybean ----------------------------------------------------------------------

soybean_uses <- read_csv("results/soybean_uses.csv") %>%
  filter(FAOStat_area_code != 255 | Region != 0)

# Load fertilizer application-rate simulations generated in Script 05.

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

# Load Monte Carlo realizations of total fertilizer use generated in Script 05.

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

## Load FAOSTAT fertilizer consumption in agriculture ----------------------
#
# National fertilizer consumption data are used to identify country-year
# observations with reported agricultural fertilizer use for N, P2O5, and K2O.
#
# Historical Belgium-Luxembourg observations are harmonized with the coding
# used elsewhere in the analysis:
# - FAOSTAT area code 15 represents Belgium-Luxembourg before 2000.
# - FAOSTAT area code 255 is used for the historical aggregate in this workflow.
# - Code 255 observations after 1999 are removed to avoid overlap with the
#   post-2000 country coding.

N_total_FAOStat <- read_csv("data/N_total_FAOStat.csv") %>% filter(FAOStat_area_code != 255 | Year > 1999) %>%
  mutate(FAOStat_area_code = ifelse(FAOStat_area_code == 15 & Year < 2000, 255, FAOStat_area_code))
P2O5_total_FAOStat <- read_csv("data/P2O5_total_FAOStat.csv")  %>% filter(FAOStat_area_code != 255 | Year > 1999) %>%
  mutate(FAOStat_area_code = ifelse(FAOStat_area_code == 15 & Year < 2000, 255, FAOStat_area_code))
K2O_total_FAOStat <- read_csv("data/K2O_total_FAOStat.csv")  %>% filter(FAOStat_area_code != 255 | Year > 1999) %>%
  mutate(FAOStat_area_code = ifelse(FAOStat_area_code == 15 & Year < 2000, 255, FAOStat_area_code))


# 3: Creating database -------------------------------------------------------
# Allocate simulated crop-specific fertilizer application rates among food,
# feed, and other end uses.
#
# For each crop and nutrient:
# 3.1. Reshape the 500 simulated application rates from wide to long format.
# 3.2. Join crop-use allocation fractions calculated in Script 04.
# 3.3. Convert application rates to total fertilizer use using harvested area.
# 3.4. Allocate total fertilizer use among food, feed, processing, seed, losses,
#    and other non-food uses.
# 3.5. Combine processing with food for wheat, maize, and rice.
# 3.6. Combine seed, losses, and other non-food uses into an "other uses" class.
# 3.7. Set fertilizer use to zero where FAOSTAT reports no corresponding
#    national fertilizer-use observation for that crop and year.
#
# Soybean is treated differently because processing is divided between food
# (19%) and feed (81%).

## Wheat fertilizer use ---------------------------------------------------

# Nitrogen
# Allocate each simulated wheat N application rate among food, feed,
# processing, seed, losses, and other non-food uses.

N_wheat_uses <- Wheat_sim_N %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Navg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., Wheat_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(N_use_food = per_Food * Value * Area_FAOStat, N_use_feed = per_Feed * Value* Area_FAOStat, N_use_processing = per_Processing * Value* Area_FAOStat, N_use_seed = per_Seed * Value* Area_FAOStat, N_use_losses = per_Losses * Value* Area_FAOStat, N_use_other_uses_nf = per_Other_Uses_nf * Value* Area_FAOStat) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others) %>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "1_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>% 
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others)


# Phosphorus
# Repeat the allocation using simulated elemental P application rates.

P_wheat_uses <- Wheat_sim_P %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Pavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., Wheat_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(P_use_food = per_Food * Value * Area_FAOStat, P_use_feed = per_Feed * Value * Area_FAOStat, P_use_processing = per_Processing * Value * Area_FAOStat, P_use_seed = per_Seed * Value * Area_FAOStat, P_use_losses = per_Losses * Value * Area_FAOStat, P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "1_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise()  %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) 


# Potassium
# Repeat the allocation using simulated elemental K application rates.

K_wheat_uses <- Wheat_sim_K %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Kavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., Wheat_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(K_use_food = per_Food * Value * Area_FAOStat, K_use_feed = per_Feed * Value * Area_FAOStat, K_use_processing = per_Processing * Value * Area_FAOStat, K_use_seed = per_Seed * Value * Area_FAOStat, K_use_losses = per_Losses * Value * Area_FAOStat, K_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others)  %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "1_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others)



## Maize fertilizer use ---------------------------------------------------

# Allocate the 500 simulated N, P, and K application rates among maize
# food, feed, and other uses following the same procedure as for wheat.
# Processing is included in food, while seed, losses, and other non-food
# uses are combined into the "other uses" category.

N_maize_uses <- maize_sim_N %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Navg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., maize_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(N_use_food = per_Food * Value * Area_FAOStat, N_use_feed = per_Feed * Value * Area_FAOStat, N_use_processing = per_Processing * Value * Area_FAOStat, N_use_seed = per_Seed * Value * Area_FAOStat, N_use_losses = per_Losses * Value * Area_FAOStat, N_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others) %>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "1_2"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>% 
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others)


P_maize_uses <- maize_sim_P %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Pavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., maize_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(P_use_food = per_Food * Value * Area_FAOStat, P_use_feed = per_Feed * Value * Area_FAOStat, P_use_processing = per_Processing * Value * Area_FAOStat, P_use_seed = per_Seed * Value * Area_FAOStat, P_use_losses = per_Losses * Value * Area_FAOStat, P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "1_2"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) 


K_maize_uses <- maize_sim_K %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Kavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., maize_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(K_use_food = per_Food * Value * Area_FAOStat, K_use_feed = per_Feed * Value * Area_FAOStat, K_use_processing = per_Processing * Value * Area_FAOStat, K_use_seed = per_Seed * Value * Area_FAOStat,  K_use_losses = per_Losses * Value * Area_FAOStat, K_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others) %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "1_2"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others)


## Rice fertilizer use ----------------------------------------------------

# Allocate the 500 simulated N, P, and K application rates among rice
# food, feed, and other uses.
# Processing is included in food, while seed, losses, and other non-food
# uses are combined into the "other uses" category.

N_rice_uses <- rice_sim_N %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Navg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., rice_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(N_use_food = per_Food * Value * Area_FAOStat, N_use_feed = per_Feed * Value* Area_FAOStat, N_use_processing = per_Processing * Value * Area_FAOStat, N_use_seed = per_Seed * Value * Area_FAOStat, N_use_losses = per_Losses * Value * Area_FAOStat, N_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others)%>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "1_3"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>% 
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others)


P_rice_uses <- rice_sim_P %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Pavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., rice_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(P_use_food = per_Food * Value * Area_FAOStat, P_use_feed = per_Feed * Value* Area_FAOStat, P_use_processing = per_Processing * Value* Area_FAOStat, P_use_seed = per_Seed * Value * Area_FAOStat, P_use_losses = per_Losses * Value * Area_FAOStat, P_use_other_uses_nf = per_Other_Uses_nf * Value * Area_FAOStat) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "1_3"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) 


K_rice_uses <- rice_sim_K %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Kavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., rice_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(K_use_food = per_Food * Value * Area_FAOStat, K_use_feed = per_Feed * Value* Area_FAOStat, K_use_processing = per_Processing * Value* Area_FAOStat, K_use_seed = per_Seed * Value* Area_FAOStat, K_use_losses = per_Losses * Value* Area_FAOStat, K_use_other_uses_nf = per_Other_Uses_nf * Value* Area_FAOStat) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others) %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "1_3"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others)


## Soybean fertilizer use -------------------------------------------------

# Allocate the 500 simulated N, P, and K application rates among soybean
# food, feed, and other uses.
#
# Unlike wheat, maize, and rice, soybean processing is divided between:
# - food: 19% of processing uses
# - feed: 81% of processing uses
#
# Seed, losses, and other non-food uses are combined into the
# "other uses" category.

N_soybean_uses <- soybean_sim_N %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Navg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., soybean_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(N_use_food = per_Food * Value * Area_FAOStat, N_use_feed = per_Feed * Value* Area_FAOStat, N_use_processing = per_Processing * Value* Area_FAOStat, N_use_seed = per_Seed * Value* Area_FAOStat, N_use_losses = per_Losses * Value* Area_FAOStat, N_use_other_uses_nf = per_Other_Uses_nf * Value* Area_FAOStat) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing*0.19, na.rm = T), N_use_feed = sum(N_use_feed, N_use_processing*0.81, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others) %>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "2_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>% 
  select(Year, FAOStat_area_code, Sim, N_use_food, N_use_feed, N_use_others)


P_soybean_uses <- soybean_sim_P %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Pavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., soybean_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(P_use_food = per_Food * Value* Area_FAOStat , P_use_feed = per_Feed * Value* Area_FAOStat, P_use_processing = per_Processing * Value* Area_FAOStat, P_use_seed = per_Seed * Value* Area_FAOStat, P_use_losses = per_Losses * Value* Area_FAOStat, P_use_other_uses_nf = per_Other_Uses_nf * Value* Area_FAOStat) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing*0.19, na.rm = T), P_use_feed = sum(P_use_feed, P_use_processing*0.81, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "2_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, P_use_food, P_use_feed, P_use_others) 


K_soybean_uses <- soybean_sim_K %>% pivot_longer(cols = !c("FAOStat_area_code", "Year", "Kavg", "Area_FAOStat"), names_to = "Sim", values_to = "Value") %>%
  left_join(., soybean_uses, by = c("FAOStat_area_code", "Year")) %>% filter(Year < 2020) %>% mutate(K_use_food = per_Food * Value * Area_FAOStat, K_use_feed = per_Feed * Value* Area_FAOStat, K_use_processing = per_Processing * Value* Area_FAOStat, K_use_seed = per_Seed * Value* Area_FAOStat, K_use_losses = per_Losses * Value* Area_FAOStat, K_use_other_uses_nf = per_Other_Uses_nf * Value* Area_FAOStat) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing*0.19, na.rm = T), K_use_feed = sum(K_use_feed, K_use_processing*0.81, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others)%>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "2_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  select(Year, FAOStat_area_code, Sim, K_use_food, K_use_feed, K_use_others)
 


## Global estimations -----------------------------------------------------

# Use the Monte Carlo realizations generated in Script 05 to estimate global
# fertilizer use by crop, nutrient, end-use category, year, and iteration.
#
# For each crop and nutrient:
# 1. Join each country-year Monte Carlo realization with the crop-use
#    allocation fractions calculated in Script 04.
# 2. Allocate total nutrient use among food, feed, processing, seed, losses,
#    and other non-food uses.
# 3. Combine processing with food for wheat, maize, and rice.
# 4. For soybean, allocate processing between food (19%) and feed (81%).
# 5. Use the FAOSTAT fertilizer-consumption datasets as an availability mask:
#    country-year values are set to zero where no corresponding FAOSTAT
#    fertilizer-use observation is available.
# 6. Sum country-level values globally for each Monte Carlo iteration and year.

# Exclude aggregate China (FAOSTAT area code 351) because China is represented
# in the analysis through its constituent FAOSTAT territories to avoid double
# counting in global totals.

## Wheat ------------------------------------------------------------------

# Nitrogen
Wheat_mc_N_global <- Wheat_mc_N   %>% filter(FAOStat_area_code != 351) %>%
  left_join(., Wheat_uses, by = c("FAOStat_area_code", "Year"))  %>% mutate(N_use_food = per_Food * N_wheat , N_use_feed = per_Feed * N_wheat, N_use_processing = per_Processing * N_wheat, N_use_seed = per_Seed * N_wheat, N_use_losses = per_Losses * N_wheat, N_use_other_uses_nf = per_Other_Uses_nf * N_wheat) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "1_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(N_food = sum(N_use_food, na.rm = T), N_feed = sum(N_use_feed, na.rm = T), N_others = sum(N_use_others, na.rm = T)) 


# Phosphorus
Wheat_mc_P_global <- Wheat_mc_P   %>% filter(FAOStat_area_code != 351) %>%
  left_join(., Wheat_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(P_use_food = per_Food * P_wheat , P_use_feed = per_Feed * P_wheat, P_use_processing = per_Processing * P_wheat, P_use_seed = per_Seed * P_wheat, P_use_losses = per_Losses * P_wheat, P_use_other_uses_nf = per_Other_Uses_nf * P_wheat) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T)) %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "1_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(P_food = sum(P_use_food, na.rm = T), P_feed = sum(P_use_feed, na.rm = T), P_others = sum(P_use_others, na.rm = T)) 


# Potassium
Wheat_mc_K_global <- Wheat_mc_K %>% filter(FAOStat_area_code != 351) %>%
  left_join(., Wheat_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(K_use_food = per_Food * K_wheat , K_use_feed = per_Feed * K_wheat, K_use_processing = per_Processing * K_wheat, K_use_seed = per_Seed * K_wheat, K_use_losses = per_Losses * K_wheat, K_use_other_uses_nf = per_Other_Uses_nf * K_wheat) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "1_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(K_food = sum(K_use_food, na.rm = T), K_feed = sum(K_use_feed, na.rm = T), K_others = sum(K_use_others, na.rm = T))


## Maize ------------------------------------------------------------------

# Repeat the global Monte Carlo allocation for maize (Crop_Code = "1_2").
# Processing is included with food; seed, losses, and other non-food uses
# are combined into the "other uses" category.

maize_mc_N_global <- maize_mc_N  %>% filter(FAOStat_area_code != 351) %>%
  left_join(., maize_uses, by = c("FAOStat_area_code", "Year")) %>%  mutate(N_use_food = per_Food * N_maize , N_use_feed = per_Feed * N_maize, N_use_processing = per_Processing * N_maize, N_use_seed = per_Seed * N_maize, N_use_losses = per_Losses * N_maize, N_use_other_uses_nf = per_Other_Uses_nf * N_maize) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>%  left_join(., N_total_FAOStat %>% filter(Crop_Code == "1_2"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(N_food = sum(N_use_food, na.rm = T), N_feed = sum(N_use_feed, na.rm = T), N_others = sum(N_use_others, na.rm = T)) 

maize_mc_P_global <- maize_mc_P  %>% filter(FAOStat_area_code != 351) %>%
  left_join(., maize_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(P_use_food = per_Food * P_maize , P_use_feed = per_Feed * P_maize, P_use_processing = per_Processing * P_maize, P_use_seed = per_Seed * P_maize, P_use_losses = per_Losses * P_maize, P_use_other_uses_nf = per_Other_Uses_nf * P_maize) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T))  %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "1_2"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(P_food = sum(P_use_food, na.rm = T), P_feed = sum(P_use_feed, na.rm = T), P_others = sum(P_use_others, na.rm = T)) 

maize_mc_K_global <- maize_mc_K %>% filter(FAOStat_area_code != 351) %>%
  left_join(., maize_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(K_use_food = per_Food * K_maize , K_use_feed = per_Feed * K_maize, K_use_processing = per_Processing * K_maize, K_use_seed = per_Seed * K_maize, K_use_losses = per_Losses * K_maize, K_use_other_uses_nf = per_Other_Uses_nf * K_maize) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "1_2"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(K_food = sum(K_use_food, na.rm = T), K_feed = sum(K_use_feed, na.rm = T), K_others = sum(K_use_others, na.rm = T)) 

## Rice -------------------------------------------------------------------

# Repeat the global Monte Carlo allocation for rice (Crop_Code = "1_3").
# Processing is included with food; seed, losses, and other non-food uses
# are combined into the "other uses" category.

rice_mc_N_global <- rice_mc_N %>% filter(FAOStat_area_code != 351) %>%
  left_join(., rice_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(N_use_food = per_Food * N_rice , N_use_feed = per_Feed * N_rice, N_use_processing = per_Processing * N_rice, N_use_seed = per_Seed * N_rice, N_use_losses = per_Losses * N_rice, N_use_other_uses_nf = per_Other_Uses_nf * N_rice) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "1_3"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(N_food = sum(N_use_food, na.rm = T), N_feed = sum(N_use_feed, na.rm = T), N_others = sum(N_use_others, na.rm = T)) 

rice_mc_P_global <- rice_mc_P %>% filter(FAOStat_area_code != 351) %>%
  left_join(., rice_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(P_use_food = per_Food * P_rice , P_use_feed = per_Feed * P_rice, P_use_processing = per_Processing * P_rice, P_use_seed = per_Seed * P_rice, P_use_losses = per_Losses * P_rice, P_use_other_uses_nf = per_Other_Uses_nf * P_rice) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T))  %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "1_3"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(P_food = sum(P_use_food, na.rm = T), P_feed = sum(P_use_feed, na.rm = T), P_others = sum(P_use_others, na.rm = T)) 

rice_mc_K_global <- rice_mc_K %>% filter(FAOStat_area_code != 351) %>%
  left_join(., rice_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(K_use_food = per_Food * K_rice , K_use_feed = per_Feed * K_rice, K_use_processing = per_Processing * K_rice, K_use_seed = per_Seed * K_rice, K_use_losses = per_Losses * K_rice, K_use_other_uses_nf = per_Other_Uses_nf * K_rice) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "1_3"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(K_food = sum(K_use_food, na.rm = T), K_feed = sum(K_use_feed, na.rm = T), K_others = sum(K_use_others, na.rm = T)) 

## Soybean ----------------------------------------------------------------

# Repeat the global Monte Carlo allocation for soybean (Crop_Code = "2_1").
#
# Soybean processing is divided between:
# - food: 19% of processing use
# - feed: 81% of processing use
#
# Seed, losses, and other non-food uses are combined into the
# "other uses" category.

soybean_mc_N_global <- soybean_mc_N %>% filter(FAOStat_area_code != 351) %>%
  left_join(., soybean_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(N_use_food = per_Food * N_soybean , N_use_feed = per_Feed * N_soybean, N_use_processing = per_Processing * N_soybean, N_use_seed = per_Seed * N_soybean, N_use_losses = per_Losses * N_soybean, N_use_other_uses_nf = per_Other_Uses_nf * N_soybean) %>% rowwise() %>% 
  mutate(N_use_food = sum(N_use_food, N_use_processing*0.19, na.rm = T), N_use_feed = sum(N_use_feed, N_use_processing*0.81, na.rm = T), N_use_others = sum(N_use_seed, N_use_losses, N_use_other_uses_nf, na.rm = T)) %>% left_join(., N_total_FAOStat %>% filter(Crop_Code == "2_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(N_use_food = ifelse(is.na(Country_total_N), 0,N_use_food), N_use_feed = ifelse(is.na(Country_total_N), 0,N_use_feed), N_use_others = ifelse(is.na(Country_total_N), 0,N_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(N_food = sum(N_use_food, na.rm = T), N_feed = sum(N_use_feed, na.rm = T), N_others = sum(N_use_others, na.rm = T)) 

soybean_mc_P_global <- soybean_mc_P %>% filter(FAOStat_area_code != 351) %>%
  left_join(., soybean_uses, by = c("FAOStat_area_code", "Year")) %>% mutate(P_use_food = per_Food * P_soybean , P_use_feed = per_Feed * P_soybean, P_use_processing = per_Processing * P_soybean, P_use_seed = per_Seed * P_soybean, P_use_losses = per_Losses * P_soybean, P_use_other_uses_nf = per_Other_Uses_nf * P_soybean) %>% rowwise() %>% 
  mutate(P_use_food = sum(P_use_food, P_use_processing*0.19, na.rm = T), P_use_feed = sum(P_use_feed, P_use_processing*0.81, na.rm = T), P_use_others = sum(P_use_seed, P_use_losses, P_use_other_uses_nf, na.rm = T))  %>% left_join(., P2O5_total_FAOStat %>% filter(Crop_Code == "2_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(P_use_food = ifelse(is.na(Country_total_P2O5), 0,P_use_food), P_use_feed = ifelse(is.na(Country_total_P2O5), 0,P_use_feed), P_use_others = ifelse(is.na(Country_total_P2O5), 0,P_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(P_food = sum(P_use_food, na.rm = T), P_feed = sum(P_use_feed, na.rm = T), P_others = sum(P_use_others, na.rm = T)) 

soybean_mc_K_global <- soybean_mc_K %>% filter(FAOStat_area_code != 351) %>%
  left_join(., soybean_uses, by = c("FAOStat_area_code", "Year"))  %>% mutate(K_use_food = per_Food * K_soybean , K_use_feed = per_Feed * K_soybean, K_use_processing = per_Processing * K_soybean, K_use_seed = per_Seed * K_soybean, K_use_losses = per_Losses * K_soybean, K_use_other_uses_nf = per_Other_Uses_nf * K_soybean) %>% rowwise() %>% 
  mutate(K_use_food = sum(K_use_food, K_use_processing*0.19, na.rm = T), K_use_feed = sum(K_use_feed, K_use_processing*0.81, na.rm = T), K_use_others = sum(K_use_seed, K_use_losses, K_use_other_uses_nf, na.rm = T)) %>% left_join(., K2O_total_FAOStat %>% filter(Crop_Code == "2_1"), by = c("FAOStat_area_code", "Year")) %>% rowwise() %>%
  mutate(K_use_food = ifelse(is.na(Country_total_K2O), 0,K_use_food), K_use_feed = ifelse(is.na(Country_total_K2O), 0,K_use_feed), K_use_others = ifelse(is.na(Country_total_K2O), 0,K_use_others)) %>%
  group_by(Iter, Year) %>%
  summarise(K_food = sum(K_use_food, na.rm = T), K_feed = sum(K_use_feed, na.rm = T), K_others = sum(K_use_others, na.rm = T)) 


# Export results ----------------------------------------------------------

## Country-level fertilizer use by end-use category ------------------------

# Wheat
write.csv(
  N_wheat_uses,
  "results/N_wheat_uses_all.csv",
  row.names = FALSE
)

write.csv(
  P_wheat_uses,
  "results/P_wheat_uses_all.csv",
  row.names = FALSE
)

write.csv(
  K_wheat_uses,
  "results/K_wheat_uses_all.csv",
  row.names = FALSE
)


# Maize
write.csv(
  N_maize_uses,
  "results/N_maize_uses_all.csv",
  row.names = FALSE
)

write.csv(
  P_maize_uses,
  "results/P_maize_uses_all.csv",
  row.names = FALSE
)

write.csv(
  K_maize_uses,
  "results/K_maize_uses_all.csv",
  row.names = FALSE
)


# Rice
write.csv(
  N_rice_uses,
  "results/N_rice_uses_all.csv",
  row.names = FALSE
)

write.csv(
  P_rice_uses,
  "results/P_rice_uses_all.csv",
  row.names = FALSE
)

write.csv(
  K_rice_uses,
  "results/K_rice_uses_all.csv",
  row.names = FALSE
)


# Soybean
write.csv(
  N_soybean_uses,
  "results/N_soybean_uses_all.csv",
  row.names = FALSE
)

write.csv(
  P_soybean_uses,
  "results/P_soybean_uses_all.csv",
  row.names = FALSE
)

write.csv(
  K_soybean_uses,
  "results/K_soybean_uses_all.csv",
  row.names = FALSE
)


## Global Monte Carlo fertilizer-use estimates -----------------------------

# Wheat
write.csv(
  Wheat_mc_N_global,
  "results/Wheat_uses_N_global_all.csv",
  row.names = FALSE
)

write.csv(
  Wheat_mc_P_global,
  "results/Wheat_uses_P_global_all.csv",
  row.names = FALSE
)

write.csv(
  Wheat_mc_K_global,
  "results/Wheat_uses_K_global_all.csv",
  row.names = FALSE
)


# Maize
write.csv(
  maize_mc_N_global,
  "results/maize_uses_N_global_all.csv",
  row.names = FALSE
)

write.csv(
  maize_mc_P_global,
  "results/maize_uses_P_global_all.csv",
  row.names = FALSE
)

write.csv(
  maize_mc_K_global,
  "results/maize_uses_K_global_all.csv",
  row.names = FALSE
)


# Rice
write.csv(
  rice_mc_N_global,
  "results/rice_uses_N_global_all.csv",
  row.names = FALSE
)

write.csv(
  rice_mc_P_global,
  "results/rice_uses_P_global_all.csv",
  row.names = FALSE
)

write.csv(
  rice_mc_K_global,
  "results/rice_uses_K_global_all.csv",
  row.names = FALSE
)


# Soybean
write.csv(
  soybean_mc_N_global,
  "results/soybean_uses_N_global_all.csv",
  row.names = FALSE
)

write.csv(
  soybean_mc_P_global,
  "results/soybean_uses_P_global_all.csv",
  row.names = FALSE
)

write.csv(
  soybean_mc_K_global,
  "results/soybean_uses_K_global_all.csv",
  row.names = FALSE
)
