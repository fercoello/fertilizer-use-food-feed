# ==============================================================================
# 03_domestic_use_allocation.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
# Estimate the proportion of domestic crop supply allocated to different use
# categories using FAOSTAT Food Balance data.
#
# The analysis is conducted for wheat, maize, rice, and soybean. For each crop,
# historical and current FAOSTAT Food Balance datasets are harmonized to derive
# the share of domestic supply allocated to:
#   - food
#   - feed
#   - seed
#   - losses
#   - processing
#   - other non-food uses
#
# Historical and current estimates are averaged during their overlapping years.
# Missing country-year allocation shares are first imputed using country-level
# median shares and, where these are unavailable, regional median shares.
#
# External data:
#   FAOSTAT Food Balances (2010-):
#   data/Food_Balances/FoodBalanceSheets_E_All_Data_(Normalized).csv
#
#   FAOSTAT Historic Food Balance Sheets (-2013):
#   data/Food_Balances/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv
#
# Additional files:
#   data/Countries_DB.xlsx
#   data/FAOSTAT_regions.csv
#
# Outputs:
#   results/FB_uses_wheat.csv
#   results/FB_uses_maize.csv
#   results/FB_uses_rice.csv
#   results/FB_uses_soybean.csv
# ==============================================================================

# Libraries --------------------------------------------------------------------

library(tidyverse)
library(readxl)


# Load data --------------------------------------------------------------------

# FAOSTAT Food Balance datasets
FB_act <- read_csv(
  "data/Food_Balances/FoodBalanceSheets_E_All_Data_(Normalized).csv"
)

FB_old <- read_csv(
  "data/Food_Balances/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv"
)

# Country-code database
Countries_DB <- read_excel(
  "data/Countries_DB.xlsx"
) %>%
  select(2, 3) %>%
  rename(
    ISO3 = 1,
    FAOStat_area_code = 2
  )

# FAOSTAT regional classification
FAOSTAT_regions <- read_csv(
  "data/FAOSTAT_regions.csv"
)

# Wheat ------------------------------------------------------------------------

## 1: Extract domestic-use categories from the historical Food Balances --------

# Extract the main domestic-use categories for wheat from the historical
# FAOSTAT Food Balance dataset.
#
# FAOSTAT codes:
#   Item 2511    = Wheat and products
#
# Element codes:
#   5301 = Domestic supply quantity
#   5521 = Feed
#   5142 = Food
#   5527 = Seed
#   5123 = Losses
#   5131 = Processing
#   5154 = Other non-food uses
#   5170 = Residuals
#
# Residuals are balancing adjustments and are therefore not included when
# calculating the distribution of domestic uses.

FB_old_uses_wheat <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2511,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`
  )


## 2: Check historical domestic supply against reported uses -------------------

# Sum the explicit domestic-use categories and compare their total with reported
# domestic supply. This is used as a consistency check and does not modify the
# underlying data.

FB_old_uses_wheat_check <- FB_old_uses_wheat %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_old_uses_wheat_check
  )
)


## 3: Calculate historical domestic-use shares ---------------------------------

# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_old_uses_wheat_perc <- FB_old_uses_wheat_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_old_{.col}"
    )
  ) %>%
  ungroup()


## 4: Extract domestic-use categories from the current Food Balances ------------

# Extract the equivalent wheat-use categories from the current FAOSTAT Food
# Balance dataset.

FB_act_uses_wheat <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2511,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`,
    Residuals = `5170`
  )


## 5: Check current domestic supply against reported uses -----------------------

# As for the historical dataset, residuals are excluded from Tot_Uses because
# they represent balancing adjustments rather than an explicit domestic use.

FB_act_uses_wheat_check <- FB_act_uses_wheat %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_act_uses_wheat_check
  )
)


# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_act_uses_wheat_perc <- FB_act_uses_wheat_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_act_{.col}"
    )
  ) %>%
  ungroup()


## 6: Compare historical and current Food Balance estimates --------------------

# Join the domestic-use shares from the historical and current Food Balance
# datasets. Their overlapping period (2010-2013) is used to assess consistency
# between the two FAOSTAT products.

FB_uses_wheat_per <- full_join(
  FB_old_uses_wheat_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_wheat_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  )
)


# Retain the overlapping years for diagnostic comparisons.

FB_uses_wheat_check_FB <- FB_uses_wheat_per %>%
  filter(
    Year >= 2010,
    Year <= 2013
  )

summary(
  lm(
    per_old_Food ~ per_act_Food,
    data = FB_uses_wheat_check_FB
  )
)

summary(
  lm(
    per_old_Feed ~ per_act_Feed,
    data = FB_uses_wheat_check_FB
  )
)

summary(
  lm(
    per_old_Seed ~ per_act_Seed,
    data = FB_uses_wheat_check_FB
  )
)

summary(
  lm(
    per_old_Processing ~ per_act_Processing,
    data = FB_uses_wheat_check_FB
  )
)

summary(
  lm(
    per_old_Other_Uses_nf ~ per_act_Other_Uses_nf,
    data = FB_uses_wheat_check_FB
  )
)


## 7: Harmonize historical and current Food Balance data ------------------------

# Join the full historical and current datasets. Historical values are used
# before 2010, current values after 2013, and the mean of the available values
# is used during the overlapping period (2010-2013).
#
# When only one dataset contains a value during the overlap period, that
# available value is retained.

FB_uses_wheat <- full_join(
  FB_old_uses_wheat_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_wheat_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Residuals,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  ),
  suffix = c("_old", "_act")
) %>%
  rowwise() %>%
  mutate(
    per_Food = case_when(
      Year < 2010 ~ per_old_Food,
      Year > 2013 ~ per_act_Food,
      TRUE ~ mean(
        c(per_old_Food, per_act_Food),
        na.rm = TRUE
      )
    ),
    
    per_Feed = case_when(
      Year < 2010 ~ per_old_Feed,
      Year > 2013 ~ per_act_Feed,
      TRUE ~ mean(
        c(per_old_Feed, per_act_Feed),
        na.rm = TRUE
      )
    ),
    
    per_Seed = case_when(
      Year < 2010 ~ per_old_Seed,
      Year > 2013 ~ per_act_Seed,
      TRUE ~ mean(
        c(per_old_Seed, per_act_Seed),
        na.rm = TRUE
      )
    ),
    
    per_Losses = case_when(
      Year < 2010 ~ per_old_Losses,
      Year > 2013 ~ per_act_Losses,
      TRUE ~ mean(
        c(per_old_Losses, per_act_Losses),
        na.rm = TRUE
      )
    ),
    
    per_Processing = case_when(
      Year < 2010 ~ per_old_Processing,
      Year > 2013 ~ per_act_Processing,
      TRUE ~ mean(
        c(per_old_Processing, per_act_Processing),
        na.rm = TRUE
      )
    ),
    
    per_Other_Uses_nf = case_when(
      Year < 2010 ~ per_old_Other_Uses_nf,
      Year > 2013 ~ per_act_Other_Uses_nf,
      TRUE ~ mean(
        c(per_old_Other_Uses_nf, per_act_Other_Uses_nf),
        na.rm = TRUE
      )
    ),
    
    Domestic_Supply = case_when(
      Year < 2010 ~ Domestic_Supply_old,
      Year > 2013 ~ Domestic_Supply_act,
      TRUE ~ mean(
        c(Domestic_Supply_old, Domestic_Supply_act),
        na.rm = TRUE
      )
    ),
    
    Food = case_when(
      Year < 2010 ~ Food_old,
      Year > 2013 ~ Food_act,
      TRUE ~ mean(
        c(Food_old, Food_act),
        na.rm = TRUE
      )
    ),
    
    Feed = case_when(
      Year < 2010 ~ Feed_old,
      Year > 2013 ~ Feed_act,
      TRUE ~ mean(
        c(Feed_old, Feed_act),
        na.rm = TRUE
      )
    ),
    
    Seed = case_when(
      Year < 2010 ~ Seed_old,
      Year > 2013 ~ Seed_act,
      TRUE ~ mean(
        c(Seed_old, Seed_act),
        na.rm = TRUE
      )
    ),
    
    Losses = case_when(
      Year < 2010 ~ Losses_old,
      Year > 2013 ~ Losses_act,
      TRUE ~ mean(
        c(Losses_old, Losses_act),
        na.rm = TRUE
      )
    ),
    
    Processing = case_when(
      Year < 2010 ~ Processing_old,
      Year > 2013 ~ Processing_act,
      TRUE ~ mean(
        c(Processing_old, Processing_act),
        na.rm = TRUE
      )
    ),
    
    Other_Uses_nf = case_when(
      Year < 2010 ~ Other_Uses_nf_old,
      Year > 2013 ~ Other_Uses_nf_act,
      TRUE ~ mean(
        c(Other_Uses_nf_old, Other_Uses_nf_act),
        na.rm = TRUE
      )
    ),
    
    Tot_Uses = case_when(
      Year < 2010 ~ Tot_Uses_old,
      Year > 2013 ~ Tot_Uses_act,
      TRUE ~ mean(
        c(Tot_Uses_old, Tot_Uses_act),
        na.rm = TRUE
      )
    )
  ) %>%
  ungroup()


# Replace NaN values generated when both historical and current estimates are
# missing with NA so that they can subsequently be imputed.

FB_uses_wheat <- FB_uses_wheat %>%
  mutate(
    across(
      c(
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf,
        Domestic_Supply,
        Food,
        Feed,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf,
        Tot_Uses
      ),
      ~ ifelse(is.nan(.x), NA, .x)
    )
  )


# Harmonize the historical Belgium-Luxembourg FAOSTAT area code with Belgium.
# This allows historical and later records to be treated as a single country
# series in the subsequent imputation procedure.

FB_uses_wheat <- FB_uses_wheat %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


## 8: Create complete country-year series ---------------------------------------

# Generate all combinations of FAOSTAT countries and years in the study period
# and join the observed domestic-use shares. Missing country-years are filled in
# subsequently using country and regional median allocation patterns.

FB_uses_wheat_imp <- expand.grid(
  FAOStat_area_code =
    unique(Countries_DB$FAOStat_area_code),
  Year = seq(1961, 2019)
) %>%
  left_join(
    FB_uses_wheat %>%
      select(
        Year,
        FAOStat_area_code,
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf
      ),
    by = c(
      "Year",
      "FAOStat_area_code"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_perc = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()


## 9: Impute missing country-years using country median shares ------------------

# Estimate the median allocation pattern for each country across all years with
# available information.

FB_uses_wheat_avg <- FB_uses_wheat %>%
  select(
    FAOStat_area_code,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize median shares so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  rename(
    per_Food_median = per_Food,
    per_Feed_median = per_Feed,
    per_Seed_median = per_Seed,
    per_Losses_median = per_Losses,
    per_Processing_median = per_Processing,
    per_Other_Uses_nf_median =
      per_Other_Uses_nf
  )


# Where an entire country-year allocation is missing, replace it with the
# country's median allocation pattern.

FB_uses_wheat_imp <- FB_uses_wheat_imp %>%
  left_join(
    FB_uses_wheat_avg,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    per_Food = ifelse(
      Tot_perc == 0,
      per_Food_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_perc == 0,
      per_Feed_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_perc == 0,
      per_Seed_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_perc == 0,
      per_Losses_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_perc == 0,
      per_Processing_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_perc == 0,
      per_Other_Uses_nf_median,
      per_Other_Uses_nf
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )


## 10: Impute remaining gaps using regional median shares -----------------------

# Calculate annual median allocation patterns for each FAOSTAT region. These
# values are used only for countries for which a country-level median could not
# be estimated.

FB_uses_wheat_regional_median <- FB_uses_wheat_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  group_by(
    Year,
    Region
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize regional medians so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  drop_na(
    Region
  ) %>%
  rename(
    per_Food_reg_median = per_Food,
    per_Feed_reg_median = per_Feed,
    per_Seed_reg_median = per_Seed,
    per_Losses_reg_median = per_Losses,
    per_Processing_reg_median =
      per_Processing,
    per_Other_Uses_nf_reg_median =
      per_Other_Uses_nf
  ) %>%
  select(
    Year,
    Region,
    per_Food_reg_median,
    per_Feed_reg_median,
    per_Seed_reg_median,
    per_Losses_reg_median,
    per_Processing_reg_median,
    per_Other_Uses_nf_reg_median
  )


# Assign regional median allocation shares to country-years that remain without
# an allocation after country-level imputation.

FB_uses_wheat_imp <- FB_uses_wheat_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  left_join(
    FB_uses_wheat_regional_median,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    per_Food = ifelse(
      Tot_per == 0,
      per_Food_reg_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_per == 0,
      per_Feed_reg_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_per == 0,
      per_Seed_reg_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_per == 0,
      per_Losses_reg_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_per == 0,
      per_Processing_reg_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_per == 0,
      per_Other_Uses_nf_reg_median,
      per_Other_Uses_nf
    )
  ) %>%
  ungroup() %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )


# Maize ------------------------------------------------------------------------

## 1: Extract domestic-use categories from the historical Food Balances --------

# Extract the main domestic-use categories for maize from the historical
# FAOSTAT Food Balance dataset.
#
# FAOSTAT codes:
#   Item 2514    = Maize and products
#
# Element codes:
#   5301 = Domestic supply quantity
#   5521 = Feed
#   5142 = Food
#   5527 = Seed
#   5123 = Losses
#   5131 = Processing
#   5154 = Other non-food uses
#   5170 = Residuals
#
# Residuals are balancing adjustments and are therefore not included when
# calculating the distribution of domestic uses.

FB_old_uses_maize <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2514,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`
  )


## 2: Check historical domestic supply against reported uses -------------------

# Sum the explicit domestic-use categories and compare their total with reported
# domestic supply. This is used as a consistency check and does not modify the
# underlying data.

FB_old_uses_maize_check <- FB_old_uses_maize %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_old_uses_maize_check
  )
)


## 3: Calculate historical domestic-use shares ---------------------------------

# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_old_uses_maize_perc <- FB_old_uses_maize_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_old_{.col}"
    )
  ) %>%
  ungroup()


## 4: Extract domestic-use categories from the current Food Balances ------------

# Extract the equivalent maize-use categories from the current FAOSTAT Food
# Balance dataset.

FB_act_uses_maize <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2514,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`,
    Residuals = `5170`
  )


## 5: Check current domestic supply against reported uses -----------------------

# As for the historical dataset, residuals are excluded from Tot_Uses because
# they represent balancing adjustments rather than an explicit domestic use.

FB_act_uses_maize_check <- FB_act_uses_maize %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_act_uses_maize_check
  )
)


# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_act_uses_maize_perc <- FB_act_uses_maize_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_act_{.col}"
    )
  ) %>%
  ungroup()


## 6: Compare historical and current Food Balance estimates --------------------

# Join the domestic-use shares from the historical and current Food Balance
# datasets. Their overlapping period (2010-2013) is used to assess consistency
# between the two FAOSTAT products.

FB_uses_maize_per <- full_join(
  FB_old_uses_maize_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_maize_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  )
)


# Retain the overlapping years for diagnostic comparisons.

FB_uses_maize_check_FB <- FB_uses_maize_per %>%
  filter(
    Year >= 2010,
    Year <= 2013
  )

summary(
  lm(
    per_old_Food ~ per_act_Food,
    data = FB_uses_maize_check_FB
  )
)

summary(
  lm(
    per_old_Feed ~ per_act_Feed,
    data = FB_uses_maize_check_FB
  )
)

summary(
  lm(
    per_old_Seed ~ per_act_Seed,
    data = FB_uses_maize_check_FB
  )
)

summary(
  lm(
    per_old_Processing ~ per_act_Processing,
    data = FB_uses_maize_check_FB
  )
)

summary(
  lm(
    per_old_Other_Uses_nf ~ per_act_Other_Uses_nf,
    data = FB_uses_maize_check_FB
  )
)


## 7: Harmonize historical and current Food Balance data ------------------------

# Join the full historical and current datasets. Historical values are used
# before 2010, current values after 2013, and the mean of the available values
# is used during the overlapping period (2010-2013).
#
# When only one dataset contains a value during the overlap period, that
# available value is retained.

FB_uses_maize <- full_join(
  FB_old_uses_maize_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_maize_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Residuals,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  ),
  suffix = c("_old", "_act")
) %>%
  rowwise() %>%
  mutate(
    per_Food = case_when(
      Year < 2010 ~ per_old_Food,
      Year > 2013 ~ per_act_Food,
      TRUE ~ mean(
        c(per_old_Food, per_act_Food),
        na.rm = TRUE
      )
    ),
    
    per_Feed = case_when(
      Year < 2010 ~ per_old_Feed,
      Year > 2013 ~ per_act_Feed,
      TRUE ~ mean(
        c(per_old_Feed, per_act_Feed),
        na.rm = TRUE
      )
    ),
    
    per_Seed = case_when(
      Year < 2010 ~ per_old_Seed,
      Year > 2013 ~ per_act_Seed,
      TRUE ~ mean(
        c(per_old_Seed, per_act_Seed),
        na.rm = TRUE
      )
    ),
    
    per_Losses = case_when(
      Year < 2010 ~ per_old_Losses,
      Year > 2013 ~ per_act_Losses,
      TRUE ~ mean(
        c(per_old_Losses, per_act_Losses),
        na.rm = TRUE
      )
    ),
    
    per_Processing = case_when(
      Year < 2010 ~ per_old_Processing,
      Year > 2013 ~ per_act_Processing,
      TRUE ~ mean(
        c(per_old_Processing, per_act_Processing),
        na.rm = TRUE
      )
    ),
    
    per_Other_Uses_nf = case_when(
      Year < 2010 ~ per_old_Other_Uses_nf,
      Year > 2013 ~ per_act_Other_Uses_nf,
      TRUE ~ mean(
        c(per_old_Other_Uses_nf, per_act_Other_Uses_nf),
        na.rm = TRUE
      )
    ),
    
    Domestic_Supply = case_when(
      Year < 2010 ~ Domestic_Supply_old,
      Year > 2013 ~ Domestic_Supply_act,
      TRUE ~ mean(
        c(Domestic_Supply_old, Domestic_Supply_act),
        na.rm = TRUE
      )
    ),
    
    Food = case_when(
      Year < 2010 ~ Food_old,
      Year > 2013 ~ Food_act,
      TRUE ~ mean(
        c(Food_old, Food_act),
        na.rm = TRUE
      )
    ),
    
    Feed = case_when(
      Year < 2010 ~ Feed_old,
      Year > 2013 ~ Feed_act,
      TRUE ~ mean(
        c(Feed_old, Feed_act),
        na.rm = TRUE
      )
    ),
    
    Seed = case_when(
      Year < 2010 ~ Seed_old,
      Year > 2013 ~ Seed_act,
      TRUE ~ mean(
        c(Seed_old, Seed_act),
        na.rm = TRUE
      )
    ),
    
    Losses = case_when(
      Year < 2010 ~ Losses_old,
      Year > 2013 ~ Losses_act,
      TRUE ~ mean(
        c(Losses_old, Losses_act),
        na.rm = TRUE
      )
    ),
    
    Processing = case_when(
      Year < 2010 ~ Processing_old,
      Year > 2013 ~ Processing_act,
      TRUE ~ mean(
        c(Processing_old, Processing_act),
        na.rm = TRUE
      )
    ),
    
    Other_Uses_nf = case_when(
      Year < 2010 ~ Other_Uses_nf_old,
      Year > 2013 ~ Other_Uses_nf_act,
      TRUE ~ mean(
        c(Other_Uses_nf_old, Other_Uses_nf_act),
        na.rm = TRUE
      )
    ),
    
    Tot_Uses = case_when(
      Year < 2010 ~ Tot_Uses_old,
      Year > 2013 ~ Tot_Uses_act,
      TRUE ~ mean(
        c(Tot_Uses_old, Tot_Uses_act),
        na.rm = TRUE
      )
    )
  ) %>%
  ungroup()


# Replace NaN values generated when both historical and current estimates are
# missing with NA so that they can subsequently be imputed.

FB_uses_maize <- FB_uses_maize %>%
  mutate(
    across(
      c(
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf,
        Domestic_Supply,
        Food,
        Feed,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf,
        Tot_Uses
      ),
      ~ ifelse(is.nan(.x), NA, .x)
    )
  )


# Harmonize the historical Belgium-Luxembourg FAOSTAT area code with Belgium.
# This allows historical and later records to be treated as a single country
# series in the subsequent imputation procedure.

FB_uses_maize <- FB_uses_maize %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


## 8: Create complete country-year series ---------------------------------------

# Generate all combinations of FAOSTAT countries and years in the study period
# and join the observed domestic-use shares. Missing country-years are filled in
# subsequently using country and regional median allocation patterns.

FB_uses_maize_imp <- expand.grid(
  FAOStat_area_code =
    unique(Countries_DB$FAOStat_area_code),
  Year = seq(1961, 2019)
) %>%
  left_join(
    FB_uses_maize %>%
      select(
        Year,
        FAOStat_area_code,
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf
      ),
    by = c(
      "Year",
      "FAOStat_area_code"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_perc = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()


## 9: Impute missing country-years using country median shares ------------------

# Estimate the median allocation pattern for each country across all years with
# available information.

FB_uses_maize_avg <- FB_uses_maize %>%
  select(
    FAOStat_area_code,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize median shares so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  rename(
    per_Food_median = per_Food,
    per_Feed_median = per_Feed,
    per_Seed_median = per_Seed,
    per_Losses_median = per_Losses,
    per_Processing_median = per_Processing,
    per_Other_Uses_nf_median =
      per_Other_Uses_nf
  )


# Where an entire country-year allocation is missing, replace it with the
# country's median allocation pattern.

FB_uses_maize_imp <- FB_uses_maize_imp %>%
  left_join(
    FB_uses_maize_avg,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    per_Food = ifelse(
      Tot_perc == 0,
      per_Food_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_perc == 0,
      per_Feed_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_perc == 0,
      per_Seed_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_perc == 0,
      per_Losses_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_perc == 0,
      per_Processing_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_perc == 0,
      per_Other_Uses_nf_median,
      per_Other_Uses_nf
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )


## 10: Impute remaining gaps using regional median shares -----------------------

# Calculate annual median allocation patterns for each FAOSTAT region. These
# values are used only for countries for which a country-level median could not
# be estimated.

FB_uses_maize_regional_median <- FB_uses_maize_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  group_by(
    Year,
    Region
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize regional medians so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  drop_na(
    Region
  ) %>%
  rename(
    per_Food_reg_median = per_Food,
    per_Feed_reg_median = per_Feed,
    per_Seed_reg_median = per_Seed,
    per_Losses_reg_median = per_Losses,
    per_Processing_reg_median =
      per_Processing,
    per_Other_Uses_nf_reg_median =
      per_Other_Uses_nf
  ) %>%
  select(
    Year,
    Region,
    per_Food_reg_median,
    per_Feed_reg_median,
    per_Seed_reg_median,
    per_Losses_reg_median,
    per_Processing_reg_median,
    per_Other_Uses_nf_reg_median
  )


# Assign regional median allocation shares to country-years that remain without
# an allocation after country-level imputation.

FB_uses_maize_imp <- FB_uses_maize_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  left_join(
    FB_uses_maize_regional_median,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    per_Food = ifelse(
      Tot_per == 0,
      per_Food_reg_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_per == 0,
      per_Feed_reg_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_per == 0,
      per_Seed_reg_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_per == 0,
      per_Losses_reg_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_per == 0,
      per_Processing_reg_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_per == 0,
      per_Other_Uses_nf_reg_median,
      per_Other_Uses_nf
    )
  ) %>%
  ungroup() %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )

# Rice -------------------------------------------------------------------------

## 1: Extract domestic-use categories from the historical Food Balances --------

# Extract the main domestic-use categories for rice from the historical
# FAOSTAT Food Balance dataset.
#
# FAOSTAT codes:
#   Historical Item 2805 = Rice and products
#
# Element codes:
#   5301 = Domestic supply quantity
#   5521 = Feed
#   5142 = Food
#   5527 = Seed
#   5123 = Losses
#   5131 = Processing
#   5154 = Other non-food uses
#   5170 = Residuals
#
# Residuals are balancing adjustments and are therefore not included when
# calculating the distribution of domestic uses.

FB_old_uses_rice <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2805,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`
  )


## 2: Check historical domestic supply against reported uses -------------------

# Sum the explicit domestic-use categories and compare their total with reported
# domestic supply. This is used as a consistency check and does not modify the
# underlying data.

FB_old_uses_rice_check <- FB_old_uses_rice %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_old_uses_rice_check
  )
)


## 3: Calculate historical domestic-use shares ---------------------------------

# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_old_uses_rice_perc <- FB_old_uses_rice_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_old_{.col}"
    )
  ) %>%
  ungroup()


## 4: Extract domestic-use categories from the current Food Balances ------------

# Extract the equivalent rice-use categories from the current FAOSTAT Food
# Balance dataset.
#
# FAOSTAT code:
#   Current Item 2807 = Rice and products

FB_act_uses_rice <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2807,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`,
    Residuals = `5170`
  )


## 5: Check current domestic supply against reported uses -----------------------

# As for the historical dataset, residuals are excluded from Tot_Uses because
# they represent balancing adjustments rather than an explicit domestic use.

FB_act_uses_rice_check <- FB_act_uses_rice %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_act_uses_rice_check
  )
)


# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_act_uses_rice_perc <- FB_act_uses_rice_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_act_{.col}"
    )
  ) %>%
  ungroup()


## 6: Compare historical and current Food Balance estimates --------------------

# Join the domestic-use shares from the historical and current Food Balance
# datasets. Their overlapping period (2010-2013) is used to assess consistency
# between the two FAOSTAT products.

FB_uses_rice_per <- full_join(
  FB_old_uses_rice_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_rice_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  )
)


# Retain the overlapping years for diagnostic comparisons.

FB_uses_rice_check_FB <- FB_uses_rice_per %>%
  filter(
    Year >= 2010,
    Year <= 2013
  )

summary(
  lm(
    per_old_Food ~ per_act_Food,
    data = FB_uses_rice_check_FB
  )
)

summary(
  lm(
    per_old_Feed ~ per_act_Feed,
    data = FB_uses_rice_check_FB
  )
)

summary(
  lm(
    per_old_Seed ~ per_act_Seed,
    data = FB_uses_rice_check_FB
  )
)

summary(
  lm(
    per_old_Processing ~ per_act_Processing,
    data = FB_uses_rice_check_FB
  )
)

summary(
  lm(
    per_old_Other_Uses_nf ~ per_act_Other_Uses_nf,
    data = FB_uses_rice_check_FB
  )
)


## 7: Harmonize historical and current Food Balance data ------------------------

# Join the full historical and current datasets. Historical values are used
# before 2010, current values after 2013, and the mean of the available values
# is used during the overlapping period (2010-2013).
#
# When only one dataset contains a value during the overlap period, that
# available value is retained.

FB_uses_rice <- full_join(
  FB_old_uses_rice_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_rice_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Residuals,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  ),
  suffix = c("_old", "_act")
) %>%
  rowwise() %>%
  mutate(
    per_Food = case_when(
      Year < 2010 ~ per_old_Food,
      Year > 2013 ~ per_act_Food,
      TRUE ~ mean(
        c(per_old_Food, per_act_Food),
        na.rm = TRUE
      )
    ),
    
    per_Feed = case_when(
      Year < 2010 ~ per_old_Feed,
      Year > 2013 ~ per_act_Feed,
      TRUE ~ mean(
        c(per_old_Feed, per_act_Feed),
        na.rm = TRUE
      )
    ),
    
    per_Seed = case_when(
      Year < 2010 ~ per_old_Seed,
      Year > 2013 ~ per_act_Seed,
      TRUE ~ mean(
        c(per_old_Seed, per_act_Seed),
        na.rm = TRUE
      )
    ),
    
    per_Losses = case_when(
      Year < 2010 ~ per_old_Losses,
      Year > 2013 ~ per_act_Losses,
      TRUE ~ mean(
        c(per_old_Losses, per_act_Losses),
        na.rm = TRUE
      )
    ),
    
    per_Processing = case_when(
      Year < 2010 ~ per_old_Processing,
      Year > 2013 ~ per_act_Processing,
      TRUE ~ mean(
        c(per_old_Processing, per_act_Processing),
        na.rm = TRUE
      )
    ),
    
    per_Other_Uses_nf = case_when(
      Year < 2010 ~ per_old_Other_Uses_nf,
      Year > 2013 ~ per_act_Other_Uses_nf,
      TRUE ~ mean(
        c(per_old_Other_Uses_nf, per_act_Other_Uses_nf),
        na.rm = TRUE
      )
    ),
    
    Domestic_Supply = case_when(
      Year < 2010 ~ Domestic_Supply_old,
      Year > 2013 ~ Domestic_Supply_act,
      TRUE ~ mean(
        c(Domestic_Supply_old, Domestic_Supply_act),
        na.rm = TRUE
      )
    ),
    
    Food = case_when(
      Year < 2010 ~ Food_old,
      Year > 2013 ~ Food_act,
      TRUE ~ mean(
        c(Food_old, Food_act),
        na.rm = TRUE
      )
    ),
    
    Feed = case_when(
      Year < 2010 ~ Feed_old,
      Year > 2013 ~ Feed_act,
      TRUE ~ mean(
        c(Feed_old, Feed_act),
        na.rm = TRUE
      )
    ),
    
    Seed = case_when(
      Year < 2010 ~ Seed_old,
      Year > 2013 ~ Seed_act,
      TRUE ~ mean(
        c(Seed_old, Seed_act),
        na.rm = TRUE
      )
    ),
    
    Losses = case_when(
      Year < 2010 ~ Losses_old,
      Year > 2013 ~ Losses_act,
      TRUE ~ mean(
        c(Losses_old, Losses_act),
        na.rm = TRUE
      )
    ),
    
    Processing = case_when(
      Year < 2010 ~ Processing_old,
      Year > 2013 ~ Processing_act,
      TRUE ~ mean(
        c(Processing_old, Processing_act),
        na.rm = TRUE
      )
    ),
    
    Other_Uses_nf = case_when(
      Year < 2010 ~ Other_Uses_nf_old,
      Year > 2013 ~ Other_Uses_nf_act,
      TRUE ~ mean(
        c(Other_Uses_nf_old, Other_Uses_nf_act),
        na.rm = TRUE
      )
    ),
    
    Tot_Uses = case_when(
      Year < 2010 ~ Tot_Uses_old,
      Year > 2013 ~ Tot_Uses_act,
      TRUE ~ mean(
        c(Tot_Uses_old, Tot_Uses_act),
        na.rm = TRUE
      )
    )
  ) %>%
  ungroup()


# Replace NaN values generated when both historical and current estimates are
# missing with NA so that they can subsequently be imputed.

FB_uses_rice <- FB_uses_rice %>%
  mutate(
    across(
      c(
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf,
        Domestic_Supply,
        Food,
        Feed,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf,
        Tot_Uses
      ),
      ~ ifelse(is.nan(.x), NA, .x)
    )
  )


# Harmonize the historical Belgium-Luxembourg FAOSTAT area code with Belgium.
# This allows historical and later records to be treated as a single country
# series in the subsequent imputation procedure.

FB_uses_rice <- FB_uses_rice %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


## 8: Create complete country-year series ---------------------------------------

# Generate all combinations of FAOSTAT countries and years in the study period
# and join the observed domestic-use shares. Missing country-years are filled in
# subsequently using country and regional median allocation patterns.

FB_uses_rice_imp <- expand.grid(
  FAOStat_area_code =
    unique(Countries_DB$FAOStat_area_code),
  Year = seq(1961, 2019)
) %>%
  left_join(
    FB_uses_rice %>%
      select(
        Year,
        FAOStat_area_code,
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf
      ),
    by = c(
      "Year",
      "FAOStat_area_code"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_perc = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()


## 9: Impute missing country-years using country median shares ------------------

# Estimate the median allocation pattern for each country across all years with
# available information.

FB_uses_rice_avg <- FB_uses_rice %>%
  select(
    FAOStat_area_code,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize median shares so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  rename(
    per_Food_median = per_Food,
    per_Feed_median = per_Feed,
    per_Seed_median = per_Seed,
    per_Losses_median = per_Losses,
    per_Processing_median = per_Processing,
    per_Other_Uses_nf_median =
      per_Other_Uses_nf
  )


# Where an entire country-year allocation is missing, replace it with the
# country's median allocation pattern.

FB_uses_rice_imp <- FB_uses_rice_imp %>%
  left_join(
    FB_uses_rice_avg,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    per_Food = ifelse(
      Tot_perc == 0,
      per_Food_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_perc == 0,
      per_Feed_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_perc == 0,
      per_Seed_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_perc == 0,
      per_Losses_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_perc == 0,
      per_Processing_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_perc == 0,
      per_Other_Uses_nf_median,
      per_Other_Uses_nf
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )


## 10: Impute remaining gaps using regional median shares -----------------------

# Calculate annual median allocation patterns for each FAOSTAT region. These
# values are used only for countries for which a country-level median could not
# be estimated.

FB_uses_rice_regional_median <- FB_uses_rice_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  group_by(
    Year,
    Region
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize regional medians so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  drop_na(
    Region
  ) %>%
  rename(
    per_Food_reg_median = per_Food,
    per_Feed_reg_median = per_Feed,
    per_Seed_reg_median = per_Seed,
    per_Losses_reg_median = per_Losses,
    per_Processing_reg_median =
      per_Processing,
    per_Other_Uses_nf_reg_median =
      per_Other_Uses_nf
  ) %>%
  select(
    Year,
    Region,
    per_Food_reg_median,
    per_Feed_reg_median,
    per_Seed_reg_median,
    per_Losses_reg_median,
    per_Processing_reg_median,
    per_Other_Uses_nf_reg_median
  )


# Assign regional median allocation shares to country-years that remain without
# an allocation after country-level imputation.

FB_uses_rice_imp <- FB_uses_rice_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  left_join(
    FB_uses_rice_regional_median,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    per_Food = ifelse(
      Tot_per == 0,
      per_Food_reg_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_per == 0,
      per_Feed_reg_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_per == 0,
      per_Seed_reg_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_per == 0,
      per_Losses_reg_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_per == 0,
      per_Processing_reg_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_per == 0,
      per_Other_Uses_nf_reg_median,
      per_Other_Uses_nf
    )
  ) %>%
  ungroup() %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )

# Soybean ----------------------------------------------------------------------

## 1: Extract domestic-use categories from the historical Food Balances --------

# Extract the main domestic-use categories for soybean from the historical
# FAOSTAT Food Balance dataset.
#
# FAOSTAT codes:
#   Item 2555    = Soyabeans
#
# Element codes:
#   5301 = Domestic supply quantity
#   5521 = Feed
#   5142 = Food
#   5527 = Seed
#   5123 = Losses
#   5131 = Processing
#   5154 = Other non-food uses
#   5170 = Residuals
#
# Residuals are balancing adjustments and are therefore not included when
# calculating the distribution of domestic uses.

FB_old_uses_soybean <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2555,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`
  )


## 2: Check historical domestic supply against reported uses -------------------

# Sum the explicit domestic-use categories and compare their total with reported
# domestic supply. This is used as a consistency check and does not modify the
# underlying data.

FB_old_uses_soybean_check <- FB_old_uses_soybean %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_old_uses_soybean_check
  )
)


## 3: Calculate historical domestic-use shares ---------------------------------

# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_old_uses_soybean_perc <- FB_old_uses_soybean_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_old_{.col}"
    )
  ) %>%
  ungroup()


## 4: Extract domestic-use categories from the current Food Balances ------------

# Extract the equivalent soybean-use categories from the current FAOSTAT Food
# Balance dataset.

FB_act_uses_soybean <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2555,
    Element_code %in% c(
      5170,
      5123,
      5521,
      5142,
      5154,
      5131,
      5527,
      5301
    )
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Seed = `5527`,
    Losses = `5123`,
    Food = `5142`,
    Feed = `5521`,
    Other_Uses_nf = `5154`,
    Processing = `5131`,
    Residuals = `5170`
  )


## 5: Check current domestic supply against reported uses -----------------------

# As for the historical dataset, residuals are excluded from Tot_Uses because
# they represent balancing adjustments rather than an explicit domestic use.

FB_act_uses_soybean_check <- FB_act_uses_soybean %>%
  rowwise() %>%
  mutate(
    Tot_Uses = sum(
      Feed,
      Food,
      Seed,
      Losses,
      Processing,
      Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    Tot_Uses ~ Domestic_Supply,
    data = FB_act_uses_soybean_check
  )
)


# Express each domestic-use category as a fraction of the total explicitly
# reported uses.

FB_act_uses_soybean_perc <- FB_act_uses_soybean_check %>%
  rowwise() %>%
  mutate(
    across(
      c(
        Feed,
        Food,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf
      ),
      ~ .x / Tot_Uses,
      .names = "per_act_{.col}"
    )
  ) %>%
  ungroup()


## 6: Compare historical and current Food Balance estimates --------------------

# Join the domestic-use shares from the historical and current Food Balance
# datasets. Their overlapping period (2010-2013) is used to assess consistency
# between the two FAOSTAT products.

FB_uses_soybean_per <- full_join(
  FB_old_uses_soybean_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_soybean_perc %>%
    select(
      FAOStat_area_code,
      Year,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  )
)


# Retain the overlapping years for diagnostic comparisons.

FB_uses_soybean_check_FB <- FB_uses_soybean_per %>%
  filter(
    Year >= 2010,
    Year <= 2013
  )

summary(
  lm(
    per_old_Food ~ per_act_Food,
    data = FB_uses_soybean_check_FB
  )
)

summary(
  lm(
    per_old_Feed ~ per_act_Feed,
    data = FB_uses_soybean_check_FB
  )
)

summary(
  lm(
    per_old_Seed ~ per_act_Seed,
    data = FB_uses_soybean_check_FB
  )
)

summary(
  lm(
    per_old_Processing ~ per_act_Processing,
    data = FB_uses_soybean_check_FB
  )
)

summary(
  lm(
    per_old_Other_Uses_nf ~ per_act_Other_Uses_nf,
    data = FB_uses_soybean_check_FB
  )
)


## 7: Harmonize historical and current Food Balance data ------------------------

# Join the full historical and current datasets. Historical values are used
# before 2010, current values after 2013, and the mean of the available values
# is used during the overlapping period (2010-2013).
#
# When only one dataset contains a value during the overlap period, that
# available value is retained.

FB_uses_soybean <- full_join(
  FB_old_uses_soybean_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_old_Feed,
      per_old_Food,
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed
    ),
  FB_act_uses_soybean_perc %>%
    select(
      FAOStat_area_code,
      Year,
      Domestic_Supply,
      Feed,
      Losses,
      Other_Uses_nf,
      Residuals,
      Food,
      Seed,
      Processing,
      Tot_Uses,
      per_act_Feed,
      per_act_Food,
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed
    ),
  by = c(
    "FAOStat_area_code",
    "Year"
  ),
  suffix = c("_old", "_act")
) %>%
  rowwise() %>%
  mutate(
    per_Food = case_when(
      Year < 2010 ~ per_old_Food,
      Year > 2013 ~ per_act_Food,
      TRUE ~ mean(
        c(per_old_Food, per_act_Food),
        na.rm = TRUE
      )
    ),
    
    per_Feed = case_when(
      Year < 2010 ~ per_old_Feed,
      Year > 2013 ~ per_act_Feed,
      TRUE ~ mean(
        c(per_old_Feed, per_act_Feed),
        na.rm = TRUE
      )
    ),
    
    per_Seed = case_when(
      Year < 2010 ~ per_old_Seed,
      Year > 2013 ~ per_act_Seed,
      TRUE ~ mean(
        c(per_old_Seed, per_act_Seed),
        na.rm = TRUE
      )
    ),
    
    per_Losses = case_when(
      Year < 2010 ~ per_old_Losses,
      Year > 2013 ~ per_act_Losses,
      TRUE ~ mean(
        c(per_old_Losses, per_act_Losses),
        na.rm = TRUE
      )
    ),
    
    per_Processing = case_when(
      Year < 2010 ~ per_old_Processing,
      Year > 2013 ~ per_act_Processing,
      TRUE ~ mean(
        c(per_old_Processing, per_act_Processing),
        na.rm = TRUE
      )
    ),
    
    per_Other_Uses_nf = case_when(
      Year < 2010 ~ per_old_Other_Uses_nf,
      Year > 2013 ~ per_act_Other_Uses_nf,
      TRUE ~ mean(
        c(per_old_Other_Uses_nf, per_act_Other_Uses_nf),
        na.rm = TRUE
      )
    ),
    
    Domestic_Supply = case_when(
      Year < 2010 ~ Domestic_Supply_old,
      Year > 2013 ~ Domestic_Supply_act,
      TRUE ~ mean(
        c(Domestic_Supply_old, Domestic_Supply_act),
        na.rm = TRUE
      )
    ),
    
    Food = case_when(
      Year < 2010 ~ Food_old,
      Year > 2013 ~ Food_act,
      TRUE ~ mean(
        c(Food_old, Food_act),
        na.rm = TRUE
      )
    ),
    
    Feed = case_when(
      Year < 2010 ~ Feed_old,
      Year > 2013 ~ Feed_act,
      TRUE ~ mean(
        c(Feed_old, Feed_act),
        na.rm = TRUE
      )
    ),
    
    Seed = case_when(
      Year < 2010 ~ Seed_old,
      Year > 2013 ~ Seed_act,
      TRUE ~ mean(
        c(Seed_old, Seed_act),
        na.rm = TRUE
      )
    ),
    
    Losses = case_when(
      Year < 2010 ~ Losses_old,
      Year > 2013 ~ Losses_act,
      TRUE ~ mean(
        c(Losses_old, Losses_act),
        na.rm = TRUE
      )
    ),
    
    Processing = case_when(
      Year < 2010 ~ Processing_old,
      Year > 2013 ~ Processing_act,
      TRUE ~ mean(
        c(Processing_old, Processing_act),
        na.rm = TRUE
      )
    ),
    
    Other_Uses_nf = case_when(
      Year < 2010 ~ Other_Uses_nf_old,
      Year > 2013 ~ Other_Uses_nf_act,
      TRUE ~ mean(
        c(Other_Uses_nf_old, Other_Uses_nf_act),
        na.rm = TRUE
      )
    ),
    
    Tot_Uses = case_when(
      Year < 2010 ~ Tot_Uses_old,
      Year > 2013 ~ Tot_Uses_act,
      TRUE ~ mean(
        c(Tot_Uses_old, Tot_Uses_act),
        na.rm = TRUE
      )
    )
  ) %>%
  ungroup()


# Replace NaN values generated when both historical and current estimates are
# missing with NA so that they can subsequently be imputed.

FB_uses_soybean <- FB_uses_soybean %>%
  mutate(
    across(
      c(
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf,
        Domestic_Supply,
        Food,
        Feed,
        Seed,
        Losses,
        Processing,
        Other_Uses_nf,
        Tot_Uses
      ),
      ~ ifelse(is.nan(.x), NA, .x)
    )
  )


# Harmonize the historical Belgium-Luxembourg FAOSTAT area code with Belgium.
# This allows historical and later records to be treated as a single country
# series in the subsequent imputation procedure.

FB_uses_soybean <- FB_uses_soybean %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


## 8: Create complete country-year series ---------------------------------------

# Generate all combinations of FAOSTAT countries and years in the study period
# and join the observed domestic-use shares. Missing country-years are filled in
# subsequently using country and regional median allocation patterns.

FB_uses_soybean_imp <- expand.grid(
  FAOStat_area_code =
    unique(Countries_DB$FAOStat_area_code),
  Year = seq(1961, 2019)
) %>%
  left_join(
    FB_uses_soybean %>%
      select(
        Year,
        FAOStat_area_code,
        per_Food,
        per_Feed,
        per_Seed,
        per_Losses,
        per_Processing,
        per_Other_Uses_nf
      ),
    by = c(
      "Year",
      "FAOStat_area_code"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_perc = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup()


## 9: Impute missing country-years using country median shares ------------------

# Estimate the median allocation pattern for each country across all years with
# available information.

FB_uses_soybean_avg <- FB_uses_soybean %>%
  select(
    FAOStat_area_code,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  group_by(
    FAOStat_area_code
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize median shares so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  rename(
    per_Food_median = per_Food,
    per_Feed_median = per_Feed,
    per_Seed_median = per_Seed,
    per_Losses_median = per_Losses,
    per_Processing_median = per_Processing,
    per_Other_Uses_nf_median =
      per_Other_Uses_nf
  )


# Where an entire country-year allocation is missing, replace it with the
# country's median allocation pattern.

FB_uses_soybean_imp <- FB_uses_soybean_imp %>%
  left_join(
    FB_uses_soybean_avg,
    by = "FAOStat_area_code"
  ) %>%
  mutate(
    per_Food = ifelse(
      Tot_perc == 0,
      per_Food_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_perc == 0,
      per_Feed_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_perc == 0,
      per_Seed_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_perc == 0,
      per_Losses_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_perc == 0,
      per_Processing_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_perc == 0,
      per_Other_Uses_nf_median,
      per_Other_Uses_nf
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )


## 10: Impute remaining gaps using regional median shares -----------------------

# Calculate annual median allocation patterns for each FAOSTAT region. These
# values are used only for countries for which a country-level median could not
# be estimated.

FB_uses_soybean_regional_median <- FB_uses_soybean_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  group_by(
    Year,
    Region
  ) %>%
  summarise(
    per_Food = median(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed = median(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed = median(
      per_Seed,
      na.rm = TRUE
    ),
    per_Losses = median(
      per_Losses,
      na.rm = TRUE
    ),
    per_Processing = median(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf = median(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    # Re-normalize regional medians so that they sum to one.
    per_Food = per_Food / Tot_per,
    per_Feed = per_Feed / Tot_per,
    per_Seed = per_Seed / Tot_per,
    per_Losses = per_Losses / Tot_per,
    per_Processing = per_Processing / Tot_per,
    per_Other_Uses_nf =
      per_Other_Uses_nf / Tot_per
  ) %>%
  ungroup() %>%
  drop_na(
    Region
  ) %>%
  rename(
    per_Food_reg_median = per_Food,
    per_Feed_reg_median = per_Feed,
    per_Seed_reg_median = per_Seed,
    per_Losses_reg_median = per_Losses,
    per_Processing_reg_median =
      per_Processing,
    per_Other_Uses_nf_reg_median =
      per_Other_Uses_nf
  ) %>%
  select(
    Year,
    Region,
    per_Food_reg_median,
    per_Feed_reg_median,
    per_Seed_reg_median,
    per_Losses_reg_median,
    per_Processing_reg_median,
    per_Other_Uses_nf_reg_median
  )


# Assign regional median allocation shares to country-years that remain without
# an allocation after country-level imputation.

FB_uses_soybean_imp <- FB_uses_soybean_imp %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code = 2
      ) %>%
      mutate(
        FAOStat_area_code =
          as.numeric(FAOStat_area_code)
      ),
    by = "FAOStat_area_code"
  ) %>%
  left_join(
    FB_uses_soybean_regional_median,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  rowwise() %>%
  mutate(
    Tot_per = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    
    per_Food = ifelse(
      Tot_per == 0,
      per_Food_reg_median,
      per_Food
    ),
    per_Feed = ifelse(
      Tot_per == 0,
      per_Feed_reg_median,
      per_Feed
    ),
    per_Seed = ifelse(
      Tot_per == 0,
      per_Seed_reg_median,
      per_Seed
    ),
    per_Losses = ifelse(
      Tot_per == 0,
      per_Losses_reg_median,
      per_Losses
    ),
    per_Processing = ifelse(
      Tot_per == 0,
      per_Processing_reg_median,
      per_Processing
    ),
    per_Other_Uses_nf = ifelse(
      Tot_per == 0,
      per_Other_Uses_nf_reg_median,
      per_Other_Uses_nf
    )
  ) %>%
  ungroup() %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  )


# Export Results ----------------------------------------------------------
write.csv(FB_uses_wheat_imp, "results/FB_uses_wheat.csv", row.names = F)
write.csv(FB_uses_maize_imp, "results/FB_uses_maize.csv", row.names = F)
write.csv(FB_uses_rice_imp, "results/FB_uses_rice.csv", row.names = F)
write.csv(FB_uses_soybean_imp, "results/FB_uses_soybean.csv", row.names = F)


# Compare historical and current Food Balance estimates ------------------------

# Combine the 2010-2013 overlap-period observations for all four crops.
# These data are used to assess agreement between the historical and current
# FAOSTAT Food Balance datasets.

FB_uses_check_FB <- bind_rows(
  FB_uses_wheat_check_FB,
  FB_uses_maize_check_FB,
  FB_uses_rice_check_FB,
  FB_uses_soybean_check_FB
)


## 1: Compare food-use shares ---------------------------------------------------

FB_uses_check_FB_food <- FB_uses_check_FB %>%
  select(
    per_old_Food,
    per_act_Food
  ) %>%
  rename(
    per_old = per_old_Food,
    per_act = per_act_Food
  )

summary(
  lm(
    per_old ~ per_act,
    data = FB_uses_check_FB_food
  )
)


## 2: Compare feed-use shares ---------------------------------------------------

FB_uses_check_FB_feed <- FB_uses_check_FB %>%
  select(
    per_old_Feed,
    per_act_Feed
  ) %>%
  rename(
    per_old = per_old_Feed,
    per_act = per_act_Feed
  )

summary(
  lm(
    per_old ~ per_act,
    data = FB_uses_check_FB_feed
  )
)


## 3: Compare remaining domestic-use shares ------------------------------------

# Combine seed, losses, processing, and other non-food uses into a single
# "other uses" category for comparison between the two Food Balance datasets.

FB_uses_check_FB_ou <- FB_uses_check_FB %>%
  rowwise() %>%
  transmute(
    per_old = sum(
      per_old_Losses,
      per_old_Processing,
      per_old_Other_Uses_nf,
      per_old_Seed,
      na.rm = TRUE
    ),
    per_act = sum(
      per_act_Losses,
      per_act_Processing,
      per_act_Other_Uses_nf,
      per_act_Seed,
      na.rm = TRUE
    )
  ) %>%
  ungroup()

summary(
  lm(
    per_old ~ per_act,
    data = FB_uses_check_FB_ou
  )
)


## 4: Plot comparisons ----------------------------------------------------------

# Food-use shares
c_FB1 <- ggplot(
  FB_uses_check_FB_food,
  aes(
    x = per_old * 100,
    y = per_act * 100
  )
) +
  geom_point(
    color = "#548235",
    alpha = 0.5
  ) +
  geom_smooth(
    color = "black",
    method = "lm"
  ) +
  labs(
    x = "Food use (%) - Historical FB",
    y = "Food use (%) - Current FB"
  ) +
  theme_minimal()


# Feed-use shares
c_FB2 <- ggplot(
  FB_uses_check_FB_feed,
  aes(
    x = per_old * 100,
    y = per_act * 100
  )
) +
  geom_point(
    color = "#a9d18e",
    alpha = 0.5
  ) +
  geom_smooth(
    color = "black",
    method = "lm"
  ) +
  labs(
    x = "Feed use (%) - Historical FB",
    y = "Feed use (%) - Current FB"
  ) +
  theme_minimal()


# Remaining domestic-use shares
c_FB3 <- ggplot(
  FB_uses_check_FB_ou,
  aes(
    x = per_old * 100,
    y = per_act * 100
  )
) +
  geom_point(
    color = "#e2f0d9",
    alpha = 0.5
  ) +
  geom_smooth(
    color = "black",
    method = "lm"
  ) +
  labs(
    x = "Other uses (%) - Historical FB",
    y = "Other uses (%) - Current FB"
  ) +
  theme_minimal()


## 5: Combine and export comparison figure -------------------------------------

C_FB <- cowplot::plot_grid(
  c_FB1,
  c_FB2,
  c_FB3,
  labels = c("a", "b", "c"),
  nrow = 1
)

ggsave(
  filename = "plots/FBs_Comp.pdf",
  plot = C_FB,
  width = 16,
  height = 5
)
