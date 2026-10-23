# ==============================================================================
# 04_Production_Uses.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
# Estimate the final allocation of crop production among domestic-use
# categories after accounting for international trade.
#
# For wheat, maize, rice, and soybean, the analysis combines:
#
#   1. Domestic-use allocation shares estimated from FAOSTAT Food Balances
#      (food, feed, seed, processing, other non-food uses, and losses).
#
#   2. Bilateral allocation proportions estimated using the trade-matrix
#      methodology.
#
#   3. The fraction of production exported, independently estimated from
#      FAOSTAT Food Balance data.
#
# For exported production, destination-specific domestic-use patterns are
# weighted by the estimated bilateral allocation proportions. These exported
# use patterns are then combined with the domestic-use pattern according to
# the fraction of production exported.
#
# For soybean cake and soybean oil, this script additionally prepares bilateral
# allocation matrices using regional averages where country-level information
# is unavailable.
#
# Inputs:
#   results/FB_uses_wheat.csv
#   results/FB_uses_maize.csv
#   results/FB_uses_rice.csv
#   results/FB_uses_soybean.csv
#
#   results/Wheat_Exp_FB.csv
#   results/Maize_Exp_FB.csv
#   results/Rice_Exp_FB.csv
#   results/Soybean_Exp_FB.csv
#
#   results/Wheat_Kastner_Percentage_df.csv
#   results/maize_Kastner_Percentage_df.csv
#   results/rice_Kastner_Percentage_df.csv
#   results/soybean_Kastner_Percentage_df.csv
#   results/soybean_cake_Kastner_Percentage_df.csv
#   results/soybean_oil_Kastner_Percentage_df.csv
#
# Additional data:
#   data/Countries_DB.xlsx
#   data/FAOSTAT_regions.csv
# ==============================================================================


# Libraries --------------------------------------------------------------------

library(tidyverse)
library(readxl)


# Load data --------------------------------------------------------------------

## 1: Domestic-use allocation shares -------------------------------------------

FB_uses_wheat <- read_csv(
  "results/FB_uses_wheat.csv"
)

FB_uses_maize <- read_csv(
  "results/FB_uses_maize.csv"
)

FB_uses_rice <- read_csv(
  "results/FB_uses_rice.csv"
)

FB_uses_soybean <- read_csv(
  "results/FB_uses_soybean.csv"
)


## 2: Exported fractions --------------------------------------------------------

# Fraction of production exported, independently estimated from FAOSTAT Food
# Balance data.

Wheat_Exp_FB <- read_csv(
  "results/Wheat_Exp_FB.csv"
)

maize_Exp_FB <- read_csv(
  "results/Maize_Exp_FB.csv"
)

rice_Exp_FB <- read_csv(
  "results/Rice_Exp_FB.csv"
)

soybean_Exp_FB <- read_csv(
  "results/Soybean_Exp_FB.csv"
)


## 3: Bilateral trade-allocation proportions -----------------------------------

Wheat_Kastner_Percentage_df <- read_csv(
  "results/Wheat_Kastner_Percentage_df.csv"
)

maize_Kastner_Percentage_df <- read_csv(
  "results/maize_Kastner_Percentage_df.csv"
)

rice_Kastner_Percentage_df <- read_csv(
  "results/rice_Kastner_Percentage_df.csv"
)

soybean_Kastner_Percentage_df <- read_csv(
  "results/soybean_Kastner_Percentage_df.csv"
)

soybean_cake_Kastner_Percentage_df <- read_csv(
  "results/soybean_cake_Kastner_Percentage_df.csv"
)

soybean_oil_Kastner_Percentage_df <- read_csv(
  "results/soybean_oil_Kastner_Percentage_df.csv"
)


## 4: Country classifications ---------------------------------------------------

Countries_DB <- read_excel(
  "data/Countries_DB.xlsx"
) %>%
  select(2, 3) %>%
  rename(
    ISO3 = 1,
    FAOStat_area_code = 2
  )

FAOSTAT_regions <- read_csv(
  "data/FAOSTAT_regions.csv"
)

# Wheat ------------------------------------------------------------------------

## 1: Estimate regional bilateral allocation patterns --------------------------

# Bilateral allocation proportions describe how production from each exporting
# country is distributed among importing countries.
#
# For exporter-years without a usable country-specific allocation pattern, a
# regional allocation pattern is estimated from exporters belonging to the same
# FAOSTAT region.
#
# The regional pattern is calculated separately for each year and importing
# country, using the mean allocation proportion across exporters in the region.

Wheat_Kastner_reg <- Wheat_Kastner_Percentage_df %>%
  mutate(
    # Harmonize historical Belgium-Luxembourg with Belgium before joins.
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  group_by(
    Region,
    Year,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 2: Normalize regional allocation patterns -----------------------------------

# Regional averages are re-normalized so that allocation proportions sum to one
# across importing countries within each region-year.
#
# Some regions may contain no usable bilateral information. In these cases no
# regional allocation pattern can be estimated and the corresponding regional
# values remain missing rather than forcing an artificial allocation.

Wheat_Kastner_reg_tot <- Wheat_Kastner_reg %>%
  group_by(
    Region,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


Wheat_Kastner_reg <- Wheat_Kastner_reg %>%
  left_join(
    Wheat_Kastner_reg_tot,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  mutate(
    Value_reg = ifelse(
      Value_tot > 0,
      Value / Value_tot,
      NA_real_
    )
  ) %>%
  select(
    Region,
    Year,
    FAOStat_area_code_Imp,
    Value_reg
  )


## 3: Identify exporter-years without bilateral allocation ---------------------

# Calculate the total bilateral allocation represented for each exporter-year.
# A total of zero indicates that no country-specific destination allocation is
# available and the regional allocation pattern will subsequently be used.

Wheat_Kastner_Percentage_df <- Wheat_Kastner_Percentage_df %>%
  mutate(
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  )

Wheat_Kastner_Percentage_df_tot <- Wheat_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


Wheat_Kastner_Percentage_df <- Wheat_Kastner_Percentage_df %>%
  left_join(
    Wheat_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  )


## 4: Apply regional allocation patterns where needed ---------------------------

# Attach the FAOSTAT region of each exporting country and use the corresponding
# regional destination pattern where the exporter-year has no estimated
# bilateral allocation.
#
# `Perc_Imp` is retained as the historical object name, but it represents the
# bilateral allocation proportion from an exporter to a destination country,
# rather than an importing fraction.

Wheat_Kastner_Percentage_df <- Wheat_Kastner_Percentage_df %>%
  rename(
    Perc_Imp = Value
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  left_join(
    Wheat_Kastner_reg,
    by = c(
      "Region",
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Value_reg,
      Perc_Imp
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 5: Re-normalize bilateral allocation proportions ----------------------------

# Re-normalize the resulting destination shares so that they sum to one for
# each exporter-year whenever bilateral or regional information is available.

Wheat_Kastner_Percentage_df_tot <- Wheat_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


Wheat_Kastner_Percentage_df <- Wheat_Kastner_Percentage_df %>%
  left_join(
    Wheat_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Perc_Imp,
      Perc_Imp / Value_tot
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 6: Estimate the use profile of exported wheat -------------------------------

# Harmonize the exporter code before joining with the bilateral allocation data.

Wheat_Exp_FB_harmonized <- Wheat_Exp_FB %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


# For each exporter-year:
#
#   1. Identify the destination countries receiving its exported production.
#   2. Attach the domestic-use allocation pattern of each destination country.
#   3. Weight each destination's use pattern by its bilateral allocation share.
#   4. Sum across destinations to obtain the expected use profile of exported
#      wheat.
#
# Exported fractions <= 0.01 are treated as negligible in the final-use allocation.

Wheat_exported_use <- Wheat_Kastner_Percentage_df %>%
  left_join(
    Wheat_Exp_FB_harmonized %>%
      select(
        FAOStat_area_code,
        Year,
        P_exp_FB
      ) %>%
      rename(
        FAOStat_area_code_Exp =
          FAOStat_area_code,
        per_Exp = P_exp_FB
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  
  # Attach the use profile of the importing/destination country.
  left_join(
    FB_uses_wheat %>%
      select(
        FAOStat_area_code,
        Year,
        per_Food,
        per_Feed,
        per_Seed,
        per_Processing,
        per_Other_Uses_nf,
        per_Losses
      ) %>%
      rename(
        FAOStat_area_code_Imp =
          FAOStat_area_code
      ),
    by = c(
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  
  # Weight destination-specific use patterns by bilateral allocation.
  mutate(
    per_Food =
      Perc_Imp * per_Food,
    per_Feed =
      Perc_Imp * per_Feed,
    per_Seed =
      Perc_Imp * per_Seed,
    per_Processing =
      Perc_Imp * per_Processing,
    per_Other_Uses_nf =
      Perc_Imp * per_Other_Uses_nf,
    per_Losses =
      Perc_Imp * per_Losses
  ) %>%
  
  # Aggregate across destinations for each exporter-year.
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    per_Imp = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    per_Food_e = sum(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed_e = sum(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed_e = sum(
      per_Seed,
      na.rm = TRUE
    ),
    per_Processing_e = sum(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf_e = sum(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    per_Losses_e = sum(
      per_Losses,
      na.rm = TRUE
    ),
    per_Exp = first(per_Exp),
    .groups = "drop"
  ) %>%
  
  # Retain exporters with a non-negligible exported fraction.
  filter(
    !is.na(per_Exp),
    per_Exp > 0.01,
    FAOStat_area_code_Exp < 351
  ) %>%
  
  # Re-normalize the exported-use profile by the represented bilateral
  # allocation share.
  mutate(
    per_Food_e = ifelse(
      per_Imp > 0,
      per_Food_e / per_Imp,
      NA_real_
    ),
    per_Feed_e = ifelse(
      per_Imp > 0,
      per_Feed_e / per_Imp,
      NA_real_
    ),
    per_Seed_e = ifelse(
      per_Imp > 0,
      per_Seed_e / per_Imp,
      NA_real_
    ),
    per_Processing_e = ifelse(
      per_Imp > 0,
      per_Processing_e / per_Imp,
      NA_real_
    ),
    per_Other_Uses_nf_e = ifelse(
      per_Imp > 0,
      per_Other_Uses_nf_e / per_Imp,
      NA_real_
    ),
    per_Losses_e = ifelse(
      per_Imp > 0,
      per_Losses_e / per_Imp,
      NA_real_
    )
  ) %>%
  rename(
    FAOStat_area_code =
      FAOStat_area_code_Exp
  ) %>%
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
  )


## 7: Prepare the domestic wheat-use profile -----------------------------------

# Rename domestic-use fractions to distinguish them from the corresponding
# exported-use fractions.

Wheat_domestic_use <- FB_uses_wheat %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Processing,
    per_Other_Uses_nf,
    per_Losses
  ) %>%
  rename(
    per_Food_d = per_Food,
    per_Feed_d = per_Feed,
    per_Seed_d = per_Seed,
    per_Processing_d = per_Processing,
    per_Other_Uses_nf_d =
      per_Other_Uses_nf,
    per_Losses_d = per_Losses
  )


# Columns for which missing values are replaced by zero after combining
# exported and domestic information.

use_cols <- c(
  "per_Food_d",
  "per_Feed_d",
  "per_Seed_d",
  "per_Processing_d",
  "per_Other_Uses_nf_d",
  "per_Losses_d",
  "per_Food_e",
  "per_Feed_e",
  "per_Seed_e",
  "per_Processing_e",
  "per_Other_Uses_nf_e",
  "per_Losses_e",
  "per_Imp"
)


## 8: Estimate final wheat-production use ---------------------------------------

# Combine the domestic and exported components.
#
# For exporter-years with a usable bilateral allocation:
#
#   final use =
#     exported fraction × exported-use profile
#     +
#     domestic fraction × domestic-use profile
#
# where the domestic fraction is (1 - exported fraction).
#
# Exported fractions <= 1% were removed above and therefore enter here as
# missing values; these are set to zero and consequently treated as entirely
# domestic.
#
# If no bilateral allocation is available (`per_Imp == 0`), the domestic-use
# profile is used directly.

Wheat_uses <- full_join(
  Wheat_exported_use,
  Wheat_domestic_use,
  by = c(
    "FAOStat_area_code",
    "Year"
  )
) %>%
  filter(
    Year < 2020
  ) %>%
  mutate(
    across(
      all_of(use_cols),
      ~ replace_na(.x, 0)
    ),
    per_Exp = replace_na(
      per_Exp,
      0
    )
  ) %>%
  rowwise() %>%
  mutate(
    per_Food = ifelse(
      per_Imp == 0,
      per_Food_d,
      per_Exp * per_Food_e +
        (1 - per_Exp) * per_Food_d
    ),
    
    per_Feed = ifelse(
      per_Imp == 0,
      per_Feed_d,
      per_Exp * per_Feed_e +
        (1 - per_Exp) * per_Feed_d
    ),
    
    per_Seed = ifelse(
      per_Imp == 0,
      per_Seed_d,
      per_Exp * per_Seed_e +
        (1 - per_Exp) * per_Seed_d
    ),
    
    per_Processing = ifelse(
      per_Imp == 0,
      per_Processing_d,
      per_Exp * per_Processing_e +
        (1 - per_Exp) * per_Processing_d
    ),
    
    per_Other_Uses_nf = ifelse(
      per_Imp == 0,
      per_Other_Uses_nf_d,
      per_Exp * per_Other_Uses_nf_e +
        (1 - per_Exp) *
        per_Other_Uses_nf_d
    ),
    
    per_Losses = ifelse(
      per_Imp == 0,
      per_Losses_d,
      per_Exp * per_Losses_e +
        (1 - per_Exp) * per_Losses_d
    )
  ) %>%
  ungroup()


## 9: Check resulting allocation shares ----------------------------------------

### 9.1: Check domestic-use shares ----------------------------------------------

# Identify country-years for which the six domestic-use fractions do not sum
# approximately to one.

Wheat_uses_ds <- FB_uses_wheat %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  rowwise() %>%
  mutate(
    per_ds = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_ds < 0.99 | per_ds > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected countries.
# `n_years` gives the number of country-years outside the accepted 0.99-1.01
# range, while min/max values indicate the magnitude of the discrepancy.

Wheat_uses_ds_countries <- Wheat_uses_ds %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall diagnostic summary.

Wheat_uses_ds_summary <- Wheat_uses_ds %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE)
  )


### 9.2: Check exported-use shares ----------------------------------------------

# For countries with a non-negligible exported fraction, verify that the
# estimated exported-use profile sums approximately to one.

Wheat_uses_e <- Wheat_uses %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food_e,
    per_Feed_e,
    per_Seed_e,
    per_Losses_e,
    per_Processing_e,
    per_Other_Uses_nf_e,
    per_Exp
  ) %>%
  rowwise() %>%
  mutate(
    per_e = sum(
      per_Food_e,
      per_Feed_e,
      per_Seed_e,
      per_Losses_e,
      per_Processing_e,
      per_Other_Uses_nf_e,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 5000,
    per_Exp > 0.01,
    per_e < 0.99 | per_e > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected exporters.

Wheat_uses_e_countries <- Wheat_uses_e %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE),
    mean_export_fraction = mean(per_Exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall exported-use diagnostic summary.

Wheat_uses_e_summary <- Wheat_uses_e %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE)
  )


### 9.3: Check final production-use shares --------------------------------------

# Verify that the final combined allocation across all use categories sums
# approximately to one for each country-year.

Wheat_uses_check <- Wheat_uses %>%
  rowwise() %>%
  mutate(
    per_t = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_t < 0.9 | per_t > 1.1
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize countries with final-use totals outside the accepted 0.9-1.1 range.

Wheat_uses_check_countries <- Wheat_uses_check %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall final-use diagnostic summary.

Wheat_uses_check_summary <- Wheat_uses_check %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE)
  )

## 10: Export results -----------------------------------------------------------

write.csv(
  Wheat_uses,
  "results/Wheat_uses.csv",
  row.names = FALSE
)

write.csv(
  Wheat_Kastner_Percentage_df,
  "results/Wheat_Kastner_df_result.csv",
  row.names = FALSE
)

# Maize ------------------------------------------------------------------------

## 1: Estimate regional bilateral allocation patterns --------------------------

# Bilateral allocation proportions describe how production from each exporting
# country is distributed among importing countries.
#
# For exporter-years without a usable country-specific allocation pattern, a
# regional allocation pattern is estimated from exporters belonging to the same
# FAOSTAT region.
#
# The regional pattern is calculated separately for each year and importing
# country, using the mean allocation proportion across exporters in the region.

maize_Kastner_reg <- maize_Kastner_Percentage_df %>%
  mutate(
    # Harmonize historical Belgium-Luxembourg with Belgium before joins.
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  group_by(
    Region,
    Year,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 2: Normalize regional allocation patterns -----------------------------------

# Regional averages are re-normalized so that allocation proportions sum to one
# across importing countries within each region-year.
#
# If a region-year contains no usable bilateral information, no regional
# allocation pattern is imposed.

maize_Kastner_reg_tot <- maize_Kastner_reg %>%
  group_by(
    Region,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


maize_Kastner_reg <- maize_Kastner_reg %>%
  left_join(
    maize_Kastner_reg_tot,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  mutate(
    Value_reg = ifelse(
      Value_tot > 0,
      Value / Value_tot,
      NA_real_
    )
  ) %>%
  select(
    Region,
    Year,
    FAOStat_area_code_Imp,
    Value_reg
  )


## 3: Identify exporter-years without bilateral allocation ---------------------

# Calculate the total bilateral allocation represented for each exporter-year.
# A total of zero indicates that no country-specific destination allocation is
# available and the regional allocation pattern will subsequently be used.

maize_Kastner_Percentage_df <- maize_Kastner_Percentage_df %>%
  mutate(
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  )

maize_Kastner_Percentage_df_tot <- maize_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


maize_Kastner_Percentage_df <- maize_Kastner_Percentage_df %>%
  left_join(
    maize_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  )


## 4: Apply regional allocation patterns where needed ---------------------------

# Attach the FAOSTAT region of each exporting country and use the corresponding
# regional destination pattern where the exporter-year has no estimated
# bilateral allocation.
#
# `Perc_Imp` is retained as the historical object name, but it represents the
# bilateral allocation proportion from an exporter to a destination country.

maize_Kastner_Percentage_df <- maize_Kastner_Percentage_df %>%
  rename(
    Perc_Imp = Value
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  left_join(
    maize_Kastner_reg,
    by = c(
      "Region",
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Value_reg,
      Perc_Imp
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 5: Re-normalize bilateral allocation proportions ----------------------------

# Re-normalize the destination shares so that they sum to one for each
# exporter-year whenever bilateral or regional information is available.

maize_Kastner_Percentage_df_tot <- maize_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


maize_Kastner_Percentage_df <- maize_Kastner_Percentage_df %>%
  left_join(
    maize_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Perc_Imp,
      Perc_Imp / Value_tot
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 6: Estimate the use profile of exported maize -------------------------------

# Harmonize the exporter code before joining with the bilateral allocation data.

maize_Exp_FB_harmonized <- maize_Exp_FB %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


# For each exporter-year:
#
#   1. Identify the destination countries receiving exported maize.
#   2. Attach the domestic-use allocation pattern of each destination country.
#   3. Weight each destination's use pattern by its bilateral allocation share.
#   4. Sum across destinations to obtain the expected use profile of exported
#      maize.
#
# <= 0.01 are treated as negligible in the final-use allocation.

maize_exported_use <- maize_Kastner_Percentage_df %>%
  left_join(
    maize_Exp_FB_harmonized %>%
      select(
        FAOStat_area_code,
        Year,
        P_exp_FB
      ) %>%
      rename(
        FAOStat_area_code_Exp =
          FAOStat_area_code,
        per_Exp = P_exp_FB
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  
  # Attach the use profile of the importing/destination country.
  left_join(
    FB_uses_maize %>%
      select(
        FAOStat_area_code,
        Year,
        per_Food,
        per_Feed,
        per_Seed,
        per_Processing,
        per_Other_Uses_nf,
        per_Losses
      ) %>%
      rename(
        FAOStat_area_code_Imp =
          FAOStat_area_code
      ),
    by = c(
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  
  # Weight destination-specific use patterns by bilateral allocation.
  mutate(
    per_Food =
      Perc_Imp * per_Food,
    per_Feed =
      Perc_Imp * per_Feed,
    per_Seed =
      Perc_Imp * per_Seed,
    per_Processing =
      Perc_Imp * per_Processing,
    per_Other_Uses_nf =
      Perc_Imp * per_Other_Uses_nf,
    per_Losses =
      Perc_Imp * per_Losses
  ) %>%
  
  # Aggregate across destinations for each exporter-year.
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    per_Imp = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    per_Food_e = sum(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed_e = sum(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed_e = sum(
      per_Seed,
      na.rm = TRUE
    ),
    per_Processing_e = sum(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf_e = sum(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    per_Losses_e = sum(
      per_Losses,
      na.rm = TRUE
    ),
    per_Exp = first(per_Exp),
    .groups = "drop"
  ) %>%
  
  # Retain exporters with a non-negligible exported fraction.
  filter(
    !is.na(per_Exp),
    per_Exp > 0.01,
    FAOStat_area_code_Exp < 351
  ) %>%
  
  # Re-normalize the exported-use profile by the represented bilateral
  # allocation share.
  #
  mutate(
    per_Food_e = ifelse(
      per_Imp > 0,
      per_Food_e / per_Imp,
      NA_real_
    ),
    per_Feed_e = ifelse(
      per_Imp > 0,
      per_Feed_e / per_Imp,
      NA_real_
    ),
    per_Seed_e = ifelse(
      per_Imp > 0,
      per_Seed_e / per_Imp,
      NA_real_
    ),
    per_Processing_e = ifelse(
      per_Imp > 0,
      per_Processing_e / per_Imp,
      NA_real_
    ),
    per_Other_Uses_nf_e = ifelse(
      per_Imp > 0,
      per_Other_Uses_nf_e / per_Imp,
      NA_real_
    ),
    per_Losses_e = ifelse(
      per_Imp > 0,
      per_Losses_e / per_Imp,
      NA_real_
    )
  ) %>%
  rename(
    FAOStat_area_code =
      FAOStat_area_code_Exp
  ) %>%
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
  )


## 7: Prepare the domestic maize-use profile -----------------------------------

# Rename domestic-use fractions to distinguish them from the corresponding
# exported-use fractions.

maize_domestic_use <- FB_uses_maize %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Processing,
    per_Other_Uses_nf,
    per_Losses
  ) %>%
  rename(
    per_Food_d = per_Food,
    per_Feed_d = per_Feed,
    per_Seed_d = per_Seed,
    per_Processing_d = per_Processing,
    per_Other_Uses_nf_d =
      per_Other_Uses_nf,
    per_Losses_d = per_Losses
  )


## 8: Estimate final maize-production use ---------------------------------------

# Combine domestic and exported use profiles.
#
# For exporter-years with a usable bilateral allocation:
#
#   final use =
#     exported fraction × exported-use profile
#     +
#     domestic fraction × domestic-use profile
#
# Export fractions <= 1% enter as zero and are consequently treated as entirely
# domestic. If no bilateral allocation is available (`per_Imp == 0`), the
# domestic-use profile is used directly.

maize_uses <- full_join(
  maize_exported_use,
  maize_domestic_use,
  by = c(
    "FAOStat_area_code",
    "Year"
  )
) %>%
  filter(
    Year < 2020
  ) %>%
  mutate(
    across(
      all_of(use_cols),
      ~ replace_na(.x, 0)
    ),
    per_Exp = replace_na(
      per_Exp,
      0
    )
  ) %>%
  rowwise() %>%
  mutate(
    per_Food = ifelse(
      per_Imp == 0,
      per_Food_d,
      per_Exp * per_Food_e +
        (1 - per_Exp) * per_Food_d
    ),
    
    per_Feed = ifelse(
      per_Imp == 0,
      per_Feed_d,
      per_Exp * per_Feed_e +
        (1 - per_Exp) * per_Feed_d
    ),
    
    per_Seed = ifelse(
      per_Imp == 0,
      per_Seed_d,
      per_Exp * per_Seed_e +
        (1 - per_Exp) * per_Seed_d
    ),
    
    per_Processing = ifelse(
      per_Imp == 0,
      per_Processing_d,
      per_Exp * per_Processing_e +
        (1 - per_Exp) * per_Processing_d
    ),
    
    per_Other_Uses_nf = ifelse(
      per_Imp == 0,
      per_Other_Uses_nf_d,
      per_Exp * per_Other_Uses_nf_e +
        (1 - per_Exp) *
        per_Other_Uses_nf_d
    ),
    
    per_Losses = ifelse(
      per_Imp == 0,
      per_Losses_d,
      per_Exp * per_Losses_e +
        (1 - per_Exp) * per_Losses_d
    )
  ) %>%
  ungroup()


## 9: Check resulting allocation shares ----------------------------------------

### 9.1: Check domestic-use shares ----------------------------------------------

maize_uses_ds <- FB_uses_maize %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  rowwise() %>%
  mutate(
    per_ds = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_ds < 0.99 | per_ds > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected countries.

maize_uses_ds_countries <- maize_uses_ds %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall domestic-use diagnostic summary.

maize_uses_ds_summary <- maize_uses_ds %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE)
  )


### 9.2: Check exported-use shares ----------------------------------------------

maize_uses_e <- maize_uses %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food_e,
    per_Feed_e,
    per_Seed_e,
    per_Losses_e,
    per_Processing_e,
    per_Other_Uses_nf_e,
    per_Exp
  ) %>%
  rowwise() %>%
  mutate(
    per_e = sum(
      per_Food_e,
      per_Feed_e,
      per_Seed_e,
      per_Losses_e,
      per_Processing_e,
      per_Other_Uses_nf_e,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 5000,
    per_Exp > 0.01,
    per_e < 0.99 | per_e > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


maize_uses_e_countries <- maize_uses_e %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE),
    mean_export_fraction = mean(per_Exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


maize_uses_e_summary <- maize_uses_e %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE)
  )


### 9.3: Check final production-use shares --------------------------------------

maize_uses_check <- maize_uses %>%
  rowwise() %>%
  mutate(
    per_t = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_t < 0.9 | per_t > 1.1
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


maize_uses_check_countries <- maize_uses_check %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


maize_uses_check_summary <- maize_uses_check %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE)
  )


## 10: Export results -----------------------------------------------------------

write.csv(
  maize_uses,
  "results/maize_uses.csv",
  row.names = FALSE
)

write.csv(
  maize_Kastner_Percentage_df,
  "results/Maize_Kastner_df_result.csv",
  row.names = FALSE
)

# Rice -------------------------------------------------------------------------

## 1: Estimate regional bilateral allocation patterns --------------------------

# Bilateral allocation proportions describe how production from each exporting
# country is distributed among importing countries.
#
# For exporter-years without a usable country-specific allocation pattern, a
# regional allocation pattern is estimated from exporters belonging to the same
# FAOSTAT region.
#
# The regional pattern is calculated separately for each year and importing
# country, using the mean allocation proportion across exporters in the region.

rice_Kastner_reg <- rice_Kastner_Percentage_df %>%
  mutate(
    # Harmonize historical Belgium-Luxembourg with Belgium before joins.
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  group_by(
    Region,
    Year,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 2: Normalize regional allocation patterns -----------------------------------

# Regional averages are re-normalized so that allocation proportions sum to one
# across importing countries within each region-year.
#
# If a region-year contains no usable bilateral information, no regional
# allocation pattern is imposed.

rice_Kastner_reg_tot <- rice_Kastner_reg %>%
  group_by(
    Region,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


rice_Kastner_reg <- rice_Kastner_reg %>%
  left_join(
    rice_Kastner_reg_tot,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  mutate(
    Value_reg = ifelse(
      Value_tot > 0,
      Value / Value_tot,
      NA_real_
    )
  ) %>%
  select(
    Region,
    Year,
    FAOStat_area_code_Imp,
    Value_reg
  )


## 3: Identify exporter-years without bilateral allocation ---------------------

# Calculate the total bilateral allocation represented for each exporter-year.
# A total of zero indicates that no country-specific destination allocation is
# available and the regional allocation pattern will subsequently be used.

rice_Kastner_Percentage_df <- rice_Kastner_Percentage_df %>%
  mutate(
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  )

rice_Kastner_Percentage_df_tot <- rice_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


rice_Kastner_Percentage_df <- rice_Kastner_Percentage_df %>%
  left_join(
    rice_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  )


## 4: Apply regional allocation patterns where needed ---------------------------

# Attach the FAOSTAT region of each exporting country and use the corresponding
# regional destination pattern where the exporter-year has no estimated
# bilateral allocation.
#
# `Perc_Imp` is retained as the historical object name, but it represents the
# bilateral allocation proportion from an exporter to a destination country.

rice_Kastner_Percentage_df <- rice_Kastner_Percentage_df %>%
  rename(
    Perc_Imp = Value
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  left_join(
    rice_Kastner_reg,
    by = c(
      "Region",
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Value_reg,
      Perc_Imp
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 5: Re-normalize bilateral allocation proportions ----------------------------

# Re-normalize the destination shares so that they sum to one for each
# exporter-year whenever bilateral or regional information is available.

rice_Kastner_Percentage_df_tot <- rice_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


rice_Kastner_Percentage_df <- rice_Kastner_Percentage_df %>%
  left_join(
    rice_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Perc_Imp,
      Perc_Imp / Value_tot
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 6: Estimate the use profile of exported rice --------------------------------

# Harmonize the exporter code before joining with the bilateral allocation data.

rice_Exp_FB_harmonized <- rice_Exp_FB %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


# For each exporter-year:
#
#   1. Identify the destination countries receiving exported rice.
#   2. Attach the domestic-use allocation pattern of each destination country.
#   3. Weight each destination's use pattern by its bilateral allocation share.
#   4. Sum across destinations to obtain the expected use profile of exported
#      rice.
#
# Exported fractions <= 0.01 are treated as negligible in the final-use

rice_exported_use <- rice_Kastner_Percentage_df %>%
  left_join(
    rice_Exp_FB_harmonized %>%
      select(
        FAOStat_area_code,
        Year,
        P_exp_FB
      ) %>%
      rename(
        FAOStat_area_code_Exp =
          FAOStat_area_code,
        per_Exp = P_exp_FB
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  
  # Attach the use profile of the importing/destination country.
  left_join(
    FB_uses_rice %>%
      select(
        FAOStat_area_code,
        Year,
        per_Food,
        per_Feed,
        per_Seed,
        per_Processing,
        per_Other_Uses_nf,
        per_Losses
      ) %>%
      rename(
        FAOStat_area_code_Imp =
          FAOStat_area_code
      ),
    by = c(
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  
  # Weight destination-specific use patterns by bilateral allocation.
  mutate(
    per_Food =
      Perc_Imp * per_Food,
    per_Feed =
      Perc_Imp * per_Feed,
    per_Seed =
      Perc_Imp * per_Seed,
    per_Processing =
      Perc_Imp * per_Processing,
    per_Other_Uses_nf =
      Perc_Imp * per_Other_Uses_nf,
    per_Losses =
      Perc_Imp * per_Losses
  ) %>%
  
  # Aggregate across destinations for each exporter-year.
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    per_Imp = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    per_Food_e = sum(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed_e = sum(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed_e = sum(
      per_Seed,
      na.rm = TRUE
    ),
    per_Processing_e = sum(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf_e = sum(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    per_Losses_e = sum(
      per_Losses,
      na.rm = TRUE
    ),
    per_Exp = first(per_Exp),
    .groups = "drop"
  ) %>%
  
  # Retain exporters with a non-negligible exported fraction.
  filter(
    !is.na(per_Exp),
    per_Exp > 0.01,
    FAOStat_area_code_Exp < 351
  ) %>%
  
  # Re-normalize the exported-use profile by the represented bilateral
  # allocation share.
  mutate(
    per_Food_e = ifelse(
      per_Imp > 0,
      per_Food_e / per_Imp,
      NA_real_
    ),
    per_Feed_e = ifelse(
      per_Imp > 0,
      per_Feed_e / per_Imp,
      NA_real_
    ),
    per_Seed_e = ifelse(
      per_Imp > 0,
      per_Seed_e / per_Imp,
      NA_real_
    ),
    per_Processing_e = ifelse(
      per_Imp > 0,
      per_Processing_e / per_Imp,
      NA_real_
    ),
    per_Other_Uses_nf_e = ifelse(
      per_Imp > 0,
      per_Other_Uses_nf_e / per_Imp,
      NA_real_
    ),
    per_Losses_e = ifelse(
      per_Imp > 0,
      per_Losses_e / per_Imp,
      NA_real_
    )
  ) %>%
  rename(
    FAOStat_area_code =
      FAOStat_area_code_Exp
  ) %>%
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
  )


## 7: Prepare the domestic rice-use profile ------------------------------------

# Rename domestic-use fractions to distinguish them from the corresponding
# exported-use fractions.

rice_domestic_use <- FB_uses_rice %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Processing,
    per_Other_Uses_nf,
    per_Losses
  ) %>%
  rename(
    per_Food_d = per_Food,
    per_Feed_d = per_Feed,
    per_Seed_d = per_Seed,
    per_Processing_d = per_Processing,
    per_Other_Uses_nf_d =
      per_Other_Uses_nf,
    per_Losses_d = per_Losses
  )


## 8: Estimate final rice-production use ----------------------------------------

# Combine domestic and exported use profiles.
#
# For exporter-years with a usable bilateral allocation:
#
#   final use =
#     exported fraction × exported-use profile
#     +
#     domestic fraction × domestic-use profile
#
# Export fractions <= 1% enter as zero and are consequently treated as entirely
# domestic. If no bilateral allocation is available (`per_Imp == 0`), the
# domestic-use profile is used directly.

rice_uses <- full_join(
  rice_exported_use,
  rice_domestic_use,
  by = c(
    "FAOStat_area_code",
    "Year"
  )
) %>%
  filter(
    Year < 2020
  ) %>%
  mutate(
    across(
      all_of(use_cols),
      ~ replace_na(.x, 0)
    ),
    per_Exp = replace_na(
      per_Exp,
      0
    )
  ) %>%
  rowwise() %>%
  mutate(
    per_Food = ifelse(
      per_Imp == 0,
      per_Food_d,
      per_Exp * per_Food_e +
        (1 - per_Exp) * per_Food_d
    ),
    
    per_Feed = ifelse(
      per_Imp == 0,
      per_Feed_d,
      per_Exp * per_Feed_e +
        (1 - per_Exp) * per_Feed_d
    ),
    
    per_Seed = ifelse(
      per_Imp == 0,
      per_Seed_d,
      per_Exp * per_Seed_e +
        (1 - per_Exp) * per_Seed_d
    ),
    
    per_Processing = ifelse(
      per_Imp == 0,
      per_Processing_d,
      per_Exp * per_Processing_e +
        (1 - per_Exp) * per_Processing_d
    ),
    
    per_Other_Uses_nf = ifelse(
      per_Imp == 0,
      per_Other_Uses_nf_d,
      per_Exp * per_Other_Uses_nf_e +
        (1 - per_Exp) *
        per_Other_Uses_nf_d
    ),
    
    per_Losses = ifelse(
      per_Imp == 0,
      per_Losses_d,
      per_Exp * per_Losses_e +
        (1 - per_Exp) * per_Losses_d
    )
  ) %>%
  ungroup()


## 9: Check resulting allocation shares ----------------------------------------

### 9.1: Check domestic-use shares ----------------------------------------------

# Identify country-years for which the six domestic-use fractions do not sum
# approximately to one.

rice_uses_ds <- FB_uses_rice %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  rowwise() %>%
  mutate(
    per_ds = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_ds < 0.99 | per_ds > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected countries.

rice_uses_ds_countries <- rice_uses_ds %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall domestic-use diagnostic summary.

rice_uses_ds_summary <- rice_uses_ds %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE)
  )


### 9.2: Check exported-use shares ----------------------------------------------

# For countries with a non-negligible exported fraction, verify that the
# estimated exported-use profile sums approximately to one.

rice_uses_e <- rice_uses %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food_e,
    per_Feed_e,
    per_Seed_e,
    per_Losses_e,
    per_Processing_e,
    per_Other_Uses_nf_e,
    per_Exp
  ) %>%
  rowwise() %>%
  mutate(
    per_e = sum(
      per_Food_e,
      per_Feed_e,
      per_Seed_e,
      per_Losses_e,
      per_Processing_e,
      per_Other_Uses_nf_e,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 5000,
    per_Exp > 0.01,
    per_e < 0.99 | per_e > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected exporters.

rice_uses_e_countries <- rice_uses_e %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE),
    mean_export_fraction = mean(per_Exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall exported-use diagnostic summary.

rice_uses_e_summary <- rice_uses_e %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE)
  )


### 9.3: Check final production-use shares --------------------------------------

# Verify that the final combined allocation across all use categories sums
# approximately to one for each country-year.

rice_uses_check <- rice_uses %>%
  rowwise() %>%
  mutate(
    per_t = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_t < 0.9 | per_t > 1.1
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize countries with final-use totals outside the accepted range.

rice_uses_check_countries <- rice_uses_check %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall final-use diagnostic summary.

rice_uses_check_summary <- rice_uses_check %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE)
  )


## 10: Export results -----------------------------------------------------------

write.csv(
  rice_uses,
  "results/rice_uses.csv",
  row.names = FALSE
)

write.csv(
  rice_Kastner_Percentage_df,
  "results/Rice_Kastner_df_result.csv",
  row.names = FALSE
)
# Soybean ----------------------------------------------------------------------

## 1: Estimate regional bilateral allocation patterns --------------------------

# Bilateral allocation proportions describe how production from each exporting
# country is distributed among importing countries.
#
# For exporter-years without a usable country-specific allocation pattern, a
# regional allocation pattern is estimated from exporters belonging to the same
# FAOSTAT region.
#
# The regional pattern is calculated separately for each year and importing
# country, using the mean allocation proportion across exporters in the region.

soybean_Kastner_reg <- soybean_Kastner_Percentage_df %>%
  mutate(
    # Harmonize historical Belgium-Luxembourg with Belgium before joins.
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  group_by(
    Region,
    Year,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 2: Normalize regional allocation patterns -----------------------------------

# Regional averages are re-normalized so that allocation proportions sum to one
# across importing countries within each region-year.
#
# If a region-year contains no usable bilateral information, no regional
# allocation pattern is imposed.

soybean_Kastner_reg_tot <- soybean_Kastner_reg %>%
  group_by(
    Region,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_Kastner_reg <- soybean_Kastner_reg %>%
  left_join(
    soybean_Kastner_reg_tot,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  mutate(
    Value_reg = ifelse(
      Value_tot > 0,
      Value / Value_tot,
      NA_real_
    )
  ) %>%
  select(
    Region,
    Year,
    FAOStat_area_code_Imp,
    Value_reg
  )


## 3: Identify exporter-years without bilateral allocation ---------------------

# Calculate the total bilateral allocation represented for each exporter-year.
# A total of zero indicates that no country-specific destination allocation is
# available and the regional allocation pattern will subsequently be used.

soybean_Kastner_Percentage_df <- soybean_Kastner_Percentage_df %>%
  mutate(
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  )

soybean_Kastner_Percentage_df_tot <- soybean_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_Kastner_Percentage_df <- soybean_Kastner_Percentage_df %>%
  left_join(
    soybean_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  )


## 4: Apply regional allocation patterns where needed ---------------------------

# Attach the FAOSTAT region of each exporting country and use the corresponding
# regional destination pattern where the exporter-year has no estimated
# bilateral allocation.
#
# `Perc_Imp` is retained as the historical object name, but it represents the
# bilateral allocation proportion from an exporter to a destination country.

soybean_Kastner_Percentage_df <- soybean_Kastner_Percentage_df %>%
  rename(
    Perc_Imp = Value
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  left_join(
    soybean_Kastner_reg,
    by = c(
      "Region",
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Value_reg,
      Perc_Imp
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 5: Re-normalize bilateral allocation proportions ----------------------------

# Re-normalize the destination shares so that they sum to one for each
# exporter-year whenever bilateral or regional information is available.

soybean_Kastner_Percentage_df_tot <- soybean_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_Kastner_Percentage_df <- soybean_Kastner_Percentage_df %>%
  left_join(
    soybean_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Perc_Imp,
      Perc_Imp / Value_tot
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 6: Estimate the use profile of exported soybean -----------------------------

# Harmonize the exporter code before joining with the bilateral allocation data.

soybean_Exp_FB_harmonized <- soybean_Exp_FB %>%
  mutate(
    FAOStat_area_code = ifelse(
      FAOStat_area_code == 15,
      255,
      FAOStat_area_code
    )
  )


# For each exporter-year:
#
#   1. Identify the destination countries receiving exported soybean.
#   2. Attach the domestic-use allocation pattern of each destination country.
#   3. Weight each destination's use pattern by its bilateral allocation share.
#   4. Sum across destinations to obtain the expected use profile of exported
#      soybean.
#
# Exported fractions <= 0.01 are treated as negligible in the final-use

soybean_exported_use <- soybean_Kastner_Percentage_df %>%
  left_join(
    soybean_Exp_FB_harmonized %>%
      select(
        FAOStat_area_code,
        Year,
        P_exp_FB
      ) %>%
      rename(
        FAOStat_area_code_Exp =
          FAOStat_area_code,
        per_Exp = P_exp_FB
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  
  # Attach the use profile of the importing/destination country.
  left_join(
    FB_uses_soybean %>%
      select(
        FAOStat_area_code,
        Year,
        per_Food,
        per_Feed,
        per_Seed,
        per_Processing,
        per_Other_Uses_nf,
        per_Losses
      ) %>%
      rename(
        FAOStat_area_code_Imp =
          FAOStat_area_code
      ),
    by = c(
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  
  # Weight destination-specific use patterns by bilateral allocation.
  mutate(
    per_Food =
      Perc_Imp * per_Food,
    per_Feed =
      Perc_Imp * per_Feed,
    per_Seed =
      Perc_Imp * per_Seed,
    per_Processing =
      Perc_Imp * per_Processing,
    per_Other_Uses_nf =
      Perc_Imp * per_Other_Uses_nf,
    per_Losses =
      Perc_Imp * per_Losses
  ) %>%
  
  # Aggregate across destinations for each exporter-year.
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    per_Imp = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    per_Food_e = sum(
      per_Food,
      na.rm = TRUE
    ),
    per_Feed_e = sum(
      per_Feed,
      na.rm = TRUE
    ),
    per_Seed_e = sum(
      per_Seed,
      na.rm = TRUE
    ),
    per_Processing_e = sum(
      per_Processing,
      na.rm = TRUE
    ),
    per_Other_Uses_nf_e = sum(
      per_Other_Uses_nf,
      na.rm = TRUE
    ),
    per_Losses_e = sum(
      per_Losses,
      na.rm = TRUE
    ),
    per_Exp = first(per_Exp),
    .groups = "drop"
  ) %>%
  
  # Retain exporters with a non-negligible exported fraction.
  filter(
    !is.na(per_Exp),
    per_Exp > 0.01,
    FAOStat_area_code_Exp < 351
  ) %>%
  
  # Re-normalize the exported-use profile by the represented bilateral
  # allocation share.
  mutate(
    per_Food_e = ifelse(
      per_Imp > 0,
      per_Food_e / per_Imp,
      NA_real_
    ),
    per_Feed_e = ifelse(
      per_Imp > 0,
      per_Feed_e / per_Imp,
      NA_real_
    ),
    per_Seed_e = ifelse(
      per_Imp > 0,
      per_Seed_e / per_Imp,
      NA_real_
    ),
    per_Processing_e = ifelse(
      per_Imp > 0,
      per_Processing_e / per_Imp,
      NA_real_
    ),
    per_Other_Uses_nf_e = ifelse(
      per_Imp > 0,
      per_Other_Uses_nf_e / per_Imp,
      NA_real_
    ),
    per_Losses_e = ifelse(
      per_Imp > 0,
      per_Losses_e / per_Imp,
      NA_real_
    )
  ) %>%
  rename(
    FAOStat_area_code =
      FAOStat_area_code_Exp
  ) %>%
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
  )


## 7: Prepare the domestic soybean-use profile ---------------------------------

# Rename domestic-use fractions to distinguish them from the corresponding
# exported-use fractions.

soybean_domestic_use <- FB_uses_soybean %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Processing,
    per_Other_Uses_nf,
    per_Losses
  ) %>%
  rename(
    per_Food_d = per_Food,
    per_Feed_d = per_Feed,
    per_Seed_d = per_Seed,
    per_Processing_d = per_Processing,
    per_Other_Uses_nf_d =
      per_Other_Uses_nf,
    per_Losses_d = per_Losses
  )


## 8: Estimate final soybean-production use ------------------------------------

# Combine domestic and exported use profiles.
#
# For exporter-years with a usable bilateral allocation:
#
#   final use =
#     exported fraction × exported-use profile
#     +
#     domestic fraction × domestic-use profile
#
# Export fractions <= 1% enter as zero and are consequently treated as entirely
# domestic. If no bilateral allocation is available (`per_Imp == 0`), the
# domestic-use profile is used directly.

soybean_uses <- full_join(
  soybean_exported_use,
  soybean_domestic_use,
  by = c(
    "FAOStat_area_code",
    "Year"
  )
) %>%
  filter(
    Year < 2020
  ) %>%
  mutate(
    across(
      all_of(use_cols),
      ~ replace_na(.x, 0)
    ),
    per_Exp = replace_na(
      per_Exp,
      0
    )
  ) %>%
  rowwise() %>%
  mutate(
    per_Food = ifelse(
      per_Imp == 0,
      per_Food_d,
      per_Exp * per_Food_e +
        (1 - per_Exp) * per_Food_d
    ),
    
    per_Feed = ifelse(
      per_Imp == 0,
      per_Feed_d,
      per_Exp * per_Feed_e +
        (1 - per_Exp) * per_Feed_d
    ),
    
    per_Seed = ifelse(
      per_Imp == 0,
      per_Seed_d,
      per_Exp * per_Seed_e +
        (1 - per_Exp) * per_Seed_d
    ),
    
    per_Processing = ifelse(
      per_Imp == 0,
      per_Processing_d,
      per_Exp * per_Processing_e +
        (1 - per_Exp) * per_Processing_d
    ),
    
    per_Other_Uses_nf = ifelse(
      per_Imp == 0,
      per_Other_Uses_nf_d,
      per_Exp * per_Other_Uses_nf_e +
        (1 - per_Exp) *
        per_Other_Uses_nf_d
    ),
    
    per_Losses = ifelse(
      per_Imp == 0,
      per_Losses_d,
      per_Exp * per_Losses_e +
        (1 - per_Exp) * per_Losses_d
    )
  ) %>%
  ungroup()


## 9: Check resulting allocation shares ----------------------------------------

### 9.1: Check domestic-use shares ----------------------------------------------

# Identify country-years for which the six domestic-use fractions do not sum
# approximately to one.

soybean_uses_ds <- FB_uses_soybean %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food,
    per_Feed,
    per_Seed,
    per_Losses,
    per_Processing,
    per_Other_Uses_nf
  ) %>%
  rowwise() %>%
  mutate(
    per_ds = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_ds < 0.99 | per_ds > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected countries.

soybean_uses_ds_countries <- soybean_uses_ds %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall domestic-use diagnostic summary.

soybean_uses_ds_summary <- soybean_uses_ds %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_ds, na.rm = TRUE),
    max_sum = max(per_ds, na.rm = TRUE),
    mean_sum = mean(per_ds, na.rm = TRUE)
  )


### 9.2: Check exported-use shares ----------------------------------------------

# For countries with a non-negligible exported fraction, verify that the
# estimated exported-use profile sums approximately to one.

soybean_uses_e <- soybean_uses %>%
  select(
    FAOStat_area_code,
    Year,
    per_Food_e,
    per_Feed_e,
    per_Seed_e,
    per_Losses_e,
    per_Processing_e,
    per_Other_Uses_nf_e,
    per_Exp
  ) %>%
  rowwise() %>%
  mutate(
    per_e = sum(
      per_Food_e,
      per_Feed_e,
      per_Seed_e,
      per_Losses_e,
      per_Processing_e,
      per_Other_Uses_nf_e,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 5000,
    per_Exp > 0.01,
    per_e < 0.99 | per_e > 1.01
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize affected exporters.

soybean_uses_e_countries <- soybean_uses_e %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE),
    mean_export_fraction = mean(per_Exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall exported-use diagnostic summary.

soybean_uses_e_summary <- soybean_uses_e %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_e, na.rm = TRUE),
    max_sum = max(per_e, na.rm = TRUE),
    mean_sum = mean(per_e, na.rm = TRUE)
  )


### 9.3: Check final production-use shares --------------------------------------

# Verify that the final combined allocation across all use categories sums
# approximately to one for each country-year.

soybean_uses_check <- soybean_uses %>%
  rowwise() %>%
  mutate(
    per_t = sum(
      per_Food,
      per_Feed,
      per_Seed,
      per_Losses,
      per_Processing,
      per_Other_Uses_nf,
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  filter(
    Year < 2020,
    FAOStat_area_code < 351,
    per_t < 0.9 | per_t > 1.1
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  ) %>%
  relocate(
    FAOStat_area_code,
    ISO3,
    Year
  )


# Summarize countries with final-use totals outside the accepted range.

soybean_uses_check_countries <- soybean_uses_check %>%
  group_by(
    FAOStat_area_code,
    ISO3
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall final-use diagnostic summary.

soybean_uses_check_summary <- soybean_uses_check %>%
  summarise(
    n_country_years = n(),
    n_countries = n_distinct(FAOStat_area_code),
    min_sum = min(per_t, na.rm = TRUE),
    max_sum = max(per_t, na.rm = TRUE),
    mean_sum = mean(per_t, na.rm = TRUE)
  )


## 10: Export results -----------------------------------------------------------

write.csv(
  soybean_uses,
  "results/soybean_uses.csv",
  row.names = FALSE
)

write.csv(
  soybean_Kastner_Percentage_df,
  "results/Soybean_Kastner_df_result.csv",
  row.names = FALSE
)

# Soybean cake -----------------------------------------------------------------

## 1: Estimate regional bilateral allocation patterns --------------------------

# Bilateral allocation proportions describe how soybean cake from each
# exporting country is distributed among importing countries.
#
# For exporter-years without a usable country-specific allocation pattern, a
# regional allocation pattern is estimated from exporters belonging to the same
# FAOSTAT region.
#
# The regional pattern is calculated separately for each year and importing
# country, using the mean allocation proportion across exporters in the region.

soybean_cake_Kastner_reg <- soybean_cake_Kastner_Percentage_df %>%
  mutate(
    # Harmonize historical Belgium-Luxembourg with Belgium before joins.
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  group_by(
    Region,
    Year,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 2: Normalize regional allocation patterns -----------------------------------

# Regional averages are re-normalized so that allocation proportions sum to one
# across importing countries within each region-year.
#
# If a region-year contains no usable bilateral information, no regional
# allocation pattern can be estimated and the corresponding values remain
# missing.

soybean_cake_Kastner_reg_tot <- soybean_cake_Kastner_reg %>%
  group_by(
    Region,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_cake_Kastner_reg <- soybean_cake_Kastner_reg %>%
  left_join(
    soybean_cake_Kastner_reg_tot,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  mutate(
    Value_reg = ifelse(
      Value_tot > 0,
      Value / Value_tot,
      NA_real_
    )
  ) %>%
  select(
    Region,
    Year,
    FAOStat_area_code_Imp,
    Value_reg
  )


## 3: Identify exporter-years without bilateral allocation ---------------------

# Calculate the total bilateral allocation represented for each exporter-year.
# A total of zero indicates that no country-specific destination allocation is
# available and the regional allocation pattern will subsequently be used.

soybean_cake_Kastner_Percentage_df <-
  soybean_cake_Kastner_Percentage_df %>%
  mutate(
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  )


soybean_cake_Kastner_Percentage_df_tot <-
  soybean_cake_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_cake_Kastner_Percentage_df <-
  soybean_cake_Kastner_Percentage_df %>%
  left_join(
    soybean_cake_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  )


## 4: Apply regional allocation patterns where needed ---------------------------

# Attach the FAOSTAT region of each exporting country and use the corresponding
# regional destination pattern where the exporter-year has no estimated
# bilateral allocation.
#
# `Perc_Imp` is retained as the historical object name, but it represents the
# bilateral allocation proportion from an exporter to an importing/destination
# country.

soybean_cake_Kastner_Percentage_df <-
  soybean_cake_Kastner_Percentage_df %>%
  rename(
    Perc_Imp = Value
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  left_join(
    soybean_cake_Kastner_reg,
    by = c(
      "Region",
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Value_reg,
      Perc_Imp
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 5: Re-normalize bilateral allocation proportions ----------------------------

# Re-normalize destination shares so that they sum to one for each exporter-year
# whenever bilateral or regional information is available.

soybean_cake_Kastner_Percentage_df_tot <-
  soybean_cake_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_cake_Kastner_Percentage_df <-
  soybean_cake_Kastner_Percentage_df %>%
  left_join(
    soybean_cake_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Perc_Imp,
      Perc_Imp / Value_tot
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 6: Check bilateral allocation proportions -----------------------------------

# Verify that bilateral allocation proportions sum approximately to one for
# each exporter-year.

soybean_cake_Kastner_check <-
  soybean_cake_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    allocation_sum = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(
    allocation_sum < 0.99 |
      allocation_sum > 1.01
  ) %>%
  left_join(
    Countries_DB %>%
      rename(
        FAOStat_area_code_Exp =
          FAOStat_area_code,
        ISO3_Exp = ISO3
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  relocate(
    FAOStat_area_code_Exp,
    ISO3_Exp,
    Year
  )


# Summarize exporters with at least one anomalous year.

soybean_cake_Kastner_check_countries <-
  soybean_cake_Kastner_check %>%
  group_by(
    FAOStat_area_code_Exp,
    ISO3_Exp
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(
      allocation_sum,
      na.rm = TRUE
    ),
    max_sum = max(
      allocation_sum,
      na.rm = TRUE
    ),
    mean_sum = mean(
      allocation_sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall diagnostic summary.

soybean_cake_Kastner_check_summary <-
  soybean_cake_Kastner_check %>%
  summarise(
    n_exporter_years = n(),
    n_exporters =
      n_distinct(FAOStat_area_code_Exp),
    min_sum = min(
      allocation_sum,
      na.rm = TRUE
    ),
    max_sum = max(
      allocation_sum,
      na.rm = TRUE
    ),
    mean_sum = mean(
      allocation_sum,
      na.rm = TRUE
    )
  )


## 7: Export results ------------------------------------------------------------

write.csv(
  soybean_cake_Kastner_Percentage_df,
  "results/Soybean_cake_Kastner_df_result.csv",
  row.names = FALSE
)

# Soybean oil ------------------------------------------------------------------

## 1: Estimate regional bilateral allocation patterns --------------------------

# Bilateral allocation proportions describe how soybean oil from each exporting
# country is distributed among importing countries.
#
# For exporter-years without a usable country-specific allocation pattern, a
# regional allocation pattern is estimated from exporters belonging to the same
# FAOSTAT region.
#
# The regional pattern is calculated separately for each year and importing
# country, using the mean allocation proportion across exporters in the region.

soybean_oil_Kastner_reg <- soybean_oil_Kastner_Percentage_df %>%
  mutate(
    # Harmonize historical Belgium-Luxembourg with Belgium before joins.
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  group_by(
    Region,
    Year,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 2: Normalize regional allocation patterns -----------------------------------

# Regional averages are re-normalized so that allocation proportions sum to one
# across importing countries within each region-year.
#
# If a region-year contains no usable bilateral information, no regional
# allocation pattern can be estimated and the corresponding values remain
# missing.

soybean_oil_Kastner_reg_tot <- soybean_oil_Kastner_reg %>%
  group_by(
    Region,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_oil_Kastner_reg <- soybean_oil_Kastner_reg %>%
  left_join(
    soybean_oil_Kastner_reg_tot,
    by = c(
      "Region",
      "Year"
    )
  ) %>%
  mutate(
    Value_reg = ifelse(
      Value_tot > 0,
      Value / Value_tot,
      NA_real_
    )
  ) %>%
  select(
    Region,
    Year,
    FAOStat_area_code_Imp,
    Value_reg
  )


## 3: Identify exporter-years without bilateral allocation ---------------------

# Calculate the total bilateral allocation represented for each exporter-year.
# A total of zero indicates that no country-specific destination allocation is
# available and the regional allocation pattern will subsequently be used.

soybean_oil_Kastner_Percentage_df <-
  soybean_oil_Kastner_Percentage_df %>%
  mutate(
    FAOStat_area_code_Exp = ifelse(
      FAOStat_area_code_Exp == 15,
      255,
      FAOStat_area_code_Exp
    )
  )


soybean_oil_Kastner_Percentage_df_tot <-
  soybean_oil_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_oil_Kastner_Percentage_df <-
  soybean_oil_Kastner_Percentage_df %>%
  left_join(
    soybean_oil_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  )


## 4: Apply regional allocation patterns where needed ---------------------------

# Attach the FAOSTAT region of each exporting country and use the corresponding
# regional destination pattern where the exporter-year has no estimated
# bilateral allocation.
#
# `Perc_Imp` is retained as the historical object name, but it represents the
# bilateral allocation proportion from an exporter to an importing/destination
# country.

soybean_oil_Kastner_Percentage_df <-
  soybean_oil_Kastner_Percentage_df %>%
  rename(
    Perc_Imp = Value
  ) %>%
  left_join(
    FAOSTAT_regions %>%
      select(2, 3) %>%
      rename(
        Region = 1,
        FAOStat_area_code_Exp = 2
      ) %>%
      mutate(
        FAOStat_area_code_Exp =
          as.numeric(FAOStat_area_code_Exp)
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  left_join(
    soybean_oil_Kastner_reg,
    by = c(
      "Region",
      "FAOStat_area_code_Imp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Value_reg,
      Perc_Imp
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 5: Re-normalize bilateral allocation proportions ----------------------------

# Re-normalize destination shares so that they sum to one for each exporter-year
# whenever bilateral or regional information is available.

soybean_oil_Kastner_Percentage_df_tot <-
  soybean_oil_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_tot = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


soybean_oil_Kastner_Percentage_df <-
  soybean_oil_Kastner_Percentage_df %>%
  left_join(
    soybean_oil_Kastner_Percentage_df_tot,
    by = c(
      "FAOStat_area_code_Exp",
      "Year"
    )
  ) %>%
  mutate(
    Perc_Imp = ifelse(
      Value_tot == 0,
      Perc_Imp,
      Perc_Imp / Value_tot
    )
  ) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Perc_Imp
  )


## 6: Check bilateral allocation proportions -----------------------------------

# Verify that bilateral allocation proportions sum approximately to one for
# each exporter-year.

soybean_oil_Kastner_check <-
  soybean_oil_Kastner_Percentage_df %>%
  group_by(
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    allocation_sum = sum(
      Perc_Imp,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(
    allocation_sum < 0.99 |
      allocation_sum > 1.01
  ) %>%
  left_join(
    Countries_DB %>%
      rename(
        FAOStat_area_code_Exp =
          FAOStat_area_code,
        ISO3_Exp = ISO3
      ),
    by = "FAOStat_area_code_Exp"
  ) %>%
  relocate(
    FAOStat_area_code_Exp,
    ISO3_Exp,
    Year
  )


# Summarize exporters with at least one anomalous year.

soybean_oil_Kastner_check_countries <-
  soybean_oil_Kastner_check %>%
  group_by(
    FAOStat_area_code_Exp,
    ISO3_Exp
  ) %>%
  summarise(
    n_years = n(),
    first_year = min(Year),
    last_year = max(Year),
    min_sum = min(
      allocation_sum,
      na.rm = TRUE
    ),
    max_sum = max(
      allocation_sum,
      na.rm = TRUE
    ),
    mean_sum = mean(
      allocation_sum,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(n_years)
  )


# Overall diagnostic summary.

soybean_oil_Kastner_check_summary <-
  soybean_oil_Kastner_check %>%
  summarise(
    n_exporter_years = n(),
    n_exporters =
      n_distinct(FAOStat_area_code_Exp),
    min_sum = min(
      allocation_sum,
      na.rm = TRUE
    ),
    max_sum = max(
      allocation_sum,
      na.rm = TRUE
    ),
    mean_sum = mean(
      allocation_sum,
      na.rm = TRUE
    )
  )


## 7: Export results ------------------------------------------------------------

write.csv(
  soybean_oil_Kastner_Percentage_df,
  "results/Soybean_oil_Kastner_df_result.csv",
  row.names = FALSE
)