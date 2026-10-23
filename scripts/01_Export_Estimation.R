# ==============================================================================
# 01_export_estimation.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
# Purpose:
# Estimate the fraction of crop commodities exported from each country using
# FAOSTAT Food Balance Sheets. Estimates are produced for wheat, maize, rice,
# soybean, soybean cake, and soybean oil.
#
# External data:
#
#   FAOSTAT Food Balances (2010-):
#   https://www.fao.org/faostat/en/#data/FBS
#   Download:
#   FoodBalanceSheets_E_All_Data_(Normalized).csv
#   Downloaded November 2023
#
#   FAOSTAT Historic Food Balance Sheets (-2013):
#   https://www.fao.org/faostat/en/#data/FBSH
#   Download:
#   FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv
#   Downloaded November 2023
#
#   FAOSTAT Commodity Balances (2010-):
#   https://www.fao.org/faostat/en/#data/CB
#   Download the normalized bulk file and filter to:
#   Item = "Cake of soya beans"
#   Save the filtered dataset as:
#   FBact_soycake.csv
#   Downloaded June 2025
#
#   FAOSTAT Historic Commodity Balance Sheets (-2013):
#   https://www.fao.org/faostat/en/#data/CBH
#   Download the normalized bulk file and filter to:
#   Item = "Soyabean cake"
#   Save the filtered dataset as:
#   FBold_soycake.csv
#   Downloaded June 2025
#
#   The original FAOSTAT files are not redistributed in this repository.
#   The datasets used in this study were downloaded in February 2024.
#   Users should place the required files in:
#       data/Food_Balances/
#
# Expected filenames:
#   FoodBalanceSheets_E_All_Data_(Normalized).csv
#   FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv
#   FBact_soycake.csv
#   FBold_soycake.csv
#
# Additional files:
#   Countries_DB.xlsx
#   FAOSTAT_regions.csv
#
#
# Outputs:
#   results/Wheat_Exp_FB.csv
#   results/Maize_Exp_FB.csv
#   results/Rice_Exp_FB.csv
#   results/Soybean_Exp_FB.csv
#   results/Soybean_cake_Exp_FB.csv
#   results/Soybean_oil_Exp_FB.csv
#
# Notes:
#   This script should be run before 02_TM_Estimations.R.
# ==============================================================================

# Loading Libraries ------------------------------------------------------------
library(tidyverse)  # Data manipulation and transformation; includes dplyr, tidyr, etc.
library(readxl)     # Import Microsoft Excel (.xls/.xlsx) files

# Loading Data ------------------------------------------------------------

## Food Balances (FB) 
# Current Food Balance Sheets (2010-)
FB_act <- read_csv("data/Food_Balances/FoodBalanceSheets_E_All_Data_(Normalized).csv")

# Historical Food Balance Sheets
FB_old <- read_csv("data/Food_Balances/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv")

# Soybean-cake records extracted from the historical and current FAOSTAT
# datasets from the Commodity Balances (FB) .

# Historical Commodity Balance Sheets (-2013)
FBold_soycake <- read_csv("data/Food_Balances/FBold_soycake.csv")

# Current Commodity Balance Sheets (2010-)
FBact_soycake <- read_csv("data/Food_Balances/FBact_soycake.csv")

## Country classifications
# Country correspondence table. Only ISO3 and FAOSTAT area codes are retained
# in Countries_DB for joining country identifiers throughout the analysis.
Countries_DB <- read_excel("data/Countries_DB.xlsx") %>% select(2, 3) %>% rename(ISO3 = 1, FAOStat_area_code =2)

# Full country-classification table, retained because additional identifiers
# (e.g. M49 codes) are required later in the script.

Countries_DB_sc <- read_excel("data/Countries_DB.xlsx")

# FAOSTAT regional classification used to assign countries to broader regions.
FAOSTAT_regions <- read_csv("data/FAOSTAT_regions.csv")

# 1. Estimate exported and domestically consumed fractions ----------------

## 1.1 Historical Food Balance Sheets -------------------------------------

# For each crop and country-year, estimate the fraction of available crop
# production that is exported:
#
#   Export fraction = Exports / (Domestic supply + Exports)
#
# Domestic supply represents the quantity remaining available within the country,
# whereas adding exports reconstructs the quantity available before separating
# domestic and exported fractions.


### Wheat ----------------------------------------------------------------------

# FAOSTAT item code:
#   2511 = Wheat and products
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity

FB_old_exp_wheat <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(Item_Code == 2511 &
           (Element_code == 5911 | Element_code == 5301)) %>%
  pivot_wider(names_from = Element_code, values_from = Value) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "1_1") %>%
  mutate(
    Export_percentage = Exports / (Domestic_Supply + Exports)
  )

# Check missing export-fraction estimates by country
Wheat_exp_nas <- FB_old_exp_wheat %>%
  mutate(na_cont = ifelse(is.na(Export_percentage), 1, 0)) %>%
  group_by(FAOStat_area_code) %>%
  summarise(nas = sum(na_cont))

print(Wheat_exp_nas %>% filter(nas > 0))

# Only 10 countries have missing export-fraction estimates.
# Missing export fractions were assigned a value of zero.
FB_old_exp_wheat <- FB_old_exp_wheat %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )

# Diagnostic checks
hist(FB_old_exp_wheat$Export_percentage)
max(FB_old_exp_wheat$Export_percentage)

### Maize ----------------------------------------------------------------------

# FAOSTAT item code:
#   2514 = Maize and products
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity

FB_old_exp_maize <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2514 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "1_2") %>%
  mutate(
    Export_percentage = Exports / (Domestic_Supply + Exports)
  )


# Check missing export-fraction estimates by country
Maize_exp_nas <- FB_old_exp_maize %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(Countries_DB)

print(
  Maize_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions occur in 41 countries, none of which are major
# maize producers in the dataset. Missing fractions were assigned a value of zero.
FB_old_exp_maize <- FB_old_exp_maize %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_old_exp_maize$Export_percentage)
max(FB_old_exp_maize$Export_percentage)                                              

### Rice -----------------------------------------------------------------------

# FAOSTAT item code:
#   2805 = Rice and products
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity

# Negative domestic-supply values in the source data were treated as
# data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_old_exp_Rice <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2805 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "1_3") %>%
  mutate(
    Export_percentage = Exports / (abs(Domestic_Supply) + Exports)
  )


# Check missing export-fraction estimates by country
Rice_exp_nas <- FB_old_exp_Rice %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(Countries_DB, by = "FAOStat_area_code")

print(
  Rice_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions occur in 29 countries.
# Missing fractions were assigned a value of zero.
FB_old_exp_rice <- FB_old_exp_Rice %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )

# Diagnostic checks
hist(FB_old_exp_rice$Export_percentage)
max(FB_old_exp_rice$Export_percentage)

### Soybean --------------------------------------------------------------------

# FAOSTAT item code:
#   2555 = Soybeans
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_old_exp_Soybean <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2555 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "2_1") %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )


# Check missing export-fraction estimates by country
Soybean_exp_nas <- FB_old_exp_Soybean %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  Soybean_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions occur in most than 100 countries. 
# However no major producer such as Brazil, the U.S.A, Argentina, Canada or China is in the list
# Missing fractions were assigned a value of zero.
FB_old_exp_Soybean <- FB_old_exp_Soybean %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )

### Soybean cake ---------------------------------------------------------------

# FAOSTAT Commodity Balance Sheets
#
# FAOSTAT element codes:
#   5300 = Domestic supply quantity
#   5910 = Export quantity
#
# Country identifiers in the Commodity Balance Sheets are given as M49 codes.
# It was downloaded later due to the review process
# These are matched to FAOSTAT area codes using Countries_DB.xlsx.
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_old_soycake <- FBold_soycake %>%
  select(3, 5, 10, 12) %>%
  rename(
    M49_code = 1,
    Element_code = 2,
    Year = 3,
    Value = 4
  ) %>%
  filter(
    Element_code == 5910 |
      Element_code == 5300
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  mutate(
    M49_code = as.numeric(M49_code)
  ) %>%
  left_join(
    Countries_DB_sc %>%
      select(3, 5) %>%
      rename(
        FAOStat_area_code = 1,
        M49_code = 2
      ),
    by = "M49_code"
  ) %>%
  rename(
    Domestic_Supply = `5300`,
    Exports = `5910`
  ) %>%
  mutate(
    Crop_Code = "2_1c"
  ) %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Export_percentage
  )


# Check missing export-fraction estimates by country
Soybean_cake_exp_nas <- FB_old_soycake %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  Soybean_cake_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions occur in most than 100 countries. 
# However no major producer such as Brazil, the U.S.A, Argentina, Canada or China is in the list
# Missing export fractions were assigned a value of zero.
FB_old_exp_soycake <- FB_old_soycake %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_old_exp_soycake$Export_percentage)
max(FB_old_exp_soycake$Export_percentage)

### Soybean oil ---------------------------------------------------------------

# FAOSTAT item code:
#   2571 = Soybean oil
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_old_exp_Soybean_oil <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2571 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "2_1o") %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )


# Missing export fractions occur in most than 100 countries. 
# However no major producer such as Brazil, the U.S.A, Argentina, Canada or China is in the list
# Check missing export-fraction estimates by country
Soybean_oil_exp_nas <- FB_old_exp_Soybean_oil %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  Soybean_oil_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions were assigned a value of zero.
FB_old_exp_Soybean_oil <- FB_old_exp_Soybean_oil %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_old_exp_Soybean_oil$Export_percentage)
max(FB_old_exp_Soybean_oil$Export_percentage)

## 1.2: Current Food Balance Sheets ---------------------------------------

### Wheat ----------------------------------------------------------------------

# FAOSTAT item code:
#   2511 = Wheat and products
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_act_exp_wheat <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2511 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "1_1") %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )


# Check missing export-fraction estimates by country
Wheat_act_exp_nas <- FB_act_exp_wheat %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  Wheat_act_exp_nas %>%
    filter(nas > 0)
)

# Only 10 countries (none of them an important producer) present nas.
# Missing export fractions were assigned a value of zero.
FB_act_exp_wheat <- FB_act_exp_wheat %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_act_exp_wheat$Export_percentage)
max(FB_act_exp_wheat$Export_percentage)

### Maize ----------------------------------------------------------------------

# FAOSTAT item code:
#   2514 = Maize and products
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_act_exp_Maize <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2514 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "1_2") %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )


# Check missing export-fraction estimates by country
Maize_act_exp_nas <- FB_act_exp_Maize %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  Maize_act_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions occur in 59 of 202 countries, none of which are major producer
# maize producers in the dataset. Missing fractions were assigned a value of zero.

# Missing export fractions were assigned a value of zero.
FB_act_exp_maize <- FB_act_exp_Maize %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_act_exp_maize$Export_percentage)
max(FB_act_exp_maize$Export_percentage)

### Rice -----------------------------------------------------------------------

# FAOSTAT item code:
#   2807 = Rice and products
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_act_exp_rice <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2807 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "1_3") %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )


# Check missing export-fraction estimates by country
Rice_act_exp_nas <- FB_act_exp_rice %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  Rice_act_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions were assigned a value of zero.
FB_act_exp_rice <- FB_act_exp_rice %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )

# Diagnostic checks
hist(FB_act_exp_rice$Export_percentage)
max(FB_act_exp_rice$Export_percentage)

### Soybean --------------------------------------------------------------------

# FAOSTAT item code:
#   2555 = Soybeans
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_act_exp_soybean <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2555 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(Crop_Code = "2_1") %>%
  mutate(
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )

# Check missing export-fraction estimates by country
soybean_act_exp_nas <- FB_act_exp_soybean %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  soybean_act_exp_nas %>%
    filter(nas > 0)
)

# Missing export fractions were assigned a value of zero.
FB_act_exp_soybean <- FB_act_exp_soybean %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )

# Diagnostic checks
hist(FB_act_exp_soybean$Export_percentage)
max(FB_act_exp_soybean$Export_percentage)

### Soybean cake ---------------------------------------------------------------

# FAOSTAT Commodity Balances
#
# FAOSTAT element codes:
#   5510 = Production
#   5610 = Imports
#   5910 = Exports
#   5071 = Stock variation
#
# Country identifiers are provided as M49 codes and are matched to FAOSTAT
# area codes using Countries_DB.xlsx.
#
# The export fraction is estimated from total available supply:
# production + imports + stock variation.
#
# Negative production, import, and export values in the source data were treated
# as data inconsistencies and converted to absolute values. Stock variation
# retains its original sign because positive and negative values represent
# changes in stocks.
#
# In a small number of country-years, stock variation can reduce the denominator
# sufficiently for the estimated export fraction to exceed 1. Because an export
# fraction cannot exceed 100%, these values are constrained to 1.

FB_act_exp_soybean_cake <- FBact_soycake %>%
  select(3, 5, 10, 12) %>%
  rename(
    M49_code = 1,
    Element_code = 2,
    Year = 3,
    Value = 4
  ) %>%
  filter(
    Element_code == 5910 |
      Element_code == 5510 |
      Element_code == 5610 |
      Element_code == 5071
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  mutate(
    M49_code = as.numeric(M49_code)
  ) %>%
  left_join(
    Countries_DB_sc %>%
      select(3, 5) %>%
      rename(
        FAOStat_area_code = 1,
        M49_code = 2
      ),
    by = "M49_code"
  ) %>%
  rename(
    Production = `5510`,
    Imports = `5610`,
    Exports = `5910`,
    Stock_Var = `5071`
  ) %>%
  mutate(
    Crop_Code = "2_1c"
  ) %>%
  rowwise() %>%
  mutate(
    Export_percentage =
      abs(Exports) /
      sum(
        abs(Production),
        abs(Imports),
        Stock_Var,
        na.rm = TRUE
      )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Export_percentage
  )


# Check missing export-fraction estimates by country
soybean_cake_act_exp_nas <- FB_act_exp_soybean_cake %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  soybean_cake_act_exp_nas %>%
    filter(nas > 0)
)


# Check export fractions greater than one before correction
print(
  FB_act_exp_soybean_cake %>%
    filter(Export_percentage > 1)
)


# Missing export fractions were assigned a value of zero.
# Export fractions greater than one were constrained to one.
FB_act_exp_soybean_cake <- FB_act_exp_soybean_cake %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    ),
    Export_percentage = ifelse(
      Export_percentage > 1,
      1,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_act_exp_soybean_cake$Export_percentage)
max(FB_act_exp_soybean_cake$Export_percentage)

### Soybean oil ---------------------------------------------------------------

# FAOSTAT item code:
#   2571 = Soybean oil
#
# FAOSTAT element codes:
#   5301 = Domestic supply quantity
#   5911 = Export quantity
#
# Note:
# Some values in the original FAOSTAT dataset are negative. These were treated
# as data inconsistencies and converted to absolute values before calculating
# the export fraction.

FB_act_exp_soybean_oil <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2571 &
      (Element_code == 5911 | Element_code == 5301)
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Domestic_Supply = `5301`,
    Exports = `5911`,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1o",
    Export_percentage =
      abs(Exports) / (abs(Domestic_Supply) + abs(Exports))
  )


# Check missing export-fraction estimates by country
soybean_oil_act_exp_nas <- FB_act_exp_soybean_oil %>%
  mutate(
    na_cont = ifelse(is.na(Export_percentage), 1, 0)
  ) %>%
  group_by(FAOStat_area_code) %>%
  summarise(
    nas = sum(na_cont)
  ) %>%
  left_join(
    Countries_DB,
    by = "FAOStat_area_code"
  )

print(
  soybean_oil_act_exp_nas %>%
    filter(nas > 0)
)


# Missing export fractions were assigned a value of zero.
FB_act_exp_soybean_oil <- FB_act_exp_soybean_oil %>%
  mutate(
    Export_percentage = ifelse(
      is.na(Export_percentage),
      0,
      Export_percentage
    )
  )


# Diagnostic checks
hist(FB_act_exp_soybean_oil$Export_percentage)
max(FB_act_exp_soybean_oil$Export_percentage)


## 1.3: Combine historical and current Food Balance estimates ------------

### Wheat ----------------------------------------------------------------------

# Join export-fraction estimates from the historical and current Food Balance
# datasets by country, year, and crop.

FB_exp_wheat <- FB_old_exp_wheat %>%
  select(1:3, 6) %>%
  rename(
    Export_percentage_old = 4
  ) %>%
  full_join(
    FB_act_exp_wheat %>%
      select(1:3, 6) %>%
      rename(
        Export_percentage_act = 4
      ),
    by = c(
      "FAOStat_area_code",
      "Year",
      "Crop_Code"
    )
  )


# Assess agreement between the two datasets during overlapping years.
summary(
  lm(
    Export_percentage_old ~ Export_percentage_act,
    data = FB_exp_wheat
  )
)

# For years available in both datasets, use the mean of the two estimates.
# If only one dataset provides an estimate, retain the available value.

FB_exp_wheat <- FB_exp_wheat %>%
  rowwise() %>%
  mutate(
    Exp_perc = ifelse(
      is.na(Export_percentage_act),
      Export_percentage_old,
      ifelse(
        is.na(Export_percentage_old),
        Export_percentage_act,
        mean(
          Export_percentage_old,
          Export_percentage_act
        )
      )
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Exp_perc
  ) %>%
  rename(
    P_exp_FB = 4
  )




### Maize ----------------------------------------------------------------------

# Join export-fraction estimates from the historical and current Food Balance
# datasets by country, year, and crop.

FB_exp_maize <- FB_old_exp_maize %>%
  select(1:3, 6) %>%
  rename(
    Export_percentage_old = 4
  ) %>%
  full_join(
    FB_act_exp_maize %>%
      select(1:3, 6) %>%
      rename(
        Export_percentage_act = 4
      ),
    by = c(
      "FAOStat_area_code",
      "Year",
      "Crop_Code"
    )
  )


# Assess agreement between the two datasets during overlapping years.
summary(
  lm(
    Export_percentage_old ~ Export_percentage_act,
    data = FB_exp_maize
  )
)


# For years available in both datasets, use the mean of the two estimates.
# If only one dataset provides an estimate, retain the available value.

FB_exp_maize <- FB_exp_maize %>%
  rowwise() %>%
  mutate(
    Exp_perc = ifelse(
      is.na(Export_percentage_act),
      Export_percentage_old,
      ifelse(
        is.na(Export_percentage_old),
        Export_percentage_act,
        mean(
          Export_percentage_old,
          Export_percentage_act
        )
      )
    )
  ) %>%
  rename(
    P_exp_FB = 6
  )

### Rice -----------------------------------------------------------------------

# Join export-fraction estimates from the historical and current Food Balance
# datasets by country, year, and crop.

FB_exp_rice <- FB_old_exp_rice %>%
  select(1:3, 6) %>%
  rename(
    Export_percentage_old = 4
  ) %>%
  full_join(
    FB_act_exp_rice %>%
      select(1:3, 6) %>%
      rename(
        Export_percentage_act = 4
      ),
    by = c(
      "FAOStat_area_code",
      "Year",
      "Crop_Code"
    )
  )


# Assess agreement between the two datasets during overlapping years.
summary(
  lm(
    Export_percentage_old ~ Export_percentage_act,
    data = FB_exp_rice
  )
)


# For years available in both datasets, use the mean of the two estimates.
# If only one dataset provides an estimate, retain the available value.

FB_exp_rice <- FB_exp_rice %>%
  rowwise() %>%
  mutate(
    Exp_perc = ifelse(
      is.na(Export_percentage_act),
      Export_percentage_old,
      ifelse(
        is.na(Export_percentage_old),
        Export_percentage_act,
        mean(
          Export_percentage_old,
          Export_percentage_act
        )
      )
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Exp_perc
  ) %>%
  rename(
    P_exp_FB = 4
  )


### Soybean --------------------------------------------------------------------

# Join export-fraction estimates from the historical and current Food Balance
# datasets by country, year, and crop.

FB_exp_soybean <- FB_old_exp_Soybean %>%
  select(1:3, 6) %>%
  rename(
    Export_percentage_old = 4
  ) %>%
  full_join(
    FB_act_exp_soybean %>%
      select(1:3, 6) %>%
      rename(
        Export_percentage_act = 4
      ),
    by = c(
      "FAOStat_area_code",
      "Year",
      "Crop_Code"
    )
  )


# Assess agreement between the two datasets during overlapping years.
summary(
  lm(
    Export_percentage_old ~ Export_percentage_act,
    data = FB_exp_soybean %>% drop_na()
  )
)


# For years available in both datasets, use the mean of the two estimates.
# If only one dataset provides an estimate, retain the available value.

FB_exp_soybean <- FB_exp_soybean %>%
  rowwise() %>%
  mutate(
    Exp_perc = ifelse(
      is.na(Export_percentage_act),
      Export_percentage_old,
      ifelse(
        is.na(Export_percentage_old),
        Export_percentage_act,
        mean(
          Export_percentage_old,
          Export_percentage_act
        )
      )
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Exp_perc
  ) %>%
  rename(
    P_exp_FB = 4
  )

### Soybean cake ---------------------------------------------------------------

# Join export-fraction estimates from the historical and current Commodity
# Balance datasets by country, year, and crop.

FB_exp_soybean_cake <- FB_old_exp_soycake %>%
  select(1:4) %>%
  rename(
    Export_percentage_old = 4
  ) %>%
  full_join(
    FB_act_exp_soybean_cake %>%
      select(1:4) %>%
      rename(
        Export_percentage_act = 4
      ),
    by = c(
      "FAOStat_area_code",
      "Year",
      "Crop_Code"
    )
  )


# Assess agreement between the two datasets during overlapping years.
summary(
  lm(
    Export_percentage_old ~ Export_percentage_act,
    data = FB_exp_soybean_cake %>% drop_na()
  )
)


# For years available in both datasets, use the mean of the two estimates.
# If only one dataset provides an estimate, retain the available value.

FB_exp_soybean_cake <- FB_exp_soybean_cake %>%
  rowwise() %>%
  mutate(
    Exp_perc = ifelse(
      is.na(Export_percentage_act),
      Export_percentage_old,
      ifelse(
        is.na(Export_percentage_old),
        Export_percentage_act,
        mean(
          Export_percentage_old,
          Export_percentage_act
        )
      )
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Exp_perc
  ) %>%
  rename(
    P_exp_FB = 4
  )


### Soybean oil ----------------------------------------------------------------

# Join export-fraction estimates from the historical and current Food Balance
# datasets by country, year, and crop.

FB_exp_soybean_oil <- FB_old_exp_Soybean_oil %>%
  select(1:3, 6) %>%
  rename(
    Export_percentage_old = 4
  ) %>%
  full_join(
    FB_act_exp_soybean_oil %>%
      select(1:3, 6) %>%
      rename(
        Export_percentage_act = 4
      ),
    by = c(
      "FAOStat_area_code",
      "Year",
      "Crop_Code"
    )
  )


# Assess agreement between the two datasets during overlapping years.
summary(
  lm(
    Export_percentage_old ~ Export_percentage_act,
    data = FB_exp_soybean_oil %>% drop_na()
  )
)


# For years available in both datasets, use the mean of the two estimates.
# If only one dataset provides an estimate, retain the available value.

FB_exp_soybean_oil <- FB_exp_soybean_oil %>%
  rowwise() %>%
  mutate(
    Exp_perc = ifelse(
      is.na(Export_percentage_act),
      Export_percentage_old,
      ifelse(
        is.na(Export_percentage_old),
        Export_percentage_act,
        mean(
          Export_percentage_old,
          Export_percentage_act
        )
      )
    )
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    Exp_perc
  ) %>%
  rename(
    P_exp_FB = 4
  )


# 2.  Export results ---------------------------------------------------------------

# Save crop-specific export-fraction estimates for use in subsequent scripts.

write.csv(
  FB_exp_wheat,
  "results/Wheat_Exp_FB.csv",
  row.names = FALSE
)

write.csv(
  FB_exp_maize,
  "results/Maize_Exp_FB.csv",
  row.names = FALSE
)

write.csv(
  FB_exp_rice,
  "results/Rice_Exp_FB.csv",
  row.names = FALSE
)

write.csv(
  FB_exp_soybean,
  "results/Soybean_Exp_FB.csv",
  row.names = FALSE
)

write.csv(
  FB_exp_soybean_cake,
  "results/Soybean_cake_Exp_FB.csv",
  row.names = FALSE
)

write.csv(
  FB_exp_soybean_oil,
  "results/Soybean_oil_Exp_FB.csv",
  row.names = FALSE
)

