# ==============================================================================
# 02_TM_Estimations.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
# Estimate international trade allocation patterns for wheat, maize, rice,
# soybean, soybean cake, and soybean oil using FAOSTAT bilateral trade data
# and crop production data.
#
# The script constructs annual bilateral trade matrices, applies a modified
# Kastner trade-matrix approach to account for indirect trade flows and
# re-exports, and estimates:
#   1) the fraction of exports from each producing country allocated to each
#      importing country; and
#   2) the exporter-level fraction associated with international trade.
#
# Trade and production quantities are expressed on a comparable dry-matter
# basis where required, and 3-year moving averages are used to reduce
# year-to-year variability in reported trade and production data.
#
# External data:
#
#   FAOSTAT Detailed Trade Matrix:
#   https://www.fao.org/faostat/en/#data/TM
#   Download:
#   Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv
#
#   FAOSTAT Food Balances (2010-):
#   https://www.fao.org/faostat/en/#data/FBS
#   Download:
#   FoodBalanceSheets_E_All_Data_(Normalized).csv
#
#   FAOSTAT Historic Food Balance Sheets (-2013):
#   https://www.fao.org/faostat/en/#data/FBSH
#   Download:
#   FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv
#
#   FAOSTAT Commodity Balances (2010-):
#   https://www.fao.org/faostat/en/#data/CB
#   Download the normalized bulk file and filter to:
#   Item = "Cake of soya beans"
#   Save the filtered dataset as:
#   FBact_soycake.csv
#
#   FAOSTAT Historic Commodity Balance Sheets (-2013):
#   https://www.fao.org/faostat/en/#data/CBH
#   Download the normalized bulk file and filter to:
#   Item = "Soyabean cake"
#   Save the filtered dataset as:
#   FBold_soycake.csv
#
#   The original FAOSTAT datasets are not redistributed in this repository.
#   The datasets used in this study were downloaded in February 2024.
#
# Expected file locations:
#   data/Detailed_TradeMatrix/
#       Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv
#
#   data/Food_Balances/
#       FoodBalanceSheets_E_All_Data_(Normalized).csv
#       FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv
#       FBact_soycake.csv
#       FBold_soycake.csv
#
# Additional files:
#   data/Countries_DB.xlsx
#
# Outputs:
#   Crop-specific tables containing:
#     - bilateral trade-allocation percentages
#     - exporter-level trade percentages
#
#   Outputs are produced for:
#     wheat
#     maize
#     rice
#     soybean
#     soybean cake
#     soybean oil
#
# Notes:
#   This script uses the Matrix package for matrix operations and the TTR
#   package to calculate 3-year simple moving averages.
#
#   The trade-allocation calculations are based on functions defined within
#   this script and adapted to handle zero production, zero trade flows, and
#   numerical instability in matrix inversion.
#
#   Run this script after 01_Export_Estimation.R.
#   Run this script before 03_Country_Uses.R.
# ==============================================================================

# Load libraries ---------------------------------------------------------------
library(Matrix)     # Sparse matrix construction, inversion, and matrix operations
library(tidyverse)  # Data manipulation, reshaping, plotting, and CSV import
library(TTR)        # Calculation of 3-year simple moving averages using SMA()
library(readxl)     # Import Microsoft Excel (.xls/.xlsx) files


# Load data --------------------------------------------------------------------

## FAOSTAT Detailed Trade Matrix

# Bilateral international trade data used to construct annual trade matrices.
ITM <- read_csv(
  "data/Detailed_TradeMatrix/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"
)


## FAOSTAT Food Balance Sheets
# Historical Food Balance Sheets (-2013)
FB_old <- read_csv(
  "data/Food_Balances/FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv"
)
# Current Food Balance Sheets (2010-)
FB_act <- read_csv(
  "data/Food_Balances/FoodBalanceSheets_E_All_Data_(Normalized).csv"
)


## FAOSTAT Commodity Balance Sheets: soybean cake
# Historical Commodity Balance Sheets, filtered to soybean cake
FBold_soycake <- read_csv(
  "data/Food_Balances/FBold_soycake.csv"
)
# Current Commodity Balances, filtered to cake of soya beans
FBact_soycake <- read_csv(
  "data/Food_Balances/FBact_soycake.csv"
)


## Country classifications

# Country correspondence table containing FAOSTAT area codes and additional
# country identifiers required for matching datasets.
Countries_DB <- read_excel(
  "data/Countries_DB.xlsx"
)

# Functions -------------------------------------------------------------------
## Trade-matrix estimation functions ------------------------------------------

# Estimate the production-attribution matrix from bilateral trade flows.
#
# This function follows the trade-matrix methodology developed by Kastner
# et al. to trace production through direct and indirect international trade
# flows.
#
# Following this formulation, the scaling quantity x is defined as adjusted
# production plus total outgoing trade flows. With exporters represented in
# rows and importers in columns, Z %*% i gives the row sums of the trade matrix,
# i.e. total exports by country. This scaling quantity is used to construct the
# bilateral trade-share matrix and propagate production through successive
# trade and re-export pathways.
#
# Two numerical modifications are introduced here. First, countries with zero
# reported production but positive imports are assigned a very small production
# value proportional to their imports. Second, a small regularization term is
# added to the diagonal of (I - A) to improve numerical stability during matrix
# inversion.
#
# The function returns attribution values rather than percentages. Bilateral
# allocation proportions are calculated subsequently by normalizing the
# off-diagonal attribution values across destination countries. This final
# normalization is the main adaptation used in the present analysis, where the
# objective is to estimate relative bilateral allocation shares rather than
# absolute attributed quantities.
#
# References:
#   Kastner, T., Kastner, M. & Nonhebel, S. Tracing distant environmental impacts of agricultural prod-ucts from a consumer perspective. Ecol. Econ. 70, 1032–1040 (2011).

## Estimate the annual production-attribution matrix --------------------------

# This function applies the trade-matrix allocation procedure to a single year.
#
# It combines:
#   1) a country-level production vector (p), and
#   2) a bilateral trade matrix (Z),
#
# to estimate how production is redistributed through direct and indirect
# international trade pathways.
#
# The bilateral trade matrix must be square, with exporters represented in rows
# and importers represented in columns. The production vector must contain the
# same countries in the same order.
#
# The function first prepares and aligns the production and trade data, removes
# domestic self-flows.
# It then constructs the bilateral trade coefficient matrix A and solves the
# corresponding matrix system to account for successive rounds of re-export.
#
# The resulting matrix R contains country-to-country attribution values. These
# values are not yet proportions. Bilateral allocation proportions are derived
# later in the script by removing the domestic diagonal and normalizing the
# remaining values across destination countries.
#
# Arguments:
#   p      Country-level production vector or data frame.
#   Z      Square bilateral trade matrix with exporters in rows and importers
#          in columns.
#   reg    Small regularization term added to the diagonal of (I - A) to improve
#          numerical stability during matrix inversion.
#   tiny_x Threshold below which x is treated as zero.
#
# Returns:
#   A square country-by-country attribution matrix R.
#

Kastner_TM_Estimation <- function(p, Z, reg = 1e-3, tiny_x = 1e-9) {
  
  # Prepare trade matrix -------------------------------------------------------
  
  # Convert Z to a numeric matrix and treat negative trade values as positive.
  Z <- as.matrix(Z)
  Z <- abs(Z)
  
  # Store country identifiers.
  rn <- as.character(rownames(Z))
  cn <- as.character(colnames(Z))
  
  
  # Prepare production vector -------------------------------------------------
  
  # If p is supplied as a data frame, extract its numeric production column
  # while retaining country identifiers as names.
  if (is.data.frame(p)) {
    num_cols <- which(sapply(p, is.numeric))
    
    if (length(num_cols) == 0) {
      stop(
        "If p is a data frame, it must have at least one numeric column ",
        "(e.g. 'Production')."
      )
    }
    
    p_vec <- p[[num_cols[1]]]
    names(p_vec) <- rownames(p)
    p <- p_vec
  }
  
  # Ensure production values are numeric while retaining country names.
  p <- setNames(as.numeric(p), names(p))
  
  
  # Check matrix dimensions and country alignment -----------------------------
  
  if (nrow(Z) != ncol(Z)) {
    stop("Z must be square: rows = exporters, columns = importers.")
  }
  
  if (!identical(rn, cn)) {
    stop(
      "rownames(Z) must match colnames(Z) ",
      "(same set and order of countries)."
    )
  }
  
  if (!is.null(names(p)) && !all(rn %in% names(p))) {
    stop("Some countries in Z were not found in p.")
  }
  
  # Reorder production values to match the country order in Z.
  if (!is.null(names(p))) {
    p <- p[rn]
  }
  
  if (length(p) != nrow(Z)) {
    stop(
      "Length of p must equal nrow(Z) and ncol(Z). ",
      "Ensure identical country ordering."
    )
  }
  
  
  # Remove domestic self-flows ------------------------------------------------
  
  diag(Z) <- 0
  
  # Re-assign country identifiers in case they were dropped during operations.
  rownames(Z) <- rn
  colnames(Z) <- cn
  
  
  # Adjust zero-production importing countries --------------------------------
  
  # Countries with zero reported production but positive imports are assigned
  # a small production value proportional to their imports. This prevents
  # numerical instability in subsequent matrix operations.
  p_adj <- p
  
  no_prod_importers <- (p_adj == 0) & (colSums(Z) > 0)
  
  p_adj[no_prod_importers] <-
    colSums(Z)[no_prod_importers] * 1e-3
  
  
  # Calculate x ---------------------------------------------------------------
  
  # With exporters in rows and importers in columns, multiplying Z by a vector
  # of ones gives the row sums of Z, i.e. total exports by country.
  #
  # Therefore, in this implementation:
  #   x = adjusted production + total exports.
  i_ones <- rep(1, ncol(Z))
  
  imports <- colSums(Z, na.rm = TRUE)
  exports <- rowSums(Z, na.rm = TRUE)
  
  x <- p_adj + as.numeric(Z %*% i_ones)
  
  
  # Construct the bilateral trade coefficient matrix --------------------------
  
  # Treat extremely small values of x as zero.
  x[abs(x) < tiny_x] <- 0
  
  # Calculate the inverse of x, assigning zero where x is zero.
  x_inv <- ifelse(x > 0, 1 / x, 0)
  
  # Scale each column of the bilateral trade matrix by the corresponding
  # value of x.
  A <- Z %*% Matrix::Diagonal(x = x_inv)
  
  dimnames(A) <- list(rn, cn)
  
  # Set columns with x = 0 to zero.
  zero_cols <- which(x == 0)
  
  if (length(zero_cols) > 0) {
    A[, zero_cols] <- 0
  }
  
  
  # Solve the trade-attribution system ----------------------------------------
  
  # Construct (I - A).
  I_minus_A <- Matrix::Diagonal(n = nrow(A)) - A
  dimnames(I_minus_A) <- list(rn, rn)
  
  # Add a small regularization term to improve numerical stability during
  # matrix inversion.
  I_minus_A <-
    I_minus_A +
    Matrix::Diagonal(x = rep(reg, nrow(A)))
  
  dimnames(I_minus_A) <- list(rn, rn)
  
  # Solve the inverse. If sparse inversion fails, use a dense matrix fallback.
  R_left <- try(
    solve(I_minus_A),
    silent = TRUE
  )
  
  if (inherits(R_left, "try-error")) {
    R_left <- solve(as.matrix(I_minus_A))
  }
  
  dimnames(R_left) <- list(rn, rn)
  
  
  # Calculate the retained-share vector ---------------------------------------
  
  # Calculate the fraction of x remaining after subtracting total exports.
  # Because x is defined above as adjusted production + total exports, this
  # quantity represents the retained fraction used in the attribution matrix.
  exports <- rowSums(Z, na.rm = TRUE)
  
  c <- ifelse(
    x > 0,
    (x - exports) / x,
    0
  )
  
  c_hat <- Matrix::Diagonal(x = c)
  dimnames(c_hat) <- list(rn, rn)
  
  
  # Calculate final attribution matrix ----------------------------------------
  
  p_hat <- Matrix::Diagonal(x = p_adj)
  dimnames(p_hat) <- list(rn, rn)
  
  R <- c_hat %*% R_left %*% p_hat
  R <- as.matrix(R)
  
  # Preserve country identifiers.
  rownames(R) <- rn
  colnames(R) <- cn
  
  return(R)
}


### Apply the trade-matrix estimation across years ----------------------------

# This function prepares the annual production vectors and bilateral trade
# matrices required by Kastner_TM_Estimation() and applies the estimation
# independently to each year.
#
# For each year, the function:
#   1) selects the corresponding production and bilateral trade data;
#   2) identifies all countries represented in either dataset;
#   3) constructs a complete exporter-by-importer matrix;
#   4) assigns zero to bilateral relationships with no reported trade flow;
#   5) creates a production vector with the same country ordering;
#   6) checks that production and trade matrices are correctly aligned; and
#   7) applies Kastner_TM_Estimation().
#
# The resulting annual attribution matrices are stored in a named list, with
# each list element corresponding to one year.
#
# Arguments:
#   production  Data frame containing FAOSTAT area code, year, and production.
#   TM          Bilateral trade data containing exporter code, importer code,
#               year, and trade quantity.
#
# Returns:
#   A named list containing one country-by-country attribution matrix for each
#   year included in the production dataset.
#

Period_TM_Estimation <- function(production, TM) {
  
  kastner_estimations <- list()
  count <- 0
  years <- c()
  
  for (y in unique(production$Year)) {
    
    count <- count + 1
    years <- c(years, y)
    
    
    # Filter production and trade data for the selected year ------------------
    
    production_y <- production %>%
      filter(Year == y) %>%
      filter(FAOStat_area_code < 351)
    
    TM_y <- TM %>%
      filter(Year == y) %>%
      filter(
        FAOStat_area_code_Exp < 351,
        FAOStat_area_code_Imp < 351
      ) %>%
      select(!Year)
    
    
    # Identify all countries represented in production or trade data ----------
    
    countries <- unique(
      c(
        unique(TM_y$FAOStat_area_code_Imp),
        unique(TM_y$FAOStat_area_code_Exp),
        unique(production_y$FAOStat_area_code)
      )
    )
    
    
    # Construct complete bilateral trade matrix -------------------------------
    
    # Generate all possible exporter-importer combinations for the countries
    # represented in the selected year.
    TM_y_df <- expand.grid(
      FAOStat_area_code_Imp = unique(countries),
      FAOStat_area_code_Exp = unique(countries)
    )
    
    # Add observed bilateral trade flows.
    TM_y_df <- full_join(
      TM_y_df,
      TM_y,
      by = c(
        "FAOStat_area_code_Imp",
        "FAOStat_area_code_Exp"
      )
    )
    
    # Convert the bilateral data from long to wide format:
    #   rows    = exporters
    #   columns = importers
    TM_y_df <- as.data.frame(
      TM_y_df %>%
        pivot_wider(
          names_from = FAOStat_area_code_Imp,
          values_from = Value
        )
    )
    
    rownames(TM_y_df) <- TM_y_df$FAOStat_area_code_Exp
    
    TM_y_df <- TM_y_df %>%
      select(!FAOStat_area_code_Exp)
    
    # Missing bilateral trade relationships are interpreted as zero trade.
    TM_y_df <- TM_y_df %>%
      mutate_all(
        ~ ifelse(is.na(.), 0, .)
      )
    
    # Sort rows and columns so that exporters and importers follow the same
    # country order.
    TM_y_df <- TM_y_df[, order(colnames(TM_y_df))]
    TM_y_df <- TM_y_df[order(rownames(TM_y_df)), ]
    
    
    # Construct annual production vector --------------------------------------
    
    # Create one production entry for every country represented in the
    # production or bilateral trade data.
    production_y_df <- expand.grid(
      FAOStat_area_code = unique(countries)
    )
    
    production_y_df <- full_join(
      production_y_df,
      production_y,
      by = "FAOStat_area_code"
    ) %>%
      mutate_all(
        ~ ifelse(is.na(.), 0.00001, .)
      )
    
    # Missing production values are therefore assigned a very small positive
    # value (0.00001) before the matrix calculation.
    
    rownames(production_y_df) <-
      production_y_df$FAOStat_area_code
    
    production_y_df <-
      production_y_df[order(rownames(production_y_df)), ]
    
    production_y_df <- production_y_df %>%
      select(Production)
    
    
    # Verify country alignment -------------------------------------------------
    
    # The trade matrix must be square and ordered identically along both axes.
    stopifnot(
      identical(
        rownames(TM_y_df),
        colnames(TM_y_df)
      )
    )
    
    # Production values must follow the same country order as the trade matrix.
    stopifnot(
      identical(
        rownames(TM_y_df),
        rownames(production_y_df)
      )
    )
    
    
    # Estimate annual attribution matrix ---------------------------------------
    
    kastner_estimations[[count]] <-
      Kastner_TM_Estimation(
        p = production_y_df,
        Z = TM_y_df
      )
    
    kastner_estimations[[count]] <-
      as.data.frame(
        as.matrix(kastner_estimations[[count]])
      )
    
    rownames(kastner_estimations[[count]]) <-
      rownames(TM_y_df)
    
    colnames(kastner_estimations[[count]]) <-
      rownames(TM_y_df)
  }
  
  
  # Name each matrix according to its corresponding year.
  names(kastner_estimations) <- years
  
  return(kastner_estimations)
}


# Wheat ------------------------------------------------------------------------

## 1: Prepare bilateral wheat trade data -----------------------------------

# Select wheat and wheat-derived products from the FAOSTAT Detailed Trade
# Matrix. Import records (Element Code 5610) are used because these contain the
# reporting importer and corresponding exporting trade partner.
#
# Trade quantities are converted from tonnes to kt and subsequently converted
# to dry matter using product-specific conversion factors. All wheat products
# are then aggregated to one exporter-importer-year flow. The commodities
# and their dry matter contents are reported in the supplementary material.

Wheat_TM <- ITM %>%
  rename(
    Item_Code = 7,
    Element_Code = 10
  ) %>%
  filter(
    Item_Code %in% c(15, 16, 17, 19) &
      Element_Code == 5610
  ) %>%
  rename(
    FAOStat_area_code_Imp = 1,
    FAOStat_area_code_Exp = 4,
    Year = 13,
    Value = 15
  ) %>%
  select(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Item_Code,
    Year,
    Value
  ) %>%
  filter(
    FAOStat_area_code_Imp < 351
  ) %>%
  mutate(
    # Convert tonnes to kt.
    Value = Value / 1000,
    
    # Convert individual wheat products to dry matter.
    Value_DM = case_when(
      Item_Code == 15 ~ Value * 0.874,  # Wheat
      Item_Code == 16 ~ Value * 0.874,  # Wheat and meslin flour
      Item_Code == 17 ~ Value * 0.917,  # Bran of wheat
      Item_Code == 19 ~ Value * 0.933   # Germ of wheat
    )
  ) %>%
  group_by(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_DM = sum(Value_DM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Value = Value_DM
  ) %>%
  mutate(
    # Identifier for each exporter-importer pair.
    com = paste(
      FAOStat_area_code_Exp,
      "_",
      FAOStat_area_code_Imp
    )
  )


## 2: Calculate 3-year moving averages of bilateral trade ------------------

# A complete annual series from 1986 to 2022 is created for each bilateral
# exporter-importer relationship. Missing annual trade flows are interpreted as
# zero before calculating the 3-year simple moving average.

Wheat_TM_ma <- as.data.frame(
  matrix(ncol = 7)
)

colnames(Wheat_TM_ma) <- c(
  "FAOStat_area_code_Exp",
  "FAOStat_area_code_Imp",
  "com",
  "Year",
  "Value",
  "Value_0",
  "Value_MA"
)

for (i in unique(Wheat_TM$com)) {
  
  df <- Wheat_TM %>%
    filter(com == i) %>%
    mutate(
      Value = abs(Value)
    )
  
  # Generate a complete annual sequence for the bilateral relationship.
  all_df <- expand.grid(
    FAOStat_area_code_Imp = unique(df$FAOStat_area_code_Imp),
    FAOStat_area_code_Exp = unique(df$FAOStat_area_code_Exp),
    Year = seq(1986, 2022),
    com = i
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code_Imp",
      "FAOStat_area_code_Exp",
      "Year",
      "com"
    )
  ) %>%
    mutate(
      # Missing bilateral trade is treated as zero.
      Value_0 = ifelse(
        is.na(Value),
        0,
        Value
      )
    ) %>%
    arrange(Year)
  
  # Calculate the 3-year simple moving average.
  moving_average <- SMA(
    df$Value_0,
    n = 3
  )
  
  df$Value_MA <- moving_average
  
  Wheat_TM_ma <- rbind(
    df,
    Wheat_TM_ma
  )
}


# Retain years for which the 3-year moving average can be calculated.
Wheat_TM_ma_fil <- Wheat_TM_ma %>%
  drop_na(Value_MA) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Value_MA
  ) %>%
  rename(
    Value = Value_MA
  )


## 3: Prepare wheat production data ----------------------------------------

# Wheat production is extracted independently from the historical and current
# FAOSTAT Food Balance Sheets.
#
# FAOSTAT codes:
#   Item 2511    = Wheat and products
#   Element 5511 = Production

FB_old_prod_wheat_period <- FB_old %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "1_1"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


FB_act_prod_wheat_period <- FB_act %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "1_1"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


# Combine historical and current Food Balance production estimates.
# Where both datasets contain an estimate for the same country-year, their mean
# is used.

Wheat_Production <- rbind(
  FB_old_prod_wheat_period,
  FB_act_prod_wheat_period
) %>%
  group_by(
    FAOStat_area_code,
    Year
  ) %>%
  summarise(
    Production = mean(
      Production,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 4: Calculate 3-year moving averages of wheat production -----------------

Wheat_Production_ma <- as.data.frame(
  matrix(ncol = 5)
)

colnames(Wheat_Production_ma) <- c(
  "FAOStat_area_code",
  "Year",
  "Production",
  "Production_0",
  "Production_MA"
)

for (i in unique(Wheat_Production$FAOStat_area_code)) {
  
  df <- Wheat_Production %>%
    filter(
      FAOStat_area_code == i
    ) %>%
    mutate(
      Production = abs(Production)
    )
  
  # Generate a complete annual production series from 1961 to 2022.
  all_df <- expand.grid(
    FAOStat_area_code = unique(df$FAOStat_area_code),
    Year = seq(1961, 2022)
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code",
      "Year"
    )
  ) %>%
    mutate(
      # Missing annual production values are treated as zero before calculating
      # the moving average.
      Production_0 = ifelse(
        is.na(Production),
        0,
        Production
      )
    ) %>%
    arrange(Year)
  
  # Calculate the 3-year simple moving average.
  moving_average <- SMA(
    df$Production_0,
    n = 3
  )
  
  df$Production_MA <- moving_average
  
  Wheat_Production_ma <- rbind(
    df,
    Wheat_Production_ma
  )
}


# Retain years with a valid moving average and convert wheat production to
# dry matter using a factor of 0.87.

Wheat_Production_ma_fil <- Wheat_Production_ma %>%
  drop_na(Production_MA) %>%
  select(
    FAOStat_area_code,
    Year,
    Production_MA
  ) %>%
  rename(
    Production = Production_MA
  ) %>%
  mutate(
    Production = 0.87 * Production
  )


## 5: Apply the trade-matrix allocation method -----------------------------

# Apply the attribution method independently to each year using the smoothed
# dry-matter production and bilateral trade series.

wheat_Kastner <- Period_TM_Estimation(
  production = Wheat_Production_ma_fil,
  TM = Wheat_TM_ma_fil
)


## Step 6: Convert attribution values to bilateral allocation proportions -------

# The diagonal of each attribution matrix represents within-country attribution
# and is removed here because the objective is to estimate the relative
# distribution of internationally allocated flows.
#
# Each remaining row is normalized to sum to one, producing the proportion of
# the internationally attributed flow associated with each destination country.

wheat_Kastner_Percentage <- wheat_Kastner

for (year in names(wheat_Kastner_Percentage)) {
  
  M <- as.matrix(
    wheat_Kastner_Percentage[[year]]
  )
  
  # Exclude within-country attribution.
  diag(M) <- 0
  
  # Replace non-finite and negative values with zero.
  M[!is.finite(M)] <- 0
  M[M < 0] <- 0
  
  # Calculate the total internationally attributed quantity for each row.
  rs <- rowSums(M)
  
  # Initialize the bilateral-proportion matrix.
  m_percent <- matrix(
    0,
    nrow = nrow(M),
    ncol = ncol(M),
    dimnames = dimnames(M)
  )
  
  # Normalize rows with positive totals.
  nz <- rs > 0
  
  m_percent[nz, ] <-
    M[nz, ] / rs[nz]
  
  # Remove numerically negligible bilateral shares.
  m_percent[m_percent < 1e-4] <- 0
  
  
  # Set allocation shares to zero for countries without a wheat production
  # record in the corresponding year.
  yr_val <- suppressWarnings(
    as.integer(year)
  )
  
  if (!is.na(yr_val) &&
      is.numeric(Wheat_Production$Year)) {
    
    prod_y <- Wheat_Production %>%
      filter(
        Year == yr_val
      )
    
  } else {
    
    prod_y <- Wheat_Production %>%
      filter(
        Year == year
      )
  }
  
  prod_y <- prod_y %>%
    mutate(
      FAOStat_area_code_chr =
        as.character(FAOStat_area_code)
    )
  
  exporter_codes <- rownames(m_percent)
  
  has_prod <-
    exporter_codes %in%
    prod_y$FAOStat_area_code_chr
  
  m_percent[!has_prod, ] <- 0
  
  wheat_Kastner_Percentage[[year]] <-
    m_percent
}


## 7: Convert annual matrices to long-format data --------------------------

wheat_Kastner_Percentage_df <- map_dfr(
  names(wheat_Kastner_Percentage),
  function(year) {
    
    wheat_Kastner_Percentage[[year]] %>%
      as.data.frame() %>%
      rownames_to_column(
        var = "FAOStat_area_code_Exp"
      ) %>%
      pivot_longer(
        cols = -FAOStat_area_code_Exp,
        names_to = "FAOStat_area_code_Imp",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Year = year
      )
  }
)


## 8: Reconstruct bilateral proportions before 1990 ------------------------

# Bilateral trade-matrix information is unavailable for the earlier part of the
# study period. The mean bilateral allocation structure from the available
# reference period is therefore used to represent 1961-1989.
#


wheat_Kastner_Percentage_df_90_20 <-
  wheat_Kastner_Percentage_df %>%
  filter(
    Year > 1986,
    Year < 2020
  ) %>%
  mutate(
    Value = ifelse(
      Value == Inf,
      0,
      Value
    )
  ) %>%
  filter(
    Year > 1987
  )


# Calculate the mean bilateral allocation share over the reference period.
wheat_Kastner_Percentage_df_90_20_avg <-
  wheat_Kastner_Percentage_df_90_20 %>%
  group_by(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the row total of the mean allocation shares.
wheat_Kastner_Percentage_df_90_20_avg_tot <-
  wheat_Kastner_Percentage_df_90_20_avg %>%
  group_by(
    FAOStat_area_code_Exp
  ) %>%
  summarise(
    Value_t = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Re-normalize the average bilateral allocation structure.
wheat_Kastner_Percentage_df_90_20_avg <-
  wheat_Kastner_Percentage_df_90_20_avg %>%
  left_join(
    wheat_Kastner_Percentage_df_90_20_avg_tot,
    by = "FAOStat_area_code_Exp"
  ) %>%
  mutate(
    Value = Value / Value_t
  )


# Apply the average bilateral allocation structure to every year from
# 1961 to 1989.

wheat_Kastner_Percentage_df_61_89 <-
  expand.grid(
    FAOStat_area_code_Imp =
      unique(
        wheat_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Imp
      ),
    FAOStat_area_code_Exp =
      unique(
        wheat_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Exp
      ),
    Year = seq(1961, 1989)
  ) %>%
  left_join(
    wheat_Kastner_Percentage_df_90_20_avg %>%
      select(
        FAOStat_area_code_Exp,
        FAOStat_area_code_Imp,
        Value
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "FAOStat_area_code_Imp"
    )
  )


# Retain observed/estimated annual proportions from 1990 onward.
wheat_Kastner_Percentage_df_90_20 <-
  wheat_Kastner_Percentage_df_90_20 %>%
  filter(
    Year > 1989
  )


# Combine reconstructed 1961-1989 proportions with the later annual estimates.
wheat_Kastner_Percentage_df <- rbind(
  wheat_Kastner_Percentage_df_61_89,
  wheat_Kastner_Percentage_df_90_20
)


## 9: Extract within-country attribution shares ----------------------------

# Normalize each full attribution matrix by its row total and extract the
# diagonal element for each country.
#
# The diagonal represents the within-country share of the normalized
# attribution matrix. These values are used only as normalization terms in
# subsequent calculations.
#
# They should not be interpreted as the exported fraction. The exported
# fraction used in the analysis is estimated independently from FAOSTAT Food
# Balance data in script 01.


wheat_Kastner_Exp_Percentage <-
  wheat_Kastner

for (year in names(wheat_Kastner_Exp_Percentage)) {
  
  row_sums <- rowSums(
    wheat_Kastner_Exp_Percentage[[year]],
    na.rm = TRUE
  )
  
  row_sums[row_sums < 0.000001] <- 0
  
  # Expand row totals to the dimensions of the attribution matrix.
  row_sums_matrix <- matrix(
    row_sums,
    nrow = nrow(
      wheat_Kastner_Exp_Percentage[[year]]
    ),
    ncol = ncol(
      wheat_Kastner_Exp_Percentage[[year]]
    ),
    byrow = FALSE
  )
  
  # Normalize each row of the attribution matrix.
  m_percent <-
    wheat_Kastner_Exp_Percentage[[year]] /
    row_sums_matrix
  
  # Remove numerically negligible shares.
  m_percent[m_percent < 0.0001] <- 0
  
  # Retain the diagonal share for each country.
  wheat_Kastner_Exp_Percentage[[year]] <-
    diag(
      as.matrix(m_percent)
    )
  
  # Assign zero to countries without a wheat production record.
  for (
    country in
    rownames(
      wheat_Kastner_Exp_Percentage[[year]]
    )
  ) {
    
    prod_val <- Wheat_Production %>%
      filter(
        Year == year,
        FAOStat_area_code == country
      )
    
    if (nrow(prod_val) == 0) {
      wheat_Kastner_Exp_Percentage[[year]][country, ] <- 0
    }
  }
}


## 10: Convert normalization shares to a data frame ---------------

wheat_Kastner_Exp_Percentage_df <-
  data.frame()

for (year in names(wheat_Kastner_Exp_Percentage)) {
  
  m_percent <-
    wheat_Kastner_Exp_Percentage[[year]]
  
  year_df <-
    as.data.frame(
      as.matrix(m_percent)
    ) %>%
    mutate(
      FAOStat_area_code =
        rownames(.)
    ) %>%
    rename(
      Perc_Exp_Kastner = 1,
      FAOStat_area_code = 2
    )
  
  year_df$Year <- year
  
  wheat_Kastner_Exp_Percentage_df <-
    rbind(
      wheat_Kastner_Exp_Percentage_df,
      year_df
    )
}

## 11: Export the results--------------
write.csv(wheat_Kastner_Percentage_df, "Wheat_Kastner_Percentage_df.csv", row.names = F)
write.csv(wheat_Kastner_Exp_Percentage_df, "Wheat_Kastner_Exp_Percentage.csv", row.names = F)

# Maize ------------------------------------------------------------------------

## 1: Prepare bilateral maize trade data ---------------------------------------

# Select maize and maize-derived products from the FAOSTAT Detailed Trade
# Matrix. Import records (Element Code 5610) are used because these contain the
# reporting importer and corresponding exporting trade partner.
#
# Trade quantities are converted from tonnes to kt and subsequently converted
# to dry matter using product-specific conversion factors. All maize products
# are then aggregated to one exporter-importer-year flow. The commodities 
# and their dry matter content are reported in the supplementary material

maize_TM <- ITM %>%
  rename(
    Item_Code = 7,
    Element_Code = 10
  ) %>%
  filter(
    Item_Code %in% c(56, 57, 58, 59, 60, 61, 446, 636) &
      Element_Code == 5610
  ) %>%
  rename(
    FAOStat_area_code_Imp = 1,
    FAOStat_area_code_Exp = 4,
    Year = 13,
    Value = 15
  ) %>%
  select(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Item_Code,
    Year,
    Value
  ) %>%
  filter(
    FAOStat_area_code_Imp < 351
  ) %>%
  mutate(
    # Convert tonnes to kt.
    Value = Value / 1000,
    
    # Convert individual maize products to dry matter.
    Value_DM = case_when(
      Item_Code == 56  ~ Value * 0.863,  # Maize
      Item_Code == 57  ~ Value * 0.932,  # Germ of maize
      Item_Code == 58  ~ Value * 0.890,  # Flour of maize
      Item_Code == 59  ~ Value * 0.909,  # Bran of maize
      Item_Code == 60  ~ Value * 1.000,  # Oil of maize
      Item_Code == 61  ~ Value * 0.916,  # Cake of maize
      Item_Code == 446 ~ Value * 0.301,  # Green corn (maize)
      Item_Code == 636 ~ Value * 0.303   # Forage and silage, maize
    )
  ) %>%
  group_by(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_DM = sum(Value_DM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Value = Value_DM
  ) %>%
  mutate(
    # Identifier for each exporter-importer pair.
    com = paste(
      FAOStat_area_code_Exp,
      "_",
      FAOStat_area_code_Imp
    )
  )

## 2: Calculate 3-year moving averages of bilateral trade ------------------

# A complete annual series from 1986 to 2022 is created for each bilateral
# exporter-importer relationship. Missing annual trade flows are interpreted as
# zero before calculating the 3-year simple moving average.

maize_TM_ma <- as.data.frame(
  matrix(ncol = 7)
)

colnames(maize_TM_ma) <- c(
  "FAOStat_area_code_Exp",
  "FAOStat_area_code_Imp",
  "com",
  "Year",
  "Value",
  "Value_0",
  "Value_MA"
)

for (i in unique(maize_TM$com)) {
  
  df <- maize_TM %>%
    filter(com == i) %>%
    mutate(
      Value = abs(Value)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code_Imp = unique(df$FAOStat_area_code_Imp),
    FAOStat_area_code_Exp = unique(df$FAOStat_area_code_Exp),
    Year = seq(1986, 2022),
    com = i
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code_Imp",
      "FAOStat_area_code_Exp",
      "Year",
      "com"
    )
  ) %>%
    mutate(
      Value_0 = ifelse(
        is.na(Value),
        0,
        Value
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Value_0,
    n = 3
  )
  
  df$Value_MA <- moving_average
  
  maize_TM_ma <- rbind(
    df,
    maize_TM_ma
  )
}


maize_TM_ma_fil <- maize_TM_ma %>%
  drop_na(Value_MA) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Value_MA
  ) %>%
  rename(
    Value = Value_MA
  )


## 3: Prepare maize production data ----------------------------------------

# Maize production is extracted independently from the historical and current
# FAOSTAT Food Balance Sheets.
#
# FAOSTAT codes:
#   Item 2514    = Maize and products
#   Element 5511 = Production

FB_old_prod_maize_period <- FB_old %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "1_2"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


FB_act_prod_maize_period <- FB_act %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "1_2"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


# Combine historical and current Food Balance production estimates.
# Where both datasets contain an estimate for the same country-year, their mean
# is used.

maize_Production <- rbind(
  FB_old_prod_maize_period,
  FB_act_prod_maize_period
) %>%
  group_by(
    FAOStat_area_code,
    Year
  ) %>%
  summarise(
    Production = mean(
      Production,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 4: Calculate 3-year moving averages of maize production -----------------

maize_Production_ma <- as.data.frame(
  matrix(ncol = 5)
)

colnames(maize_Production_ma) <- c(
  "FAOStat_area_code",
  "Year",
  "Production",
  "Production_0",
  "Production_MA"
)

for (i in unique(maize_Production$FAOStat_area_code)) {
  
  df <- maize_Production %>%
    filter(
      FAOStat_area_code == i
    ) %>%
    mutate(
      Production = abs(Production)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code = unique(df$FAOStat_area_code),
    Year = seq(1961, 2022)
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code",
      "Year"
    )
  ) %>%
    mutate(
      Production_0 = ifelse(
        is.na(Production),
        0,
        Production
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Production_0,
    n = 3
  )
  
  df$Production_MA <- moving_average
  
  maize_Production_ma <- rbind(
    df,
    maize_Production_ma
  )
}


# Retain years with a valid moving average and convert maize production to
# dry matter using a factor of 0.863.

maize_Production_ma_fil <- maize_Production_ma %>%
  drop_na(Production_MA) %>%
  select(
    FAOStat_area_code,
    Year,
    Production_MA
  ) %>%
  rename(
    Production = Production_MA
  ) %>%
  mutate(
    Production = 0.863 * Production
  )


## 5: Apply the trade-matrix allocation method -----------------------------

maize_Kastner <- Period_TM_Estimation(
  production = maize_Production_ma_fil,
  TM = maize_TM_ma_fil
)


## 6: Convert attribution values to bilateral allocation proportions -------

# Remove within-country attribution and normalize each row so that the remaining
# international attribution values represent bilateral allocation proportions.

maize_Kastner_Percentage <- maize_Kastner

for (year in names(maize_Kastner_Percentage)) {
  
  M <- as.matrix(
    maize_Kastner_Percentage[[year]]
  )
  
  # Exclude within-country attribution.
  diag(M) <- 0
  
  # Replace non-finite and negative values with zero.
  M[!is.finite(M)] <- 0
  M[M < 0] <- 0
  
  # Calculate total internationally attributed quantity for each row.
  rs <- rowSums(M)
  
  # Initialize bilateral-proportion matrix.
  m_percent <- matrix(
    0,
    nrow = nrow(M),
    ncol = ncol(M),
    dimnames = dimnames(M)
  )
  
  # Normalize rows with positive totals.
  nz <- rs > 0
  
  m_percent[nz, ] <-
    M[nz, ] / rs[nz]
  
  # Remove numerically negligible bilateral shares.
  m_percent[m_percent < 1e-4] <- 0
  
  # Set allocation shares to zero for countries without a maize production
  # record in the corresponding year.
  yr_val <- suppressWarnings(
    as.integer(year)
  )
  
  if (!is.na(yr_val) &&
      is.numeric(maize_Production$Year)) {
    
    prod_y <- maize_Production %>%
      filter(
        Year == yr_val
      )
    
  } else {
    
    prod_y <- maize_Production %>%
      filter(
        Year == year
      )
  }
  
  prod_y <- prod_y %>%
    mutate(
      FAOStat_area_code_chr =
        as.character(FAOStat_area_code)
    )
  
  exporter_codes <- rownames(m_percent)
  
  has_prod <-
    exporter_codes %in%
    prod_y$FAOStat_area_code_chr
  
  m_percent[!has_prod, ] <- 0
  
  maize_Kastner_Percentage[[year]] <-
    m_percent
}


## 7: Convert annual matrices to long-format data --------------------------

maize_Kastner_Percentage_df <- map_dfr(
  names(maize_Kastner_Percentage),
  function(year) {
    
    maize_Kastner_Percentage[[year]] %>%
      as.data.frame() %>%
      rownames_to_column(
        var = "FAOStat_area_code_Exp"
      ) %>%
      pivot_longer(
        cols = -FAOStat_area_code_Exp,
        names_to = "FAOStat_area_code_Imp",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Year = year
      )
  }
)


## 8: Reconstruct bilateral proportions before 1990 ------------------------

# Bilateral trade-matrix information is unavailable for the earlier part of the
# study period. The mean bilateral allocation structure from 1988-2019 is used
# to represent the period 1961-1989.

maize_Kastner_Percentage_df_90_20 <-
  maize_Kastner_Percentage_df %>%
  filter(
    Year > 1987,
    Year < 2020
  ) %>%
  mutate(
    Value = ifelse(
      Value == Inf,
      0,
      Value
    )
  )


# Calculate mean bilateral allocation shares over the reference period.
maize_Kastner_Percentage_df_90_20_avg <-
  maize_Kastner_Percentage_df_90_20 %>%
  group_by(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the total mean allocation share for each exporter.
maize_Kastner_Percentage_df_90_20_avg_tot <-
  maize_Kastner_Percentage_df_90_20_avg %>%
  group_by(
    FAOStat_area_code_Exp
  ) %>%
  summarise(
    Value_t = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Re-normalize the average bilateral allocation structure.
maize_Kastner_Percentage_df_90_20_avg <-
  maize_Kastner_Percentage_df_90_20_avg %>%
  left_join(
    maize_Kastner_Percentage_df_90_20_avg_tot,
    by = "FAOStat_area_code_Exp"
  ) %>%
  mutate(
    Value = Value / Value_t
  )


# Apply the average allocation structure to every year from 1961 to 1989.
maize_Kastner_Percentage_df_61_89 <-
  expand.grid(
    FAOStat_area_code_Imp =
      unique(
        maize_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Imp
      ),
    FAOStat_area_code_Exp =
      unique(
        maize_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Exp
      ),
    Year = seq(1961, 1989)
  ) %>%
  left_join(
    maize_Kastner_Percentage_df_90_20_avg %>%
      select(
        FAOStat_area_code_Exp,
        FAOStat_area_code_Imp,
        Value
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "FAOStat_area_code_Imp"
    )
  )


# Retain annual estimates from 1990 onward.
maize_Kastner_Percentage_df_90_20 <-
  maize_Kastner_Percentage_df_90_20 %>%
  filter(
    Year > 1989
  )


# Combine reconstructed 1961-1989 proportions with later annual estimates.
maize_Kastner_Percentage_df <- rbind(
  maize_Kastner_Percentage_df_61_89,
  maize_Kastner_Percentage_df_90_20
)


## 9: Extract within-country attribution shares ----------------------------

# Normalize each full attribution matrix by its row total and extract the
# diagonal element for each country.
#
# The diagonal represents the within-country share of the normalized
# attribution matrix. These values are used only as normalization terms in
# subsequent calculations.
#
# They should not be interpreted as the exported fraction. The exported
# fraction used in the analysis is estimated independently from FAOSTAT Food
# Balance data in script 01.

maize_Kastner_Exp_Percentage <-
  maize_Kastner

for (year in names(maize_Kastner_Exp_Percentage)) {
  
  row_sums <- rowSums(
    maize_Kastner_Exp_Percentage[[year]],
    na.rm = TRUE
  )
  
  row_sums[row_sums < 0.000001] <- 0
  
  row_sums_matrix <- matrix(
    row_sums,
    nrow = nrow(
      maize_Kastner_Exp_Percentage[[year]]
    ),
    ncol = ncol(
      maize_Kastner_Exp_Percentage[[year]]
    ),
    byrow = FALSE
  )
  
  # Normalize each row.
  m_percent <-
    maize_Kastner_Exp_Percentage[[year]] /
    row_sums_matrix
  
  # Remove numerically negligible shares.
  m_percent[m_percent < 0.0001] <- 0
  
  # Retain diagonal attribution share.
  maize_Kastner_Exp_Percentage[[year]] <-
    diag(
      as.matrix(m_percent)
    )
  
  # Assign zero to countries without a maize production record.
  for (
    country in
    rownames(
      maize_Kastner_Exp_Percentage[[year]]
    )
  ) {
    
    prod_val <- maize_Production %>%
      filter(
        Year == year,
        FAOStat_area_code == country
      )
    
    if (nrow(prod_val) == 0) {
      maize_Kastner_Exp_Percentage[[year]][country, ] <- 0
    }
  }
}


## 10: Convert country-level diagonal shares to a data frame ---------------

maize_Kastner_Exp_Percentage_df <-
  data.frame()

for (year in names(maize_Kastner_Exp_Percentage)) {
  
  m_percent <-
    maize_Kastner_Exp_Percentage[[year]]
  
  year_df <-
    as.data.frame(
      as.matrix(m_percent)
    ) %>%
    mutate(
      FAOStat_area_code =
        rownames(.)
    ) %>%
    rename(
      Perc_Exp_Kastner = 1,
      FAOStat_area_code = 2
    )
  
  year_df$Year <- year
  
  maize_Kastner_Exp_Percentage_df <-
    rbind(
      maize_Kastner_Exp_Percentage_df,
      year_df
    )
}


## 11: Export results ---------------------------------------------------------------

write.csv(
  maize_Kastner_Percentage_df,
  "results/maize_Kastner_Percentage_df.csv",
  row.names = FALSE
)

write.csv(
  maize_Kastner_Exp_Percentage_df,
  "results/maize_Kastner_Exp_Percentage.csv",
  row.names = FALSE
)

# Rice -------------------------------------------------------------------------

## 1: Prepare bilateral rice trade data ----------------------------------------

# Select rice and rice-derived products from the FAOSTAT Detailed Trade
# Matrix. Import records (Element Code 5610) are used because these contain the
# reporting importer and corresponding exporting trade partner.
#
# Trade quantities are converted from tonnes to kt and subsequently converted
# to dry matter using product-specific conversion factors. All rice products
# are then aggregated to one exporter-importer-year flow. The commodities
# and their dry matter contents are reported in the supplementary material.

rice_TM <- ITM %>%
  rename(
    Item_Code = 7,
    Element_Code = 10
  ) %>%
  filter(
    Item_Code %in% c(27, 28, 29, 30, 31, 32, 35, 36, 37, 38, 39) &
      Element_Code == 5610
  ) %>%
  rename(
    FAOStat_area_code_Imp = 1,
    FAOStat_area_code_Exp = 4,
    Year = 13,
    Value = 15
  ) %>%
  select(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Item_Code,
    Year,
    Value
  ) %>%
  filter(
    FAOStat_area_code_Imp < 351
  ) %>%
  mutate(
    # Convert tonnes to kt.
    Value = Value / 1000,
    
    # Convert individual rice products to dry matter.
    Value_DM = case_when(
      Item_Code == 27 ~ Value * 0.880,  # Rice
      Item_Code == 28 ~ Value * 0.891,  # Husked rice
      Item_Code == 29 ~ Value * 0.875,  # Rice, milled (husked)
      Item_Code == 30 ~ Value * 0.880,  # Rice, paddy (rice milled equivalent)
      Item_Code == 31 ~ Value * 0.875,  # Rice, milled
      Item_Code == 32 ~ Value * 0.876,  # Rice, broken
      Item_Code == 35 ~ Value * 0.939,  # Bran of rice
      Item_Code == 36 ~ Value * 1.000,  # Oil of rice bran
      Item_Code == 37 ~ Value * 0.942,  # Cake of rice bran
      Item_Code == 38 ~ Value * 0.892,  # Flour of rice
      Item_Code == 39 ~ Value * 0.221   # Rice-fermented beverages
    )
  ) %>%
  group_by(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_DM = sum(Value_DM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Value = Value_DM
  ) %>%
  mutate(
    # Identifier for each exporter-importer pair.
    com = paste(
      FAOStat_area_code_Exp,
      "_",
      FAOStat_area_code_Imp
    )
  )


## 2: Calculate 3-year moving averages of bilateral trade ----------------------

# A complete annual series from 1986 to 2022 is created for each bilateral
# exporter-importer relationship. Missing annual trade flows are interpreted as
# zero before calculating the 3-year simple moving average.

rice_TM_ma <- as.data.frame(
  matrix(ncol = 7)
)

colnames(rice_TM_ma) <- c(
  "FAOStat_area_code_Exp",
  "FAOStat_area_code_Imp",
  "com",
  "Year",
  "Value",
  "Value_0",
  "Value_MA"
)

for (i in unique(rice_TM$com)) {
  
  df <- rice_TM %>%
    filter(com == i) %>%
    mutate(
      Value = abs(Value)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code_Imp = unique(df$FAOStat_area_code_Imp),
    FAOStat_area_code_Exp = unique(df$FAOStat_area_code_Exp),
    Year = seq(1986, 2022),
    com = i
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code_Imp",
      "FAOStat_area_code_Exp",
      "Year",
      "com"
    )
  ) %>%
    mutate(
      Value_0 = ifelse(
        is.na(Value),
        0,
        Value
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Value_0,
    n = 3
  )
  
  df$Value_MA <- moving_average
  
  rice_TM_ma <- rbind(
    df,
    rice_TM_ma
  )
}


rice_TM_ma_fil <- rice_TM_ma %>%
  drop_na(Value_MA) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Value_MA
  ) %>%
  rename(
    Value = Value_MA
  )


## 3: Prepare rice production data ---------------------------------------------

# Rice production is extracted independently from the historical and current
# FAOSTAT Food Balance Sheets.
#
# FAOSTAT codes:
#   Historical Item 2805 = Rice and products
#   Current Item 2807    = Rice and products
#   Element 5511         = Production

FB_old_prod_rice_period <- FB_old %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "1_3"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


FB_act_prod_rice_period <- FB_act %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "1_3"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


# Combine historical and current Food Balance production estimates.
# Where both datasets contain an estimate for the same country-year, their mean
# is used.

rice_Production <- rbind(
  FB_old_prod_rice_period,
  FB_act_prod_rice_period
) %>%
  group_by(
    FAOStat_area_code,
    Year
  ) %>%
  summarise(
    Production = mean(
      Production,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 4: Calculate 3-year moving averages of rice production ----------------------

rice_Production_ma <- as.data.frame(
  matrix(ncol = 5)
)

colnames(rice_Production_ma) <- c(
  "FAOStat_area_code",
  "Year",
  "Production",
  "Production_0",
  "Production_MA"
)

for (i in unique(rice_Production$FAOStat_area_code)) {
  
  df <- rice_Production %>%
    filter(
      FAOStat_area_code == i
    ) %>%
    mutate(
      Production = abs(Production)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code = unique(df$FAOStat_area_code),
    Year = seq(1961, 2022)
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code",
      "Year"
    )
  ) %>%
    mutate(
      Production_0 = ifelse(
        is.na(Production),
        0,
        Production
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Production_0,
    n = 3
  )
  
  df$Production_MA <- moving_average
  
  rice_Production_ma <- rbind(
    df,
    rice_Production_ma
  )
}


# Retain years with a valid moving average and convert rice production to
# dry matter using a factor of 0.880.

rice_Production_ma_fil <- rice_Production_ma %>%
  drop_na(Production_MA) %>%
  select(
    FAOStat_area_code,
    Year,
    Production_MA
  ) %>%
  rename(
    Production = Production_MA
  ) %>%
  mutate(
    Production = 0.880 * Production
  ) %>%
  filter(
    FAOStat_area_code < 300
  )


## 5: Apply the trade-matrix allocation method ---------------------------------

rice_Kastner <- Period_TM_Estimation(
  production = rice_Production_ma_fil,
  TM = rice_TM_ma_fil
)


## 6: Convert attribution values to bilateral allocation proportions -----------

# Remove within-country attribution and normalize each row so that the remaining
# international attribution values represent bilateral allocation proportions.

rice_Kastner_Percentage <- rice_Kastner

for (year in names(rice_Kastner_Percentage)) {
  
  M <- as.matrix(
    rice_Kastner_Percentage[[year]]
  )
  
  # Exclude within-country attribution.
  diag(M) <- 0
  
  # Replace non-finite and negative values with zero.
  M[!is.finite(M)] <- 0
  M[M < 0] <- 0
  
  # Calculate total internationally attributed quantity for each row.
  rs <- rowSums(M)
  
  # Initialize bilateral-proportion matrix.
  m_percent <- matrix(
    0,
    nrow = nrow(M),
    ncol = ncol(M),
    dimnames = dimnames(M)
  )
  
  # Normalize rows with positive totals.
  nz <- rs > 0
  
  m_percent[nz, ] <-
    M[nz, ] / rs[nz]
  
  # Remove numerically negligible bilateral shares.
  m_percent[m_percent < 1e-4] <- 0
  
  # Set allocation shares to zero for countries without a rice production
  # record in the corresponding year.
  yr_val <- suppressWarnings(
    as.integer(year)
  )
  
  if (!is.na(yr_val) &&
      is.numeric(rice_Production$Year)) {
    
    prod_y <- rice_Production %>%
      filter(
        Year == yr_val
      )
    
  } else {
    
    prod_y <- rice_Production %>%
      filter(
        Year == year
      )
  }
  
  prod_y <- prod_y %>%
    mutate(
      FAOStat_area_code_chr =
        as.character(FAOStat_area_code)
    )
  
  exporter_codes <- rownames(m_percent)
  
  has_prod <-
    exporter_codes %in%
    prod_y$FAOStat_area_code_chr
  
  m_percent[!has_prod, ] <- 0
  
  rice_Kastner_Percentage[[year]] <-
    m_percent
}


## 7: Convert annual matrices to long-format data ------------------------------

rice_Kastner_Percentage_df <- map_dfr(
  names(rice_Kastner_Percentage),
  function(year) {
    
    rice_Kastner_Percentage[[year]] %>%
      as.data.frame() %>%
      rownames_to_column(
        var = "FAOStat_area_code_Exp"
      ) %>%
      pivot_longer(
        cols = -FAOStat_area_code_Exp,
        names_to = "FAOStat_area_code_Imp",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Year = year
      )
  }
)


## 8: Reconstruct bilateral proportions before 1990 ----------------------------

# Bilateral trade-matrix information is unavailable for the earlier part of the
# study period. The mean bilateral allocation structure from 1988-2019 is used
# to represent the period 1961-1989.

rice_Kastner_Percentage_df_90_20 <-
  rice_Kastner_Percentage_df %>%
  filter(
    Year > 1987,
    Year < 2020
  ) %>%
  mutate(
    Value = ifelse(
      Value == Inf,
      0,
      Value
    )
  )


# Calculate mean bilateral allocation shares over the reference period.
rice_Kastner_Percentage_df_90_20_avg <-
  rice_Kastner_Percentage_df_90_20 %>%
  group_by(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the total mean allocation share for each exporter.
rice_Kastner_Percentage_df_90_20_avg_tot <-
  rice_Kastner_Percentage_df_90_20_avg %>%
  group_by(
    FAOStat_area_code_Exp
  ) %>%
  summarise(
    Value_t = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Re-normalize the average bilateral allocation structure.
rice_Kastner_Percentage_df_90_20_avg <-
  rice_Kastner_Percentage_df_90_20_avg %>%
  left_join(
    rice_Kastner_Percentage_df_90_20_avg_tot,
    by = "FAOStat_area_code_Exp"
  ) %>%
  mutate(
    Value = Value / Value_t
  )


# Apply the average allocation structure to every year from 1961 to 1989.
rice_Kastner_Percentage_df_61_89 <-
  expand.grid(
    FAOStat_area_code_Imp =
      unique(
        rice_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Imp
      ),
    FAOStat_area_code_Exp =
      unique(
        rice_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Exp
      ),
    Year = seq(1961, 1989)
  ) %>%
  left_join(
    rice_Kastner_Percentage_df_90_20_avg %>%
      select(
        FAOStat_area_code_Exp,
        FAOStat_area_code_Imp,
        Value
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "FAOStat_area_code_Imp"
    )
  )


# Retain annual estimates from 1990 onward.
rice_Kastner_Percentage_df_90_20 <-
  rice_Kastner_Percentage_df_90_20 %>%
  filter(
    Year > 1989
  )


# Combine reconstructed 1961-1989 proportions with later annual estimates.
rice_Kastner_Percentage_df <- rbind(
  rice_Kastner_Percentage_df_61_89,
  rice_Kastner_Percentage_df_90_20
)


## 9: Extract within-country attribution shares --------------------------------

# Normalize each full attribution matrix by its row total and extract the
# diagonal element for each country.
#
# The diagonal represents the within-country share of the normalized
# attribution matrix. These values are used only as normalization terms in
# subsequent calculations.
#
# They should not be interpreted as the exported fraction. The exported
# fraction used in the analysis is estimated independently from FAOSTAT Food
# Balance data in script 01.

rice_Kastner_Exp_Percentage <-
  rice_Kastner

for (year in names(rice_Kastner_Exp_Percentage)) {
  
  row_sums <- rowSums(
    rice_Kastner_Exp_Percentage[[year]],
    na.rm = TRUE
  )
  
  row_sums[row_sums < 0.000001] <- 0
  
  row_sums_matrix <- matrix(
    row_sums,
    nrow = nrow(
      rice_Kastner_Exp_Percentage[[year]]
    ),
    ncol = ncol(
      rice_Kastner_Exp_Percentage[[year]]
    ),
    byrow = FALSE
  )
  
  # Normalize each row.
  m_percent <-
    rice_Kastner_Exp_Percentage[[year]] /
    row_sums_matrix
  
  # Remove numerically negligible shares.
  m_percent[m_percent < 0.0001] <- 0
  
  # Retain diagonal attribution share.
  rice_Kastner_Exp_Percentage[[year]] <-
    diag(
      as.matrix(m_percent)
    )
  
  # Assign zero to countries without a rice production record.
  for (
    country in
    rownames(
      rice_Kastner_Exp_Percentage[[year]]
    )
  ) {
    
    prod_val <- rice_Production %>%
      filter(
        Year == year,
        FAOStat_area_code == country
      )
    
    if (nrow(prod_val) == 0) {
      rice_Kastner_Exp_Percentage[[year]][country, ] <- 0
    }
  }
}


## 10: Convert normalization shares to a data frame ----------------------------

rice_Kastner_Exp_Percentage_df <-
  data.frame()

for (year in names(rice_Kastner_Exp_Percentage)) {
  
  m_percent <-
    rice_Kastner_Exp_Percentage[[year]]
  
  year_df <-
    as.data.frame(
      as.matrix(m_percent)
    ) %>%
    mutate(
      FAOStat_area_code =
        rownames(.)
    ) %>%
    rename(
      Perc_Exp_Kastner = 1,
      FAOStat_area_code = 2
    )
  
  year_df$Year <- year
  
  rice_Kastner_Exp_Percentage_df <-
    rbind(
      rice_Kastner_Exp_Percentage_df,
      year_df
    )
}


## 11: Export results -----------------------------------------------------------

write.csv(
  rice_Kastner_Percentage_df,
  "results/rice_Kastner_Percentage_df.csv",
  row.names = FALSE
)

write.csv(
  rice_Kastner_Exp_Percentage_df,
  "results/rice_Kastner_Exp_Percentage.csv",
  row.names = FALSE
)

# Soybean ----------------------------------------------------------------------

## 1: Prepare bilateral soybean trade data -------------------------------------

# Select soybean and soybean-derived products from the FAOSTAT Detailed Trade
# Matrix. Import records (Element Code 5610) are used because these contain the
# reporting importer and corresponding exporting trade partner.
#
# Trade quantities are converted from tonnes to kt and subsequently converted
# to dry matter using product-specific conversion factors. All soybean products
# are then aggregated to one exporter-importer-year flow. The commodities and
# their dry matter contents are reported in the supplementary material.

soybean_TM <- ITM %>%
  rename(
    Item_Code = 7,
    Element_Code = 10
  ) %>%
  filter(
    Item_Code %in% c(236, 239, 240, 241) &
      Element_Code == 5610
  ) %>%
  rename(
    FAOStat_area_code_Imp = 1,
    FAOStat_area_code_Exp = 4,
    Year = 13,
    Value = 15
  ) %>%
  select(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Item_Code,
    Year,
    Value
  ) %>%
  filter(
    FAOStat_area_code_Imp < 351
  ) %>%
  mutate(
    # Convert tonnes to kt.
    Value = Value / 1000,
    
    # Convert individual soybean products to dry matter.
    Value_DM = case_when(
      Item_Code == 236 ~ Value * 0.915,  # Soya beans
      Item_Code == 239 ~ Value * 0.290,  # Soya sauce
      Item_Code == 240 ~ Value * 0.470,  # Soya paste
      Item_Code == 241 ~ Value * 0.231   # Soya curd
    )
  ) %>%
  group_by(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_DM = sum(Value_DM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Value = Value_DM
  ) %>%
  mutate(
    # Identifier for each exporter-importer pair.
    com = paste(
      FAOStat_area_code_Exp,
      "_",
      FAOStat_area_code_Imp
    )
  )


## 2: Calculate 3-year moving averages of bilateral trade ----------------------

# A complete annual series from 1986 to 2022 is created for each bilateral
# exporter-importer relationship. Missing annual trade flows are interpreted as
# zero before calculating the 3-year simple moving average.

soybean_TM_ma <- as.data.frame(
  matrix(ncol = 7)
)

colnames(soybean_TM_ma) <- c(
  "FAOStat_area_code_Exp",
  "FAOStat_area_code_Imp",
  "com",
  "Year",
  "Value",
  "Value_0",
  "Value_MA"
)

for (i in unique(soybean_TM$com)) {
  
  df <- soybean_TM %>%
    filter(com == i) %>%
    mutate(
      Value = abs(Value)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code_Imp = unique(df$FAOStat_area_code_Imp),
    FAOStat_area_code_Exp = unique(df$FAOStat_area_code_Exp),
    Year = seq(1986, 2022),
    com = i
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code_Imp",
      "FAOStat_area_code_Exp",
      "Year",
      "com"
    )
  ) %>%
    mutate(
      Value_0 = ifelse(
        is.na(Value),
        0,
        Value
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Value_0,
    n = 3
  )
  
  df$Value_MA <- moving_average
  
  soybean_TM_ma <- rbind(
    df,
    soybean_TM_ma
  )
}


soybean_TM_ma_fil <- soybean_TM_ma %>%
  drop_na(Value_MA) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Value_MA
  ) %>%
  rename(
    Value = Value_MA
  )


## 3: Prepare soybean production data ------------------------------------------

# Soybean production is extracted independently from the historical and current
# FAOSTAT Food Balance Sheets.
#
# FAOSTAT codes:
#   Item 2555    = Soyabeans
#   Element 5511 = Production

FB_old_prod_soybean_period <- FB_old %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


FB_act_prod_soybean_period <- FB_act %>%
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
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


# Combine historical and current Food Balance production estimates.
# Where both datasets contain an estimate for the same country-year, their mean
# is used.

soybean_Production <- rbind(
  FB_old_prod_soybean_period,
  FB_act_prod_soybean_period
) %>%
  group_by(
    FAOStat_area_code,
    Year
  ) %>%
  summarise(
    Production = mean(
      Production,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 4: Calculate 3-year moving averages of soybean production -------------------

soybean_Production_ma <- as.data.frame(
  matrix(ncol = 5)
)

colnames(soybean_Production_ma) <- c(
  "FAOStat_area_code",
  "Year",
  "Production",
  "Production_0",
  "Production_MA"
)

for (i in unique(soybean_Production$FAOStat_area_code)) {
  
  df <- soybean_Production %>%
    filter(
      FAOStat_area_code == i
    ) %>%
    mutate(
      Production = abs(Production)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code = unique(df$FAOStat_area_code),
    Year = seq(1961, 2022)
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code",
      "Year"
    )
  ) %>%
    mutate(
      Production_0 = ifelse(
        is.na(Production),
        0,
        Production
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Production_0,
    n = 3
  )
  
  df$Production_MA <- moving_average
  
  soybean_Production_ma <- rbind(
    df,
    soybean_Production_ma
  )
}


# Retain years with a valid moving average and convert soybean production to
# dry matter using a factor of 0.915.

soybean_Production_ma_fil <- soybean_Production_ma %>%
  drop_na(Production_MA) %>%
  select(
    FAOStat_area_code,
    Year,
    Production_MA
  ) %>%
  rename(
    Production = Production_MA
  ) %>%
  mutate(
    Production = 0.915 * Production
  )


## 5: Apply the trade-matrix allocation method ---------------------------------

soybean_Kastner <- Period_TM_Estimation(
  production = soybean_Production_ma_fil,
  TM = soybean_TM_ma_fil
)


## 6: Convert attribution values to bilateral allocation proportions -----------

# Remove within-country attribution and normalize each row so that the remaining
# international attribution values represent bilateral allocation proportions.

soybean_Kastner_Percentage <- soybean_Kastner

for (year in names(soybean_Kastner_Percentage)) {
  
  M <- as.matrix(
    soybean_Kastner_Percentage[[year]]
  )
  
  # Exclude within-country attribution.
  diag(M) <- 0
  
  # Replace non-finite and negative values with zero.
  M[!is.finite(M)] <- 0
  M[M < 0] <- 0
  
  # Calculate total internationally attributed quantity for each row.
  rs <- rowSums(M)
  
  # Initialize bilateral-proportion matrix.
  m_percent <- matrix(
    0,
    nrow = nrow(M),
    ncol = ncol(M),
    dimnames = dimnames(M)
  )
  
  # Normalize rows with positive totals.
  nz <- rs > 0
  
  m_percent[nz, ] <-
    M[nz, ] / rs[nz]
  
  # Remove numerically negligible bilateral shares.
  m_percent[m_percent < 1e-4] <- 0
  
  # Set allocation shares to zero for countries without a soybean production
  # record in the corresponding year.
  yr_val <- suppressWarnings(
    as.integer(year)
  )
  
  if (!is.na(yr_val) &&
      is.numeric(soybean_Production$Year)) {
    
    prod_y <- soybean_Production %>%
      filter(
        Year == yr_val
      )
    
  } else {
    
    prod_y <- soybean_Production %>%
      filter(
        Year == year
      )
  }
  
  prod_y <- prod_y %>%
    mutate(
      FAOStat_area_code_chr =
        as.character(FAOStat_area_code)
    )
  
  exporter_codes <- rownames(m_percent)
  
  has_prod <-
    exporter_codes %in%
    prod_y$FAOStat_area_code_chr
  
  m_percent[!has_prod, ] <- 0
  
  soybean_Kastner_Percentage[[year]] <-
    m_percent
}


## 7: Convert annual matrices to long-format data ------------------------------

soybean_Kastner_Percentage_df <- map_dfr(
  names(soybean_Kastner_Percentage),
  function(year) {
    
    soybean_Kastner_Percentage[[year]] %>%
      as.data.frame() %>%
      rownames_to_column(
        var = "FAOStat_area_code_Exp"
      ) %>%
      pivot_longer(
        cols = -FAOStat_area_code_Exp,
        names_to = "FAOStat_area_code_Imp",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Year = year
      )
  }
)


## 8: Reconstruct bilateral proportions before 1990 ----------------------------

# Bilateral trade-matrix information is unavailable for the earlier part of the
# study period. The mean bilateral allocation structure from 1988-2019 is used
# to represent the period 1961-1989.

soybean_Kastner_Percentage_df_90_20 <-
  soybean_Kastner_Percentage_df %>%
  filter(
    Year > 1987,
    Year < 2020
  ) %>%
  mutate(
    Value = ifelse(
      Value == Inf,
      0,
      Value
    )
  )


# Calculate mean bilateral allocation shares over the reference period.
soybean_Kastner_Percentage_df_90_20_avg <-
  soybean_Kastner_Percentage_df_90_20 %>%
  group_by(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the total mean allocation share for each exporter.
soybean_Kastner_Percentage_df_90_20_avg_tot <-
  soybean_Kastner_Percentage_df_90_20_avg %>%
  group_by(
    FAOStat_area_code_Exp
  ) %>%
  summarise(
    Value_t = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Re-normalize the average bilateral allocation structure.
soybean_Kastner_Percentage_df_90_20_avg <-
  soybean_Kastner_Percentage_df_90_20_avg %>%
  left_join(
    soybean_Kastner_Percentage_df_90_20_avg_tot,
    by = "FAOStat_area_code_Exp"
  ) %>%
  mutate(
    Value = Value / Value_t
  )


# Apply the average allocation structure to every year from 1961 to 1989.
soybean_Kastner_Percentage_df_61_89 <-
  expand.grid(
    FAOStat_area_code_Imp =
      unique(
        soybean_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Imp
      ),
    FAOStat_area_code_Exp =
      unique(
        soybean_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Exp
      ),
    Year = seq(1961, 1989)
  ) %>%
  left_join(
    soybean_Kastner_Percentage_df_90_20_avg %>%
      select(
        FAOStat_area_code_Exp,
        FAOStat_area_code_Imp,
        Value
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "FAOStat_area_code_Imp"
    )
  )


# Retain annual estimates from 1990 onward.
soybean_Kastner_Percentage_df_90_20 <-
  soybean_Kastner_Percentage_df_90_20 %>%
  filter(
    Year > 1989
  )


# Combine reconstructed 1961-1989 proportions with later annual estimates.
soybean_Kastner_Percentage_df <- rbind(
  soybean_Kastner_Percentage_df_61_89,
  soybean_Kastner_Percentage_df_90_20
)


## 9: Extract within-country attribution shares --------------------------------

# Normalize each full attribution matrix by its row total and extract the
# diagonal element for each country.
#
# The diagonal represents the within-country share of the normalized
# attribution matrix. These values are used only as normalization terms in
# subsequent calculations.
#
# They should not be interpreted as the exported fraction. The exported
# fraction used in the analysis is estimated independently from FAOSTAT Food
# Balance data in script 01.

soybean_Kastner_Exp_Percentage <-
  soybean_Kastner

for (year in names(soybean_Kastner_Exp_Percentage)) {
  
  row_sums <- rowSums(
    soybean_Kastner_Exp_Percentage[[year]],
    na.rm = TRUE
  )
  
  row_sums[row_sums < 0.000001] <- 0
  
  row_sums_matrix <- matrix(
    row_sums,
    nrow = nrow(
      soybean_Kastner_Exp_Percentage[[year]]
    ),
    ncol = ncol(
      soybean_Kastner_Exp_Percentage[[year]]
    ),
    byrow = FALSE
  )
  
  # Normalize each row.
  m_percent <-
    soybean_Kastner_Exp_Percentage[[year]] /
    row_sums_matrix
  
  # Remove numerically negligible shares.
  m_percent[m_percent < 0.0001] <- 0
  
  # Retain diagonal attribution share.
  soybean_Kastner_Exp_Percentage[[year]] <-
    diag(
      as.matrix(m_percent)
    )
  
  # Assign zero to countries without a soybean production record.
  for (
    country in
    rownames(
      soybean_Kastner_Exp_Percentage[[year]]
    )
  ) {
    
    prod_val <- soybean_Production %>%
      filter(
        Year == year,
        FAOStat_area_code == country
      )
    
    if (nrow(prod_val) == 0) {
      soybean_Kastner_Exp_Percentage[[year]][country, ] <- 0
    }
  }
}


## 10: Convert normalization shares to a data frame ----------------------------

soybean_Kastner_Exp_Percentage_df <-
  data.frame()

for (year in names(soybean_Kastner_Exp_Percentage)) {
  
  m_percent <-
    soybean_Kastner_Exp_Percentage[[year]]
  
  year_df <-
    as.data.frame(
      as.matrix(m_percent)
    ) %>%
    mutate(
      FAOStat_area_code =
        rownames(.)
    ) %>%
    rename(
      Perc_Exp_Kastner = 1,
      FAOStat_area_code = 2
    )
  
  year_df$Year <- year
  
  soybean_Kastner_Exp_Percentage_df <-
    rbind(
      soybean_Kastner_Exp_Percentage_df,
      year_df
    )
}


## 11: Export results -----------------------------------------------------------

write.csv(
  soybean_Kastner_Percentage_df,
  "results/soybean_Kastner_Percentage_df.csv",
  row.names = FALSE
)

write.csv(
  soybean_Kastner_Exp_Percentage_df,
  "results/soybean_Kastner_Exp_Percentage.csv",
  row.names = FALSE
)

# Soybean cake -----------------------------------------------------------------

## 1: Prepare bilateral soybean-cake trade data --------------------------------

# Select soybean cake from the FAOSTAT Detailed Trade Matrix.
# Import records (Element Code 5610) are used because these contain the
# reporting importer and corresponding exporting trade partner.
#
# Trade quantities are converted from tonnes to kt. No additional dry-matter
# conversion is applied to soybean cake because no it is the only item used in
# this analysis

soybean_cake_TM <- ITM %>%
  rename(
    Item_Code = 7,
    Element_Code = 10
  ) %>%
  filter(
    Item_Code == 238,
    Unit == "t",
    Element_Code == 5610
  ) %>%
  rename(
    FAOStat_area_code_Imp = 1,
    FAOStat_area_code_Exp = 4,
    Year = 13,
    Value = 15
  ) %>%
  select(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Item_Code,
    Year,
    Value
  ) %>%
  filter(
    FAOStat_area_code_Imp < 351
  ) %>%
  mutate(
    # Convert tonnes to kt.
    Value = Value / 1000,
    
    # No additional dry-matter conversion applied.
    Value_DM = Value
  ) %>%
  group_by(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_DM = sum(Value_DM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Value = Value_DM
  ) %>%
  mutate(
    # Identifier for each exporter-importer pair.
    com = paste(
      FAOStat_area_code_Exp,
      "_",
      FAOStat_area_code_Imp
    )
  )


## 2: Calculate 3-year moving averages of bilateral trade ----------------------

# A complete annual series from 1986 to 2022 is created for each bilateral
# exporter-importer relationship. Missing annual trade flows are interpreted as
# zero before calculating the 3-year simple moving average.

soybean_crushed_TM_ma <- as.data.frame(
  matrix(ncol = 7)
)

colnames(soybean_crushed_TM_ma) <- c(
  "FAOStat_area_code_Exp",
  "FAOStat_area_code_Imp",
  "com",
  "Year",
  "Value",
  "Value_0",
  "Value_MA"
)

for (i in unique(soybean_cake_TM$com)) {
  
  df <- soybean_cake_TM %>%
    filter(
      com == i
    ) %>%
    mutate(
      Value = abs(Value)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code_Imp = unique(df$FAOStat_area_code_Imp),
    FAOStat_area_code_Exp = unique(df$FAOStat_area_code_Exp),
    Year = seq(1986, 2022),
    com = i
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code_Imp",
      "FAOStat_area_code_Exp",
      "Year",
      "com"
    )
  ) %>%
    mutate(
      Value_0 = ifelse(
        is.na(Value),
        0,
        Value
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Value_0,
    n = 3
  )
  
  df$Value_MA <- moving_average
  
  soybean_crushed_TM_ma <- rbind(
    df,
    soybean_crushed_TM_ma
  )
}


soybean_crushed_TM_ma_fil <- soybean_crushed_TM_ma %>%
  drop_na(Value_MA) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Value_MA
  ) %>%
  rename(
    Value = Value_MA
  )


## 3: Prepare soybean-cake production data -------------------------------------

# Soybean-cake production is obtained from the historical and current FAOSTAT
# Commodity Balance datasets.
#
# Element Code 5510 = Production.
#
# These datasets use M49 country codes, which are converted to FAOSTAT area
# codes using Countries_DB.

FB_old_prod_soybean_crushed_period <- FBold_soycake %>%
  select(3, 5, 8, 10, 12) %>%
  rename(
    M49_code = 1,
    Element_code = 2,
    Item = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Element_code == 5510
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1c",
    M49_code = as.numeric(M49_code)
  ) %>%
  select(
    M49_code,
    Year,
    Production
  ) %>%
  left_join(
    Countries_DB %>%
      select(3, 5) %>%
      rename(
        FAOStat_area_code = 1,
        M49_code = 2
      ),
    by = "M49_code"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


FB_act_prod_soybean_crushed_period <- FBact_soycake %>%
  select(3, 5, 8, 10, 12) %>%
  rename(
    M49_code = 1,
    Element_code = 2,
    Item = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Element_code == 5510,
    M49_code != 159
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1c",
    M49_code = as.numeric(M49_code)
  ) %>%
  select(
    M49_code,
    Year,
    Production
  ) %>%
  left_join(
    Countries_DB %>%
      select(3, 5) %>%
      rename(
        FAOStat_area_code = 1,
        M49_code = 2
      ),
    by = "M49_code"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


# Combine historical and current Commodity Balance production estimates.
# Where both datasets contain an estimate for the same country-year, their mean
# is used.

soybean_crushed_Production <- rbind(
  FB_old_prod_soybean_crushed_period,
  FB_act_prod_soybean_crushed_period
) %>%
  group_by(
    FAOStat_area_code,
    Year
  ) %>%
  summarise(
    Production = mean(
      Production,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 4: Calculate 3-year moving averages of soybean-cake production --------------

soybean_crushed_Production_ma <- as.data.frame(
  matrix(ncol = 5)
)

colnames(soybean_crushed_Production_ma) <- c(
  "FAOStat_area_code",
  "Year",
  "Production",
  "Production_0",
  "Production_MA"
)

for (i in unique(soybean_crushed_Production$FAOStat_area_code)) {
  
  df <- soybean_crushed_Production %>%
    filter(
      FAOStat_area_code == i
    ) %>%
    mutate(
      Production = abs(Production)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code = unique(df$FAOStat_area_code),
    Year = seq(1961, 2022)
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code",
      "Year"
    )
  ) %>%
    mutate(
      Production_0 = ifelse(
        is.na(Production),
        0,
        Production
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Production_0,
    n = 3
  )
  
  df$Production_MA <- moving_average
  
  soybean_crushed_Production_ma <- rbind(
    df,
    soybean_crushed_Production_ma
  )
}


# Retain years with a valid moving average. No additional dry-matter conversion
# is applied to soybean-cake production.

soybean_crushed_Production_ma_fil <- soybean_crushed_Production_ma %>%
  drop_na(Production_MA) %>%
  select(
    FAOStat_area_code,
    Year,
    Production_MA
  ) %>%
  rename(
    Production = Production_MA
  ) %>%
  mutate(
    Production = ifelse(
      Production < 0,
      0,
      Production
    )
  )


## 5: Apply the trade-matrix allocation method ---------------------------------

soybean_crushed_Kastner <- Period_TM_Estimation(
  production = soybean_crushed_Production_ma_fil,
  TM = soybean_crushed_TM_ma_fil
)


## 6: Convert attribution values to bilateral allocation proportions -----------

# Remove within-country attribution and normalize each row so that the remaining
# international attribution values represent bilateral allocation proportions.

soybean_crushed_Kastner_Percentage <-
  soybean_crushed_Kastner

for (year in names(soybean_crushed_Kastner_Percentage)) {
  
  M <- as.matrix(
    soybean_crushed_Kastner_Percentage[[year]]
  )
  
  # Exclude within-country attribution.
  diag(M) <- 0
  
  # Replace non-finite and negative values with zero.
  M[!is.finite(M)] <- 0
  M[M < 0] <- 0
  
  # Calculate total internationally attributed quantity for each row.
  rs <- rowSums(M)
  
  # Initialize bilateral-proportion matrix.
  m_percent <- matrix(
    0,
    nrow = nrow(M),
    ncol = ncol(M),
    dimnames = dimnames(M)
  )
  
  # Normalize rows with positive totals.
  nz <- rs > 0
  
  m_percent[nz, ] <-
    M[nz, ] / rs[nz]
  
  # Remove numerically negligible bilateral shares.
  m_percent[m_percent < 1e-4] <- 0
  
  # Set allocation shares to zero for countries without a soybean-cake
  # production record in the corresponding year.
  yr_val <- suppressWarnings(
    as.integer(year)
  )
  
  if (!is.na(yr_val) &&
      is.numeric(soybean_crushed_Production$Year)) {
    
    prod_y <- soybean_crushed_Production %>%
      filter(
        Year == yr_val
      )
    
  } else {
    
    prod_y <- soybean_crushed_Production %>%
      filter(
        Year == year
      )
  }
  
  prod_y <- prod_y %>%
    mutate(
      FAOStat_area_code_chr =
        as.character(FAOStat_area_code)
    )
  
  exporter_codes <- rownames(m_percent)
  
  has_prod <-
    exporter_codes %in%
    prod_y$FAOStat_area_code_chr
  
  m_percent[!has_prod, ] <- 0
  
  soybean_crushed_Kastner_Percentage[[year]] <-
    m_percent
}


## 7: Convert annual matrices to long-format data ------------------------------

soybean_crushed_Kastner_Percentage_df <- map_dfr(
  names(soybean_crushed_Kastner_Percentage),
  function(year) {
    
    soybean_crushed_Kastner_Percentage[[year]] %>%
      as.data.frame() %>%
      rownames_to_column(
        var = "FAOStat_area_code_Exp"
      ) %>%
      pivot_longer(
        cols = -FAOStat_area_code_Exp,
        names_to = "FAOStat_area_code_Imp",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Year = year
      )
  }
)


## 8: Reconstruct bilateral proportions before 1990 ----------------------------

# Bilateral trade-matrix information is unavailable for the earlier part of the
# study period. The mean bilateral allocation structure from 1988-2019 is used
# to represent the period 1961-1989.

soybean_crushed_Kastner_Percentage_df_90_20 <-
  soybean_crushed_Kastner_Percentage_df %>%
  filter(
    Year > 1987,
    Year < 2020
  ) %>%
  mutate(
    Value = ifelse(
      Value == Inf,
      0,
      Value
    )
  )


# Calculate mean bilateral allocation shares over the reference period.
soybean_crushed_Kastner_Percentage_df_90_20_avg <-
  soybean_crushed_Kastner_Percentage_df_90_20 %>%
  group_by(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the total mean allocation share for each exporter.
soybean_crushed_Kastner_Percentage_df_90_20_avg_tot <-
  soybean_crushed_Kastner_Percentage_df_90_20_avg %>%
  group_by(
    FAOStat_area_code_Exp
  ) %>%
  summarise(
    Value_t = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Re-normalize the average bilateral allocation structure.
soybean_crushed_Kastner_Percentage_df_90_20_avg <-
  soybean_crushed_Kastner_Percentage_df_90_20_avg %>%
  left_join(
    soybean_crushed_Kastner_Percentage_df_90_20_avg_tot,
    by = "FAOStat_area_code_Exp"
  ) %>%
  mutate(
    Value = Value / Value_t
  )


# Apply the average allocation structure to every year from 1961 to 1989.
soybean_crushed_Kastner_Percentage_df_61_89 <-
  expand.grid(
    FAOStat_area_code_Imp =
      unique(
        soybean_crushed_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Imp
      ),
    FAOStat_area_code_Exp =
      unique(
        soybean_crushed_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Exp
      ),
    Year = seq(1961, 1989)
  ) %>%
  left_join(
    soybean_crushed_Kastner_Percentage_df_90_20_avg %>%
      select(
        FAOStat_area_code_Exp,
        FAOStat_area_code_Imp,
        Value
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "FAOStat_area_code_Imp"
    )
  )


# Retain annual estimates from 1990 onward.
soybean_crushed_Kastner_Percentage_df_90_20 <-
  soybean_crushed_Kastner_Percentage_df_90_20 %>%
  filter(
    Year > 1989
  )


# Combine reconstructed 1961-1989 proportions with later annual estimates.
soybean_crushed_Kastner_Percentage_df <- rbind(
  soybean_crushed_Kastner_Percentage_df_61_89,
  soybean_crushed_Kastner_Percentage_df_90_20
)


## 9: Extract within-country attribution shares --------------------------------

# Normalize each full attribution matrix by its row total and extract the
# diagonal element for each country.
#
# The diagonal represents the within-country share of the normalized
# attribution matrix. These values are used only as normalization terms in
# subsequent calculations.
#
# They should not be interpreted as the exported fraction. The exported
# fraction used in the analysis is estimated independently from FAOSTAT
# balance data in script 01.

soybean_crushed_Kastner_Exp_Percentage <-
  soybean_crushed_Kastner

for (year in names(soybean_crushed_Kastner_Exp_Percentage)) {
  
  row_sums <- rowSums(
    soybean_crushed_Kastner_Exp_Percentage[[year]],
    na.rm = TRUE
  )
  
  row_sums[row_sums < 0.000001] <- 0
  
  row_sums_matrix <- matrix(
    row_sums,
    nrow = nrow(
      soybean_crushed_Kastner_Exp_Percentage[[year]]
    ),
    ncol = ncol(
      soybean_crushed_Kastner_Exp_Percentage[[year]]
    ),
    byrow = FALSE
  )
  
  # Normalize each row.
  m_percent <-
    soybean_crushed_Kastner_Exp_Percentage[[year]] /
    row_sums_matrix
  
  # Remove numerically negligible shares.
  m_percent[m_percent < 0.0001] <- 0
  
  # Retain diagonal attribution share.
  soybean_crushed_Kastner_Exp_Percentage[[year]] <-
    diag(
      as.matrix(m_percent)
    )
  
  # Assign zero to countries without a soybean-cake production record.
  for (
    country in
    rownames(
      soybean_crushed_Kastner_Exp_Percentage[[year]]
    )
  ) {
    
    prod_val <- soybean_crushed_Production %>%
      filter(
        Year == year,
        FAOStat_area_code == country
      )
    
    if (nrow(prod_val) == 0) {
      soybean_crushed_Kastner_Exp_Percentage[[year]][country, ] <- 0
    }
  }
}


## 10: Convert normalization shares to a data frame ----------------------------

soybean_crushed_Kastner_Exp_Percentage_df <-
  data.frame()

for (year in names(soybean_crushed_Kastner_Exp_Percentage)) {
  
  m_percent <-
    soybean_crushed_Kastner_Exp_Percentage[[year]]
  
  year_df <-
    as.data.frame(
      as.matrix(m_percent)
    ) %>%
    mutate(
      FAOStat_area_code =
        rownames(.)
    ) %>%
    rename(
      Perc_Exp_Kastner = 1,
      FAOStat_area_code = 2
    )
  
  year_df$Year <- year
  
  soybean_crushed_Kastner_Exp_Percentage_df <-
    rbind(
      soybean_crushed_Kastner_Exp_Percentage_df,
      year_df
    )
}


## 11: Export results -----------------------------------------------------------

write.csv(
  soybean_crushed_Kastner_Percentage_df,
  "results/soybean_cake_Kastner_Percentage_df.csv",
  row.names = FALSE
)

write.csv(
  soybean_crushed_Kastner_Exp_Percentage_df,
  "results/soybean_cake_Kastner_Exp_Percentage.csv",
  row.names = FALSE
)
# Soybean oil ------------------------------------------------------------------

## 1: Prepare bilateral soybean-oil trade data ---------------------------------

# Select soybean oil from the FAOSTAT Detailed Trade Matrix.
# Import records (Element Code 5610) are used because these contain the
# reporting importer and corresponding exporting trade partner.
#
# Trade quantities are converted from tonnes to kt. Because soybean oil is
# represented by a single traded commodity, no dry-matter standardization
# across different products is required.

soybean_oil_TM <- ITM %>%
  rename(
    Item_Code = 7,
    Element_Code = 10
  ) %>%
  filter(
    Item_Code == 237,
    Unit == "t",
    Element_Code == 5610
  ) %>%
  rename(
    FAOStat_area_code_Imp = 1,
    FAOStat_area_code_Exp = 4,
    Year = 13,
    Value = 15
  ) %>%
  select(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Item_Code,
    Year,
    Value
  ) %>%
  filter(
    FAOStat_area_code_Imp < 351
  ) %>%
  mutate(
    # Convert tonnes to kt.
    Value = Value / 1000,
    
    # No standardization among products is required because soybean oil
    # is represented by a single commodity.
    Value_DM = Value
  ) %>%
  group_by(
    FAOStat_area_code_Imp,
    FAOStat_area_code_Exp,
    Year
  ) %>%
  summarise(
    Value_DM = sum(Value_DM, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Value = Value_DM
  ) %>%
  mutate(
    # Identifier for each exporter-importer pair.
    com = paste(
      FAOStat_area_code_Exp,
      "_",
      FAOStat_area_code_Imp
    )
  )


## 2: Calculate 3-year moving averages of bilateral trade ----------------------

# A complete annual series from 1986 to 2022 is created for each bilateral
# exporter-importer relationship. Missing annual trade flows are interpreted as
# zero before calculating the 3-year simple moving average.

soybean_oil_TM_ma <- as.data.frame(
  matrix(ncol = 7)
)

colnames(soybean_oil_TM_ma) <- c(
  "FAOStat_area_code_Exp",
  "FAOStat_area_code_Imp",
  "com",
  "Year",
  "Value",
  "Value_0",
  "Value_MA"
)

for (i in unique(soybean_oil_TM$com)) {
  
  df <- soybean_oil_TM %>%
    filter(
      com == i
    ) %>%
    mutate(
      Value = abs(Value)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code_Imp = unique(df$FAOStat_area_code_Imp),
    FAOStat_area_code_Exp = unique(df$FAOStat_area_code_Exp),
    Year = seq(1986, 2022),
    com = i
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code_Imp",
      "FAOStat_area_code_Exp",
      "Year",
      "com"
    )
  ) %>%
    mutate(
      Value_0 = ifelse(
        is.na(Value),
        0,
        Value
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Value_0,
    n = 3
  )
  
  df$Value_MA <- moving_average
  
  soybean_oil_TM_ma <- rbind(
    df,
    soybean_oil_TM_ma
  )
}


soybean_oil_TM_ma_fil <- soybean_oil_TM_ma %>%
  drop_na(Value_MA) %>%
  select(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp,
    Year,
    Value_MA
  ) %>%
  rename(
    Value = Value_MA
  )


## 3: Prepare soybean-oil production data --------------------------------------

# Soybean-oil production is extracted independently from the historical and
# current FAOSTAT Food Balance Sheets.
#
# FAOSTAT codes:
#   Item 2571    = Soyabean oil
#   Element 5511 = Production

FB_old_prod_soybean_oil_period <- FB_old %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2571,
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1o"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


FB_act_prod_soybean_oil_period <- FB_act %>%
  select(1, 4, 7, 10, 12) %>%
  rename(
    FAOStat_area_code = 1,
    Item_Code = 2,
    Element_code = 3,
    Year = 4,
    Value = 5
  ) %>%
  filter(
    Item_Code == 2571,
    Element_code == 5511
  ) %>%
  pivot_wider(
    names_from = Element_code,
    values_from = Value
  ) %>%
  rename(
    Production = 4,
    Crop_Code = 2
  ) %>%
  mutate(
    Crop_Code = "2_1o"
  ) %>%
  select(
    FAOStat_area_code,
    Year,
    Production
  )


# Combine historical and current Food Balance production estimates.
# Where both datasets contain an estimate for the same country-year, their mean
# is used.

soybean_oil_Production <- rbind(
  FB_old_prod_soybean_oil_period,
  FB_act_prod_soybean_oil_period
) %>%
  group_by(
    FAOStat_area_code,
    Year
  ) %>%
  summarise(
    Production = mean(
      Production,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 4: Calculate 3-year moving averages of soybean-oil production ---------------

soybean_oil_Production_ma <- as.data.frame(
  matrix(ncol = 5)
)

colnames(soybean_oil_Production_ma) <- c(
  "FAOStat_area_code",
  "Year",
  "Production",
  "Production_0",
  "Production_MA"
)

for (i in unique(soybean_oil_Production$FAOStat_area_code)) {
  
  df <- soybean_oil_Production %>%
    filter(
      FAOStat_area_code == i
    ) %>%
    mutate(
      Production = abs(Production)
    )
  
  all_df <- expand.grid(
    FAOStat_area_code = unique(df$FAOStat_area_code),
    Year = seq(1961, 2022)
  )
  
  df <- full_join(
    df,
    all_df,
    by = c(
      "FAOStat_area_code",
      "Year"
    )
  ) %>%
    mutate(
      Production_0 = ifelse(
        is.na(Production),
        0,
        Production
      )
    ) %>%
    arrange(Year)
  
  moving_average <- SMA(
    df$Production_0,
    n = 3
  )
  
  df$Production_MA <- moving_average
  
  soybean_oil_Production_ma <- rbind(
    df,
    soybean_oil_Production_ma
  )
}


# Retain years with a valid moving average. No standardization among different
# products is required because soybean oil is represented by a single commodity.

soybean_oil_Production_ma_fil <- soybean_oil_Production_ma %>%
  drop_na(Production_MA) %>%
  select(
    FAOStat_area_code,
    Year,
    Production_MA
  ) %>%
  rename(
    Production = Production_MA
  )


## 5: Apply the trade-matrix allocation method ---------------------------------

soybean_oil_Kastner <- Period_TM_Estimation(
  production = soybean_oil_Production_ma_fil,
  TM = soybean_oil_TM_ma_fil
)


## 6: Convert attribution values to bilateral allocation proportions -----------

# Remove within-country attribution and normalize each row so that the remaining
# international attribution values represent bilateral allocation proportions.

soybean_oil_Kastner_Percentage <-
  soybean_oil_Kastner

for (year in names(soybean_oil_Kastner_Percentage)) {
  
  M <- as.matrix(
    soybean_oil_Kastner_Percentage[[year]]
  )
  
  # Exclude within-country attribution.
  diag(M) <- 0
  
  # Replace non-finite and negative values with zero.
  M[!is.finite(M)] <- 0
  M[M < 0] <- 0
  
  # Calculate total internationally attributed quantity for each row.
  rs <- rowSums(M)
  
  # Initialize bilateral-proportion matrix.
  m_percent <- matrix(
    0,
    nrow = nrow(M),
    ncol = ncol(M),
    dimnames = dimnames(M)
  )
  
  # Normalize rows with positive totals.
  nz <- rs > 0
  
  m_percent[nz, ] <-
    M[nz, ] / rs[nz]
  
  # Remove numerically negligible bilateral shares.
  m_percent[m_percent < 1e-4] <- 0
  
  # Set allocation shares to zero for countries without a soybean-oil
  # production record in the corresponding year.
  yr_val <- suppressWarnings(
    as.integer(year)
  )
  
  if (!is.na(yr_val) &&
      is.numeric(soybean_oil_Production$Year)) {
    
    prod_y <- soybean_oil_Production %>%
      filter(
        Year == yr_val
      )
    
  } else {
    
    prod_y <- soybean_oil_Production %>%
      filter(
        Year == year
      )
  }
  
  prod_y <- prod_y %>%
    mutate(
      FAOStat_area_code_chr =
        as.character(FAOStat_area_code)
    )
  
  exporter_codes <- rownames(m_percent)
  
  has_prod <-
    exporter_codes %in%
    prod_y$FAOStat_area_code_chr
  
  m_percent[!has_prod, ] <- 0
  
  soybean_oil_Kastner_Percentage[[year]] <-
    m_percent
}


## 7: Convert annual matrices to long-format data ------------------------------

soybean_oil_Kastner_Percentage_df <- map_dfr(
  names(soybean_oil_Kastner_Percentage),
  function(year) {
    
    soybean_oil_Kastner_Percentage[[year]] %>%
      as.data.frame() %>%
      rownames_to_column(
        var = "FAOStat_area_code_Exp"
      ) %>%
      pivot_longer(
        cols = -FAOStat_area_code_Exp,
        names_to = "FAOStat_area_code_Imp",
        values_to = "Value"
      ) %>%
      mutate(
        Value = as.numeric(Value),
        Year = year
      )
  }
)


## 8: Reconstruct bilateral proportions before 1990 ----------------------------

# Bilateral trade-matrix information is unavailable for the earlier part of the
# study period. The mean bilateral allocation structure from 1988-2019 is used
# to represent the period 1961-1989.

soybean_oil_Kastner_Percentage_df_90_20 <-
  soybean_oil_Kastner_Percentage_df %>%
  filter(
    Year > 1987,
    Year < 2020
  ) %>%
  mutate(
    Value = ifelse(
      Value == Inf,
      0,
      Value
    )
  )


# Calculate mean bilateral allocation shares over the reference period.
soybean_oil_Kastner_Percentage_df_90_20_avg <-
  soybean_oil_Kastner_Percentage_df_90_20 %>%
  group_by(
    FAOStat_area_code_Exp,
    FAOStat_area_code_Imp
  ) %>%
  summarise(
    Value = mean(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the total mean allocation share for each exporter.
soybean_oil_Kastner_Percentage_df_90_20_avg_tot <-
  soybean_oil_Kastner_Percentage_df_90_20_avg %>%
  group_by(
    FAOStat_area_code_Exp
  ) %>%
  summarise(
    Value_t = sum(
      Value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Re-normalize the average bilateral allocation structure.
soybean_oil_Kastner_Percentage_df_90_20_avg <-
  soybean_oil_Kastner_Percentage_df_90_20_avg %>%
  left_join(
    soybean_oil_Kastner_Percentage_df_90_20_avg_tot,
    by = "FAOStat_area_code_Exp"
  ) %>%
  mutate(
    Value = Value / Value_t
  )


# Apply the average allocation structure to every year from 1961 to 1989.
soybean_oil_Kastner_Percentage_df_61_89 <-
  expand.grid(
    FAOStat_area_code_Imp =
      unique(
        soybean_oil_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Imp
      ),
    FAOStat_area_code_Exp =
      unique(
        soybean_oil_Kastner_Percentage_df_90_20_avg$
          FAOStat_area_code_Exp
      ),
    Year = seq(1961, 1989)
  ) %>%
  left_join(
    soybean_oil_Kastner_Percentage_df_90_20_avg %>%
      select(
        FAOStat_area_code_Exp,
        FAOStat_area_code_Imp,
        Value
      ),
    by = c(
      "FAOStat_area_code_Exp",
      "FAOStat_area_code_Imp"
    )
  )


# Retain annual estimates from 1990 onward.
soybean_oil_Kastner_Percentage_df_90_20 <-
  soybean_oil_Kastner_Percentage_df_90_20 %>%
  filter(
    Year > 1989
  )


# Combine reconstructed 1961-1989 proportions with later annual estimates.
soybean_oil_Kastner_Percentage_df <- rbind(
  soybean_oil_Kastner_Percentage_df_61_89,
  soybean_oil_Kastner_Percentage_df_90_20
)


## 9: Extract within-country attribution shares --------------------------------

# Normalize each full attribution matrix by its row total and extract the
# diagonal element for each country.
#
# The diagonal represents the within-country share of the normalized
# attribution matrix. These values are used only as normalization terms in
# subsequent calculations.
#
# They should not be interpreted as the exported fraction. The exported
# fraction used in the analysis is estimated independently from FAOSTAT Food
# Balance data in script 01.

soybean_oil_Kastner_Exp_Percentage <-
  soybean_oil_Kastner

for (year in names(soybean_oil_Kastner_Exp_Percentage)) {
  
  row_sums <- rowSums(
    soybean_oil_Kastner_Exp_Percentage[[year]],
    na.rm = TRUE
  )
  
  row_sums[row_sums < 0.000001] <- 0
  
  row_sums_matrix <- matrix(
    row_sums,
    nrow = nrow(
      soybean_oil_Kastner_Exp_Percentage[[year]]
    ),
    ncol = ncol(
      soybean_oil_Kastner_Exp_Percentage[[year]]
    ),
    byrow = FALSE
  )
  
  # Normalize each row.
  m_percent <-
    soybean_oil_Kastner_Exp_Percentage[[year]] /
    row_sums_matrix
  
  # Remove numerically negligible shares.
  m_percent[m_percent < 0.0001] <- 0
  
  # Retain diagonal attribution share.
  soybean_oil_Kastner_Exp_Percentage[[year]] <-
    diag(
      as.matrix(m_percent)
    )
  
  # Assign zero to countries without a soybean-oil production record.
  for (
    country in
    rownames(
      soybean_oil_Kastner_Exp_Percentage[[year]]
    )
  ) {
    
    prod_val <- soybean_oil_Production %>%
      filter(
        Year == year,
        FAOStat_area_code == country
      )
    
    if (nrow(prod_val) == 0) {
      soybean_oil_Kastner_Exp_Percentage[[year]][country, ] <- 0
    }
  }
}


## 10: Convert normalization shares to a data frame ----------------------------

soybean_oil_Kastner_Exp_Percentage_df <-
  data.frame()

for (year in names(soybean_oil_Kastner_Exp_Percentage)) {
  
  m_percent <-
    soybean_oil_Kastner_Exp_Percentage[[year]]
  
  year_df <-
    as.data.frame(
      as.matrix(m_percent)
    ) %>%
    mutate(
      FAOStat_area_code =
        rownames(.)
    ) %>%
    rename(
      Perc_Exp_Kastner = 1,
      FAOStat_area_code = 2
    )
  
  year_df$Year <- year
  
  soybean_oil_Kastner_Exp_Percentage_df <-
    rbind(
      soybean_oil_Kastner_Exp_Percentage_df,
      year_df
    )
}


## 11: Export results -----------------------------------------------------------

write.csv(
  soybean_oil_Kastner_Percentage_df,
  "results/soybean_oil_Kastner_Percentage_df.csv",
  row.names = FALSE
)

write.csv(
  soybean_oil_Kastner_Exp_Percentage_df,
  "results/soybean_oil_Kastner_Exp_Percentage.csv",
  row.names = FALSE
)