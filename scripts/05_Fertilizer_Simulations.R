# ==============================================================================
# 05_Fertilizer_Simulations.R
#
# Author: Fernando Coello
# Contact: f.coello@creaf.uab.cat
#
# Purpose:
# Estimate uncertainty in nitrogen (N), phosphorus (P), and potassium (K)
# fertilizer use for wheat, maize, rice, and soybean using Monte Carlo
# simulations.
#
# The analysis:
#
#   1. Combines corrected fertilizer application-rate predictions with the
#      original crop-specific prediction datasets, using the latter to fill
#      missing corrected estimates.
#
#   2. Expresses phosphorus and potassium fertilizer application rates as
#      elemental P and K rather than P2O5 and K2O.
#
#   3. Estimates prediction uncertainty by comparing modeled fertilizer
#      application rates with available national fertilizer-use observations.
#      Median absolute percentage errors (MAPE) across crops are subsequently
#      used to parameterize the uncertainty distributions.
#
#   4. Generates 500 simulated fertilizer application rates for each
#      country-year observation using a lower-truncated normal distribution,
#      preventing simulated fertilizer application rates from becoming
#      negative.
#
#   5. Converts simulated application rates into total nutrient use using the
#      corresponding crop area.
#
#   6. Performs 500 Monte Carlo iterations for each crop and nutrient by
#      randomly selecting one simulated nutrient-use estimate for each
#      country-year in each iteration.
#
# Outputs:
#   Simulated fertilizer-use datasets and Monte Carlo realizations for N, P,
#   and K for wheat, maize, rice, and soybean.
#
# Random seed:
#   A fixed random seed is used to improve reproducibility of the simulations.
# ==============================================================================

set.seed(123)

# Libraries --------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(truncnorm)      # truncnorm: generate random values from truncated normal distributions.
library(parallel)       # parallel: run Monte Carlo simulations using multiple CPU cores.


# Load data --------------------------------------------------------------------

## 1: Fertilizer application-rate predictions ----------------------------------

# Crop-specific fertilizer application-rate predictions from:
## Coello, F., Decorte, T., Janssens, I. et al. Global Crop-Specific Fertilization Dataset from 1961–2019. Sci Data 12, 40 (2025). https://doi.org/10.1038/s41597-024-04215-x
# The dataset contains both the original machine-learning (ML) estimates and
# the adjusted estimates used in the analysis.

Fertilizer_predictions <- read_csv(
  "data/FUBC_predictions_Coello_etal_2025.csv"
)


## 2: FAOSTAT crop area ---------------------------------------------------------

# Harvested crop area was obtained from FAOSTAT and is not redistributed
# with the archived prediction dataset.

FAOSTAT_area <- read_csv(
  "data/FAOSTAT_crop_area.csv"
)


## 3: National fertilizer-use observations -------------------------------------

# National observations used to estimate prediction uncertainty.
# The compilation of the national results is described in Coello and Decorte et al 2025.

National_db <- read_excel(
  "data/FUBC_National_Reports_v3.xlsx"
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

# Functions --------------------------------------------------------------------

## 1: Generate truncated normal simulations ------------------------------------

# Generate non-negative simulated fertilizer application rates around an
# estimated mean value.
#
# Prediction uncertainty is expressed as MAPE (median absolute percentage
# error). For each observation:
#
#   MAE = mean application rate × MAPE
#
# For a normal distribution, the expected absolute deviation from the mean is
# approximately 0.8 × sigma. Therefore:
#
#   sigma ≈ MAE / 0.8
#
# The distribution is truncated at zero by default because fertilizer
# application rates cannot be negative.

generate_truncated_normal <- function(
    mean,
    MAPE,
    n,
    lower_bound = 0
) {
  
  # Return zeros when the estimated application rate is zero.
  # This also avoids attempting to simulate from a distribution with sd = 0.
  if (is.na(mean)) {
    return(rep(NA_real_, n))
  }
  
  if (mean <= lower_bound) {
    return(rep(lower_bound, n))
  }
  
  MAE <- mean * MAPE
  sigma <- MAE / 0.8
  
  truncnorm::rtruncnorm(
    n = n,
    a = lower_bound,
    mean = mean,
    sd = sigma
  )
}


## 2: Simulate fertilizer application rates ------------------------------------

# Generate `n_simulations` possible fertilizer application rates for every
# observation in a data frame.
#
# `column` identifies the application-rate estimate to simulate (e.g. Navg,
# Pavg, or Kavg).

simulate_data <- function(
    df,
    column,
    MAPE,
    n_simulations,
    lower_bound = 0
) {
  
  simulated_data <- lapply(
    df[[column]],
    function(mean_value) {
      generate_truncated_normal(
        mean = mean_value,
        MAPE = MAPE,
        n = n_simulations,
        lower_bound = lower_bound
      )
    }
  )
  
  simulated_df <- as.data.frame(
    do.call(
      rbind,
      simulated_data
    )
  )
  
  names(simulated_df) <- paste0(
    "Sim_",
    seq_len(n_simulations)
  )
  
  simulated_df
}


## 3: Perform parallel Monte Carlo simulations ---------------------------------

# For every Monte Carlo iteration, randomly select one simulated nutrient-use
# value for each country-year.
#
# `simulation_prefix` identifies the columns containing total nutrient-use
# simulations, for example:
#
#   "N_wheat_"
#   "P_wheat_"
#   "K_wheat_"
#
# This avoids relying on fixed column positions such as 505:ncol(df).
#
# The function uses parallel processing to distribute Monte Carlo iterations
# across available CPU cores.

monte_carlo_simulations_parallel <- function(
    simulated_df_full,
    n_iterations,
    simulation_prefix,
    n_cores = parallel::detectCores()
) {
  
  # Identify columns containing total nutrient-use simulations.
  simulation_cols <- grep(
    paste0("^", simulation_prefix),
    names(simulated_df_full),
    value = TRUE
  )
  
  if (length(simulation_cols) == 0) {
    stop(
      paste(
        "No simulation columns found with prefix:",
        simulation_prefix
      )
    )
  }
  
  # Extract years with available data.
  years <- unique(
    simulated_df_full$Year
  )
  
  years <- years[
    !is.na(years)
  ]
  
  # Number of parallel workers.
  n_workers <- min(
    n_cores,
    n_iterations
  )
  
  cl <- parallel::makeCluster(
    n_workers
  )
  
  on.exit(
    parallel::stopCluster(cl),
    add = TRUE
  )
  
  # Reproducible random-number streams across workers.
  parallel::clusterSetRNGStream(
    cl,
    iseed = 123
  )
  
  parallel::clusterExport(
    cl,
    varlist = c(
      "simulated_df_full",
      "years",
      "simulation_cols"
    ),
    envir = environment()
  )
  
  results <- parallel::parLapply(
    cl,
    seq_len(n_iterations),
    function(i) {
      
      simulation_df_list <- vector(
        "list",
        length(years)
      )
      
      for (y in seq_along(years)) {
        
        year <- years[y]
        
        yearly_data <- simulated_df_full[
          simulated_df_full$Year == year,
          ,
          drop = FALSE
        ]
        
        countries <- unique(
          yearly_data$ISO_3_Code
        )
        
        countries <- countries[
          !is.na(countries)
        ]
        
        num_countries <- length(
          countries
        )
        
        # Skip years without usable country observations.
        if (num_countries == 0) {
          next
        }
        
        simulation_df <- data.frame(
          ISO_3_Code = countries,
          Year = rep(
            year,
            num_countries
          ),
          Nut_crop = rep(
            NA_real_,
            num_countries
          ),
          Iter = rep(
            i,
            num_countries
          ),
          stringsAsFactors = FALSE
        )
        
        for (j in seq_along(countries)) {
          
          selected_row <- yearly_data[
            yearly_data$ISO_3_Code == countries[j],
            ,
            drop = FALSE
          ]
          
          # One row per country-year is expected.
          simulation_df$Nut_crop[j] <- sample(
            as.numeric(
              selected_row[
                1,
                simulation_cols,
                drop = TRUE
              ]
            ),
            size = 1
          )
        }
        
        simulation_df_list[[y]] <- simulation_df
      }
      
      # Remove empty list elements generated by skipped years.
      simulation_df_list <- Filter(
        Negate(is.null),
        simulation_df_list
      )
      
      do.call(
        rbind,
        simulation_df_list
      )
    }
  )
  
  do.call(
    rbind,
    results
  )
}
# Main code --------------------------------------------------------------------
## 1: Prepare fertilizer application-rate data ---------------------------------

# Combine fertilizer application-rate predictions with FAOSTAT harvested area.
#
# Adjusted fertilizer estimates are used whenever available. If an adjusted
# estimate is missing, the corresponding original machine-learning estimate is
# used instead.
#
# N is already expressed as elemental N.
# P2O5 and K2O application rates are converted to elemental P and K,
# respectively.

Fertilizer_use <- Fertilizer_predictions %>%
  left_join(
    FAOSTAT_area %>%
      select(
        M49_Code,
        ISO_3_Code,
        Year,
        Crop_Code,
        Area_FAOStat
      ),
    by = c(
      "M49_Code",
      "ISO_3_Code",
      "Year",
      "Crop_Code"
    )
  ) %>%
  mutate(
    # Prefer adjusted predictions; use ML estimates where adjusted values
    # are unavailable.
    Navg = coalesce(
      N_avg_app_adj,
      N_avg_app_ML
    ),
    
    P2O5_avg = coalesce(
      P2O5_avg_app_adj,
      P2O5_avg_app_ML
    ),
    
    K2O_avg = coalesce(
      K2O_avg_app_adj,
      K2O_avg_app_ML
    ),
    
    # Convert fertilizer oxide units to elemental nutrients.
    Pavg = P2O5_avg * (62 / 142),
    Kavg = K2O_avg * (78.2 / 94.2)
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Crop_Code,
    crops_names,
    Area_FAOStat,
    Navg,
    Pavg,
    Kavg
  ) %>%
  distinct()

## 2: Create crop-specific nutrient datasets -----------------------------------

### Wheat ----------------------------------------------------------------------

Wheat_N_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_1"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Navg,
    Area_FAOStat
  )

Wheat_P_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_1"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Pavg,
    Area_FAOStat
  )

Wheat_K_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_1"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Kavg,
    Area_FAOStat
  )


### Maize ----------------------------------------------------------------------

Maize_N_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_2"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Navg,
    Area_FAOStat
  )

Maize_P_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_2"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Pavg,
    Area_FAOStat
  )

Maize_K_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_2"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Kavg,
    Area_FAOStat
  )


### Rice -----------------------------------------------------------------------

Rice_N_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_3"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Navg,
    Area_FAOStat
  )

Rice_P_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_3"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Pavg,
    Area_FAOStat
  )

Rice_K_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "1_3"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Kavg,
    Area_FAOStat
  )


### Soybean --------------------------------------------------------------------

Soybean_N_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "2_1"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Navg,
    Area_FAOStat
  )

Soybean_P_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "2_1"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Pavg,
    Area_FAOStat
  )

Soybean_K_use <- Fertilizer_use %>%
  filter(
    Crop_Code == "2_1"
  ) %>%
  select(
    M49_Code,
    ISO_3_Code,
    Year,
    Kavg,
    Area_FAOStat
  )

## 3: Estimate uncertainty in fertilizer application rates ---------------------

# National fertilizer-use observations are used as an independent reference to
# quantify prediction error.
#
# For each matched country-year, absolute percentage error is calculated as:
#
#   |observed - predicted| / observed
#
# Crop-specific and pooled estimates are then calculated as the median absolute
# percentage error. The pooled values across wheat, maize, rice, and soybean are
# subsequently used to parameterize the uncertainty simulations.

National_db_fil <- National_db %>%
  select(
    FAOStat_area_code,
    Year,
    Crop_Code,
    N_avg_app,
    P2O5_avg_app,
    K2O_avg_app
  ) %>%
  mutate(
    Year = as.numeric(Year)
  ) %>%
  left_join(
    Countries_DB %>%
      select(
        FAOStat_area_code,
        ISO3
      ),
    by = "FAOStat_area_code"
  ) %>%
  rename(
    ISO_3_Code = ISO3
  )

### Nitrogen -------------------------------------------------------------------

Wheat_N_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_1"
  ) %>%
  inner_join(
    Wheat_N_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (N_avg_app - Navg) /
        N_avg_app
    )
  ) %>%
  drop_na(
    N_avg_app,
    Navg,
    APE
  )


Maize_N_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_2"
  ) %>%
  inner_join(
    Maize_N_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (N_avg_app - Navg) /
        N_avg_app
    )
  ) %>%
  drop_na(
    N_avg_app,
    Navg,
    APE
  )


Rice_N_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_3"
  ) %>%
  inner_join(
    Rice_N_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (N_avg_app - Navg) /
        N_avg_app
    )
  ) %>%
  drop_na(
    N_avg_app,
    Navg,
    APE
  )


Soybean_N_error <- National_db_fil %>%
  filter(
    Crop_Code == "2_1"
  ) %>%
  inner_join(
    Soybean_N_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (N_avg_app - Navg) /
        N_avg_app
    )
  ) %>%
  drop_na(
    N_avg_app,
    Navg,
    APE
  )

Wheat_N_CV <- Wheat_N_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Maize_N_CV <- Maize_N_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Rice_N_CV <- Rice_N_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Soybean_N_CV <- Soybean_N_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )


All_crops_N_CV <- bind_rows(
  Wheat_N_error,
  Maize_N_error,
  Rice_N_error,
  Soybean_N_error
) %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

### Phosphorus -----------------------------------------------------------------

Wheat_P_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_1"
  ) %>%
  inner_join(
    Wheat_P_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        P2O5_avg_app -
          Pavg * (142 / 62)
      ) /
        P2O5_avg_app
    )
  ) %>%
  drop_na(
    P2O5_avg_app,
    Pavg,
    APE
  )


Maize_P_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_2"
  ) %>%
  inner_join(
    Maize_P_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        P2O5_avg_app -
          Pavg * (142 / 62)
      ) /
        P2O5_avg_app
    )
  ) %>%
  drop_na(
    P2O5_avg_app,
    Pavg,
    APE
  )


Rice_P_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_3"
  ) %>%
  inner_join(
    Rice_P_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        P2O5_avg_app -
          Pavg * (142 / 62)
      ) /
        P2O5_avg_app
    )
  ) %>%
  drop_na(
    P2O5_avg_app,
    Pavg,
    APE
  )


Soybean_P_error <- National_db_fil %>%
  filter(
    Crop_Code == "2_1"
  ) %>%
  inner_join(
    Soybean_P_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        P2O5_avg_app -
          Pavg * (142 / 62)
      ) /
        P2O5_avg_app
    )
  ) %>%
  drop_na(
    P2O5_avg_app,
    Pavg,
    APE
  )

Wheat_P_CV <- Wheat_P_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Maize_P_CV <- Maize_P_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Rice_P_CV <- Rice_P_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Soybean_P_CV <- Soybean_P_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )


# Pooled uncertainty across all four crops.
#

All_crops_P_CV <- bind_rows(
  Wheat_P_error,
  Maize_P_error,
  Rice_P_error,
  Soybean_P_error
) %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

### Potassium ------------------------------------------------------------------

Wheat_K_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_1"
  ) %>%
  inner_join(
    Wheat_K_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        K2O_avg_app -
          Kavg * (94.2 / 78.2)
      ) /
        K2O_avg_app
    )
  ) %>%
  drop_na(
    K2O_avg_app,
    Kavg,
    APE
  )


Maize_K_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_2"
  ) %>%
  inner_join(
    Maize_K_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        K2O_avg_app -
          Kavg * (94.2 / 78.2)
      ) /
        K2O_avg_app
    )
  ) %>%
  drop_na(
    K2O_avg_app,
    Kavg,
    APE
  )


Rice_K_error <- National_db_fil %>%
  filter(
    Crop_Code == "1_3"
  ) %>%
  inner_join(
    Rice_K_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        K2O_avg_app -
          Kavg * (94.2 / 78.2)
      ) /
        K2O_avg_app
    )
  ) %>%
  drop_na(
    K2O_avg_app,
    Kavg,
    APE
  )


Soybean_K_error <- National_db_fil %>%
  filter(
    Crop_Code == "2_1"
  ) %>%
  inner_join(
    Soybean_K_use,
    by = c(
      "ISO_3_Code",
      "Year"
    )
  ) %>%
  mutate(
    APE = abs(
      (
        K2O_avg_app -
          Kavg * (94.2 / 78.2)
      ) /
        K2O_avg_app
    )
  ) %>%
  drop_na(
    K2O_avg_app,
    Kavg,
    APE
  )

Wheat_K_CV <- Wheat_K_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Maize_K_CV <- Maize_K_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Rice_K_CV <- Rice_K_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

Soybean_K_CV <- Soybean_K_error %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )


All_crops_K_CV <- bind_rows(
  Wheat_K_error,
  Maize_K_error,
  Rice_K_error,
  Soybean_K_error
) %>%
  summarise(
    MAPE = median(
      APE,
      na.rm = TRUE
    )
  )

## 4: Generate fertilizer application-rate simulations -------------------------

# Number of simulated fertilizer application rates generated for each
# country-year observation.

n_simulations <- 500

### Wheat ----------------------------------------------------------------------

Wheat_N_use_sim_N <- bind_cols(
  Wheat_N_use,
  simulate_data(
    df = Wheat_N_use,
    column = "Navg",
    MAPE = All_crops_N_CV$MAPE,
    n_simulations = n_simulations
  )
)

Wheat_P_use_sim_P <- bind_cols(
  Wheat_P_use,
  simulate_data(
    df = Wheat_P_use,
    column = "Pavg",
    MAPE = All_crops_P_CV$MAPE,
    n_simulations = n_simulations
  )
)

Wheat_K_use_sim_K <- bind_cols(
  Wheat_K_use,
  simulate_data(
    df = Wheat_K_use,
    column = "Kavg",
    MAPE = All_crops_K_CV$MAPE,
    n_simulations = n_simulations
  )
)


### Maize ----------------------------------------------------------------------

Maize_N_use_sim_N <- bind_cols(
  Maize_N_use,
  simulate_data(
    df = Maize_N_use,
    column = "Navg",
    MAPE = All_crops_N_CV$MAPE,
    n_simulations = n_simulations
  )
)

Maize_P_use_sim_P <- bind_cols(
  Maize_P_use,
  simulate_data(
    df = Maize_P_use,
    column = "Pavg",
    MAPE = All_crops_P_CV$MAPE,
    n_simulations = n_simulations
  )
)

Maize_K_use_sim_K <- bind_cols(
  Maize_K_use,
  simulate_data(
    df = Maize_K_use,
    column = "Kavg",
    MAPE = All_crops_K_CV$MAPE,
    n_simulations = n_simulations
  )
)


### Rice -----------------------------------------------------------------------

Rice_N_use_sim_N <- bind_cols(
  Rice_N_use,
  simulate_data(
    df = Rice_N_use,
    column = "Navg",
    MAPE = All_crops_N_CV$MAPE,
    n_simulations = n_simulations
  )
)

Rice_P_use_sim_P <- bind_cols(
  Rice_P_use,
  simulate_data(
    df = Rice_P_use,
    column = "Pavg",
    MAPE = All_crops_P_CV$MAPE,
    n_simulations = n_simulations
  )
)

Rice_K_use_sim_K <- bind_cols(
  Rice_K_use,
  simulate_data(
    df = Rice_K_use,
    column = "Kavg",
    MAPE = All_crops_K_CV$MAPE,
    n_simulations = n_simulations
  )
)


### Soybean --------------------------------------------------------------------

Soybean_N_use_sim_N <- bind_cols(
  Soybean_N_use,
  simulate_data(
    df = Soybean_N_use,
    column = "Navg",
    MAPE = All_crops_N_CV$MAPE,
    n_simulations = n_simulations
  )
)

Soybean_P_use_sim_P <- bind_cols(
  Soybean_P_use,
  simulate_data(
    df = Soybean_P_use,
    column = "Pavg",
    MAPE = All_crops_P_CV$MAPE,
    n_simulations = n_simulations
  )
)

Soybean_K_use_sim_K <- bind_cols(
  Soybean_K_use,
  simulate_data(
    df = Soybean_K_use,
    column = "Kavg",
    MAPE = All_crops_K_CV$MAPE,
    n_simulations = n_simulations
  )
)


## 5: Convert simulated application rates to total nutrient use ----------------

# Convert simulated fertilizer application rates (kg nutrient ha-1) into total
# nutrient use (tonnes) by multiplying each simulated rate by harvested area
# (ha) and dividing by 1,000.
#
# `prefix` defines the names of the resulting total nutrient-use simulations.

calculate_total_nutrient_use <- function(
    df,
    prefix
) {
  
  simulation_cols <- grep(
    "^Sim_",
    names(df),
    value = TRUE
  )
  
  total_use <- df %>%
    select(
      all_of(simulation_cols)
    ) %>%
    mutate(
      across(
        everything(),
        ~ .x * df$Area_FAOStat / 1000
      )
    )
  
  names(total_use) <- paste0(
    prefix,
    seq_along(simulation_cols)
  )
  
  bind_cols(
    df,
    total_use
  )
}

### Wheat ----------------------------------------------------------------------

Wheat_N_use_sim_N <- calculate_total_nutrient_use(
  Wheat_N_use_sim_N,
  prefix = "N_wheat_"
)

Wheat_P_use_sim_P <- calculate_total_nutrient_use(
  Wheat_P_use_sim_P,
  prefix = "P_wheat_"
)

Wheat_K_use_sim_K <- calculate_total_nutrient_use(
  Wheat_K_use_sim_K,
  prefix = "K_wheat_"
)


### Maize ----------------------------------------------------------------------

Maize_N_use_sim_N <- calculate_total_nutrient_use(
  Maize_N_use_sim_N,
  prefix = "N_maize_"
)

Maize_P_use_sim_P <- calculate_total_nutrient_use(
  Maize_P_use_sim_P,
  prefix = "P_maize_"
)

Maize_K_use_sim_K <- calculate_total_nutrient_use(
  Maize_K_use_sim_K,
  prefix = "K_maize_"
)


### Rice -----------------------------------------------------------------------

Rice_N_use_sim_N <- calculate_total_nutrient_use(
  Rice_N_use_sim_N,
  prefix = "N_rice_"
)

Rice_P_use_sim_P <- calculate_total_nutrient_use(
  Rice_P_use_sim_P,
  prefix = "P_rice_"
)

Rice_K_use_sim_K <- calculate_total_nutrient_use(
  Rice_K_use_sim_K,
  prefix = "K_rice_"
)


### Soybean --------------------------------------------------------------------

Soybean_N_use_sim_N <- calculate_total_nutrient_use(
  Soybean_N_use_sim_N,
  prefix = "N_soybean_"
)

Soybean_P_use_sim_P <- calculate_total_nutrient_use(
  Soybean_P_use_sim_P,
  prefix = "P_soybean_"
)

Soybean_K_use_sim_K <- calculate_total_nutrient_use(
  Soybean_K_use_sim_K,
  prefix = "K_soybean_"
)

## 6: Run Monte Carlo simulations ----------------------------------------------

# Number of Monte Carlo iterations.
#
# In each iteration, one simulated total nutrient-use value is randomly selected
# for every country-year. This produces 500 alternative realizations of crop-
# specific N, P, and K fertilizer use.

n_iterations <- 500


### Wheat ----------------------------------------------------------------------

Wheat_mc_N <- monte_carlo_simulations_parallel(
  simulated_df_full = Wheat_N_use_sim_N,
  n_iterations = n_iterations,
  simulation_prefix = "N_wheat_"
)

write.csv(
  Wheat_mc_N,
  "results/Wheat_mc_N_all.csv",
  row.names = FALSE
)

write.csv(
  Wheat_N_use_sim_N,
  "results/Wheat_sim_N_all.csv",
  row.names = FALSE
)

rm(
  Wheat_mc_N
)

gc()

Wheat_mc_P <- monte_carlo_simulations_parallel(
  simulated_df_full = Wheat_P_use_sim_P,
  n_iterations = n_iterations,
  simulation_prefix = "P_wheat_"
)

write.csv(
  Wheat_mc_P,
  "results/Wheat_mc_P_all.csv",
  row.names = FALSE
)

write.csv(
  Wheat_P_use_sim_P,
  "results/Wheat_sim_P_all.csv",
  row.names = FALSE
)

rm(
  Wheat_mc_P
)

gc()


Wheat_mc_K <- monte_carlo_simulations_parallel(
  simulated_df_full = Wheat_K_use_sim_K,
  n_iterations = n_iterations,
  simulation_prefix = "K_wheat_"
)

write.csv(
  Wheat_mc_K,
  "results/Wheat_mc_K_all.csv",
  row.names = FALSE
)

write.csv(
  Wheat_K_use_sim_K,
  "results/Wheat_sim_K_all.csv",
  row.names = FALSE
)

rm(
  Wheat_mc_K
)

gc()

### Maize ----------------------------------------------------------------------

Maize_mc_N <- monte_carlo_simulations_parallel(
  simulated_df_full = Maize_N_use_sim_N,
  n_iterations = n_iterations,
  simulation_prefix = "N_maize_"
)

write.csv(
  Maize_mc_N,
  "results/Maize_mc_N_all.csv",
  row.names = FALSE
)

write.csv(
  Maize_N_use_sim_N,
  "results/Maize_sim_N_all.csv",
  row.names = FALSE
)

rm(
  Maize_mc_N
)

gc()


Maize_mc_P <- monte_carlo_simulations_parallel(
  simulated_df_full = Maize_P_use_sim_P,
  n_iterations = n_iterations,
  simulation_prefix = "P_maize_"
)

write.csv(
  Maize_mc_P,
  "results/Maize_mc_P_all.csv",
  row.names = FALSE
)

write.csv(
  Maize_P_use_sim_P,
  "results/Maize_sim_P_all.csv",
  row.names = FALSE
)

rm(
  Maize_mc_P
)

gc()


Maize_mc_K <- monte_carlo_simulations_parallel(
  simulated_df_full = Maize_K_use_sim_K,
  n_iterations = n_iterations,
  simulation_prefix = "K_maize_"
)

write.csv(
  Maize_mc_K,
  "results/Maize_mc_K_all.csv",
  row.names = FALSE
)

write.csv(
  Maize_K_use_sim_K,
  "results/Maize_sim_K_all.csv",
  row.names = FALSE
)

rm(
  Maize_mc_K
)

gc()

### Rice -----------------------------------------------------------------------

Rice_mc_N <- monte_carlo_simulations_parallel(
  simulated_df_full = Rice_N_use_sim_N,
  n_iterations = n_iterations,
  simulation_prefix = "N_rice_"
)

write.csv(
  Rice_mc_N,
  "results/Rice_mc_N_all.csv",
  row.names = FALSE
)

write.csv(
  Rice_N_use_sim_N,
  "results/Rice_sim_N_all.csv",
  row.names = FALSE
)

rm(
  Rice_N_use_sim_N
)

gc()


Rice_mc_P <- monte_carlo_simulations_parallel(
  simulated_df_full = Rice_P_use_sim_P,
  n_iterations = n_iterations,
  simulation_prefix = "P_rice_"
)

write.csv(
  Rice_mc_P,
  "results/Rice_mc_P_all.csv",
  row.names = FALSE
)

write.csv(
  Rice_P_use_sim_P,
  "results/Rice_sim_P_all.csv",
  row.names = FALSE
)

rm(
  Rice_P_use_sim_P
)

gc()


Rice_mc_K <- monte_carlo_simulations_parallel(
  simulated_df_full = Rice_K_use_sim_K,
  n_iterations = n_iterations,
  simulation_prefix = "K_rice_"
)

write.csv(
  Rice_mc_K,
  "results/Rice_mc_K_all.csv",
  row.names = FALSE
)

write.csv(
  Rice_K_use_sim_K,
  "results/Rice_sim_K_all.csv",
  row.names = FALSE
)

rm(
  Rice_K_use_sim_K
)

gc()


### Soybean --------------------------------------------------------------------

Soybean_mc_N <- monte_carlo_simulations_parallel(
  simulated_df_full = Soybean_N_use_sim_N,
  n_iterations = n_iterations,
  simulation_prefix = "N_soybean_"
)

write.csv(
  Soybean_mc_N,
  "results/Soybean_mc_N_all.csv",
  row.names = FALSE
)

write.csv(
  Soybean_N_use_sim_N,
  "results/Soybean_sim_N_all.csv",
  row.names = FALSE
)

rm(
  Soybean_N_use_sim_N
)

gc()


Soybean_mc_P <- monte_carlo_simulations_parallel(
  simulated_df_full = Soybean_P_use_sim_P,
  n_iterations = n_iterations,
  simulation_prefix = "P_soybean_"
)

write.csv(
  Soybean_mc_P,
  "results/Soybean_mc_P_all.csv",
  row.names = FALSE
)

write.csv(
  Soybean_P_use_sim_P,
  "results/Soybean_sim_P_all.csv",
  row.names = FALSE
)

rm(
  Soybean_P_use_sim_P
)

gc()


Soybean_mc_K <- monte_carlo_simulations_parallel(
  simulated_df_full = Soybean_K_use_sim_K,
  n_iterations = n_iterations,
  simulation_prefix = "K_soybean_"
)

write.csv(
  Soybean_mc_K,
  "results/Soybean_mc_K_all.csv",
  row.names = FALSE
)

write.csv(
  Soybean_K_use_sim_K,
  "results/Soybean_sim_K_all.csv",
  row.names = FALSE
)

rm(
  Soybean_K_use_sim_K
)

gc()

