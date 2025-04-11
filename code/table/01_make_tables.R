here::i_am(
  "code/table/01_make_tables.R"
)

cdc_data2021 <- readRDS(
  file = here::here("data/cdc_data2021.rds")
)

# Tabular analysis

# Load libraries
library(here)
library(dplyr)
library(gtsummary)
library(gt) 

# Create Table 1: Summarize data by sample location
table1 <- cdc_data2021 %>%
  group_by(sample_location) %>% 
  summarize(
    # For each sample_location, population_served is unique, so take the first value.
    population_served = first(population_served),
    # Calculate the average percent change in SARS-CoV-2 RNA
    avg_ptc_15d = mean(ptc_15d, na.rm = TRUE),
    # Calculate the average proportion of tests with SARS-CoV-2 detected
    avg_detect_prop_15d = mean(detect_prop_15d, na.rm = TRUE)
  ) %>%
  # If population_served is too large (>1e6), take its natural logarithm.
  mutate(
    population_served = if_else(population_served > 1e6, log(population_served), population_served)
  ) %>%
  # Round all numeric values to 3 decimal places and convert avg_detect_prop_15d to percentage format
  mutate(
    population_served = round(population_served, 3),
    avg_ptc_15d = round(avg_ptc_15d, 3),
    avg_detect_prop_15d = paste0(round(avg_detect_prop_15d, 3), "%")
  ) %>%
  arrange(desc(population_served))


saveRDS(
  table1,
  file = here::here("data", "table1.rds")
)

# Table 2
# Step 1: Create a county-level summary.
# For each county (unique county_names and corresponding state in wwtp_jurisdiction),
# extract the unique population_served and the median of other variables.
county_summary <- cdc_data2021 %>%
  group_by(county_names, wwtp_jurisdiction) %>%
  summarize(
    # Since population_served is unique for each county, take the first value.
    population_served = first(population_served),
    # Take the median values for ptc_15d, detect_prop_15d, and percentile.
    ptc_15d = median(ptc_15d, na.rm = TRUE),
    detect_prop_15d = median(detect_prop_15d, na.rm = TRUE),
    percentile = median(percentile, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Create a state-level summary.
# For each state (wwtp_jurisdiction), sum the unique county population_served
# and calculate the median of the remaining variables.
state_summary <- county_summary %>%
  group_by(wwtp_jurisdiction) %>%
  summarize(
    total_population_served = sum(population_served, na.rm = TRUE),
    median_ptc_15d = median(ptc_15d, na.rm = TRUE),
    median_detect_prop_15d = median(detect_prop_15d, na.rm = TRUE),
    median_percentile = median(percentile, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # If total_population_served is too large (>1e6), take its natural log.
  mutate(
    total_population_served = if_else(total_population_served > 1e6,
                                      log(total_population_served),
                                      total_population_served)
  ) %>%
  # Round all numeric values to two decimal places.
  mutate(
    total_population_served = round(total_population_served, 2),
    median_ptc_15d = round(median_ptc_15d, 2),
    median_detect_prop_15d = round(median_detect_prop_15d, 2),
    median_percentile = round(median_percentile, 2)
  )

# Step 3: Extract the top 5 states based on total_population_served.
table2 <- state_summary %>%
  arrange(desc(total_population_served)) %>%
  slice_head(n = 5) %>%
  select(wwtp_jurisdiction, median_ptc_15d, median_detect_prop_15d, median_percentile)

saveRDS(
  table2,
  file = here::here("data", "table2.rds")
)
