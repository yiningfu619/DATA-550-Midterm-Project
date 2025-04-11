here::i_am(
  "code/table/00_clean_data.R"
)

library(dplyr)
library(readr)

cdc_data <- read_csv(here::here("data", "cdc_data.csv"))

cdc_data2021 <- cdc_data %>%
  # Convert to Date class if needed
  mutate(
    date_start = as.Date(date_start),
    date_end   = as.Date(date_end)
  ) %>%
  # Filter for 2021
  filter(
    date_start >= as.Date("2021-01-01"),
    date_start <= as.Date("2021-12-31"),
    date_end   >= as.Date("2021-01-01"),
    date_end   <= as.Date("2021-12-31")
  )

saveRDS(
  cdc_data2021,
  file = here::here("data", "cdc_data2021.rds")
)