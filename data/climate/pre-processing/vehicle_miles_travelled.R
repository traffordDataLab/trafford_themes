# Vehicle miles travelled on roads.
# Created: 2022-01-07.  Last updated: 2024-08-22.  Data: 2024-05-22

# Source: Department for Transport (DfT)
#         https://www.gov.uk/government/statistical-data-sets/road-traffic-statistics-tra#traffic-by-local-authority-tra89
#         https://assets.publishing.service.gov.uk/media/664b861eae748c43d3793ee2/tra8901-miles-by-local-authority.ods


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(httr)

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/media/664b861eae748c43d3793ee2/tra8901-miles-by-local-authority.ods",
    write_disk(tmp))

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (as published on LG Inform in July 2024):
authorities <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Get the raw data ---------------------------
df_raw <- read_ods(tmp, sheet = "TRA8901", col_names = TRUE, col_types = NA, skip = 4)

# Tidy the data ---------------------------
df_vehicle_miles <- df_raw %>%
  # renaming columns via select for ease
  select(area_code = `Local Authority or Region Code`,
         area_name = `Local Authority`,
         `2012`,
         `2013`,
         `2014`,
         `2015`,
         `2016`,
         `2017`,
         `2018`,
         `2019`,
         `2020` = `2020 [note 8]`, # The addition of [note 8] refers to the figures being affected by COVID-19
         `2021` = `2021 [note 8]`,
         `2022` = `2022 [note 8] [r]`,
         `2023`) %>%
  # Filter out rows with area_codes not in the format "E06xxxxxx", "E08xxxxxx", "E09xxxxxx" or "E10xxxxxx" as these are region aggregations
  filter(str_detect(area_code, pattern = "E06|E08|E09|E10[0-9]{6}")) %>%
  # convert to 'tidy' data by transposing the dataset to long format
  pivot_longer(c(-area_code, -area_name), names_to = "period", values_to = "value") %>%
  # Remove rows with no data, signified with "[x]"
  filter(value != "[x]") %>%
  mutate(value = as.numeric(value))

# Calculate England averages for each year ---------------------------
df_england_averages <- df_vehicle_miles %>%
  mutate(area_code = "E92000001",
         area_name = "England LA average") %>%
  group_by(period, area_code, area_name) %>%
  summarise(value = round(mean(value), digits = 1)) %>%
  select(area_code, area_name, period, value)

# Get the data for Trafford and CIPFA neighbours and bind it with the England averages ---------------------------
df_vehicle_miles <- df_vehicle_miles %>%
  filter(area_code %in% authorities$area_code) %>%
  bind_rows(df_england_averages) %>%
  mutate(indicator = "Vehicle miles travelled",
         measure = "Frequency",
         unit = "Miles (million)") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_vehicle_miles, "../vehicle_miles_travelled.csv")

# Cleanup the downloaded ODS
unlink(tmp)
