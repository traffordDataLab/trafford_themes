# Household Waste Recycling
# Created: 2021-12-23, updated: 2025-04-24, data: 2025-03-27

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistics/local-authority-collected-waste-management-annual-results

# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readxl) ; library(readODS)


# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (as published on LG Inform in July 2024):
authorities <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")


# Download the data --------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/media/67ffbbdced87b816085467bf/LA_and_Regional_Spreadsheet_2023-21_rev1.ods",
    write_disk(tmp))


# Extract the raw data ---------------------------
df_raw_tonnes <- read_ods(tmp, sheet = "Table_1", skip = 3)
df_raw_percentages <- read_ods(tmp, sheet = "Table_3", skip = 3)

unlink(tmp) # cleanup the downloaded data


# Household waste collected for recycling ---------------------------

# Percentage data for England
england_recycling_percentages <- df_raw_percentages %>%
  filter(str_like(Region, "Total England%")) %>% # Use of str_like to catch cases where the England figure is provisional and so is recorded as "Total England (Provisional)" etc.
  mutate(area_code = "E92000001",
         area_name = "England") %>%
  select(area_code, area_name,
         period = Year,
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`)

# Percentage data for Trafford and similar neighbours
df_household_waste_recycled <- df_raw_percentages %>%
  rename(area_code = `ONS code`) %>%
  filter(area_code %in% authorities$area_code) %>%
  left_join(authorities) %>%
  select(area_code,
         area_name,
         period = Year,
         value = `Percentage of household waste sent for reuse, recycling or composting (Ex NI192)`) %>%
  # Join with the data for England
  bind_rows(england_recycling_percentages) %>%
  mutate(indicator = "Household waste sent for reuse, recycling or composting",
         measure = "Percentage",
         unit = "Waste",
         period = str_replace(period, "-", "/"),
         value = round((as.numeric(value) * 100), digits = 1)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Tonnage data for Trafford and similar neighbours - data for England not included as tonnage can't be compared with individual LAs
df_recycling_tonnes <- df_raw_tonnes %>%
  rename(area_code = `ONS Code`) %>%
  filter(area_code %in% authorities$area_code) %>%
  left_join(authorities) %>%
  select(area_code,
         area_name,
         period = `Financial Year`,
         value = `Household - waste sent for recycling-composting-reuse (tonnes)`) %>%
  mutate(indicator = "Household waste sent for reuse, recycling or composting",
         measure = "Frequency",
         unit = "Tonnes",
         period = str_replace(period, "-", "/"),
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Bind the percentage and tonnage datasets together
df_household_waste_recycled <- bind_rows(df_household_waste_recycled, df_recycling_tonnes)


# Household waste not sent for recycling ---------------------------

# Percentage data for Trafford, similar neighbours and England
# NOTE: The percentages values are the opposite of those in the recycling dataset, i.e. 100 - recycled % = non recycled %
df_household_waste_not_recycled <- df_household_waste_recycled %>%
  filter(measure == "Percentage") %>%
  mutate(value = 100 - value,
         indicator = "Household waste not sent for reuse, recycling or composting")

# Tonnage data for Trafford and similar neighbours
df_non_recycling_tonnes <- df_raw_tonnes %>%
  rename(area_code = `ONS Code`) %>%
  filter(area_code %in% authorities$area_code) %>%
  left_join(authorities) %>%
  select(area_code,
         area_name,
         period = `Financial Year`,
         value = `Household - regular collection (not recycled) (tonnes)`) %>%
  mutate(indicator = "Household waste not sent for reuse, recycling or composting",
         measure = "Frequency",
         unit = "Tonnes",
         period = str_replace(period, "-", "/"),
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Bind the percentage and tonnage datasets together
df_household_waste_not_recycled <- bind_rows(df_household_waste_not_recycled, df_non_recycling_tonnes)

# Export the tidied data
write_csv(df_household_waste_recycled, "../household_waste_recycling.csv")
write_csv(df_household_waste_not_recycled, "../household_waste_not_recycled.csv")
