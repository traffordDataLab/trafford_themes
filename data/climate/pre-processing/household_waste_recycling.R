# Household Waste Recycling
# Created: 2021-12-23

# Source: Department for Environment, Food & Rural Affairs
# URL: https://www.gov.uk/government/statistical-data-sets/env18-local-authority-collected-waste-annual-results-tables
# URL: https://www.gov.uk/government/statistical-data-sets/env18-local-authority-collected-waste-annual-results-tables-202122


# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readxl)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1144270/LA_and_Regional_Spreadsheet_202122.xlsx",
    write_disk(tmp))

# Extract the raw data ---------------------------
df_raw_tonnes <- read_xlsx(tmp, sheet = 4, skip = 3)
df_raw_percentages <- read_xlsx(tmp, sheet = 8, skip = 3)


# Household waste collected for recycling ---------------------------

# Percentage data for England
england_recycling_percentages <- df_raw_percentages %>%
  filter(Region == "Total England") %>%
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
         value = round((value * 100), digits = 1)) %>%
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
         period = str_replace(period, "-", "/")) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Bind the percentage and tonnage datasets together
df_household_waste_recycled <- bind_rows(df_household_waste_recycled, df_recycling_tonnes)

# Export the tidied data
write_csv(df_household_waste_recycled, "../household_waste_recycling.csv")


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
         period = str_replace(period, "-", "/")) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Bind the percentage and tonnage datasets together
df_household_waste_not_recycled <- bind_rows(df_household_waste_not_recycled, df_non_recycling_tonnes)

# Export the tidied data
write_csv(df_household_waste_not_recycled, "../household_waste_not_recycled.csv")

# Cleanup the downloaded data
unlink(tmp)
