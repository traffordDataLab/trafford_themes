# Electric vehicle charging points.
# Created: 2022-01-10  Updated: 2024-05-21  Data: 2023-04-30

# Source: Department for Transport (DfT) and Office for Zero Emission Vehicles (OZEV)
#         https://www.gov.uk/government/collections/electric-vehicle-charging-infrastructure-statistics
#         https://www.gov.uk/government/statistics/electric-vehicle-public-charging-infrastructure-statistics-april-2024
#         https://assets.publishing.service.gov.uk/media/662fa4e087bdbae4ab19ad75/electric-vehicle-public-charging-infrastructure-statistics-april-2024.ods


# Load required packages ---------------------------
library(tidyverse); library(tidyselect); library(readODS); library(httr); library(janitor)

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/media/662fa4e087bdbae4ab19ad75/electric-vehicle-public-charging-infrastructure-statistics-april-2024.ods",
    write_disk(tmp))

# Setup objects ---------------------------
# Trafford, its CIPFA nearest neighbours (2019) and England:
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Get the raw data ---------------------------
df_raw <- read_ods(tmp, sheet = "2a", col_names = TRUE, col_types = NA, skip = 2) %>%
    clean_names()

# Tidy the data ---------------------------
df_charging_points_rate <- df_raw %>%
    # The data are now ordered in columns oldest to latest. We only want the last 12 columns of data plus the first 2 (area code and area name).
    select(1:2, (ncol(.)-11):ncol(.)) %>%
    rename(area_code = local_authority_region_code_note_5,
           area_name = local_authority_region_name,
           `2024-04` = 14,
           `2024-01` = 13,
           `2023-10` = 12,
           `2023-07` = 11,
           `2023-04` = 10,
           `2023-01` = 9,
           `2022-10` = 8,
           `2022-07` = 7,
           `2022-04` = 6,
           `2022-01` = 5,
           `2021-10` = 4,
           `2021-07` = 3
    ) %>%
    filter(area_code %in% authorities$area_code) %>%
    mutate(area_name = if_else(area_name == "ENGLAND", "England", area_name)) %>%
    # convert to 'tidy' data by transposing the dataset to long format
    pivot_longer(c(-area_code, -area_name), names_to = "period", values_to = "value") %>%
    mutate(value = round(as.numeric(value), 1),
           indicator = "Publicly available electric vehicle charging devices at all speeds",
           measure = "Per 100,000 population",
           unit = "Devices") %>%
    arrange(area_name, period) %>%
    select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_charging_points_rate, "../electric_vehicle_charging_points.csv")
