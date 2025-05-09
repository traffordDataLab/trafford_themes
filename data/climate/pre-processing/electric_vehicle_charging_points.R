# Electric vehicle charging points.
# Created: 2022-01-10  Updated: 2025-04-24  Data: 2025-01-31

# Source: Department for Transport (DfT) and Office for Zero Emission Vehicles (OZEV)
#         https://www.gov.uk/government/collections/electric-vehicle-charging-infrastructure-statistics
#         https://www.gov.uk/government/statistics/electric-vehicle-public-charging-infrastructure-statistics-january-2025
#         https://assets.publishing.service.gov.uk/media/67a1f433567402152f553bce/electric-vehicle-public-charging-infrastructure-statistics-january-2025.ods


# Load required packages ---------------------------
library(tidyverse); library(tidyselect); library(readODS); library(httr); library(janitor)


# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (as published on LG Inform in July 2024), plus England:
authorities <- read_csv("../../cipfalga0724.csv") %>%
    add_row(area_code = "E08000009", area_name = "Trafford") %>%
    add_row(area_code = "E92000001", area_name = "England")


# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/media/67a1f433567402152f553bce/electric-vehicle-public-charging-infrastructure-statistics-january-2025.ods",
    write_disk(tmp))

df_raw <- read_ods(tmp, sheet = "2a", col_names = TRUE, col_types = NA, skip = 2) %>%
    clean_names()

unlink(tmp) # cleanup the downloaded ODS


# Tidy the data ---------------------------
df_charging_points_rate <- df_raw %>%
    # The data are now ordered in columns oldest to latest. We only want the last 12 columns of data plus the first 2 (area code and area name).
    select(area_code = local_authority_region_code_note_5,
           area_name = local_authority_region_name,
           `2025-01` = jan_25,
           `2024-10` = oct_24_note_14,
           `2024-07` = jul_24,
           `2024-04` = apr_24,
           `2024-01` = jan_24,
           `2023-10` = oct_23,
           `2023-07` = jul_23,
           `2023-04` = apr_23,
           `2023-01` = jan_23,
           `2022-10` = oct_22,
           `2022-07` = jul_22,
           `2022-04` = apr_22
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
