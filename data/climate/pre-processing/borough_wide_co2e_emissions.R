# Borough wide CO2e emissions (Full set data)
# Created: 2022-01-27
# Latest data: 2023-07-06
# Next publication: 2024-06

# Source: Department for Energy Security and Net Zero
#         https://www.gov.uk/government/collections/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics
#         https://www.gov.uk/government/statistics/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics-2005-to-2021

# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readxl)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2021):
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".xlsx")
GET(url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1168130/2005-21-uk-local-authority-ghg-emissions-update-060723.xlsx",
    write_disk(tmp))

# Extract the raw data ---------------------------
df_co2e_raw <- read_xlsx(tmp, sheet = "1_2", skip = 4)

# Tidy the data ---------------------------
df_co2e <- df_co2e_raw %>%
  rename(area_code = "Local Authority Code",
         area_name = "Local Authority",
         period = "Calendar Year",
         value = "Grand Total") %>%
  mutate(indicator = "Local Authority territorial carbon dioxide (CO2) emissions estimates",
         measure = "Frequency",
         unit = "kilotonnes (kt CO2e)") %>%
  filter(area_code %in% authorities$area_code, period >= 2010) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_co2e, "../borough_wide_co2_emissions.csv")

# Clean up the downloaded data
unlink(tmp)

