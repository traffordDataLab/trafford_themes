# Domestic Energy Performance Certificates (EPC).
# Created: 2022-01-17  Updated: 2024-05-20  Data: 2024-04-25

# Source: Department for Levelling Up, Housing & Communities
#         https://www.gov.uk/government/statistical-data-sets/live-tables-on-energy-performance-of-buildings-certificates
#         (Also data available at: https://epc.opendatacommunities.org e.g. Trafford: https://epc.opendatacommunities.org/files/domestic-E08000009-Trafford.zip)

# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readODS) ; library(lubridate) ; library(janitor)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours:
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/media/6626ce4f1cbbb3400ba7e612/D1-_Domestic_Properties.ods",
    write_disk(tmp))

# Extract the raw data ---------------------------
df_epc_england_raw <- read_ods(tmp, sheet = 5, skip = 3) %>%
    clean_names()
df_epc_la_raw <- read_ods(tmp, sheet = 8, skip = 3) %>%
    clean_names()

# Prepare England data ---------------------------
df_epc_england <- df_epc_england_raw %>%
  filter(is.na(quarter) == FALSE) %>% # This removes the totals at the top of the sheet
  mutate(area_code = "E92000001",
         area_name = "England") %>%
  select(area_code, area_name, quarter, number_lodgements, a, b, c)

# Prepare LA data ---------------------------
df_epc_la <- df_epc_la_raw %>%
  select(area_code = local_authority_code,
         area_name = region,
         quarter, number_lodgements, a, b, c) %>%
  filter(area_code %in% authorities$area_code)

# Join both datasets together and tidy ---------------------------
df_epc <- bind_rows(df_epc_england, df_epc_la) %>%
  separate(quarter, into = c("year", "quarter"), sep = "/") %>%
  rename(value_certificates_lodged = number_lodgements,
         value_rating_A = a,
         value_rating_B = b,
         value_rating_C = c) %>%
  mutate(period = ymd(paste0(year, "-", case_when(quarter == "1" ~ "03-31",
                                                  quarter == "2" ~ "06-30",
                                                  quarter == "3" ~ "09-30",
                                                  quarter == "4" ~ "12-31"))),
         indicator = "Domestic Energy Performance Certificates (EPC) lodged on the Buildings Register",
         measure = "Frequency",
         unit = "Certificates") %>%
  select(area_code, area_name, period, indicator, measure, unit, value_certificates_lodged, value_rating_A, value_rating_B, value_rating_C)

# Export the tidied data ---------------------------
write_csv(df_epc, "../energy_performance_certificates.csv")

# Remove downloaded data
unlink(tmp)
