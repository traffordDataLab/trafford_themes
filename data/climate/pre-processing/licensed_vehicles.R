# Licensed Vehicles.
# Created: 2021-11-29, last update: 2023-12-12 (data: 2023-09-30)

# Source: Department for Transport (DfT) & Driver and Vehicle Licensing Authority (DVLA)
#         https://www.gov.uk/government/statistical-data-sets/all-vehicles-veh01
# All Vehicles by body type:       https://assets.publishing.service.gov.uk/media/657335bf33b7f2000db72125/veh0105.ods
# All Ultra Low Emission Vehicles: https://assets.publishing.service.gov.uk/media/65733542049516000f49bf18/veh0132.ods


# Load required packages ---------------------------
library(tidyverse); library(readxl); library(lubridate); library(janitor)

# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2021):
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the datasets for all vehicles and ULEV ---------------------------
download.file("https://assets.publishing.service.gov.uk/media/657335bf33b7f2000db72125/veh0105.ods", "veh0105.ods")
download.file("https://assets.publishing.service.gov.uk/media/65733542049516000f49bf18/veh0132.ods", "veh0132.ods")

# These files are too large to process using readODS or tidyODS so we need to convert them to XLSX.
# Use LibreOffice in its headless state (doesn't open the application window) via the terminal.
# LibreOffice needs to be installed and the path to the application is different depending on the OS.
# For Windows try: libreoffice --headless --convert-to xlsx veh0105.ods.
# The following works on Mac and should work on similar *nix-based OSes.
#cd data/climate/pre-processing/
#/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to xlsx veh0105.ods
#/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to xlsx veh0132.ods

# Cleanup the downloaded ODS
if (file.exists("veh0105.ods")) file.remove("veh0105.ods")
if (file.exists("veh0132.ods")) file.remove("veh0132.ods")

# Process the XLSX data for all vehicles ---------------------------
df_raw <- read_xlsx(path = "veh0105.xlsx", sheet = "VEH0105", skip = 4)

df_all_vehicles <- df_raw %>%
  clean_names() %>% # Tidy up the variable names with Janitor
  rename_with(~str_remove(., "_note_[0-9]")) %>% # get rid of the all the note references
  filter(body_type == "Total",
         fuel == "Total",
         keepership == "Total",
         ons_code %in% authorities$area_code) %>%
  # This looks very bad - magic numbers rather than naming variables. However, we want 3 years-worth
  # of quarterly results and each successive quarter will be added at column position 8, pushing older
  # years to the right.  We also don't need the first 5 columns anymore, so this seems a succinct and
  # reproducible way to achieve it.
  select(6:19) %>%
  pivot_longer(c(-ons_code, -ons_geography),
               names_to = "period",
               values_to = "value_all_vehicles") %>%
  mutate(value_all_vehicles = as.numeric(value_all_vehicles) * 1000) # first column in raw data was units in thousands


# Process the XLSX data for ultra low emission vehicles ---------------------------
df_raw <- read_xlsx(path = "veh0132.xlsx", sheet = "VEH0132", skip = 4)

df_ulev <- df_raw %>%
  clean_names() %>% # Tidy up the variable names with Janitor
  rename_with(~str_remove(., "_note_[0-9]")) %>% # get rid of the all the note references
  filter(fuel == "Total",
         keepership == "Total",
         ons_code %in% authorities$area_code) %>%
  # This looks very bad - magic numbers rather than naming variables. However, we want 3 years-worth
  # of quarterly results and each successive quarter will be added at column position 7, pushing older
  # years to the right.  We also don't need the first 4 columns anymore, so this seems a succinct and
  # reproducible way to achieve it.
  select(5:18) %>%
  pivot_longer(c(-ons_code, -ons_geography),
               names_to = "period",
               values_to = "value_ulev") %>%
  mutate(value_ulev = as.numeric(value_ulev))


# Now combine the ulev and all vehicles datasets ---------------------------
df_licensed_vehicles <- df_ulev %>%
  left_join(df_all_vehicles, by = c("ons_code", "period")) %>%
  separate(period, into = c("year", "quarter"), sep = "_") %>% # Need to format the period correctly, turning the year and quarter number into an actual date
  mutate(period = ymd(paste0(str_remove(year, "x"), "-", case_when(quarter == "q1" ~ "03-31",
                                                                   quarter == "q2" ~ "06-30",
                                                                   quarter == "q3" ~ "09-30",
                                                                   quarter == "q4" ~ "12-31"))),
         indicator = "Licensed vehicles - ultra low emission vehicles (ulev) and all vehicles including ulev",
         measure = "Frequency",
         unit = "Vehicles") %>%
  arrange(period, ons_geography.x) %>%
  select(area_code = ons_code,
         area_name = ons_geography.x,
         period,
         indicator,
         measure,
         unit,
         value_ulev,
         value_all_vehicles)


# Write out the data and cleanup the converted XLSX ---------------------------
write_csv(df_licensed_vehicles, "../licensed_vehicles.csv")

if (file.exists("veh0105.xlsx")) file.remove("veh0105.xlsx")
if (file.exists("veh0132.xlsx")) file.remove("veh0132.xlsx")
