# Percentage of all non-major planning applications decided in time

# Source: Ministry of Housing, Communities and Local Government
# URL: https://www.gov.uk/government/statistical-data-sets/live-tables-on-planning-application-statistics
# Licence: Open Government Licence


# Load required packages ---------------------------
library(tidyverse); library(readODS)

# Setup objects ---------------------------
# Trafford and CIPFA nearest neighbours:
authorities <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download and tidy the data ---------------------------

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=17532&area=",paste(c(authorities$area_code), collapse = ','),"&period=latest:7&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df <- df_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Applications",
         indicator = "Percentage of all non-major development planning applications decided in time",
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../planning_applications_non-major.csv")

