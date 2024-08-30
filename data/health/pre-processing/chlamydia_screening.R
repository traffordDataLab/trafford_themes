# Chlamydia proportion of females aged 15 to 24 screened #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/sexualhealth
# Licence: Open Government Licence v3.0

# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and NHS peer group:
authorities <- read_csv("../../nhsennpg.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
lginform_key <- ""

# Download and tidy the data ---------------------------

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=8166&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

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
         unit = "Females",
         indicator = "Chlamydia proportion aged 15-24 screened",
         value = as.numeric(value),
         period = period) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../chlamydia_screening.csv")
