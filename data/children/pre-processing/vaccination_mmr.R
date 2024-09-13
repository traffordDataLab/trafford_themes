# Population vaccination coverage - MMR for one dose (2 years old) and MMR for two doses (5 years old)

# Source: Office for Health Improvement and Disparities (OHID).
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence


# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and its Children's Services nearest neighbours:
authorities <- read_csv("../../cssn.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
lginform_key <- ""

# Download and tidy the data ---------------------------

df_raw1 <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=12108&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

df_raw2 <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=12109&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))


# Tidy the LG Inform+ data ---------------------------
df <- df_raw1 %>%
  mutate(indicator = "Population vaccination coverage: MMR for one dose (2 years old)") %>%
  bind_rows(df_raw2 %>% mutate(indicator = "Population vaccination coverage: MMR for two doses (5 years old)")) %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code,-indicator), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         value = as.numeric(value)
         ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../vaccination_mmr.csv")
