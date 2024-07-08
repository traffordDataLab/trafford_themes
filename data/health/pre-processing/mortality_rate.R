# Under 75 mortality rate from causes considered preventable #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/mortality-profile
# Licence: Open Government Licence v3.0

library(tidyverse)

mortality_rate_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93721&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`)

mortality_rate_england <- mortality_rate_trend %>%
  filter(`area_code` == "E92000001") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(measure = "Age-standardised rate",
         area_type = "Country")

mortality_rate_districsts <- mortality_rate_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Age-standardised rate")

df <- bind_rows(mortality_rate_england, mortality_rate_districsts) %>%
  filter(period %in% c("2010":"2022")) %>%
  mutate(value = round(value, 1),
         indicator = "Under 75 mortality rate from causes considered preventable (per 100,000 population)") %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../mortality_rate.csv")
