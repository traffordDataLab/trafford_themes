# Active Adults #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/physical-activity
# Licence: Open Government Licence v3.0

library(tidyverse) 

active_adults_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93014&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


active_adults_england <- active_adults_trend  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  filter(Age == "19+ yrs") %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

active_adults_districsts <- active_adults_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Percentage")

df <- bind_rows(active_adults_england, active_adults_districsts) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../active_adults.csv")
