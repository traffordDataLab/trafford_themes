# Adults classified as overweight or obese #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) 

overweight_adult_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93088&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


overweight_adult_england <- overweight_adult_trend %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  filter(Age == "18+ yrs") %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

overweight_adult_districsts <- overweight_adult_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Percentage")

df <- bind_rows(overweight_adult_england, overweight_adult_districsts) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../overweight_adult.csv")
