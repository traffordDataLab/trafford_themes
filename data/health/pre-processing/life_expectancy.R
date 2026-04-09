# Life expectancy at birth #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) 

life_expectancy_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90366&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, compared_to_England = `Compared to England value or percentiles`, inequality = Sex, `Category Type`, `Area Type`)


life_expectancy_england <- life_expectancy_trend %>%
  filter(`area_code` == "E92000001") %>% 
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(area_type = "Country")

life_expectancy_districsts <- life_expectancy_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  filter(is.na(`Category Type`)) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`))

df <- bind_rows(life_expectancy_england, life_expectancy_districsts) %>%
  filter(grepl("-", period)) %>%
  filter(period > "2014 - 16") %>%
  mutate(value = round(value, 1),
         indicator = "Life expectancy at birth",
         measure = "Life expectancy",
         unit = "Years") %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../life_expectancy.csv")
