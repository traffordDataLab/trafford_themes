# Healthy life expectancy at birth #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) 

healthy_life_expectancy_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90362&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, compared_to_England = `Compared to England value or percentiles`, inequality = Sex, `Category Type`, `Area Type`)


healthy_life_expectancy_england <- healthy_life_expectancy_trend %>%
  filter(`area_code` == "E92000001") %>% 
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(area_type = "Country")

healthy_life_expectancy_districsts <- healthy_life_expectancy_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`))

df <- bind_rows(healthy_life_expectancy_england, healthy_life_expectancy_districsts) %>%
  filter(period != "2009 - 11") %>%
  mutate(value = round(value, 1),
         indicator = "Healthy life expectancy at birth",
         measure = "Life expectancy",
         unit = "Years") %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../healthy_life_expectancy.csv")
