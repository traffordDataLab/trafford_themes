# People receiving an NHS Health Check (cumulative)#

# Source: PHE Fingertips, (OHID), NHS Health Check Programme
# URL: https://fingertips.phe.org.uk/profile/sexualhealth
# Licence: Open Government Licence v3.0

library(tidyverse) 

health_checks_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=91112&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


health_checks_england <- health_checks_trend  %>%
  filter(`area_code` == "E92000001") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Cumulative percentage from eligible",
         area_type = "Country")

health_checks_districts <- health_checks_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Cumulative percentage from eligible",
         value = round(value,1))


df <- bind_rows(health_checks_england, health_checks_districts) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../nhs_health_checks.csv")
