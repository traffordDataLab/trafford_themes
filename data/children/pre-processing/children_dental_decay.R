# Children with Dental Decay#

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) 

children_dental_decay <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93563&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


children_dental_decay_england <- children_dental_decay  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

children_dental_decay_districsts <- children_dental_decay %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Percentage")

cssn <- read_csv("../../cssn.csv") %>%
  select(area_code)

children_dental_decay_cssn <- children_dental_decay %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Percentage")

df <- bind_rows(children_dental_decay_england, children_dental_decay_districsts, children_dental_decay_cssn) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  filter(period %in% c("2014/15", "2016/17", "2018/19", "2021/22")) %>%
  mutate(unit = "5 years children") %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../children_dental_decay.csv")

