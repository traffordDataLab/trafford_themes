# Adults with depression #

# Source: PHE Fingertips
# URL: http://fingertips.phe.org.uk/common-mental-disorders
# Licence: Open Government Licence v3.0

library(tidyverse) 

adults_depression_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=848&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


adults_depression_england <- adults_depression_trend  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  filter(Age == "18+ yrs") %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

adults_depression_districsts <- adults_depression_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Percentage")

df <- bind_rows(adults_depression_england, adults_depression_districsts) %>%
  mutate(value = round(value, 1)) %>%
  filter(period %in% c("2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../adults_depression.csv")
