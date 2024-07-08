# 10-11 year old children classified as obese #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/public-health-outcomes-framework
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(sf)

cssn <- read_csv("../../cssn.csv") %>%
  select(area_code)

obese_year6_quintiles <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=92033&area_type_id=101") %>%
  filter(`Area Code` %in% c("E08000009",cssn$area_code,"E92000001"), Sex == "Persons", `Time period` == "2018/19 - 22/23", `Category Type` == "LSOA11 deprivation quintiles within area (IMD  trend)") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, area_type = `Area Type`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category) %>%
  mutate(measure = "Percentage",
         area_type = if_else(area_type=="England","Country",area_type))


obese_year6_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90323&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`)


obese_year6_england <- obese_year6_trend %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

obese_year6_districsts <- obese_year6_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Percentage")

obese_year6_cssn <- obese_year6_trend %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`)) %>%
  mutate(measure = "Percentage")

lookup <- read_csv("https://www.trafforddatalab.io/spatial_data/lookups/administrative_lookup.csv") %>%
  filter(lad17nm == "Trafford")

obese_year6_wards <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=93107&area_type_id=101") %>%
  filter(`Area Code` %in% lookup$wd17cd) %>%
  select(area_code = `Area Code`, area_name = `Area Name`, area_type = `Area Type`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category) %>%
  mutate(measure = "Percentage") %>%
  filter(period == "2020/21 - 22/23")


df <- bind_rows(obese_year6_quintiles, obese_year6_england, obese_year6_districsts, obese_year6_cssn, obese_year6_wards) %>%
  mutate(value = round(value, 1)) %>%
  filter(!period %in% c("2006/07", "2007/08", "2008/09", "2009/10")) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../obese_year6.csv")
