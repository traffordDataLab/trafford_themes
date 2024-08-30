# Smoking Prevalence among persons 18 years and over #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/tobacco-control
# Licence: Open Government Licence v3.0

library(tidyverse) 

adults_smoking_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=92443&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


adults_smoking_england <- adults_smoking_trend  %>%
  filter(`area_code` == "E92000001") %>%
  filter(unit == "Persons") %>%
  filter(is.na(`Category Type`)) %>%
  filter(Age == "18+ yrs") %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Percentage",
         area_type = "Country")

adults_smoking_districts <- adults_smoking_trend %>%
  filter(`Area Type` %in% c("UA", "District"), unit == "Persons", Age == "18+ yrs" , is.na(inequality)) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Percentage")


df <- bind_rows(adults_smoking_england, adults_smoking_districts) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../adults_smoking.csv")
