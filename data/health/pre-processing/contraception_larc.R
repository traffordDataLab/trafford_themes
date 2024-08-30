# Total prescribed LARC excluding injections rate / 1,000 #

# Source: PHE Fingertips
# URL: https://fingertips.phe.org.uk/profile/sexualhealth
# Licence: Open Government Licence v3.0

library(tidyverse) 

contraception_larc_trend <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=92254&area_type_id=101") %>%
  select(area_code = `Area Code`, area_name = `Area Name`, period = `Time period`, value = Value, indicator = `Indicator Name`, unit = Sex, compared_to_England = `Compared to England value or percentiles`, inequality = Category, `Category Type`, `Area Type`, Age)


contraception_larc_england <- contraception_larc_trend  %>%
  filter(`area_code` == "E92000001") %>%
  filter(is.na(`Category Type`)) %>%
  select(-c(`Category Type`, `Area Type`, Age)) %>%
  mutate(measure = "Per 1,000",
         area_type = "Country")

contraception_larc_districts <- contraception_larc_trend %>%
  filter(`Area Type` %in% c("UA", "District")) %>%
  rename(area_type = `Area Type`) %>%
  select(-c(`Category Type`, Age)) %>%
  mutate(measure = "Per 1,000",
         value = round(value,1))


df <- bind_rows(contraception_larc_england, contraception_larc_districts) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../contraception_larc.csv")
