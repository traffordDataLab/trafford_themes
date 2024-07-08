# Fairly active Adults #

# Source: LG inform
# URL: http://id.esd.org.uk/metricType/10270
# Licence: Open Government Licence v3.0

library(tidyverse) 

lginform_key = ""

cipfa <- read_csv("../../cipfa2021.csv") %>%
  select(area_code)

fairly_active_adults <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=10270&area=",paste(c("E92000001",cipfa$area_code, "E08000009"), collapse = ','),"&period=latest:6&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key)) %>%
  filter(area != "area") %>%
  pivot_longer("2017":"2022", names_to = 'period', values_to = 'value') %>%
  select(area_name = `area label`, area_code = area, period, value) %>%
  mutate(indicator = "Percentage of adults aged 16+ who are fairly active",
         measure = "Percentage", unit = "Persons", compared_to_England = NA_character_, inequality = NA_character_,
         area_type = ifelse(area_name == "England", "Country", "UA"),
         value = as.double(value)) %>%
  mutate(value = round(value, 1)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(fairly_active_adults, "../fairly_active_adults.csv")
