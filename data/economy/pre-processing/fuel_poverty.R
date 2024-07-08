# Reduce % of households fuel poverty levels #

# Source: Department for Business, Energy & Industrial Strategy
# URL: https://www.gov.uk/government/collections/fuel-poverty-sub-regional-statistics
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(readxl) ; library(httr) 

lginform_key = ""

cipfa <- read_csv("../../cipfa2021.csv") %>%
  select(area_code)

fuel_poverty_LIHC <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=2131&area=",paste(c("E92000001",cipfa$area_code, "E08000009"), collapse = ','),"&period=latest:5&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key)) %>%
  filter(area != "area") %>%
  pivot_longer("2014":"2018", names_to = 'period', values_to = 'value') %>%
  select(area_name = `area label`, area_code = area, period, value) %>%
  mutate(indicator = "Fuel poverty (low income, high cost methodology)",
         measure = "Percentage", unit = "Persons", compared_to_England = NA_character_, inequality = NA_character_,
         area_type = ifelse(area_name == "England", "Country", "UA"),
         value = as.double(value)) %>%
  mutate(value = round(value, 1),
         period = as.numeric(period)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

fuel_poverty_LILEE <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=16259&area=",paste(c("E92000001",cipfa$area_code, "E08000009"), collapse = ','),"&period=latest:5&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key)) %>%
  filter(area != "area") %>%
  pivot_longer("2019":"2022", names_to = 'period', values_to = 'value') %>%
  select(area_name = `area label`, area_code = area, period, value) %>%
  mutate(indicator = "Fuel poverty (low income, low energy efficiency methodology)",
         measure = "Percentage", unit = "Persons", compared_to_England = NA_character_, inequality = NA_character_,
         area_type = ifelse(area_name == "England", "Country", "UA"),
         value = as.double(value)) %>%
  mutate(value = round(value, 1),
         period = as.numeric(period)) %>%
  unique() %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)


df <- bind_rows(fuel_poverty_LIHC, fuel_poverty_LILEE) %>%
  mutate(value = round(value, 1)) %>%
  mutate(measure = "Percentage", unit = "Households") %>%
  select(area_code, area_name, period, indicator, measure, unit, value) %>%
  arrange(period)

write_csv(df, "../fuel_poverty.csv")
