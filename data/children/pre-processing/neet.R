# 16-17  year olds not in education, employment or training (NEET). NEET and not known
# Created: 2022-02-16

# Source: Department for Education. Data obtained via the following:
#LG Inform+ (Requires API key)
#                 https://developertools.esd.org.uk/data?value.valueType=raw&metricType=9613&


# Load required packages ---------------------------
library(tidyverse) ; library(httr) ; library(readxl)

# Setup objects ---------------------------
# Trafford and its cssn neighgours:
authorities <- read_csv("../../cssn.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data from LG Inform+: NEET and unknown combined ---------------------------
# NOTE: this requires an API key
lginform_key <- ""

df_neet_and_unknown_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=9612&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:7&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy data ---------------------------
df_neet_and_unknown <- df_neet_and_unknown_raw %>%
  select(area_code = area,
         area_name = `area label`,
         everything(),
         -`area long label`) %>%
  filter(area_code %in% authorities$area_code) %>%
  pivot_longer(-c(area_code,area_name), names_to = "period", values_to = "value") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         indicator = "Not in education, employment or training (NEET)(inc not know)(16-17 years old)",
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


# Download the data NEET only ---------------------------
df_neet_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=9613&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:7&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))


# Tidy the LG Inform+ data ---------------------------
df_neet <- df_neet_raw %>%
  select(area_code = area,
         area_name = `area label`,
         everything(),
         -`area long label`) %>%
  filter(area_code %in% authorities$area_code) %>%
  pivot_longer(-c(area_code,area_name), names_to = "period", values_to = "value") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         indicator = "Not in education, employment or training (NEET)(16-17 years old)",
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


# Combine the datasets ---------------------------
df_neet <- bind_rows(df_neet_and_unknown, df_neet) %>%
arrange(indicator,period,area_code)
  
# Export the tidied data ---------------------------
write_csv(df_neet, "../neet.csv")
