# Pupils reaching the expected standard at the end of key stage 2 in reading, writing and mathematics
# Created: 2022-02-24

# Source: Department for Education. Data obtained via the following:
# LG Inform+ (Requires API key)
# https://developertools.esd.org.uk/data?value.valueType=raw&metricType=6080&area=E92000001%2CE08000009%2CNearNeighboursChildrenServices_Trafford&period=latest%3A4&rowGrouping=area


# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and its Children's Services nearest neighbours:
authorities <- read_csv("../../cssn.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
api_key <- ""

# Download and tidy the data ---------------------------

df_ks2_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=6080&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df_ks2 <- df_ks2_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         indicator = "Pupils reaching the expected standard at the end of key stage 2 in reading, writing and mathematics",
         value = as.numeric(value),
         period = str_replace(period, "\\s\\(academic\\)", "")) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_ks2, "../expected_standard_ks2.csv")
