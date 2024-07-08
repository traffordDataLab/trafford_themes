# School readiness: percentage of children achieving a good level of development at the end of Reception
# Created: 2022-02-23

# Source: Department for Education. Data obtained via PHE Fingertips
#         https://fingertips.phe.org.uk/profile/child-health-profiles

# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and its Children's Services nearest neighbours:
authorities <- read_csv("../../cssn.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download and tidy the data ---------------------------
df_pupils_raw <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90631&area_type_id=101")

df_pupils <- df_pupils_raw %>%
  select(area_code = `Area Code`,
         area_name = `Area Name`,
         period = `Time period`,
         value = Value,
         indicator = `Indicator Name`,
         unit = Sex,
         Category) %>%
  filter(unit == "Persons",
         area_code %in% authorities$area_code,
         is.na(Category)) %>%
  mutate(measure = "Percentage",
         value = round(value, 1),
         period = as.character(period)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


# Download and tidy the data - with Free School Meal status (FSM) ---------------------------
df_pupils_raw <- read_csv("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=90632&area_type_id=101")

df_pupils_fsm <- df_pupils_raw %>%
  select(area_code = `Area Code`,
         area_name = `Area Name`,
         period = `Time period`,
         value = Value,
         indicator = `Indicator Name`,
         unit = Sex,
         Category) %>%
  filter(unit == "Persons",
         area_code %in% authorities$area_code,
         is.na(Category)) %>%
  mutate(measure = "Percentage",
         value = round(value, 1),
         period = as.character(period)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


# Combine the datasets ---------------------------
df_pupils <- bind_rows(df_pupils, df_pupils_fsm) %>%
  mutate(area_name = if_else(area_name == "Buckinghamshire UA", "Buckinghamshire", area_name))

# Export the tidied data ---------------------------
write_csv(df_pupils, "../school_readiness.csv")
