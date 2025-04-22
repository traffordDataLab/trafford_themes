# Proportion of adults who do any walking or cycling, for any purpose, five times per week

# Source: Department for Transport (DfT)
#         https://www.gov.uk/government/statistical-data-sets/walking-and-cycling-statistics-cw#participation-in-walking-and-cycling


# Load required packages ---------------------------
library(tidyverse); library(readODS); library(httr);

# Setup objects ---------------------------
# Trafford and its NHS pair groups :
authorities <- read_csv("../../nhsennpg.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download the data ---------------------------
tmp <- tempfile(fileext = ".ods")
GET(url = "https://assets.publishing.service.gov.uk/media/66ceecef704a0794913a898e/cw0301.ods",
    write_disk(tmp))

df_raw <- read_ods(tmp, sheet = "CW0301", skip = 4)

# Tidy the data ---------------------------
df_wlk_cyc <- df_raw %>%
  rename(area_code = `ONS Code`,
         area_name = `Area name`) %>%
  filter(Frequency == "At least 5 times per week",
         Purpose == "Any",
         area_code %in% authorities$area_code) %>%
  select(-Class, -Mode, -Purpose, -Frequency) %>% # remove unwanted columns
    mutate(`2021` = as.character(`2021`),
           `2022` = as.character(`2022`)) %>% # annoyingly these columns are num when the others are char - need all the same to pivot
  pivot_longer(cols = c(-area_code, -area_name),
               names_to = "period",
               values_to = "value") %>%
  mutate(value = round(as.numeric(value), 1),
         indicator = "Proportion of adults who do any walking or cycling, for any purpose, five times per week",
         measure = "Percentage",
         unit = "Persons") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_wlk_cyc, "../adults_walking_or_cycling.csv")

# delete the downloaded raw data
unlink(tmp)
