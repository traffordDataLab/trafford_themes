# Children cautioned or sentenced #

# Source: Youth Justice Board for England and Wales
# URL:  https://www.gov.uk/government/collections/youth-justice-statistics
# Licence: Open Government Licence v3.0

library(tidyverse) ; library(readODS)

url <- "https://assets.publishing.service.gov.uk/media/679a05a01c041dcc469daea9/Local_level_open_data_tables.zip"
download.file(url, dest = "Local_level_open_data_tables.zip")
unzip("Local_level_open_data_tables.zip")
file.remove("Local_level_open_data_tables.zip")

authorities <- read_csv("../../cssn.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Source:Mid-year population estimates for local authorities in England
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: Open Government Licence v3.0

population <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",paste(c(authorities$area_code), collapse = ','),"&date=latestMINUS9-latest&gender=0&c_age=4,116...118&measures=20100")) %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop_10to17 = OBS_VALUE,
         year_pop = DATE) %>%
  group_by(area_code,area_name,year_pop) %>%
  summarise(pop_10to17 = sum(pop_10to17))

children_offending_raw <- read_ods("Children_Table.ods", sheet = 3)

df_E <- children_offending_raw %>%
  filter(England_Wales == "England") %>%
  group_by(Financial_Year) %>%
  summarise(Count = sum(Number_Children)) %>%
  ungroup() %>%
  mutate(YJS = "England")

df <- children_offending_raw %>%
  filter(YJS %in% authorities$area_name) %>%
  group_by(Financial_Year, YJS) %>%
  summarise(Count = sum(Number_Children)) %>%
  ungroup() %>%
  bind_rows(df_E) %>%
  rename(period = Financial_Year, area_name = YJS) %>%
  mutate(year_pop = as.numeric(sub("[0-9][0-9]-","",period))) %>%
  left_join(population, by = c("area_name", "year_pop")) %>%
  group_by(area_name) %>%
  fill(c(pop_10to17, area_code), .direction = "down") %>%
  mutate(`Per 10,000 aged 10-17` = round(Count/pop_10to17*10000)) %>%
  select(-year_pop, -pop_10to17) %>%
  pivot_longer(c(Count,`Per 10,000 aged 10-17`), names_to = "measure", values_to = "value") %>%
mutate(indicator = "Children cautioned or sentenced",
       unit = "Children", compared_to_England = NA_character_, inequality = NA_character_,
       area_type = ifelse(area_name == "England", "Country", "UA"),
       value = as.double(value),
       period = sub("-", "/",period)) %>%
  select(area_code, area_name, area_type, period, indicator, measure, unit, value, compared_to_England, inequality)

write_csv(df, "../children_offending.csv")
