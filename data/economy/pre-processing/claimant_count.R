# Claimant Count rate #
# Source: ONS
# URL: https://www.nomisweb.co.uk/sources/cc
# Licence: Open Government Licence

library(tidyverse)

cipfa <- read_csv("../../cipfa2021.csv") %>%
  select(area_code) 

df <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=",paste(c("E92000001",cipfa$area_code, "E08000009"), collapse = ','),"&date=latestMINUS36-latest&gender=0&age=0&measure=1,2&measures=20100")) %>%
  mutate(units = "Persons",
         measure = ifelse(MEASURE_NAME == "Claimant count", "Count", "Percentage")) %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         period = DATE_NAME,
         indicator = MEASURE_NAME, measure, units,
         value = OBS_VALUE) %>%
  unique()

df_wards <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1648362073...1648362087,1648362089,1648362088,1648362090...1648362093&date=latest&gender=0&age=0&measure=1,2&measures=20100") %>%
  mutate(units = "Persons",
         measure = ifelse(MEASURE_NAME == "Claimant count", "Count", "Percentage")) %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         period = DATE_NAME,
         indicator = MEASURE_NAME, measure, units,
         value = OBS_VALUE)


write_csv(bind_rows(df, df_wards), "../claimant_count.csv")
