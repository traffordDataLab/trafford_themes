# Employment Rate#

# Source: Annual Population Survey, Office for National Statistics
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=17
# Licence: Open Government Licence v3.0

library(tidyverse) 

bm <- read_csv("../../cipfalga0724.csv") %>%
  select(area_code)

df  <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=",paste(c("E92000001",bm$area_code, "E08000009"), collapse = ','),"&date=latest-latestMINUS13&variable=45&measures=20599,21001,21002,21003")) %>%
  filter(MEASURES_NAME == "Variable") %>%
  mutate(measure = "Percentage",
         unit = "Persons") %>% 
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         indicator = VARIABLE_NAME,
         period = DATE_NAME, measure, unit,
         value = OBS_VALUE) %>%
  unique()


write_csv(df, "../employment_rate.csv")
