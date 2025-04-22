# Business births and deaths 

# Source: Annual Survey of Hours and Earnings, ONS
# URL: https://www.ons.gov.uk/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyquarterlyexperimentalstatisticslowlevelgeographicbreakdownuk
# Licence: Open Government Licence


# Load required packages ---------------------------

library(tidyverse) ; library(readxl)

bm <- read_csv("../../cipfalga0724.csv") %>%
  select(area_code)

population  <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",paste(c("E92000001",bm$area_code, "E08000009"), collapse = ','),"&date=latestMINUS6-latest&gender=0&c_age=200&measures=20100")) %>%
  select(year = DATE_NAME, area_code = GEOGRAPHY, pop = OBS_VALUE) 
  

tmp <- tempfile(fileext = ".xlsx")


GET(url = "https://www.ons.gov.uk/file?uri=/businessindustryandtrade/business/activitysizeandlocation/datasets/businessdemographyquarterlyexperimentalstatisticslowlevelgeographicbreakdownuk/quarter4octobertodecember2024/finalq42024lowlevelgeobreakdown.xlsx",
    write_disk(tmp))

sheets <- excel_sheets(tmp) 

df <- set_names(sheets[3:12]) %>%
  map_df(~ read_xlsx(path = tmp, sheet = .x, skip = 3), .id = "sheet") %>%
  mutate(indicator = sub(" .*", "", sheet)) %>%
  mutate(area_code = sub(" : .*", "", Geography), area_name = sub(".* : ", "", Geography)) %>%
  select(-sheet,-Geography) %>%
  pivot_longer(`Q4 2017`:`Q4 2024`, names_to = "period", values_to = "Count") %>%
  filter(!is.na(Count)) %>%
  mutate(year = as.numeric( sub(".* ", "", period))) %>%
  filter(area_code %in% c("E92000001",bm$area_code, "E08000009")) %>%
  left_join(population, by = c("area_code" = "area_code", "year" = "year")) %>%
  arrange(area_code) %>%
  fill(pop) %>%
  mutate(`Per 1000 population` = round((Count/pop)*1000, 2)) %>%
  mutate(indicator = paste0("Business ", indicator)) %>%
  select(area_code,area_name,period,indicator,Count,`Per 1000 population`) %>%
  pivot_longer(c(Count,`Per 1000 population`), names_to = "measure", values_to = "value")

# Export the tidied data ---------------------------
write_csv(df, "../business_births_deaths.csv")

