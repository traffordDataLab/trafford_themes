# Children Poverty #

# Source: DWP
# URL: https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-2014-to-2021
# Licence: Open Government Licence

library(httr) ; library (tidyverse) ; library (jsonlite)

cssn <- read_csv("../../cssn.csv") %>%
  select(area_code) %>%
  mutate(for_query = paste0("UTLA_TO_REGION_NI:",area_code)) %>%
  bind_rows(data.frame (area_code  = c("E08000009","E92000001"),
                        for_query = c("UTLA_TO_REGION_NI:E08000009","COUNTRY_TO_UK:E92000001")))

# Source:Mid-2021 population estimates for local authorities in England
# Source: Census 2021 for wards
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: Open Government Licence v3.0

pop_ward <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2027_1.data.csv?date=latest&geography=641728593...641728607,641728609,641728608,641728610...641728613&c2021_age_102=1001...1003&measures=20100") %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop0_15 = OBS_VALUE,
         year_pop = DATE) %>%
  group_by(area_code,area_name,year_pop) %>%
  summarise(pop0_15 = sum(pop0_15)) %>%
  ungroup() %>%
  mutate(area_name = sub(" \\(.*", "", area_name))

population <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",paste(c(cssn$area_code), collapse = ','),"&date=latestMINUS6-latest&gender=0&c_age=201&measures=20100")) %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop0_15 = OBS_VALUE,
         year_pop = DATE) %>%
  bind_rows(pop_ward)


codes <- pop_ward %>% 
  select(area_code) %>%
  mutate(for_query = paste0("WARD_TO_LA_NI:",area_code)) %>%
  bind_rows(cssn) 


api_key <- ""

# API endpoint
path <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"

# Children in relative low income families #
# Source: DWP
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/CILIF_REL/CILIF_REL.html
# Licence: Open Government Licence

query <- list(database = unbox("str:database:CILIF_REL"),
              measures = "str:count:CILIF_REL:V_F_CILIF_REL",
              dimensions = c("str:field:CILIF_REL:V_F_CILIF_REL:WARD_CODE",
                             "str:field:CILIF_REL:F_CILIF_DATE:DATE_NAME",
                             "str:field:CILIF_REL:V_F_CILIF_REL:CHILD_AGE") %>% matrix(),
              recodes = list(
                `str:field:CILIF_REL:V_F_CILIF_REL:WARD_CODE` = list(
                  map = as.list(paste0("str:value:CILIF_REL:V_F_CILIF_REL:WARD_CODE:V_C_MASTERGEOG11_", c(codes$for_query)))),
                `str:field:CILIF_REL:V_F_CILIF_REL:CHILD_AGE` = list(
                  map = list(paste0("str:value:CILIF_REL:V_F_CILIF_REL:CHILD_AGE:C_CILIF_AGE_BAND:",c(1:3))))
              )) %>% toJSON()
request <- POST(
  url = path,
  body = query,
  config = add_headers(APIKey = api_key),
  encode = "json")
response <- fromJSON(content(request, as = "text"), flatten = TRUE)
# extract list items and convert to a dataframe
tabnames <- response$fields$items %>% map(~.$labels %>% unlist)
values <- response$cubes[[1]]$values
dimnames(values) <- tabnames[1:2]

df <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(c(response$fields$label,"value")) %>%
  select(area_name = "National - Regional - LA - Wards",period = Year,value) %>%
  mutate(year_pop = as.numeric( str_sub(period, end = 4))) %>%
  left_join(population, by = c("area_name", "year_pop")) %>%
  filter( !is.na(area_code)) %>%
  mutate(value = round(value*100/pop0_15,1),
         indicator = "Children in relative low income families (under 16s)",
         measure = "Percentage",
         unit = "Persons") %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Children in absolute low income families #
# Source: DWP
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/CILIF_ABS/CILIF_ABS.html
# Licence: Open Government Licence

query2 <- list(database = unbox("str:database:CILIF_ABS"),
               measures = "str:count:CILIF_ABS:V_F_CILIF_ABS",
               dimensions = c("str:field:CILIF_ABS:V_F_CILIF_ABS:WARD_CODE",
                              "str:field:CILIF_ABS:F_CILIF_DATE:DATE_NAME",
                              "str:field:CILIF_ABS:V_F_CILIF_ABS:CHILD_AGE") %>% matrix(),
               recodes = list(
                 `str:field:CILIF_ABS:V_F_CILIF_ABS:WARD_CODE` = list(
                   map = as.list(paste0("str:value:CILIF_ABS:V_F_CILIF_ABS:WARD_CODE:V_C_MASTERGEOG11_", c(cssn$for_query)))),
                 `str:field:CILIF_ABS:V_F_CILIF_ABS:CHILD_AGE` = list(
                   map = list(paste0("str:value:CILIF_ABS:V_F_CILIF_ABS:CHILD_AGE:C_CILIF_AGE_BAND:",c(1:3))))
               )) %>% toJSON()
request <- POST(
  url = path,
  body = query2,
  config = add_headers(APIKey = api_key),
  encode = "json")
response <- fromJSON(content(request, as = "text"), flatten = TRUE)
# extract list items and convert to a dataframe
tabnames <- response$fields$items %>% map(~.$labels %>% unlist)
values <- response$cubes[[1]]$values
dimnames(values) <- tabnames[1:2]

df2 <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(c(response$fields$label,"value")) %>%
  select(area_name = "National - Regional - LA - Wards",period = Year,value) %>%
  mutate(year_pop = as.numeric( str_sub(period, end = 4))) %>%
  left_join(population, by = c("area_name", "year_pop")) %>%
  filter( !is.na(area_code)) %>%
  mutate(value = round(value*100/pop0_15,1),
         indicator = "Children in absolute low income families (under 16s)",
         measure = "Percentage",
         unit = "Persons") %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

write_csv(bind_rows(df,df2), "../children_poverty.csv")

