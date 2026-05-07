# Proportion of people claiming Universal Credit #
# Source: DWP
# URL: https://stat-xplore.dwp.gov.uk/webapi/metadata/UC_Monthly/UC_Monthly.html
# Licence: Open Government Licence

library(httr) ; library (tidyverse) ; library (jsonlite) ; library (zoo)


# Source:Mid-2020 population estimates for local authorities in England
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: Open Government Licence v3.0

bm <- read_csv("../../cipfalga0724.csv") %>%
  select(area_code) %>%
  mutate(for_query = paste0("LA_TO_REGION:",area_code)) %>%
  bind_rows(data.frame (area_code  = c("E08000009","E92000001"),
                        for_query = c("LA_TO_REGION:E08000009","COUNTRY_TO_GB:E92000001")
  ))

population <- read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",paste(c(bm$area_code), collapse = ','),"&date=latestMINUS6-latest&gender=0&c_age=203&measures=20100")) %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop16_64 = OBS_VALUE,
         year = DATE) %>%
  mutate(year = as.character(year))

api_key <- ""

# API endpoint
path <- "https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table"

query <- list(database = unbox("str:database:UC_Monthly"),
              measures = "str:count:UC_Monthly:V_F_UC_CASELOAD_FULL",
              dimensions = c("str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:COA_CODE",
                             "str:field:UC_Monthly:F_UC_DATE:DATE_NAME") %>% matrix(),
              recodes = list(
                `str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:COA_CODE` = list(
                  map = as.list(paste0("str:value:UC_Monthly:V_F_UC_CASELOAD_FULL:COA_CODE:V_C_MASTERGEOG21_", c(bm$for_query)))),
                `str:field:UC_Monthly:F_UC_DATE:DATE_NAME` = list(
                  map = as.list(paste0("str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:",c(202303,202304,202305,202306,202307,202308,202309,202310,202311,202312,202401,202402,202403,202404,202405,202406,202407,202408,202409,202410,202411,202412,202501,202502,202503,202504,202505,202506,202507,202508,202509,202510,202511,202512,202601,202602,202603))))
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
dimnames(values) <- tabnames

df <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(c(response$fields$label,"value")) %>%
  select(area_name = "National - Regional - LA - OAs",period = Month,value) %>%
  mutate(year = str_sub(period, start = -4),) %>%
  left_join(population, by = c("area_name", "year")) %>%
  arrange(area_name) %>%
  mutate(area_code = na.locf(area_code),
         pop16_64 = na.locf(pop16_64))

pop_ward <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2014_1.data.csv?geography=763369116...763369136&date=latest&gender=0&c_age=203&measures=20100") %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop16_64 = OBS_VALUE) %>%
  mutate(area_name = gsub(" \\(Trafford\\)","",area_name))


query <- list(database = unbox("str:database:UC_Monthly"),
              measures = "str:count:UC_Monthly:V_F_UC_CASELOAD_FULL",
              dimensions = c("str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:WARD_CODE",
                             "str:field:UC_Monthly:F_UC_DATE:DATE_NAME") %>% matrix(),
              recodes = list(
                `str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:WARD_CODE` = list(
                  map = as.list(paste0("str:value:UC_Monthly:V_F_UC_CASELOAD_FULL:WARD_CODE:V_C_MASTERGEOG21_WARD_TO_LA:E0", seq(5015239, 5015259, 1)))),
                `str:field:UC_Monthly:F_UC_DATE:DATE_NAME` = list(
                  map = as.list(paste0("str:value:UC_Monthly:F_UC_DATE:DATE_NAME:C_UC_DATE:",c(202603))))
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
dimnames(values) <- tabnames

universal_credit_ward <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(c(response$fields$label,"value"))  %>%
  select(area_name = "National - Regional - LA - Ward", period = Month, value) %>%
  left_join(pop_ward, by="area_name") 

df_t <- df %>%
  bind_rows(universal_credit_ward) %>%
  mutate(rate = round((value/pop16_64)*100,1)) %>%
  select(area_code, area_name, period, count = value, rate) %>%
  pivot_longer(c(count,rate), names_to = "measure", values_to = "value") %>%
  mutate(indicator = if_else(measure =="rate","People on Universal Credit as a proportion of residents aged 16-64","People on Universal Credit"), 
         unit ="persons") %>%
  select(area_code, area_name, indicator, period, measure, unit, value)


write_csv(df_t, "../universal_credit.csv")

