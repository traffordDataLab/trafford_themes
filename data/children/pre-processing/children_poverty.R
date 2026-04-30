# Children Poverty #

# Source: DWP
# URL: https://www.gov.uk/government/statistics/children-in-low-income-families-local-area-statistics-2014-to-2021
# Licence: Open Government Licence

library(httr) ; library (tidyverse) ; library (jsonlite)

# Setup objects ---------------------------
# Trafford and its Children's Services nearest neighbours:
authorities <- read_csv("../../cssn.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
lginform_key <- ""

# Download and tidy the data ---------------------------

df_raw1 <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=15369&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df1 <- df_raw1 %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         indicator = "% of children living in relative low income families: Aged 0-15",
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

df_raw2 <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=15368&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df2 <- df_raw2 %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Persons",
         indicator = "% of children living in absolute low income families: Aged 0-15",
         value = as.numeric(value)) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)




# Source:Mid-2021 population estimates for local authorities in England
# Source: Census 2021 for wards
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: Open Government Licence v3.0

#pop_ward <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2027_1.data.csv?date=latest&geography=641728593...641728607,641728609,641728608,641728610...641728613&c2021_age_102=1001...1003&measures=20100") %>%
pop_ward <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2014_1.data.csv?geography=763369116...763369136&date=latest&gender=0&c_age=201&measures=20100") %>%
  select(area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         pop0_15 = OBS_VALUE,
         year_pop = DATE) %>%
  group_by(area_code,area_name,year_pop) %>%
  summarise(pop0_15 = sum(pop0_15)) %>%
  ungroup() %>%
  mutate(area_name = sub(" \\(.*", "", area_name))


codes <- pop_ward %>% 
  select(area_code) %>%
  mutate(for_query = paste0("WARD_TO_LA_NI:",area_code))


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
                  map = as.list(paste0("str:value:CILIF_REL:V_F_CILIF_REL:WARD_CODE:V_C_MASTERGEOG21_", c(codes$for_query)))),
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

df3 <- as.data.frame.table(values, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  set_names(c(response$fields$label,"value")) %>%
  select(area_name = "National - Regional - LA - Wards",period = Year,value) %>%
  mutate(year_pop = as.numeric( str_sub(period, end = 4))) %>%
  left_join(pop_ward, by = c("area_name", "year_pop")) %>%
  filter( !is.na(area_code)) %>%
  mutate(value = round(value*100/pop0_15,1),
         indicator = "Children in relative low income families (under 16s)",
         measure = "Percentage",
         unit = "Persons") %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


write_csv(bind_rows(df1,df2,df3), "../children_poverty.csv")

