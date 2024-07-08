# Nitrogen Dioxide (NO2) and Particulate Matter (PM10) concentrations 
# Created: 2022-01-27.  Last updated: 2024-05-20

# Source: Ricardo EE
#         https://www.airqualityengland.co.uk/

# Load required packages ---------------------------
library(openair) ; library(tidyverse) ; library(lubridate) ; library(rvest)


# Function to download data ---------------------------
airqualityengland <- function(site_id, start_date, end_date, pollutant) {
  
  if(pollutant=="PM10"){poll="GE10"}else{poll=pollutant}
  
  url <- paste0("https://www.airqualityengland.co.uk/site/data.php?site_id=", site_id, "&parameter_id%5B%5D=" ,poll , "&f_query_id=1818812&data=%3C%3Fphp+print+htmlentities%28%24data%29%3B+%3F%3E&f_date_started=", start_date, "&f_date_ended=", end_date, "&la_id=368&action=download&submit=Download+Data")
  
  readings <- read_html(url) %>%
    html_node("a.b_xls.valignt") %>%
    html_attr('href') %>%
    read_csv(skip = 5) %>%
    rename(value = 3) %>% # column 3 is given the name of the pollutant, e.g. NO2, PM10 in the API data
    filter(!is.na(value)) %>%
    mutate(period = as.Date(end_date),
           station_code = site_id,
           station_name = case_when(station_code == "TRF2" ~ "Trafford A56",
                                    station_code == "TRF3" ~ "Trafford Wellacre Academy",
                                    TRUE ~ "Trafford Moss Park")) %>%
    group_by(period, station_code, station_name) %>%
    summarise(value = round(mean(value), digits = 1)) %>%
    select(period, station_code, station_name, value)
  
  return(readings)
}

# Monitoring site IDs
A56 <- "TRF2"
MossPark <- "TRAF"
Wellacre <- "TRF3"

# Pollutants: Nitrogen Dioxide = "NO2", Particulate Matter 10mg = "GE10" in the API

# Latest year of data we want
max_year <- 2024
df_no2 <- NULL
df_pm10 <- NULL

# Get NO2 data for 3 complete years-worth by quarter ---------------------------
# After getting each period, merge it into a single dataset
# Un-comment the current calendar year as each quarter is completed, and comment out the corresponding last data periods to always have 12 periods of data

# 12 months up to current calendar year Q4
# df_no2 <- df_no2 %>% 
#   bind_rows(airqualityengland(A56, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "NO2"),
#             airqualityengland(MossPark, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "NO2"),
#             airqualityengland(Wellacre, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "NO2"))

# 12 months up to current calendar year Q3
# df_no2 <- df_no2 %>%
#   bind_rows(airqualityengland(A56, paste0(max_year-1, "-10-01"), paste0(max_year, "-09-30"), "NO2"),
#             airqualityengland(MossPark, paste0(max_year-1, "-10-01"), paste0(max_year, "-09-30"), "NO2"),
#             airqualityengland(Wellacre, paste0(max_year-1, "-10-01"), paste0(max_year, "-09-30"), "NO2"))

# 12 months up to current calendar year Q2
# df_no2 <- df_no2 %>% 
#   bind_rows(airqualityengland(A56, paste0(max_year-1, "-07-01"), paste0(max_year, "-06-30"), "NO2"),
#             airqualityengland(MossPark, paste0(max_year-1, "-07-01"), paste0(max_year, "-06-30"), "NO2"),
#             airqualityengland(Wellacre, paste0(max_year-1, "-07-01"), paste0(max_year, "-06-30"), "NO2"))

# 12 months up to current calendar year Q1
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-1, "-04-01"), paste0(max_year, "-03-31"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-1, "-04-01"), paste0(max_year, "-03-31"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-1, "-04-01"), paste0(max_year, "-03-31"), "NO2"))


# 12 months up to previous calendar year Q4
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "NO2"))

# 12 months up to previous calendar year Q3
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-10-01"), paste0(max_year-1, "-09-30"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-2, "-10-01"), paste0(max_year-1, "-09-30"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-2, "-10-01"), paste0(max_year-1, "-09-30"), "NO2"))

# 12 months up to previous calendar year Q2
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-07-01"), paste0(max_year-1, "-06-30"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-2, "-07-01"), paste0(max_year-1, "-06-30"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-2, "-07-01"), paste0(max_year-1, "-06-30"), "NO2"))

# 12 months up to previous calendar year Q1
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-04-01"), paste0(max_year-1, "-03-31"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-2, "-04-01"), paste0(max_year-1, "-03-31"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-2, "-04-01"), paste0(max_year-1, "-03-31"), "NO2"))


# 12 months up to 2 calendar years ago Q4
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "NO2"))

# 12 months up to 2 calendar years ago Q3
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-3, "-10-01"), paste0(max_year-2, "-09-30"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-3, "-10-01"), paste0(max_year-2, "-09-30"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-3, "-10-01"), paste0(max_year-2, "-09-30"), "NO2"))

# 12 months up to 2 calendar years ago Q2
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-3, "-07-01"), paste0(max_year-2, "-06-30"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-3, "-07-01"), paste0(max_year-2, "-06-30"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-3, "-07-01"), paste0(max_year-2, "-06-30"), "NO2"))

# 12 months up to 2 calendar years ago Q1
df_no2 <- df_no2 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-3, "-04-01"), paste0(max_year-2, "-03-31"), "NO2"),
            airqualityengland(MossPark, paste0(max_year-3, "-04-01"), paste0(max_year-2, "-03-31"), "NO2"),
            airqualityengland(Wellacre, paste0(max_year-3, "-04-01"), paste0(max_year-2, "-03-31"), "NO2"))


# 12 months up to 3 calendar years ago Q4
df_no2 <- df_no2 %>%
 bind_rows(airqualityengland(A56, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "NO2"),
           airqualityengland(MossPark, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "NO2"),
           airqualityengland(Wellacre, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "NO2"))

# 12 months up to 3 calendar years ago Q3
df_no2 <- df_no2 %>%
 bind_rows(airqualityengland(A56, paste0(max_year-4, "-10-01"), paste0(max_year-3, "-09-30"), "NO2"),
           airqualityengland(MossPark, paste0(max_year-4, "-10-01"), paste0(max_year-3, "-09-30"), "NO2"),
           airqualityengland(Wellacre, paste0(max_year-4, "-10-01"), paste0(max_year-3, "-09-30"), "NO2"))

# 12 months up to 3 calendar years ago Q2
df_no2 <- df_no2 %>%
 bind_rows(airqualityengland(A56, paste0(max_year-4, "-07-01"), paste0(max_year-3, "-06-30"), "NO2"),
           airqualityengland(Wellacre, paste0(max_year-4, "-07-01"), paste0(max_year-3, "-06-30"), "NO2"),
           airqualityengland(MossPark, paste0(max_year-4, "-07-01"), paste0(max_year-3, "-06-30"), "NO2"))

# 12 months up to 3 calendar years ago Q1
#df_no2 <- df_no2 %>% 
#  bind_rows(airqualityengland(A56, paste0(max_year-4, "-04-01"), paste0(max_year-3, "-03-31"), "NO2"),
#            airqualityengland(MossPark, paste0(max_year-4, "-04-01"), paste0(max_year-3, "-03-31"), "NO2"),
#            airqualityengland(Wellacre, paste0(max_year-4, "-04-01"), paste0(max_year-3, "-03-31"), "NO2"))


# Finalise the NO2 data ---------------------------
df_no2 <- df_no2 %>%
  mutate(indicator = "Nitrogen Dioxide (NO2)",
         measure = "Annual mean concentration",
         unit = "µg/m3") %>%
  arrange(period) %>%
  select(station_code, station_name, period, indicator, measure, unit, value)
  
# Export the tidied NO2 data ---------------------------
write_csv(df_no2, "../no2_concentration.csv")


# Get PM10 data for 3 complete years-worth by quarter ---------------------------
# After getting each period, merge it into a single dataset
# Un-comment the current calendar year as each quarter is completed, and comment out the corresponding last data periods to always have 12 periods of data

# 12 months up to current calendar year Q4
# df_pm10 <- df_pm10 %>% 
#   bind_rows(airqualityengland(A56, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "PM10"),
#             airqualityengland(MossPark, paste0(max_year, "-01-01"), paste0(max_year, "-12-31"), "PM10"))

# 12 months up to current calendar year Q3
# df_pm10 <- df_pm10 %>%
#   bind_rows(airqualityengland(A56, paste0(max_year-1, "-10-01"), paste0(max_year, "-09-30"), "PM10"),
#             airqualityengland(MossPark, paste0(max_year-1, "-10-01"), paste0(max_year, "-09-30"), "PM10"))

# 12 months up to current calendar year Q2
# df_pm10 <- df_pm10 %>% 
#   bind_rows(airqualityengland(A56, paste0(max_year-1, "-07-01"), paste0(max_year, "-06-30"), "PM10"),
#             airqualityengland(MossPark, paste0(max_year-1, "-07-01"), paste0(max_year, "-06-30"), "PM10"))

# 12 months up to current calendar year Q1
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-1, "-04-01"), paste0(max_year, "-03-31"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-1, "-04-01"), paste0(max_year, "-03-31"), "PM10"))


# 12 months up to previous calendar year Q4
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-1, "-01-01"), paste0(max_year-1, "-12-31"), "PM10"))

# 12 months up to previous calendar year Q3
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-10-01"), paste0(max_year-1, "-09-30"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-2, "-10-01"), paste0(max_year-1, "-09-30"), "PM10"))

# 12 months up to previous calendar year Q2
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-07-01"), paste0(max_year-1, "-06-30"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-2, "-07-01"), paste0(max_year-1, "-06-30"), "PM10"))

# 12 months up to previous calendar year Q1
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-04-01"), paste0(max_year-1, "-03-31"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-2, "-04-01"), paste0(max_year-1, "-03-31"), "PM10"))


# 12 months up to 2 calendar years ago Q4
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-2, "-01-01"), paste0(max_year-2, "-12-31"), "PM10"))

# 12 months up to 2 calendar years ago Q3
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-3, "-10-01"), paste0(max_year-2, "-09-30"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-3, "-10-01"), paste0(max_year-2, "-09-30"), "PM10"))

# 12 months up to 2 calendar years ago Q2
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-3, "-07-01"), paste0(max_year-2, "-06-30"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-3, "-07-01"), paste0(max_year-2, "-06-30"), "PM10"))

# 12 months up to 2 calendar years ago Q1
df_pm10 <- df_pm10 %>% 
  bind_rows(airqualityengland(A56, paste0(max_year-3, "-04-01"), paste0(max_year-2, "-03-31"), "PM10"),
            airqualityengland(MossPark, paste0(max_year-3, "-04-01"), paste0(max_year-2, "-03-31"), "PM10"))


# 12 months up to 3 calendar years ago Q4
df_pm10 <- df_pm10 %>%
 bind_rows(airqualityengland(A56, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "PM10"),
           airqualityengland(MossPark, paste0(max_year-3, "-01-01"), paste0(max_year-3, "-12-31"), "PM10"))

# 12 months up to 3 calendar years ago Q3
df_pm10 <- df_pm10 %>%
 bind_rows(airqualityengland(A56, paste0(max_year-4, "-10-01"), paste0(max_year-3, "-09-30"), "PM10"),
           airqualityengland(MossPark, paste0(max_year-4, "-10-01"), paste0(max_year-3, "-09-30"), "PM10"))

# 12 months up to 3 calendar years ago Q2
df_pm10 <- df_pm10 %>%
 bind_rows(airqualityengland(A56, paste0(max_year-4, "-07-01"), paste0(max_year-3, "-06-30"), "PM10"),
           airqualityengland(MossPark, paste0(max_year-4, "-07-01"), paste0(max_year-3, "-06-30"), "PM10"))

# 12 months up to 3 calendar years ago Q1
#df_pm10 <- df_pm10 %>% 
#  bind_rows(airqualityengland(A56, paste0(max_year-4, "-04-01"), paste0(max_year-3, "-03-31"), "PM10"),
#            airqualityengland(MossPark, paste0(max_year-4, "-04-01"), paste0(max_year-3, "-03-31"), "PM10"))


# Finalise the PM10 data ---------------------------
df_pm10 <- df_pm10 %>%
  mutate(indicator = "Particulate Matter (PM10)",
         measure = "Annual mean concentration",
         unit = "µg/m3") %>%
  arrange(period) %>%
  select(station_code, station_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df_pm10, "../pm10_concentration.csv")
