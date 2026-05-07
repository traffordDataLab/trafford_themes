# Percentage of all non-major planning applications decided in time

# Source: Ministry of Housing, Communities and Local Government
# URL: https://www.gov.uk/government/statistical-data-sets/live-tables-on-planning-application-statistics
# Licence: Open Government Licence


# Load required packages ---------------------------
library(tidyverse); library(readODS)

# Setup objects ---------------------------
# Trafford and CIPFA nearest neighbours:
authorities <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Download and tidy the data ---------------------------

download.file("https://assets.publishing.service.gov.uk/media/69b97adff6acd3f43e9612bb/Table_P153.ods", "Table_P153.ods")

cols <-  read_ods("Table_P153.ods", sheet = 2, range = "A5:AW6") %>%
  pivot_longer(`January to March 2024`:`...49`, names_to = "col", values_to = "values") %>%
  select(col,values)%>%
  mutate(col = ifelse(col == "October to December 2025P","October to December 2025",col)) %>%
  mutate(col2 = ifelse(values == "Total non-major\ndevelopment\ndecisions", col,NA)) %>%
  fill(col2) %>%
  mutate(col3 = case_when(grepl("Apr", col2) ~ paste0(as.numeric(word(col2,4)), "/",as.numeric(word(col2,4))-2000+1, " Q1"),
                          grepl("Jul", col2) ~ paste0(as.numeric(word(col2,4)), "/",as.numeric(word(col2,4))-2000+1, " Q2"),
                          grepl("Oct", col2) ~ paste0(as.numeric(word(col2,4)), "/",as.numeric(word(col2,4))-2000+1, " Q3"),
                          grepl("Jan", col2) ~ paste0(as.numeric(word(col2,4))-1,"/", as.numeric(word(col2,4))-2000, " Q4"),)) %>%
  mutate(col_names = paste0(values, " - ", col3))

cols <- c("area_name", "area_code", cols$col_names)


df_raw <- read_ods("Table_P153.ods", sheet = 2, skip= 6) %>%
  select(`...1`:`...49`) 

colnames(df_raw) <- cols


df <- df_raw %>%
  filter(area_code %in% authorities$area_code) %>%
  mutate_at(c(3:49), as.numeric) %>%
  pivot_longer(`Total non-major\ndevelopment\ndecisions - 2023/24 Q4`:`Imputed\nblank=No\n1=yes - 2025/26 Q3`, names_to = "col", values_to = "values") %>%
  separate(col,c("indicator", "period"), sep = " - ") %>%
  filter(indicator %in% c("Total non-major\ndevelopment\ndecisions", "Total non-major development\ndecisions\nwithin\n8 weeks2", "Planning\nPerformance\nAgreement,\nagreed\nExtension of\nTime or\nEnvironmental\nImpact\nAssessment\ndecisions\nwithin agreed\ntime3")) %>%
  pivot_wider(names_from = "indicator", values_from = "values") %>%
  mutate(value = round((`Total non-major development\ndecisions\nwithin\n8 weeks2`+`Planning\nPerformance\nAgreement,\nagreed\nExtension of\nTime or\nEnvironmental\nImpact\nAssessment\ndecisions\nwithin agreed\ntime3`)/`Total non-major\ndevelopment\ndecisions`*100,1)) %>%
  mutate(measure = "Percentage",
         unit = "Applications",
         indicator = "Percentage of all non-major development planning applications decided in time",
         value = as.numeric(value)
  ) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../planning_applications_non-major.csv")

file.remove("Table_P153.ods")
