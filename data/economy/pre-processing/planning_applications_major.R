# Percentage of all major development planning applications decided within 13 weeks or agreed time

# Source: Ministry of Housing, Communities and Local Government
# URL: https://www.gov.uk/government/statistical-data-sets/live-tables-on-planning-application-statistics
# Licence: Open Government Licence


# Load required packages ---------------------------
library(tidyverse) 

# Setup objects ---------------------------
# Trafford and its Children's Services nearest neighbours:
authorities <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
lginform_key <- ""

# Download and tidy the data ---------------------------

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=17482&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:13&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df <- df_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Percentage",
         unit = "Applications",
         indicator = "Percentage of major planning applications decided within 13 weeks or agreed time",
         value = as.numeric(value)#,
         #period = str_replace(period, "\\s\\(academic\\)", "")
  ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../planning_applications_major.csv")
