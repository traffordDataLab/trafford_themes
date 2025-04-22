# Proportion of Social Care Service users who are satisfied with their care and support  #

# Source: NHS England
# URL: https://digital.nhs.uk/data-and-information/publications/statistical/adult-social-care-outcomes-framework-ascof
# Licence: Open Government Licence v3.0

library(tidyverse) 

lginform_key = ""

authorities <- read_csv("../../nhsennpg.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")


# Download and tidy the data ---------------------------

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=10680&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:16&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

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
         unit = "Persons",
         indicator = "Social Care Service users who are satisfied with their care and support",
         value = as.numeric(value)
  ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../social_care_satisfaction.csv")
