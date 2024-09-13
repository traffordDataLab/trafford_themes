# Ratio of median house price to median gross annual workplace-based earnings

# Source: Office for National Statistics
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ratioofhousepricetoworkplacebasedearningslowerquartileandmedian
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

df_raw1 <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=9147&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

df_raw2 <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=9150&area=",paste(c("E92000001",authorities$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))


# Tidy the LG Inform+ data ---------------------------
df <- df_raw1 %>%
  mutate(indicator = "Ratio of median house price to median gross annual workplace-based earnings") %>%
  bind_rows(df_raw2 %>% mutate(indicator = "Ratio of median house price to median gross annual residence-based earnings")) %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code,-indicator), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
  mutate(measure = "Ratio",
         unit = "House price / earnings",
         value = round(as.numeric(value),1)
  ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)

# Export the tidied data ---------------------------
write_csv(df, "../housing_affordability.csv")
