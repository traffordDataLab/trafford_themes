# Gross Domestic Product per head at current market prices (£)

# Source: ONS
# URL: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductlocalauthorities
# https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions
# Licence: Open Government Licence


# Load required packages ---------------------------

library(tidyverse) ; library(readxl)

bm <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
lginform_key <- ""

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=21170&area=",paste(c(bm$area_code), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df<- df_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(bm, by = "area_code") %>%
  mutate(measure = "Rate per head",
         units = "pounds (£)",
         indicator = "GDP per head at current market prices",
         value = as.numeric(str_replace(value,",","")),
         period = str_replace(period, "\\s\\(academic\\)", "")
  ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, units, value)


# Export the tidied data ---------------------------
write_csv(df, "../gdp.csv")
