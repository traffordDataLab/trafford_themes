# Gross Value Added

# Source: ONS
# URL: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossvalueaddedbalancedbyindustrylocalauthoritiesbyitl1region
# Licence: Open Government Licence


# Load required packages ---------------------------

library(tidyverse) ; library(readxl)

bm <- read_csv("../../cipfalga0724.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# API key for LGInform+
lginform_key <- ""

#All industries 15298

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=15298&area=",paste(c(bm$area_code), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df1<- df_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(bm, by = "area_code") %>%
  mutate(measure = "Count",
         units = "Pounds millions (£)",
         indicator = "GVA, all industries at current prices",
         value = as.numeric(str_replace(value,",","")),
         period = str_replace(period, "\\s\\(academic\\)", "")
  ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, units, value)

#GVA per hour worked

# Source: ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivityindicesbylocalauthoritydistrict
# Licence: Open Government Licence

# Download and tidy the data ---------------------------

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=20737&area=",paste(c(bm$area_code), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df2 <- df_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(bm, by = "area_code") %>%
  mutate(measure = "Rate per hour worked",
         units = "Pounds (£)",
         indicator = "GVA per hour worked at current prices",
         value = as.numeric(str_replace(value,",","")),
         period = str_replace(period, "\\s\\(academic\\)", "")
  ) %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, units, value)

df <- df1 %>%
  bind_rows(df2)

# Export the tidied data ---------------------------
write_csv(df, "../gva.csv")
