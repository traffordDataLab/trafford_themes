# Gross Value Added

# Source: ONS
# URL: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductlocalauthorities
# Licence: Open Government Licence


# Load required packages ---------------------------

library(tidyverse) ; library(readxl)

bm <- read_csv("../../cipfalga0724.csv") %>%
  select(area_code)

#All industries

tmp <- tempfile(fileext = ".xlsx")


GET(url = "https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/regionalgrossvalueaddedbalancedbyindustrylocalauthoritiesbyitl1region/tldnorthwest/regionalgrossvalueaddedbalancedbyindustrylocalauthoritiestldnorthwest.xlsx",
    write_disk(tmp))

df1 <- read_xlsx(path = tmp, sheet = 6, skip = 1) %>%
  filter(`SIC07 description` == "All industries") %>%
  select(area_code = `LA code`, area_name = `LA name`, `2013`:`2023`) %>%
  filter(`area_code` %in% c(bm$area_code, "E08000009")) %>%
  mutate(across(`2013`:`2023`, as.numeric)) %>%
  pivot_longer(`2013`:`2023`, names_to = "period", values_to = "value") %>%
  mutate(indicator = "GVA, all industries at current prices",
         measure = "Count",
         units = "Pounds millions (£)")

#GVA per hour worked

# Source: ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/subregionalproductivitylabourproductivityindicesbylocalauthoritydistrict
# Licence: Open Government Licence


# API key for LGInform+
lginform_key <- ""

# Download and tidy the data ---------------------------

df_raw <- read_csv(paste0("https://webservices.esd.org.uk/data.csv?metricType=20737&area=",paste(c("E92000001",bm$area_code, "E08000009"), collapse = ','),"&period=latest:8&columnGrouping=period&rowGrouping=area&ApplicationKey=",lginform_key))

# Tidy the LG Inform+ data ---------------------------
df2 <- df_raw %>%
  select(area_code = area,
         everything(),
         -`area label`,
         -`area long label`) %>%
  filter(area_code != "area") %>%
  pivot_longer(c(-area_code), names_to = "period", values_to = "value") %>%
  left_join(authorities, by = "area_code") %>%
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
