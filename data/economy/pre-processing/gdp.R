# Gross Domestic Product per head at current market prices (£)

# Source: ONS
# URL: https://www.ons.gov.uk/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductlocalauthorities
# Licence: Open Government Licence


# Load required packages ---------------------------

library(tidyverse) ; library(readxl)

bm <- read_csv("../../cipfalga0724.csv") %>%
  select(area_code)

tmp <- tempfile(fileext = ".xlsx")


GET(url = "https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductlocalauthorities/1998to2023/regionalgrossdomesticproductgdplocalauthorities.xlsx",
    write_disk(tmp))

df1 <- read_xlsx(path = tmp, sheet = 10, skip = 1) %>%
  select(area_code = `LA code`, area_name = `LA name`, `2013`:`2023`) %>%
  filter(`area_code` %in% c(bm$area_code, "E08000009")) %>%
  mutate(across(`2013`:`2023`, as.numeric))

tmp <- tempfile(fileext = ".xlsx")

GET(url = "https://www.ons.gov.uk/file?uri=/economy/grossdomesticproductgdp/datasets/regionalgrossdomesticproductallnutslevelregions/1998to2023/regionalgrossdomesticproductgdpbyallinternationalterritoriallevelitlregions.xlsx",
    write_disk(tmp))

df2 <- read_xlsx(path = tmp, sheet = 10, skip = 1) %>%
  select(area_name = `Region name`, `2013`:`2023`) %>%
  filter(`area_name` == "England") %>%
  mutate(area_code = "E92000001") %>% 
  mutate(across(`2013`:`2023`, as.numeric))

df <- df1 %>%
  bind_rows(df2) %>%
  pivot_longer(`2013`:`2023`, names_to = "period", values_to = "value") %>%
  mutate(indicator = "GDP per head at current market prices",
         measure = "Rate per head",
         units = "pounds (£)") %>%
  select(area_code, area_name, period, indicator, measure, units, value)


# Export the tidied data ---------------------------
write_csv(df, "../gdp.csv")
