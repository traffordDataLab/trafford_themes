# Broad Spectrum antibiotics as a % of total prescribed#

# Source: openprescribing.net
# URL: https://openprescribing.net/
# Licence: Open Government Licence v3.0

# Load required packages ---------------------------
library(tidyverse) 

# Download and tidy the data ---------------------------

#Data downloaded from https://openprescribing.net/. Under the "Area and Practice Dashboards" dropdown select Sub-ICB locations option, choose your area e. g. "NHS Trafford", under the plots choose "Antimicrobial stewardship" topic, search for "Prescription items for co-amoxiclav, cephalosporins and quinolones as a percentage of total prescription items for oral antibiotics (excluding antituberculosis drugs and antileprotic drugs)." and download the data. Repeat for National data

df_raw_trafford <- read_csv("Antibiotic stewardship co-amoxiclav cephalosporins quinolones-3.csv")

df_raw_england <- read_csv("Antibiotic stewardship co-amoxiclav cephalosporins quinolones-4.csv")

df_raw <- bind_rows(df_raw_trafford,df_raw_england)

# Tidy the data ---------------------------
df <- df_raw %>%
  mutate(area_name = ifelse(is.na(org_name), "England", "Trafford")) %>%
  mutate(area_code = ifelse(area_name == "Trafford", "E08000009", "E92000001")) %>%
  mutate(measure = "Percentage",
         unit = "prescriptions",
         indicator = "Broad spectrum antibiotics as a % of total prescribed",
         value = round(ratio*100,1),
         period = date) %>%
  select(area_code, area_name, period, indicator, measure, unit, value)


# Export the tidied data ---------------------------
write_csv(df, "../antibiotics_broad_s.csv")
