# Employees paid at/above real living wage defined by the Living Wage Foundation https://www.livingwage.org.uk/what-real-living-wage
# Created: 2022-02-18

# Source: Annual Survey of Hours and Earnings, ONS
# URL: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage
# Licence: Open Government Licence


# Load required packages ---------------------------
library(tidyverse); library(readxl)


# Setup objects ---------------------------
# Trafford and its CIPFA nearest neighbours (2019):
authorities <- read_csv("../../cipfa2021.csv") %>%
  add_row(area_code = "E08000009", area_name = "Trafford") %>%
  add_row(area_code = "E92000001", area_name = "England")

# Tibble to hold the completed dataset
df_rlw <- tibble();

# Function to download ZIP data file and extract the data in a consistent format
get_data <- function (url, workbook, data_year) {
  # Get the filename from the url (all characters after the last "/")
  file_name <- str_replace(url, "^.+/", "")
  
  # Download the file to temporary location  
  download.file(url, dest = file_name)
  
  # Extract the required Excel workbook from the zip
  unzip(file_name, files = workbook, exdir = ".")
  
  # Open the workbook, extract the data and store it, ready to return it to the caller
  # NOTE: value in all instances is the % BELOW the real Living wage. need to mutate value = 100 - value to get % at or above the real living wage.
  df_temp <- read_xls(workbook, sheet = 2, skip = 4) %>%
    select(area_code = "Code", area_name = "Description", value = 4) %>%
    filter(area_code %in% authorities$area_code) %>%
    mutate(period = data_year,
           value = 100 - as.numeric(value))
    
  # Tidy up the filesystem by deleting the zip and workbook
  file.remove(c(file_name, workbook))
  
  # Return the data to the caller
  df_temp %>%
    select(area_code, area_name, period, value)
}


# Download, extract and tidy the data for each year and bind it together into a complete dataset ---------------------------

# 2016
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/april2016and2017/livingwagebylaandpc2016and2017.zip",
                               workbook = "Work Geography LW Table 7.1a   lpmgx 2016.xls",
                               data_year = "2016")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2017
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/april2017and2018/20172018livingwagebyworkgeographyv2.zip",
                               workbook = "Work Geography LW Table 7.1a   lpmgx 2017.xls",
                               data_year = "2017")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2018
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/april2018and2019/20182019livingwagebyworkgeography.zip",
                               workbook = "Work Geography LW Table 7.1a   lwfmgx 2018.xls",
                               data_year = "2018")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2019
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/april2019and2020/20192020livingwagebyworkgeography.zip",
                               workbook = "Work Geography LW Table 7.1a   lwfmgx 2019.xls",
                               data_year = "2019")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2020
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/2020revised/livingwagebyworkgeography2020revised.zip",
                               workbook = "Work Geography LWF Table 7 LWF.1a   lwfmgx 2020.xls",
                               data_year = "2020")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2021
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/2021revised/livingwagebyworkgeography2021revised.zip",
                               workbook = "Work Geography LWF Table 7 LWF.1a   lwfmgx 2021.xls",
                               data_year = "2021")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2022
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/2022revised/livingwagebyworkgeography2022revised.zip",
                               workbook = "Work Geography LWF Table 7 LWF.1a   lwfmgx 2022.xls",
                               data_year = "2022")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset

# 2023 provisional
df_rlw_single_year <- get_data(url = "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage/2023provisional/livingwagebyworkgeography2023provisional.zip",
                               workbook = "PROV - Work Geography LWF Table 7 LWF.1a   lwfmgx 2023.xls",
                               data_year = "2023")

df_rlw <- bind_rows(df_rlw, df_rlw_single_year) # add the data to the full dataset


# Finalise the completed dataset with the common variables ---------------------------
df_rlw <- df_rlw %>%
  mutate(indicator = "Employees paid at or above the real living wage by work geography",
         measure = "Percentage",
         unit = "Persons") %>%
  arrange(period, area_name) %>%
  select(area_code, area_name, period, indicator, measure, unit, value) %>%
  mutate(area_name = gsub(" UA", "", area_name))


# Export the tidied data ---------------------------
write_csv(df_rlw, "../real_living_wage.csv")
