library(knitr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

fip_year_employment <- fip_year_employment |> 
  rename(employed = employedN, unemployed = unemployedN, unemploy_rate = unemploy_rateN, labor_force = laborforceN)

fip_year_employment$fips_year <- paste(fip_year_employment$fips, fip_year_employment$year, sep = "_")

employment_migration = merge(fip_year_employment, census_migration, by = c("fips", "year", "fips_year"), all.x = TRUE, all.y = FALSE)

file_path <- "/Users/ellianehall/Desktop/Montana State University/ECNS 561/Research Project/employment_migration.Rda"
save(employment_migration, file = file_path)

precipitation_data = subset(precipitation_data, select = -c(X.jan., X.feb., X.mar., X.apr., X.may., X.jun., X.jul., X.aug., X.sep., X.oct., X.nov., X.dec., X..))
precipitation_data = subset(precipitation_data, select = -X.drought.)

precipitation_data <- precipitation_data |> 
  rename(fips = X.fips., year = X.year., precip = X.tot. , avg_precip = X.ave.)

precipitation_data = subset(precipitation_data, select = -c(X.meanprecip., X.diff., X.percent., X.sd.))
precipitation_data = subset(precipitation_data, select = -avg_precip) 

precipitation_data = precipitation_data |>
  mutate_all(~sub('"',"",.))
precipitation_data = precipitation_data |>
  mutate_all(~sub('"', "", .))

precipitation_data$fips_year <- paste(precipitation_data$fips, precipitation_data$year, sep = "_")

master_data = merge(employment_migration, precipitation_data, by = c("fips", "year", "fips_year"), all.x = TRUE, all.y = FALSE)

keep_rows <- complete.cases(master_data$pop_estimates)
master_data <- master_data[keep_rows, ]

file_path <- "/Users/ellianehall/Desktop/Montana State University/ECNS 561/Research Project/master_data.Rda"
save(master_data, file = file_path)

unique_fip_codes <- unique(master_data$fip)
# Count the number of unique FIP codes
count_unique_fip_codes <- length(unique_fip_codes)

df_cleaned <- master_data[!is.na(master_data$precip), ]

unique_fip_codes <- unique(df_cleaned$fips)
# Count the number of unique FIP codes
count_unique_fip_codes <- length(unique_fip_codes)


