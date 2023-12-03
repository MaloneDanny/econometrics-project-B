library(knitr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tidyselect)

###########################################################################################################

# Read data into R 

data = read_csv("CAINC1__ALL_AREAS_1969_2021.csv")

###########################################################################################################

# Remove unnecessary columns 

data = data |> 
  select(-GeoName, -Region, -TableName, -LineCode, -IndustryClassification) |> 
  rename(fips = GeoFIPS)

# Create separate data sets 

data_income <- subset(data, Description == "Personal income (thousands of dollars)")
data_pop <- subset(data, Description == "Population (persons) 1/")
data_per_cap <- subset(data, Description == "Per capita personal income (dollars) 2/")

# Pivot long

long_income = data_income |> 
  pivot_longer(cols = 4:56, names_to = "year", values_to = "income")

long_pop = data_pop |> 
  pivot_longer(cols = 4:56, names_to = "year", values_to = "population")

long_per_cap = data_per_cap |> 
  pivot_longer(cols = 4:56, names_to = "year", values_to = "income_percap")

# Remove columns in each data set 

long_income = long_income |> 
  select(-Description, -Unit)

long_pop = long_pop |> 
  select(-Description, -Unit)

long_per_cap = long_per_cap |> 
  select(-Description, -Unit)

# Merge to one large data set using fips and year 

clean_income1 = merge(long_income, long_pop, by = c("fips", "year"), all.x = TRUE, all.y = FALSE)
clean_income = merge(clean_income1, long_per_cap, by = c("fips", "year"), all.x = TRUE, all.y = FALSE)

save(clean_income, file = "clean_income.Rda")








