#This script will import the combinedpreciplabor data set into R while preserving the leading zeros.  Make sure the data file is in 
#your working directory before you begin.

library(tidyverse)


combinedpreciplabor = read.csv("combinedpreciplabor", colClasses = c(fips = "character"))
combinedpreciplabor = combinedpreciplabor|>
  select(!X)
