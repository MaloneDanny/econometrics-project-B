
library(stargazer)
library(tidyverse)
library(zoo)
library(future)
library(future.apply)
library(tictoc)
library(did)
library(plm)

#import droughtandmaster and clean_income into the workspace
clean_income2 = subset(clean_income, select = -c(population, income_percap))

incomeandmaster = merge(clean_income2, droughtandmaster)

write_csv(incomeandmaster, file = "incomeandmaster.csv")

ols = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + state + year, data = incomeandmaster)
stargazer(ols, type = "text", omit = c(incomeandmaster$state, incomeandmaster$year))
