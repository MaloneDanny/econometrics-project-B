
library(stargazer)
library(tidyverse)
library(zoo)
library(future)
library(future.apply)
library(tictoc)
library(did)


#import droughtandmaster and clean_income into the workspace

#remove the unimportant variables from the income dataset.  we will use the census population and income per capita
clean_income2 = subset(clean_income, select = -c(population, income_percap))

#merge the drought master and clean income datasets, converts relevant variables to the appropriate type
incomeandmaster = merge(clean_income2, droughtandmaster)
incomeandmaster$pop_estimates = as.numeric(incomeandmaster$pop_estimates)
incomeandmaster$income = as.numeric(incomeandmaster$income)
incomeandmaster$year = as.factor(incomeandmaster$year)

#create a new income per capita variable, renames year to time to make stargazer work better, create lagged last year precip and drought severity
incomeandmaster2 = incomeandmaster|>
  mutate(incomeperpop = income / pop_estimates)|>
  rename(time = year)
incomeandmaster2$droughtseverity = as.factor(incomeandmaster2$droughtseverity)
incomeandmaster2 = incomeandmaster2|>
  group_by(fips)|>
  mutate(twoyearprecip = lag(lag(precip)),
         lastyeardrought = lag(droughtseverity))

#save the new dataset, makes sure it can be re-read
write_csv(incomeandmaster2, file = "incomeandmaster.csv")
incomeandmaster2 = read_csv("incomeandmaster.csv")

#regressions using drought severity
ols = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + state + time, data = incomeandmaster2)
ols2 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + state + time, data = incomeandmaster2)
ols3 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)
ols4 = lm(net_migration ~ droughtseverity + twoyearprecip * droughtseverity + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)
ols7 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + twoyearprecip + twoyearprecip * droughtseverity + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)

#regressions using last year drought severity
ols8 = lm(net_migration ~ lastyeardrought + lastyearprecip * lastyeardrought + state + time, data = incomeandmaster2)
ols9 = lm(net_migration ~ lastyeardrought + lastyearprecip * lastyeardrought + unemploy_rate + state + time, data = incomeandmaster2)
ols5 = lm(net_migration ~ lastyeardrought + lastyearprecip * lastyeardrought + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)
ols10 = lm(net_migration ~ lastyeardrought + twoyearprecip * lastyeardrought + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)
ols6 = lm(net_migration ~ lastyeardrought + lastyearprecip * lastyeardrought + twoyearprecip + twoyearprecip * lastyeardrought + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)

#regression with everything
ols11 = lm(net_migration ~ droughtseverity + lastyeardrought + lastyearprecip + twoyearprecip + lastyearprecip * droughtseverity + twoyearprecip * droughtseverity + lastyeardrought * twoyearprecip + unemploy_rate + incomeperpop + state + time, data = incomeandmaster2)

#displays regressions grouped by drought severity or last year drought.  last stargazer has everything.
stargazer(ols, ols2, ols3, ols4, ols7, type = "text", omit = c("time","state"))
stargazer(ols8, ols9, ols5, ols10, ols6, type = "text", omit = c("time", "state"))
stargazer(ols11, type = "text", omit = c("time", "state"))

#displays a clean, presentation ready format of the regressions.  Covariate labels need to be updated if new variables get used.
stargazer(ols, ols2, ols3,  type = "text", omit = c("time", "state") ,
          covariate.labels = c("Drought Category 0", "Drought Category 1", "Drought Category 2", "Drought Category 3",
                               "Drought Category 4", "Last Year Precipitation (Inches)", "Unemployment Rate", 
                               "Income Per Population", "Drought Category 0 * Last Year Precipitation",
                               "Drought Category 1 * Last Year Precipitation", "Drought Category 2 * Last Year Precipitation",
                               "Drought Category 3 * Last Year Precipitation", "Drought Category 4 * Last Year Precipitaiton"),
          dep.var.labels = "Net Migration (Individuals)")

# Create a panel data frame
pdata = pdata.frame(incomeandmaster2, index = c("statefip", "countyfip", "year"))

