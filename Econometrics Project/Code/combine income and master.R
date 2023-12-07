
library(stargazer)
library(tidyverse)
library(zoo)
library(future)
library(future.apply)
library(tictoc)
library(did)
library(vtable)

setwd("C:/Users/malon/OneDrive/Documents/GitHub/econometrics-project-B/Econometrics Project/Data")

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



incomeandmaster2$time = as.factor(incomeandmaster2$time)


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
ols12 = lm(net_migration ~ droughtseverity + lastyeardrought + lastyearprecip + twoyearprecip + lastyearprecip * droughtseverity + twoyearprecip * droughtseverity + lastyeardrought * twoyearprecip + unemploy_rate + state + time, data = incomeandmaster2)
ols13 = lm(net_migration ~ droughtseverity + lastyeardrought + lastyearprecip + twoyearprecip + lastyearprecip * droughtseverity + twoyearprecip * droughtseverity + lastyeardrought * twoyearprecip + state + time, data = incomeandmaster2)

#displays regressions grouped by drought severity or last year drought.  last stargazer has everything.
stargazer(ols, ols2, ols3, ols4, ols7, type = "text", omit = c("time","state")) #t = 0 drought severity
stargazer(ols8, ols9, ols5, ols10, ols6, type = "text", omit = c("time", "state")) #t = -1 drought severity
stargazer(ols11, ols12, ols13, type = "text", omit = c("time", "state")) #both drought severity

#displays a clean, presentation ready format of the regressions.  Covariate labels need to be updated if new variables get used.
stargazer(ols, ols2, ols3,  type = "text", omit = c("time", "state") ,
          covariate.labels = c("Drought Category 0", "Drought Category 1", "Drought Category 2", "Drought Category 3",
                               "Drought Category 4", "Last Year Precipitation (Inches)", "Unemployment Rate", 
                               "Income Per Population", "Drought Category 0 * Last Year Precipitation",
                               "Drought Category 1 * Last Year Precipitation", "Drought Category 2 * Last Year Precipitation",
                               "Drought Category 3 * Last Year Precipitation", "Drought Category 4 * Last Year Precipitaiton"),
          dep.var.labels = "Net Migration (Individuals)")

#regressions with county fixed effects.  these take a very long time
incomeandmaster2$fips = as.factor(incomeandmaster2$fips)
cols = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + fips + time, data = incomeandmaster2)
cols2 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + fips + time, data = incomeandmaster2)
cols3 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + incomeperpop + fips + time, data = incomeandmaster2)
cols4 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + income + fips + time, data = incomeandmaster2)
cols5 = lm(net_migration ~ droughtseverity + lastyearprecip * droughtseverity + unemployed + income + fips + time, data = incomeandmaster2)

#compare county fixed effects to state fixed effects models, then display all county fixed effects models in a single table.
 stargazer(cols, ols, type = "text", omit = c("time", "fips", "state"))
stargazer(cols2, ols2, type = "text", omit = c("time", "fips", "state"))
stargazer(cols3, ols3, type = "text", omit = c("time", "fips", "state"))
stargazer(cols, cols2, cols3, cols4, cols5, type = "text", omit = c("time", "fips"))

#create a new variable which records migration as a function of population in a county
incomeandmaster2 = incomeandmaster2|>
  mutate(migrationperpop = net_migration/pop_estimates)

#run regressions with migration per population as the dependant variable
cols4 = lm(migrationperpop ~ droughtseverity + lastyearprecip * droughtseverity + fips + time, data = incomeandmaster2)
cols5 = lm(migrationperpop ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + fips + time, data = incomeandmaster2)
cols6 = lm(migrationperpop ~ droughtseverity + lastyearprecip * droughtseverity + unemploy_rate + incomeperpop + fips + time, data = incomeandmaster2)

#compares regressions with net migration with migration per pop
stargazer(cols, cols4, type = "text", omit = c("time", "fips"))
stargazer(cols2, cols5, type = "text", omit = c("time", "fips"))
stargazer(cols3, cols6, type = "text", omit = c("time", "fips"))
stargazer(cols4, cols5, cols6, type = "text", omit = c("time", "fips"))

#create variables which record LFPS, migration per 10000 pops, a control variable for population
incomeandmaster3 = incomeandmaster2|>
  mutate(pop10k = pop_estimates / 10000,
         lf = unemployed + employed,
         lfpr = lf / pop_estimates,
         migrate10k = net_migration / pop10k)|>
  group_by(fips)|>
         mutate(lastyeardrought = lag(droughtseverity, n = 1))

incomeandmaster3$droughtseverity = as.factor(incomeandmaster3$droughtseverity)
incomeandmaster3$lastyeardrought = as.factor(incomeandmaster3$lastyeardrought)
incomeandmaster3$time = as.factor(incomeandmaster3$time)

fols1 = lm(migrate10k ~ droughtseverity + precip + droughtseverity * precip + unemploy_rate + lfpr + incomeperpop + fips + time, data = incomeandmaster3)
fols2 = lm(migrate10k ~ lastyeardrought + lastyearprecip + lastyeardrought * lastyearprecip + unemploy_rate + lfpr + incomeperpop + fips + time, data = incomeandmaster3)
fols3 = lm(migrate10k ~ droughtseverity + lastyeardrought + precip + lastyearprecip + droughtseverity * precip + lastyeardrought * lastyearprecip + unemploy_rate + lfpr + incomeperpop + fips + time, data = incomeandmaster3)

stargazer(fols1, fols2, fols3, type = "text", omit = c("time", "fips"))

#final regressions using current drought year
fols4 = lm(migrate10k ~ droughtseverity + precip + droughtseverity*precip + fips + time, data = incomeandmaster3)
fols5 = lm(migrate10k ~ droughtseverity + precip + droughtseverity*precip + unemploy_rate + fips + time ,data = incomeandmaster3)
fols6 = lm(migrate10k ~ droughtseverity + precip + droughtseverity*precip + unemploy_rate + lfpr + fips + time, data = incomeandmaster3)
fols7 = lm(migrate10k ~ droughtseverity + precip + droughtseverity*precip + unemploy_rate + lfpr + pop10k + fips + time, data = incomeandmaster3)
fols8 = lm(migrate10k ~ droughtseverity + precip + droughtseverity*precip + unemploy_rate + lfpr + pop10k + incomeperpop + fips + time, data = incomeandmaster3)

#final regressions using last drought year
fols9 = lm(migrate10k ~ lastyeardrought + lastyearprecip + lastyeardrought * lastyearprecip + fips + time, data = incomeandmaster3)
fols10 = lm(migrate10k ~ lastyeardrought + lastyearprecip + lastyeardrought * lastyearprecip + unemploy_rate + fips + time, data = incomeandmaster3)
fols11 = lm(migrate10k ~ lastyeardrought + lastyearprecip + lastyeardrought * lastyearprecip + unemploy_rate + lfpr + fips + time, data = incomeandmaster3)
fols12 = lm(migrate10k ~ lastyeardrought + lastyearprecip + lastyeardrought * lastyearprecip + unemploy_rate + lfpr + pop10k + fips + time, data = incomeandmaster3)
fols13 = lm(migrate10k ~ lastyeardrought + lastyearprecip + lastyeardrought * lastyearprecip + unemploy_rate + lfpr + pop10k + incomeperpop + fips + time, data = incomeandmaster3)

#exports the final regressions
stargazer(fols4, fols5, fols6, fols7, fols8, type = "html", omit = c("time", "fips"), out = "finalreg.html")
stargazer(fols9, fols10, fols11, fols12, fols13, type = "html", omit = c("time", "fips"), out = "finalreg2.html")
stargazer(fols13, type = "html", omit = c("time", "fips"), out = "finalreg3.html")

#make a new drought severity summary table
table = summary(incomeandmaster3$droughtseverity)
total = 67815
percent = c(table[1] / total, table[2]/total, table[3]/total, table[4]/total, table[5]/total, table[6]/total)
droughtsummary = cbind(table, percent)
rownames(droughtsummary) = c("none", "category 0", "category 1", "category 2", "category 3", "category 4")
droughtsummary = as.data.frame(droughtsummary)
write_csv(droughtsummary, file = "final drought summary.csv")

#save the new dataset, makes sure it can be re-read
write_csv(incomeandmaster3, file = "incomeandmaster3.csv")
incomeandmaster2 = read_csv("incomeandmaster2.csv")

incomeandmaster3 = read_csv("incomeandmaster3.csv")
write_csv(incomeandmaster3, file = "finalincomemaster.csv")

