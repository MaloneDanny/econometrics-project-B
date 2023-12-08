library(tidyverse)
library(dslabs)

setwd("C:/Users/malon/OneDrive/Documents/GitHub/econometrics-project-B/Econometrics Project/Data")
dat = read_csv("finalincomemaster.csv")

dat2 = dat|>
  filter(migrate10k > -10000)

ggplot(data = dat2)+
  geom_point(aes(x = precip, y = migrate10k))
