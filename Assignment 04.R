# Clear out Console and Environment
rm(list=ls(all=TRUE))
cat("\014")


# We'll use package readr, which is part of the tidyverse
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

library(here)
Counties <- read_csv(here("us-counties.csv"))
PaCount <- filter(Counties, state == "Pennsylvania")

C1 <- group_by(PaCount, county, cases, deaths)
COUNTS1 <- summarise(C1, county, deaths= sum(deaths), cases = sum(cases))
c2 <- aggregate(cbind(deaths, cases) ~ county, data = COUNTS1, sum, na.rm = TRUE)

ggplot(c2, aes(x=cases, y=deaths)) + geom_point(stat = "identity", position = "dodge") + 
  labs(
    title = "COVID-19 Deaths vs Cases for PA as of 2021-03-04"
  )+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + geom_text(aes(label = county), size = 2) +
  scale_x_continuous(labels = comma)
                                                                                                                                          decimal.mark = ","))



