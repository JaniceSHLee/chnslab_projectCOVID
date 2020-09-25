# Load libraries
library(here)
library(tidyverse)

# Read covid data
covid <- read.csv(here::here("covid_cases.csv"))
covid %>% head(5)

# Read fire data
fire <- read.csv(here::here("national_fire.csv"))
fire %>%  head(5)

# Combine covid and fire
covid_fire <- left_join(covid, fire, by = c("country", "month")) %>% 
  # Change the unit
  dplyr::mutate(cases_sc = scale(cases), 
                carbon_sc = scale(carbon))
covid_fire %>% head()

# Make some plots
ggplot(data=covid_fire, aes(x=cases_sc, y=carbon_sc))+
  geom_point()+
  facet_wrap(~month)

# Conduct and plot correlation between # cases and # carbon
plot(covid_fire$cases_sc, covid_fire$carbon_sc, xlab="# cases", ylab="# fire",
     main="Correllation between 
     number of COVID-19 cases and 
     amount of carbon realeased by forest fire")
abline(lm(covid_fire$cases_sc~covid_fire$carbon_sc), col="red")

# Calculate correlation analysis
cor.test(covid_fire$cases_sc, covid_fire$carbon_sc)
# add text
text(15,19, "r = 0.28")
text(16,17, "p < 0.0001")





