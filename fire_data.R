library(tidyverse)
library(rnaturalearth)
library(sf)

# original data from the 2d matrix
df_carbon <- read_csv('data/carbon2020.csv') %>%
  rename(index = X1) %>%
  pivot_longer(cols = -index, names_to = 'month', values_to = 'carbon')

# geological information in accordance with each index pixel
grid_info <- read_csv('data/grid_info.csv') %>%
  rename(index = X1) %>%
  select(index, long, lat) 

world <- ne_countries(scale = "medium", returnclass = "sf")

point_in_country <- function(long, lat){
  country <- st_point(c(long, lat)) %>%
    st_intersects(world$geometry) %>%
    .[[1]] %>%
    world$name[.]
  if (length(country) == 0) return(NA)
  else return(country)
}

grid_country <- grid_info %>%
  mutate(
    country = map2_chr(long, lat, point_in_country)
    ) %>%
  write_csv('data/grid_country.csv')

# match carbon emission with geographical information
df_carbon <- df_carbon %>% 
  left_join(grid_country, by = 'index') %>%
  group_by(country, month) %>%
  summarize(carbon = sum(carbon)) %>%
  write_csv('national_fire.csv')

