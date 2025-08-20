library(dplyr)
library(tidyr)
library(zoo)

gdp_growth <- read.csv("data/raw/SQGDP9__ALL_AREAS_2005_2025.csv") %>%
  filter(
    GeoFIPS <= 56000,
    LineCode == 1
  ) %>%
  select(-c(GeoFIPS, Region, TableName, LineCode, IndustryClassification, 
            Description, Unit)) %>%
  mutate(across(-GeoName, as.numeric)) %>%
  pivot_longer(
    cols = -GeoName,
    names_to = "Quarter",
    values_to = "RealGDP"
  ) %>%
  pivot_wider(
    names_from = GeoName,
    values_from = RealGDP
  ) %>%
  mutate(Quarter = as.yearqtr(Quarter, format = "X%Y.Q%q")) %>%
  mutate(across(where(is.numeric), ~ (.x / dplyr::lag(.x, 4) - 1))) %>%
  drop_na()

gdp_growth

write.csv(gdp_growth, "data/processed/gdp_growth.csv")
