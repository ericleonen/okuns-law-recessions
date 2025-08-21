library(dplyr)
library(tidyr)
library(zoo)

# Preprocess BEA Real GDP Data
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
  mutate(across(where(is.numeric), ~ (.x / lag(.x) - 1))) %>%
  drop_na()

write.csv(gdp_growth, "data/processed/gdp_growth.csv")

# Preprocess BLS Unemployment Data

us_states <- read.csv("data/raw/la.area.csv", sep = "\t") %>%
  mutate(area_code = as.numeric(substr(area_code, 3, 4))) %>%
  filter(
    area_code <= 56,
    area_type_code == "A"
  ) %>%
  select(c(area_code, area_text))

unrate.states <- read.csv("data/raw/la.data.3.AllStatesS.csv", sep = "\t") %>%
  mutate(
    area_code = as.numeric(substr(series_id, 6, 7)),
    series_code = as.numeric(substr(series_id, 20, 21)),
    Month = as.yearmon(paste(year, period, sep = ""), format = "%YM%m"),
    UNRATE = as.numeric(value)
  ) %>%
  filter(
    area_code <= 56,
    series_code == 3
  ) %>%
  select(c(Month, area_code, UNRATE))
unrate_diff.states <- merge(us_states, unrate.states, by = "area_code", all = T) %>%
  select(-area_code) %>%
  pivot_wider(
    names_from = area_text,
    values_from = UNRATE
  ) %>%
  mutate(Quarter = as.yearqtr(Month)) %>%
  group_by(Quarter) %>%
  summarise(across(-Month, ~ mean(.x, na.rm = T))) %>%
  arrange(Quarter) %>%
  mutate(across(-Quarter, ~ .x - lag(.x))) %>%
  drop_na()

unrate_diff.us <- read.csv("data/raw/ln.data.14.USS.csv") %>%
  mutate(
    Month = as.yearmon(paste(Year, Period, sep = ""), format = "%YM%m"),
    Quarter = as.yearqtr(Month),
    UNRATE = as.numeric(Value)
  ) %>%
  group_by(Quarter) %>%
  summarise(UNRATE = mean(UNRATE, na.rm = TRUE), .groups = "drop") %>%
  mutate("United States" = UNRATE - lag(UNRATE)) %>%
  select(-UNRATE) %>%
  drop_na()

unrate_diff <- inner_join(unrate_diff.us, unrate_diff.states, by = "Quarter")

write.csv(unrate_diff, "data/processed/unrate_diff.csv")

# clear all variables
rm(list = ls())
