library(dplyr)
library(car)
library(broom)
library(stringr)
library(sandwich)
library(ggplot2)

data.states <- read.csv("data/processed.csv") %>%
  filter(Area != "United States")

mod.states <- lm(gdp_growth ~ unrate_diff*Area, data = data.states)

linearHypothesis(mod.states,
                 matchCoefs(mod.states, "unrate_diff:Area"),
                 vcov. = vcovHC(mod.states, type = "HC1"))

# --- RESULTS ---
# Model 1: restricted model
# Model 2: gdp_growth ~ unrate_diff * Area
# 
# Note: Coefficient covariance matrix supplied.
# 
# Res.Df Df      F    Pr(>F)    
# 1   4028                        
# 2   3978 50 4.3041 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

okun_coefficients.base <- tidy(mod.states) %>%
  filter(term == "unrate_diff") %>%
  pull(estimate)

okun_coefficients <- tidy(mod.states) %>%
  filter(str_detect(term, "unrate_diff")) %>%
  mutate(
    Area = ifelse(term == "unrate_diff", "Alabama", str_remove(term, "unrate_diff:Area")),
    coef = ifelse(Area == "Alabama", okun_coefficients.base, okun_coefficients.base + estimate)
  ) %>%
  select(Area, coef)

okun_intercepts.base <- tidy(mod.states) %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

okun_intercepts <- tidy(mod.states) %>%
  filter(!str_detect(term, "unrate_diff")) %>%
  mutate(
    Area = ifelse(term == "(Intercept)", "Alabama", str_remove(term, "Area")),
    intercept = ifelse(Area == "Alabama", okun_intercepts.base, okun_intercepts.base + estimate)
  ) %>%
  select(Area, intercept)
  
okun_results <- full_join(okun_intercepts, okun_coefficients, by = "Area") %>%
  arrange(coef)
  
print(
  okun_results,
  n = 51)

write.csv(okun_results, "results/okun_results.states.csv", row.names = F)

# clear variables
rm(list = ls())