library(dplyr)
library(car)
library(broom)
library(stringr)

data <- read.csv("data/processed/panel_data.csv") %>%
  filter(Area != "United States")

mod.states <- lm(gdp_growth ~ unrate_diff*Area, data = data)

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
    Area = str_remove(term, "unrate_diff:Area"),
    coef = ifelse(Area == "", okun_coefficients.base, okun_coefficients.base + estimate)
  ) %>%
  select(Area, coef)

print(
  okun_coefficients,
  n = 51)

# --- COEFFICIENTS ---
# Area                     coef
# <chr>                   <dbl>
# 1 Alabama               -0.0236 
# 2 Alaska                -0.0114 
# 3 Arizona               -0.0121 
# 4 Arkansas              -0.0126 
# 5 California            -0.00909
# 6 Colorado              -0.0107 
# 7 Connecticut           -0.0119 
# 8 Delaware              -0.00713
# 9 District of Columbia  -0.0104 
# 10 Florida              -0.0102 
# 11 Georgia              -0.0140 
# 12 Hawaii               -0.00766
# 13 Idaho                -0.0128 
# 14 Illinois             -0.00855
# 15 Indiana              -0.0123 
# 16 Iowa                 -0.0111 
# 17 Kansas               -0.0139 
# 18 Kentucky             -0.0139 
# 19 Louisiana            -0.00819
# 20 Maine                -0.0138 
# 21 Maryland             -0.0126 
# 22 Massachusetts        -0.00783
# 23 Michigan             -0.00968
# 24 Minnesota            -0.0146 
# 25 Mississippi          -0.0131 
# 26 Missouri             -0.0137 
# 27 Montana              -0.0131 
# 28 Nebraska             -0.0217 
# 29 Nevada               -0.00868
# 30 New Hampshire        -0.0113 
# 31 New Jersey           -0.00983
# 32 New Mexico           -0.0161 
# 33 New York             -0.00854
# 34 North Carolina       -0.0116 
# 35 North Dakota         -0.0161 
# 36 Ohio                 -0.0115 
# 37 Oklahoma             -0.0120 
# 38 Oregon               -0.0108 
# 39 Pennsylvania         -0.0129 
# 40 Rhode Island         -0.00865
# 41 South Carolina       -0.0125 
# 42 South Dakota         -0.0171 
# 43 Tennessee            -0.0138 
# 44 Texas                -0.0113 
# 45 Utah                 -0.00961
# 46 Vermont              -0.0145 
# 47 Virginia             -0.00992
# 48 Washington           -0.00795
# 49 West Virginia        -0.0113 
# 50 Wisconsin            -0.0104 
# 51 Wyoming              -0.0280 
