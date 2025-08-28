# Testing script to see if Okun slopes change significantly due to major
# recessions. Estimated models for each period are saved.

library(dplyr)
library(lmtest)
library(sandwich)
library(car)

# --- Load US data and create period flags ---
data.us <- read.csv("data/processed.csv") %>%
  filter(Area == "United States")

after_2008 <- data.us$Quarter >= 2008
after_2020 <- data.us$Quarter >= 2020

# --- Estimate model ---
mod.us.recessions <- lm(gdp_growth ~ unrate_diff*after_2008 + unrate_diff*after_2020,
                        data = data.us)

# --- Results ---
linearHypothesis(mod.us.recessions,
                 c("unrate_diff:after_2008TRUE = 0",
                   "unrate_diff:after_2020TRUE = 0"),
                 vcov. = vcovHC, type = "HC1")
  #   Res.Df Df      F Pr(>F)
  # 1     76                 
  # 2     74  2 0.0123 0.9878

coeftest(mod.us.recessions, vcov. = vcovHC, type = "HC1")
  #                            Estimate Std. Error t value  Pr(>|t|)    
  # (Intercept)                 2.20057    0.32503  6.7703 2.627e-09 ***
  # unrate_diff                -4.68935    2.96170 -1.5833    0.1176    
  # after_2008TRUE             -0.46456    0.44128 -1.0528    0.2959    
  # after_2020TRUE              1.06176    1.09901  0.9661    0.3371    
  # unrate_diff:after_2008TRUE  0.36401    3.17615  0.1146    0.9091    
  # unrate_diff:after_2020TRUE  0.35265    1.40722  0.2506    0.8028    
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(mod.us.recessions)
  # Residuals:
  #     Min      1Q  Median      3Q     Max 
  # -7.3253 -1.0655  0.1719  1.0441 15.7124 
  #
  # Residual standard error: 2.939 on 74 degrees of freedom
  # Multiple R-squared:  0.742,	Adjusted R-squared:  0.7246 
  # F-statistic: 42.56 on 5 and 74 DF,  p-value: < 2.2e-16

# --- Save model's coefficients ---
mod.us.recessions$coefficients
intercept.pre <- mod.us.recessions$coefficients[1]
intercept.GR <- intercept.pre + mod.us.recessions$coefficients[3]
intercept.COVID <- intercept.GR + mod.us.recessions$coefficients[4]
slope.pre <- mod.us.recessions$coefficients[2]
slope.GR <- slope.pre + mod.us.recessions$coefficients[5]
slope.COVID <- slope.GR + mod.us.recessions$coefficients[6]

coefs.us.recessions <- cbind("Period" = c("pre", "GR", "COVID"),
                             "intercept"= c(intercept.pre, intercept.GR, intercept.COVID),
                             "slope" = c(slope.pre, slope.GR, slope.COVID))

write.csv(coefs.us.recessions, "results/coefs.us.recessions.csv", row.names = F)

# --- Clear all variables ---
rm(list = ls())