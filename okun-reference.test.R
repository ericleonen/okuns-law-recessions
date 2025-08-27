# Testing script to see if the US Okun slope is significantly negative over all
# periods. Estimated model is saved and used for reference in recessions and
# state tests.

library(dplyr)
library(lmtest)
library(sandwich)

# --- Estimate model ---
data.us <- read.csv("data/processed.csv") %>%
  filter(Area == "United States")

mod.us.ref <- lm(gdp_growth ~ unrate_diff, data = data.us)

# --- Results ---
coeftest(mod.us.ref, vcov. = vcovHC, type = "HC1")
  #             Estimate Std. Error t value  Pr(>|t|)    
  # (Intercept)  2.08815    0.32046  6.5162 6.460e-09 ***
  # unrate_diff -3.98717    0.80701 -4.9407 4.348e-06 ***
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(mod.us.ref)
  # Residuals:
  #     Min      1Q  Median      3Q     Max 
  # -7.1054 -1.3029  0.2313  1.0241 16.3612 
  # 
  # Coefficients:
  #             Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)   2.0881     0.3244   6.438 9.06e-09 ***
  # unrate_diff  -3.9872     0.2711 -14.709  < 2e-16 ***
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  # 
  # Residual standard error: 2.901 on 78 degrees of freedom
  # Multiple R-squared:  0.735,	Adjusted R-squared:  0.7316 
  # F-statistic: 216.3 on 1 and 78 DF,  p-value: < 2.2e-16

# --- Save model's coefficients ---
coefs.us.ref <- cbind("intercept" = mod.us.ref$coefficients[1],
                      "slope" = mod.us.ref$coefficients[2])
write.csv(coefs.us.ref, "results/coefs.us.ref.csv", row.names = F)

# --- Clear all variables ---
rm(list = ls()) 