library(dplyr)
library(lmtest)
library(sandwich)
library(car)

col.overall = "black"
col.before_2008 = "steelblue"
col.between_2008_2020 = "orange"
col.after_2020 = "red"

data.us <- read.csv("data/processed.csv") %>%
  filter(Area == "United States")

after_2008 <- data.us$Quarter >= 2008
after_2020 <- data.us$Quarter >= 2020

plot(gdp_growth ~ unrate_diff,
     data = data.us,
     col = "white",
     xlab = "QoQ Unemployment Rate Change (%)",
     ylab = "QoQ Real GDP Annualized Growth (%)",
     main = sub("\\.", " ", "Okun's Law for United.States, 2006-2025"))
points(gdp_growth ~ unrate_diff,
       data = data.us[!after_2008, ],
       col = col.before_2008,
       pch = 19)
points(gdp_growth ~ unrate_diff,
       data = data.us[after_2008 & !after_2020, ],
       col = col.between_2008_2020,
       pch = 19)
points(gdp_growth ~ unrate_diff,
       data = data.us[after_2020, ],
       col = col.after_2020,
       pch = 19)

# Label outliers
outliers <- data.us[data.us$Quarter %in% c("2020 Q2", "2020 Q3"), ]

with(outliers[outliers$Quarter == "2020 Q2", ],
     text(unrate_diff, gdp_growth + 1, labels = Quarter, pos = 3, cex = 0.8, col = col.after_2020))

with(outliers[outliers$Quarter == "2020 Q3", ],
     text(unrate_diff, gdp_growth - 2, labels = Quarter, pos = 4, cex = 0.8, col = col.after_2020))

# Test if Okun's Law holds overall
mod.us.overall <- lm(gdp_growth ~ unrate_diff, data = data.us)
coeftest(mod.us.overall, vcov. = vcovHC(mod.us.overall, type = "HC1"))
# --- RESULTS ---
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  2.08815    0.32046  6.5162 6.460e-09 ***
#   unrate_diff -3.98717    0.80701 -4.9407 4.348e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

abline(mod.us.overall,
       col = col.overall,
       lwd = 2,
       lty = 2)

coefs.us.overall <- mod.us.overall$coefficients

# Test if Okun's Law is stable over periods
mod.us.recessions <- lm(gdp_growth ~ unrate_diff*after_2008 + unrate_diff*after_2020,
                        data = data.us)
coefs.us.recessions <- mod.us.recessions$coefficients

linearHypothesis(mod.us.recessions,
                 c("unrate_diff:after_2008TRUE = 0",
                   "unrate_diff:after_2020TRUE = 0"),
                 vcov. = vcovHC(mod.us.recessions, type = "HC1"))

# --- RESULTS ---
# Linear hypothesis test:
#   unrate_diff:after_2008TRUE = 0
# unrate_diff:after_2020TRUE = 0
# 
# Model 1: restricted model
# Model 2: gdp_growth ~ unrate_diff * after_2008 + unrate_diff * after_2020
# 
# Note: Coefficient covariance matrix supplied.
# 
# Res.Df Df      F Pr(>F)
# 1     76                 
# 2     74  2 0.0508 0.9505

abline(a = coefs.us.recessions[1],
       b = coefs.us.recessions[2],
       col = col.before_2008,
       lwd = 2)
abline(a = coefs.us.recessions[1] + coefs.us.recessions[3],
       b = coefs.us.recessions[2] + coefs.us.recessions[4],
       col = col.between_2008_2020,
       lwd = 2)
abline(a = coefs.us.recessions[1] + coefs.us.recessions[3] + coefs.us.recessions[5],
       b = coefs.us.recessions[2] + coefs.us.recessions[4] + coefs.us.recessions[6],
       col = col.after_2020,
       lwd = 2)

legend("topright",
       legend = c("overall", "before 2008", "between 2008 and 2020", "after 2020"),
       col = c(col.overall, col.before_2008, col.between_2008_2020, col.after_2020),
       lwd = 2,
       lty= c(2, 1, 1, 1),
       cex = 0.9)

okun_results.periods <- 
  data.frame(
    "Period" = c("2006-2020", "2006-2008", "2008-2020", "2020-2025"),
    "intercept" = c(coefs.us.overall[1], 
                     coefs.us.recessions[1], 
                     coefs.us.recessions[1] + coefs.us.recessions[3], 
                     coefs.us.recessions[1] + coefs.us.recessions[3] + coefs.us.recessions[5]),
    "coef" = c(coefs.us.overall[2],
                coefs.us.recessions[2],
                coefs.us.recessions[2] + coefs.us.recessions[4],
                coefs.us.recessions[2] + coefs.us.recessions[4] + coefs.us.recessions[6]))

write.csv(okun_results.periods, "results/okun_results.periods.csv", row.names = F)

# clear all variables
rm(list = ls())