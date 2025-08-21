library(dplyr)
library(lmtest)
library(sandwich)
library(car)

gdp_growth <- read.csv("data/processed/gdp_growth.csv")
unrate_diff <- read.csv("data/processed/unrate_diff.csv")

min_quarter <- max(gdp_growth$Quarter[1], unrate_diff$Quarter[1])
max_quarter <- min(last(gdp_growth$Quarter), last(unrate_diff$Quarter))

gdp_growth <- gdp_growth[between(gdp_growth$Quarter, min_quarter, max_quarter), ]
unrate_diff <- unrate_diff[between(unrate_diff$Quarter, min_quarter, max_quarter), ]

after_2008 <- gdp_growth$Quarter >= 2008
after_2020 <- gdp_growth$Quarter >= 2020

plot(gdp_growth$United.States ~ unrate_diff$United.States,
     col = "white",
     xlab = "QoQ Unemployment Rate Change",
     ylab = "QoQ Real GDP Growth",
     main = sub("\\.", " ", "Okun's Law for United.States, 2006-2025"))
points(gdp_growth$United.States[!after_2008] ~ unrate_diff$United.States[!after_2008],
       col = "red",
       pch = 19)
points(gdp_growth$United.States[after_2008 & !after_2020] ~ unrate_diff$United.States[after_2008 & !after_2020],
       col = "violet",
       pch = 19)
points(gdp_growth$United.States[after_2020] ~ unrate_diff$United.States[after_2020],
       col = "steelblue",
       pch = 19)

# Test if Okun's Law holds overall
mod.us.overall <- lm(gdp_growth$United.States ~ unrate_diff$United.States)
coeftest(mod.us.overall, vcov. = vcovHC(mod.us.overall, type = "HC1"))
abline(mod.us.overall,
       col = "black",
       lwd = 2,
       lty = 2)

# Test if Okun's Law is stable over periods
mod.us.recessions <- lm(gdp_growth$United.States ~ unrate_diff$United.States*after_2008 + unrate_diff$United.States*after_2020)
coefs.us.recessions <- mod.us.recessions$coefficients

linearHypothesis(mod.us.recessions,
                 c("unrate_diff$United.States:after_2008TRUE = 0",
                   "unrate_diff$United.States:after_2020TRUE = 0"),
                 vcov. = vcovHC(mod.us.recessions, type = "HC1"))

abline(a = coefs.us.recessions[1],
       b = coefs.us.recessions[2],
       col = "red",
       lwd = 2)
abline(a = coefs.us.recessions[1] + coefs.us.recessions[3],
       b = coefs.us.recessions[2] + coefs.us.recessions[4],
       col = "violet",
       lwd = 2)
abline(a = coefs.us.recessions[1] + coefs.us.recessions[3] + coefs.us.recessions[5],
       b = coefs.us.recessions[2] + coefs.us.recessions[4] + coefs.us.recessions[6],
       col = "steelblue",
       lwd = 2)

legend("topright",
       legend = c("overall", "before 2008", "between 2008 and 2020", "after 2020"),
       col = c("black", "red", "violet", "steelblue"),
       lwd = 2,
       lty= c(2, 1, 1, 1))
