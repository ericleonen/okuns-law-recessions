library(dplyr)
library(lmtest)
library(sandwich)

gdp_growth <- read.csv("data/processed/gdp_growth.csv")
unrate_diff <- read.csv("data/processed/unrate_diff.csv")

min_quarter <- max(gdp_growth$Quarter[1], unrate_diff$Quarter[1])
max_quarter <- min(last(gdp_growth$Quarter), last(unrate_diff$Quarter))

gdp_growth <- gdp_growth[between(gdp_growth$Quarter, min_quarter, max_quarter), ]
unrate_diff <- unrate_diff[between(unrate_diff$Quarter, min_quarter, max_quarter), ]

plot(x = unrate_diff$United.States,
     y = gdp_growth$United.States,
     pch = 19,
     col = "steelblue",
     xlab = "QoQ Unemployment Rate Change",
     ylab = "YoY Real GDP Growth",
     main = "Okun's Law for the United States, 2006-2025")
scatter(x = unrate_diff$United.States[gdp_growth$Quarter <= 2008],
        y = gdp_growth$United.States[gdp_growth$Quarter <= 2008],
        col = "red",
        pch = 19,
        add = T)

mod.us <- lm(Y ~ X)
abline(mod.us,
       col = "red",
       lwd = 2)

coeftest(mod.us, vcov. = vcovHC(mod.us, type = "HC1"))