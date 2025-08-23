library(dplyr)
library(stringr)

okun_results.states <- read.csv("results/okun_results.states.csv")

gdp_growth <- read.csv("data/processed/gdp_growth.csv")
unrate_diff <- read.csv("data/processed/unrate_diff.csv")

min_quarter <- max(gdp_growth$Quarter[1], unrate_diff$Quarter[1])
max_quarter <- min(last(gdp_growth$Quarter), last(unrate_diff$Quarter))

gdp_growth <- gdp_growth[between(gdp_growth$Quarter, min_quarter, max_quarter), ]
unrate_diff <- unrate_diff[between(unrate_diff$Quarter, min_quarter, max_quarter), ]

par(mfrow = c(1, 3))

# Top 3 States with Steepest Okun Coefficients
for (i in 1:3) {
  state_results <- okun_results.states[i, ]
  state_col <- str_replace(state_results$Area, " ", ".")
  
  plot(unrate_diff[[state_col]], gdp_growth[[state_col]],
       pch = 19,
       col = "steelblue",
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Growth (%)", ""),
       main = paste(state_results$Area, " (", round(state_results$coef, 3), ")"))
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
}

# Top 3 States with Flattest Okun Coefficients
for (i in 1:3) {
  state_results <- okun_results.states[nrow(okun_results.states) + 1 - i, ]
  state_col <- str_replace(state_results$Area, " ", ".")
  
  plot(unrate_diff[[state_col]], gdp_growth[[state_col]],
       pch = 19,
       col = "steelblue",
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Growth (%)", ""),
       main = paste(state_results$Area, " (", round(state_results$coef, 3), ")"))
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
}
