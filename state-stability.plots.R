library(dplyr)
library(stringr)

okun_results.states <- read.csv("results/okun_results.states.csv")
data.states <- read.csv("data/processed.csv") %>%
  filter(Area != "United States")

par(mfrow = c(1, 3))

# Top 3 States with Steepest Okun Coefficients
for (i in 1:3) {
  state_results <- okun_results.states[i, ]
  state <- str_replace(state_results$Area, " ", ".")
  
  plot(gdp_growth ~ unrate_diff,
       data = data.states[data.states$Area == state_results$Area, ],
       pch = 19,
       col = "steelblue",
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Growth (%)", ""),
       main = paste(state_results$Area, " (", round(state_results$coef, 2), ")"))
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
}

# Top 3 States with Flattest Okun Coefficients
for (i in 1:3) {
  state_results <- okun_results.states[nrow(okun_results.states) + 1 - i, ]
  state <- str_replace(state_results$Area, " ", ".")
  
  plot(gdp_growth ~ unrate_diff,
       data = data.states[data.states$Area == state_results$Area, ],
       pch = 19,
       col = "steelblue",
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Growth (%)", ""),
       main = paste(state_results$Area, " (", round(state_results$coef, 2), ")"))
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
}
