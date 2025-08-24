library(dplyr)
library(stringr)

okun_results.states <- read.csv("results/okun_results.states.csv")
data.states <- read.csv("data/processed.csv") %>%
  filter(Area != "United States")

hist(okun_results.states$intercept,
     xlab = "Okun Intercept",
     main = "Distribution of U.S. Okun Intercepts Across States",
     breaks = 10)

hist(okun_results.states$coef,
     xlab = "Okun Slope",
     main = "Distribution of U.S. Okun Slopes Across States",
     breaks = 10)

par(mfrow = c(1, 3))

add_outlier_label <- function(data.state, q, pos) {
  with(data.state[data.state$Quarter == q, ],
       text(unrate_diff, gdp_growth, labels = Quarter, pos = pos, col = "black"))
}

# Top 3 States with Steepest Okun Coefficients
for (i in 1:3) {
  state_results <- okun_results.states[i, ]
  state <- str_replace(state_results$Area, " ", ".")
  data.state <- data.states[data.states$Area == state_results$Area, ]
  
  plot(gdp_growth ~ unrate_diff,
       data = data.state,
       pch = 19,
       col = "steelblue",
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Annualized Growth (%)", ""),
       main = paste(state_results$Area, " (", round(state_results$coef, 2), ")"))
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
  
  add_outlier_label(data.state, "2020 Q2", 2)
  
  if (state == "Wyoming") {
    add_outlier_label(data.state, "2006 Q1", 2)
    add_outlier_label(data.state, "2008 Q4", 2)
  } else {
    add_outlier_label(data.state, "2020 Q3", 4)
  }
  
  if (state == "South.Dakota") {
    add_outlier_label(data.state, "2008 Q1", 4)
    add_outlier_label(data.state, "2012 Q3", 2)
  }
}

# Top 3 States with Flattest Okun Coefficients
for (i in 1:3) {
  state_results <- okun_results.states[nrow(okun_results.states) + 1 - i, ]
  state <- str_replace(state_results$Area, " ", ".")
  data.state <- data.states[data.states$Area == state_results$Area, ]
  
  plot(gdp_growth ~ unrate_diff,
       data = data.state,
       pch = 19,
       col = "steelblue",
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Annualized Growth (%)", ""),
       main = paste(state_results$Area, " (", round(state_results$coef, 2), ")"))
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
  
  abline(a = state_results$intercept,
         b = state_results$coef,
         col = "red",
         lwd = 2)
  
  add_outlier_label(data.state, "2020 Q2", 2)
  
  if (state != "Hawaii") {
    add_outlier_label(data.state, "2020 Q3", 4)
  }
  
  if (state == "Delaware") {
    add_outlier_label(data.state, "2005 Q4", 4)
    add_outlier_label(data.state, "2009 Q1", 4)
  }
}
