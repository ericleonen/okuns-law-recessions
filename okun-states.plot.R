# Plotting script to visually compare states with steepest and flattest Okun
# slopes to each other and national reference.

library(dplyr)
library(stringr)

# --- Define plotting styles ---
col.points <- "steelblue"
col.line.ref <- "black"
col.line <- "red"

pch <- 19

lwd <- 2

lty.ref <- 2

# --- Outlier labeling function ---
add_outlier_label <- function(data.state, q, pos) {
  with(subset(data.state, Quarter == q),
       text(unrate_diff, gdp_growth, labels = Quarter, pos = pos, col = "black"))
}

# --- Load state data ---
data.states <- read.csv("data/processed.csv") %>%
  filter(Area != "United States")

# --- Load saved reference and state model coefficients ---
coefs.us.ref <- read.csv("results/coefs.us.ref.csv")
results.states <- read.csv("results/coefs.states.csv")

# --- Plot histogram comparing reference slope to distribution of state slopes ---
png("plots/hist.state-slopes-distribution.png", width = 800, height = 600)
hist(results.states$slope,
     xlab = "Okun Slope",
     main = "Distribution of Okun Slopes Across States",
     breaks = 10,
     col = col.points)
abline(v = coefs.us.ref[1, "slope"],
       col = col.line.ref,
       lwd = lwd,
       lty = lty.ref)
legend("topright",
       legend = "national",
       lwd = lwd,
       lty = lty.ref,
       col = col.line.ref)

dev.off()

# --- Plot points and regressions of states with steepest Okun slopes with reference
png("plots/scatter.state-steepest-slopes.png", width = 1200, height = 400)
par(mfrow = c(1, 3))

for (i in 1:3) {
  state <- results.states[i, "Area"]
  coefs.state <- results.states[i, c("intercept", "slope")]
  data.state <- data.states[data.states$Area == state, ]
  
  plot(gdp_growth ~ unrate_diff,
       data = data.state,
       pch = pch,
       col = col.points,
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Annualized Growth (%)", ""),
       main = paste(str_replace(state, "\\.", " "), " (", round(coefs.state$slope, 2), ")"))
  
  abline(a = coefs.us.ref[1, "intercept"],
         b = coefs.us.ref[1, "slope"],
         lty = lty.ref,
         col = col.line.ref)
  
  abline(a = coefs.state$intercept,
         b = coefs.state$slope,
         col = col.line,
         lwd = lwd)
  
  add_outlier_label(data.state, "2020 Q2", 2)
  
  if (state == "Wyoming") {
    add_outlier_label(data.state, "2006 Q1", 2)
    add_outlier_label(data.state, "2008 Q4", 2)
  } else {
    add_outlier_label(data.state, "2020 Q3", 4)
  }
  
  if (state == "South Dakota") {
    add_outlier_label(data.state, "2008 Q1", 4)
    add_outlier_label(data.state, "2012 Q3", 2)
  }
  
  if (i == 3) {
    legend("topright",
           legend = c("national", "state"),
           lty = c(lty.ref, 1),
           col = c(col.line.ref, col.line))
  }
}

dev.off()

# --- Plot points and regressions of states with flattest Okun slopes with reference
png("plots/scatter.state-flattest-slopes.png", width = 1200, height = 400)
par(mfrow = c(1, 3))

for (i in 1:3) {
  j <- nrow(results.states) + 1 - i
  state <- results.states[j, "Area"]
  coefs.state <- results.states[j, c("intercept", "slope")]
  data.state <- data.states[data.states$Area == state, ]
  
  plot(gdp_growth ~ unrate_diff,
       data = data.state,
       pch = pch,
       col = col.points,
       xlab = "QoQ Unemployment Rate Change (%)",
       ylab = ifelse(i == 1, "QoQ Real GDP Annualized Growth (%)", ""),
       main = paste(str_replace(state, "\\.", " "), " (", round(coefs.state$slope, 2), ")"))
  
  abline(a = coefs.us.ref[1, "intercept"],
         b = coefs.us.ref[1, "slope"],
         lty = lty.ref,
         col = col.line.ref)
  
  abline(a = coefs.state$intercept,
         b = coefs.state$slope,
         col = col.line,
         lwd = lwd)
  
  add_outlier_label(data.state, "2020 Q2", 2)
  
  if (state != "Hawaii") {
    add_outlier_label(data.state, "2020 Q3", 4)
  }
  
  if (state == "Delaware") {
    add_outlier_label(data.state, "2005 Q4", 4)
    add_outlier_label(data.state, "2009 Q1", 4)
  }
  
  if (i == 3) {
    legend("topright",
           legend = c("national", "state"),
           lty = c(lty.ref, 1),
           col = c(col.line.ref, col.line))
  }
}

dev.off()

# --- Clear all variables and turn of device ---
rm(list = ls())
dev.off()