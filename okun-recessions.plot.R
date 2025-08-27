# Plotting script to visually compare Okun coefficients across recessions along
# with US overall reference.

library(car)

# --- Define plotting styles ---
col.ref <- "black"
col.pre <- "steelblue"
col.GR <- "orange"
col.COVID <- "red"

pch <- 19

lwd <- 2

lty.ref <- 2

# --- Outlier labeling function ---
add_outlier_label <- function(data.us, q, pos) {
  with(subset(data.us, Quarter == q),
       text(unrate_diff, gdp_growth, labels = Quarter, pos = pos, col = "black"))
}

# --- Load US data and create period flags ---
data.us <- read.csv("data/processed.csv") %>%
  filter(Area == "United States")

after_2008 <- data.us$Quarter >= 2008
after_2020 <- data.us$Quarter >= 2020

# --- Load saved reference and recession model coefficients ---
coefs.us.ref <- read.csv("results/coefs.us.ref.csv")
coefs.us.recessions <- read.csv("results/coefs.us.recessions.csv", row.names = "Period")

# --- Plot data points colored by period
png("plots/scatter.recessions.png", width = 800, height = 600)
plot(gdp_growth ~ unrate_diff,
     data = data.us,
     col = "white",
     xlab = "QoQ Unemployment Rate Change (%)",
     ylab = "QoQ Real GDP Annualized Growth (%)",
     main = "Okun's Law for United States, 2005-2025")
points(gdp_growth ~ unrate_diff,
       data = data.us[after_2020, ],
       col = col.COVID,
       pch = pch)
points(gdp_growth ~ unrate_diff,
       data = data.us[after_2008 & !after_2020, ],
       col = col.GR,
       pch = pch)
points(gdp_growth ~ unrate_diff,
       data = data.us[!after_2008, ],
       col = col.pre,
       pch = pch)

# --- Plot linear regressions ---
abline(a = coefs.us.ref[1, "intercept"],
       b = coefs.us.ref[1, "slope"],
       col = col.ref,
       lwd = lwd,
       lty = lty.ref)
abline(a = coefs.us.recessions["pre", "intercept"],
       b = coefs.us.recessions["pre", "slope"],
       col = col.pre,
       lwd = lwd)
abline(a = coefs.us.recessions["GR", "intercept"],
       b = coefs.us.recessions["GR", "slope"],
       col = col.GR,
       lwd = lwd)
abline(a = coefs.us.recessions["COVID", "intercept"],
       b = coefs.us.recessions["COVID", "slope"],
       col = col.COVID,
       lwd = lwd)

# --- Label outliers ---
add_outlier_label(data.us, "2020 Q2", pos = 3)
add_outlier_label(data.us, "2020 Q3", pos = 1)

# -- Add legend ---
legend("topright",
       legend = c("all periods", "before 2008", "2008-2020", "after 2020"),
       col = c(col.ref, col.pre, col.GR, col.COVID),
       lwd = lwd,
       lty= c(lty.ref, 1, 1, 1))

# --- Clear all variables and turn of device ---
rm(list = ls())
dev.off()