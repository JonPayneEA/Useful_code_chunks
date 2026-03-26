# ==============================================================================
# 4-Parameter Rainfall-Runoff Model (GR4J-style lumped conceptual model)
# ==============================================================================
# 
# Parameters:
# x1 - Maximum capacity of the production store (mm)
# x2 - Groundwater exchange coefficient (mm/day)
# x3 - Maximum capacity of the routing store (mm)
# x4 - Unit hydrograph time base (days)
# 
# Inputs:  Precipitation (P), Potential Evapotranspiration (PET)
# Outputs: Simulated streamflow (Q)
# ==============================================================================

library(data.table)

# — 1. Generate synthetic rainfall & PET data ––––––––––––––––
set.seed(42)
n_days <- 730 # 2 years

dates <- seq.Date(as.Date("2024-01-01"), by = "day", length.out = n_days)
day_of_year <- as.integer(format(dates, "%j"))

# Seasonal rainfall: wetter in winter, drier in summer (UK-ish climate)
seasonal_mean <- 3.0 + 2.5 * cos(2 * pi * (day_of_year - 15) / 365)
rain_raw       <- rgamma(n_days, shape = 0.6, scale = seasonal_mean / 0.6)
rain_raw[runif(n_days) < 0.45] <- 0  # ~45% dry days
P <- round(rain_raw, 2)

# Inject a few storm events

storm_days <- c(45, 120, 200, 310, 410, 550, 680)
for (sd in storm_days) {
  idx <- sd:min(sd + sample(2:5, 1), n_days)
  P[idx] <- P[idx] + runif(length(idx), 15, 45)
}

# PET: sinusoidal seasonal cycle (higher in summer)
PET <- round(pmax(0, 1.5 + 2.5 * sin(2 * pi * (day_of_year - 80) / 365) +
                    rnorm(n_days, 0, 0.3)), 2)

climate_dt <- data.table(date = dates, P = P, PET = PET)

cat("Synthetic climate data:\n")
print(summary(climate_dt[, .(P, PET)]))
cat(sprintf("\nTotal rainfall:  %.0f mm over %d days\n", sum(P), n_days))
cat(sprintf("Total PET:       %.0f mm over %d days\n", sum(PET), n_days))

# — 2. GR4J-inspired 4-parameter model –––––––––––––––––––
gr4j_run <- function(P, PET, x1, x2, x3, x4) {

  # P   = precipitation vector (mm/day)
  # PET = potential ET vector (mm/day)
  # x1  = production store capacity (mm), typical range [100, 1500]
  # x2  = groundwater exchange coeff (mm/day), typical range [-5, 5]
  # x3  = routing store capacity (mm), typical range [10, 500]
  # x4  = UH time base (days), typical range [0.5, 4]
  n <- length(P)
  
  # –– Unit hydrograph ordinates (SH1 - half based on x4) ––
  
  nh    <- ceiling(x4)
  UH    <- numeric(nh)
  for (k in seq_len(nh)) {
    UH[k] <- ifelse(k <= x4, (k / x4)^2.5 - ((k - 1) / x4)^2.5, 0)
  }
  UH <- UH / sum(UH)  # normalise
  
  # –– State variables ––
  
  S  <- x1 * 0.5   # production store level (start half-full)
  R  <- x3 * 0.5   # routing store level (start half-full)
  UH_queue <- rep(0, nh)
  
  Q_sim <- numeric(n)
  
  for (t in seq_len(n)) {
    # – Production store –
    # Net rainfall vs net ET
    if (P[t] >= PET[t]) {
      Pn  <- P[t] - PET[t]
      En  <- 0
      # Fraction entering production store
      ps  <- pmin(1, S / x1)
      Ps  <- x1 * (1 - ps^2) * tanh(Pn / x1) / (1 + ps * tanh(Pn / x1))
      S   <- S + Ps
      Pr  <- Pn - Ps  # excess rainfall (effective rainfall)
    } else {
      Pn  <- 0
      En  <- PET[t] - P[t]
      ps  <- pmin(1, S / x1)
      Es  <- S * (2 - ps) * tanh(En / x1) / (1 + (1 - ps) * tanh(En / x1))
      S   <- S - Es
      Pr  <- 0
    }
    
    # Percolation from production store
    perc <- S * (1 - (1 + (4 / 9 * S / x1)^4)^(-0.25))
    S    <- S - perc
    Pr   <- Pr + perc
    
    # -- Unit hydrograph convolution --
    UH_queue <- UH_queue + UH * Pr
    QUH      <- UH_queue[1]
    UH_queue <- c(UH_queue[-1], 0)
    
    # -- Routing store --
    # Groundwater exchange
    F <- x2 * (R / x3)^3.5
    R <- pmax(0, R + QUH * 0.9 + F)
    
    # Routing store outflow
    R_ratio <- pmin(1, R / x3)
    Qr      <- R * (1 - (1 + R_ratio^4)^(-0.25))
    R       <- R - Qr
    
    # Direct flow component
    Qd <- pmax(0, QUH * 0.1 + F)
    
    Q_sim[t] <- Qr + Qd

  }
  
  return(Q_sim)
}

# — 3. Run model with default parameters ————————————

params <- list(
  x1 = 350,   # production store capacity (mm)
  x2 = -0.5,  # groundwater exchange (mm/day) - slight loss
  x3 = 90,    # routing store capacity (mm)
  x4 = 1.7    # UH time base (days)
)

cat("\nModel parameters:\n")
cat(sprintf("  x1 (production store capacity) = %.0f mm\n", params$x1))
cat(sprintf("  x2 (groundwater exchange)      = %.1f mm/day\n", params$x2))
cat(sprintf("  x3 (routing store capacity)    = %.0f mm\n", params$x3))
cat(sprintf("  x4 (UH time base)              = %.1f days\n", params$x4))

Q_sim <- gr4j_run(P, PET, params$x1, params$x2, params$x3, params$x4)

climate_dt[, Q := round(Q_sim, 3)]

cat(sprintf("\nSimulated flow summary:\n"))
print(summary(climate_dt$Q))
cat(sprintf("Total runoff:    %.0f mm\n", sum(climate_dt$Q)))
cat(sprintf("Runoff ratio:    %.2f\n", sum(climate_dt$Q) / sum(P)))

# — 4. Sensitivity: vary each parameter ———————————––

cat("\n— Parameter sensitivity (% change in total Q) —\n")

base_Q <- sum(Q_sim)
deltas <- c(x1 = 500, x2 = 0.5, x3 = 150, x4 = 2.5)

for (nm in names(deltas)) {
  p_test       <- params
  p_test[[nm]] <- deltas[[nm]]
  Q_test       <- gr4j_run(P, PET, p_test$x1, p_test$x2, p_test$x3, p_test$x4)
  pct          <- (sum(Q_test) - base_Q) / base_Q * 100
  cat(sprintf("  %s: %.0f -> %.0f  =>  %+.1f%% change in total Q\n",
               nm, params[[nm]], deltas[[nm]], pct))
}

# — 5. Plots ––––––––––––––––––––––––––––––––

# Hydrograph + rainfall

par(mar = c(4, 4, 3, 4), mfrow = c(2, 1))

# Top panel: rainfall (inverted) + flow

plot(dates, Q_sim, type = "l", col = "steelblue", lwd = 1.5,
     xlab = "", ylab = "Flow (mm/day)",
     main = "GR4J 4-Parameter Model: Simulated Hydrograph",
     ylim = c(0, max(Q_sim) * 1.1))
par(new = TRUE)
barplot(P, col = rgb(0.2, 0.4, 0.8, 0.3), border = NA,
        axes = FALSE, xlab = "", ylab = "", space = 0,
        ylim = c(max(P) * 3, 0))
axis(4, at = pretty(c(0, max(P))), labels = pretty(c(0, max(P))))
mtext("Rainfall (mm/day)", side = 4, line = 2.5)
legend("topright", legend = c("Simulated Q", "Rainfall"),
        col = c("steelblue", rgb(0.2, 0.4, 0.8, 0.3)),
        lwd = c(2, NA), pch = c(NA, 15), pt.cex = 2, bg = "white")

# Bottom panel: flow duration curve

Q_sorted <- sort(Q_sim, decreasing = TRUE)
exceedance <- seq_along(Q_sorted) / length(Q_sorted) * 100
plot(exceedance, Q_sorted, type = "l", col = "darkred", lwd = 2,
     log = "y", xlab = "% Time Exceeded", ylab = "Flow (mm/day, log scale)",
     main = "Flow Duration Curve")
grid()

cat("\nDone. climate_dt contains date, P, PET, and simulated Q.\n")
