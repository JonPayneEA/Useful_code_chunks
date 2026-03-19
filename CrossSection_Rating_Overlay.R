library(ggplot2)
library(dplyr)
library(scales)

# ── 1. Cross-section geometry ────────────────────────────────────────────────
cross_section <- tibble(
  distance_m  = c(0, 2, 5, 8, 10, 12, 15, 18, 22, 26, 30, 34, 38, 40, 42, 45),
  elevation_m = c(12.0, 11.8, 10.5, 9.2, 8.6, 8.3, 8.0, 8.1, 8.4, 9.0,
                  9.8, 10.6, 11.5, 11.9, 12.1, 12.3)
)

water_levels <- tibble(
  label       = c("Low flow", "Bankfull", "Flood"),
  elevation_m = c(8.8, 10.2, 11.4),
  colour      = c("#4fc3f7", "#0288d1", "#01579b")
)

channel_bottom <- min(cross_section$elevation_m)

make_water_poly <- function(wl, xs) {
  wet <- xs |> filter(elevation_m <= wl)
  bind_rows(
    tibble(distance_m = max(wet$distance_m), elevation_m = wl),
    wet,
    tibble(distance_m = min(wet$distance_m), elevation_m = wl)
  )
}

water_polys <- bind_rows(
  lapply(seq_len(nrow(water_levels)), function(i) {
    make_water_poly(water_levels$elevation_m[i], cross_section) |>
      mutate(stage = water_levels$label[i])
  })
)

# ── 2. Rating curve ──────────────────────────────────────────────────────────
H0 <- channel_bottom
a  <- 12
b  <- 1.65

rating_curve <- tibble(
  stage_m = seq(H0 + 0.05, 11.8, by = 0.05)
) |>
  mutate(discharge_m3s = a * (stage_m - H0)^b)

gauged_pts <- tibble(
  Q = c(1.5, 5, 14, 35, 80, 150, 260)
) |>
  mutate(H = H0 + (Q / a)^(1 / b))

# ── 3. Dual-axis scaling ─────────────────────────────────────────────────────
# Map discharge (x-axis of rating) onto distance_m (x-axis of cross-section)
# Rating curve will be plotted with distance_m on x, elevation on y —
# so we need to transform Q -> a pseudo-distance for the secondary axis trick.
# Instead, we place the rating curve in its own x-space on the RIGHT side,
# using sec_axis scaling on the x-axis.

Q_max    <- max(rating_curve$discharge_m3s) * 1.05
dist_max <- max(cross_section$distance_m)

# Scale: Q mapped onto [0, dist_max]
q_to_dist <- function(q) q / Q_max * dist_max
dist_to_q <- function(d) d / dist_max * Q_max

rating_curve <- rating_curve |>
  mutate(dist_scaled = q_to_dist(discharge_m3s))

gauged_pts <- gauged_pts |>
  mutate(dist_scaled = q_to_dist(Q))

# ── 4. Build plot ────────────────────────────────────────────────────────────
ggplot() +

  # Water fill polygons
  geom_polygon(data = water_polys |> filter(stage == "Flood"),
               aes(distance_m, elevation_m), fill = "#01579b", alpha = 0.20) +
  geom_polygon(data = water_polys |> filter(stage == "Bankfull"),
               aes(distance_m, elevation_m), fill = "#0288d1", alpha = 0.30) +
  geom_polygon(data = water_polys |> filter(stage == "Low flow"),
               aes(distance_m, elevation_m), fill = "#4fc3f7", alpha = 0.45) +

  # Water surface dashed lines
  geom_hline(data = water_levels,
             aes(yintercept = elevation_m, colour = label),
             linetype = "dashed", linewidth = 0.65) +

  # Stage labels on the left
  geom_label(data = water_levels,
             aes(x = 0.3, y = elevation_m + 0.13, label = label, colour = label),
             hjust = 0, size = 2.9, label.size = 0, fill = "white", alpha = 0.85) +

  # Channel bed
  geom_ribbon(data = cross_section,
              aes(x = distance_m, ymin = 6.5, ymax = elevation_m),
              fill = "#8d6e63") +
  geom_line(data = cross_section,
            aes(distance_m, elevation_m),
            colour = "#4e342e", linewidth = 1.3) +

  # ── Rating curve overlaid (using scaled x) ──
  # Below bankfull
  geom_line(data = rating_curve |> filter(stage_m <= water_levels$elevation_m[2]),
            aes(x = dist_scaled, y = stage_m),
            colour = "#f57c00", linewidth = 1.5) +
  # Above bankfull
  geom_line(data = rating_curve |> filter(stage_m >= water_levels$elevation_m[2]),
            aes(x = dist_scaled, y = stage_m),
            colour = "#e64a19", linewidth = 1.5) +

  # Gauged observation points
  geom_point(data = gauged_pts,
             aes(x = dist_scaled, y = H),
             shape = 21, size = 3, fill = "#fff9c4", colour = "#f57c00", stroke = 1.2) +

  # Rating curve label
  annotate("text", x = q_to_dist(200), y = 9.6,
           label = "Rating curve\n(discharge →)", colour = "#e64a19",
           size = 3, fontface = "bold", hjust = 0.5) +
  annotate("segment",
           x = q_to_dist(30), xend = q_to_dist(5),
           y = 9.2, yend = 8.95,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           colour = "#f57c00", linewidth = 0.6) +

  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_x_continuous(
    name   = "Distance across channel (m)",
    limits = c(0, dist_max),
    expand = c(0, 0),
    sec.axis = sec_axis(
      transform = dist_to_q,
      name      = "Discharge (m³/s)",
      labels    = comma_format()
    )
  ) +
  scale_y_continuous(
    name   = "Elevation (m AOD)",
    limits = c(6.5, 12.8),
    expand = c(0, 0)
  ) +
  scale_colour_manual(
    values = setNames(water_levels$colour, water_levels$label),
    guide  = "none"
  ) +

  # ── Theme ────────────────────────────────────────────────────────────────────
  labs(
    title   = "River Cross-Section with Rating Curve Overlay",
    caption = "Power-law rating: Q = 12·(H − H₀)^1.65  |  Orange circles = gauged observations"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.caption      = element_text(colour = "grey50", size = 9, hjust = 0.5),
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(colour = "grey75", fill = NA),
    axis.title.x.top  = element_text(colour = "#e64a19", face = "bold"),
    axis.text.x.top   = element_text(colour = "#e64a19")
  )
