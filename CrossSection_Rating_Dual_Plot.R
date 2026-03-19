library(ggplot2)
library(patchwork)
library(dplyr)

# ── 1. Cross-section geometry ────────────────────────────────────────────────
# Simulate a natural channel bed (distance, elevation)
cross_section <- tibble(
  distance_m = c(0, 2, 5, 8, 10, 12, 15, 18, 22, 26, 30, 34, 38, 40, 42, 45),
  elevation_m = c(12.0, 11.8, 10.5, 9.2, 8.6, 8.3, 8.0, 8.1, 8.4, 9.0,
                  9.8, 10.6, 11.5, 11.9, 12.1, 12.3)
)

# Bank-full / water surface levels to annotate
water_levels <- tibble(
  label      = c("Low flow",  "Bankfull",  "Flood"),
  elevation_m = c(8.8,         10.2,         11.4),
  colour     = c("#4fc3f7",   "#0288d1",   "#01579b")
)

channel_bottom <- min(cross_section$elevation_m)

# Build water-filled polygons for each stage
make_water_poly <- function(wl, xs) {
  wet <- xs |>
    filter(elevation_m <= wl) |>
    bind_rows(
      tibble(distance_m = max(xs$distance_m[xs$elevation_m <= wl]),
             elevation_m = wl),
      tibble(distance_m = min(xs$distance_m[xs$elevation_m <= wl]),
             elevation_m = wl)
    )
  wet
}

water_polys <- bind_rows(
  lapply(seq_len(nrow(water_levels)), function(i) {
    make_water_poly(water_levels$elevation_m[i], cross_section) |>
      mutate(stage = water_levels$label[i])
  })
)

# ── 2. Rating curve (Stage–Discharge) ───────────────────────────────────────
# Manning-style power law:  Q = a * (H - H0)^b
H0  <- channel_bottom        # zero-flow datum
a   <- 12                    # scale coefficient
b   <- 1.65                  # exponent

rating_curve <- tibble(
  stage_m = seq(H0 + 0.05, 11.8, by = 0.05)
) |>
  mutate(
    discharge_m3s = a * (stage_m - H0)^b,
    above_bankfull = stage_m >= water_levels$elevation_m[2]
  )

# ── 3. Plot A – Cross section ────────────────────────────────────────────────
p_xs <- ggplot() +

  # Water fill polygons (lowest stage on top so colours stack nicely)
  geom_polygon(
    data = water_polys |> filter(stage == "Flood"),
    aes(distance_m, elevation_m),
    fill = "#01579b", alpha = 0.25
  ) +
  geom_polygon(
    data = water_polys |> filter(stage == "Bankfull"),
    aes(distance_m, elevation_m),
    fill = "#0288d1", alpha = 0.35
  ) +
  geom_polygon(
    data = water_polys |> filter(stage == "Low flow"),
    aes(distance_m, elevation_m),
    fill = "#4fc3f7", alpha = 0.5
  ) +

  # Water surface lines
  geom_hline(
    data = water_levels,
    aes(yintercept = elevation_m, colour = label),
    linetype = "dashed", linewidth = 0.7
  ) +

  # Channel bed
  geom_ribbon(
    data = cross_section,
    aes(x = distance_m, ymin = 6.5, ymax = elevation_m),
    fill = "#8d6e63", colour = NA
  ) +
  geom_line(
    data = cross_section,
    aes(distance_m, elevation_m),
    colour = "#4e342e", linewidth = 1.2
  ) +

  # Water surface labels
  geom_label(
    data = water_levels,
    aes(x = 1, y = elevation_m, label = label, colour = label),
    hjust = 0, size = 3, label.size = 0, fill = "white", alpha = 0.8
  ) +

  scale_colour_manual(
    values = setNames(water_levels$colour, water_levels$label),
    guide  = "none"
  ) +
  scale_y_continuous(limits = c(6.5, 12.8), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title   = "River Cross-Section with Stage Levels",
    x       = "Distance across channel (m)",
    y       = "Elevation (m AOD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    panel.border    = element_rect(colour = "grey80", fill = NA)
  )

# ── 4. Plot B – Rating curve ─────────────────────────────────────────────────
p_rc <- ggplot(rating_curve, aes(x = discharge_m3s, y = stage_m)) +

  # Flood zone shading
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = water_levels$elevation_m[2],
           ymax = Inf,
           fill = "#01579b", alpha = 0.08
  ) +

  # Bankfull line
  geom_hline(
    yintercept = water_levels$elevation_m[2],
    colour = "#0288d1", linetype = "dashed", linewidth = 0.7
  ) +
  annotate("text",
           x    = max(rating_curve$discharge_m3s) * 0.95,
           y    = water_levels$elevation_m[2] + 0.12,
           label = "Bankfull", colour = "#0288d1",
           hjust = 1, size = 3
  ) +

  # Rating curve — colour changes above bankfull
  geom_line(
    data = rating_curve |> filter(!above_bankfull),
    colour = "#0288d1", linewidth = 1.4
  ) +
  geom_line(
    data = rating_curve |> filter(above_bankfull),
    colour = "#01579b", linewidth = 1.4, linetype = "solid"
  ) +

  # Observed gauged points (simulated)
  geom_point(
    data = tibble(
      Q = c(1.5, 5, 14, 35, 80, 150, 260),
      H = H0 + (c(1.5, 5, 14, 35, 80, 150, 260) / a)^(1 / b)
    ),
    aes(Q, H),
    colour = "#e65100", size = 2.5, shape = 21,
    fill = "#ff8a65", stroke = 1
  ) +
  annotate("text", x = 30, y = 8.45, label = "Gauged observations",
           colour = "#e65100", size = 3, hjust = 0) +

  scale_x_continuous(labels = scales::comma_format(suffix = " m³/s"),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(channel_bottom, 11.8),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Rating Curve (Stage–Discharge)",
    x     = "Discharge (m³/s)",
    y     = "Stage / Water surface elevation (m AOD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey80", fill = NA)
  )

# ── 5. Combine and save ───────────────────────────────────────────────────────
combined <- p_xs / p_rc +
  plot_annotation(
    title   = "Hydrometric Station – Cross-Section & Rating Curve",
    caption = "Synthetic example  |  Manning-derived power-law rating  |  Q = 12·(H − H₀)^1.65",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.caption = element_text(colour = "grey50", size = 9)
    )
  )

print(combined)

# Optionally save:
# ggsave("river_cross_section_rating.png", combined, width = 10, height = 10, dpi = 200)
