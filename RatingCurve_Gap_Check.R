# RatingCurve_Gap_Check.R
#
# Utilities for detecting and resolving discontinuities ("gaps") between
# rating-curve limbs. When a rating curve is built from multiple independently-
# fitted segments (limbs), the end discharge of one limb often does not exactly
# match the start discharge of the next at the shared breakpoint stage, leaving
# a visible gap in plots and a hydraulic inconsistency in the table.
#
# Functions exported:
#   detect_rc_gaps()   -- report gaps at every limb junction
#   resolve_rc_gaps()  -- close gaps by one of three strategies
#   plot_rc_gaps()     -- before/after diagnostic plot
#
# Expected input format (data frame / tibble):
#   stage_col      -- water surface elevation (or depth), numeric, ascending
#   discharge_col  -- corresponding discharge, numeric
#   limb_col       -- integer or character limb identifier; rows must be
#                     ordered stage-ascending within each limb and limbs must
#                     be ordered so limb boundaries are contiguous.
#                     If NULL the functions attempt to auto-detect limbs from
#                     monotonicity breaks in discharge.

library(dplyr)
library(ggplot2)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Linear interpolation: given two (x, y) pairs, return y at x_new.
.interp_y <- function(x1, y1, x2, y2, x_new) {
  y1 + (y2 - y1) * (x_new - x1) / (x2 - x1)
}

# Auto-detect limb boundaries from monotonicity breaks in discharge.
# A break is flagged where the *proportional* jump in Q exceeds `tol_frac`.
.auto_limb <- function(q, tol_frac = 0.05) {
  n <- length(q)
  limb <- integer(n)
  limb[1L] <- 1L
  k <- 1L
  for (i in seq(2L, n)) {
    dq <- abs(q[i] - q[i - 1L])
    ref <- max(abs(q[i - 1L]), 1e-9)
    if (dq / ref > tol_frac && q[i] < q[i - 1L]) {
      # Discharge dropped — treat as a new limb starting here
      k <- k + 1L
    }
    limb[i] <- k
  }
  limb
}


# ---------------------------------------------------------------------------
# detect_rc_gaps()
# ---------------------------------------------------------------------------
#
# Scans every junction between consecutive limbs and measures the absolute and
# relative discharge gap.
#
# Arguments:
#   rc             data frame with rating curve data (stage ascending)
#   stage_col      name of stage column          (default "stage")
#   discharge_col  name of discharge column      (default "discharge")
#   limb_col       name of limb-ID column        (default "limb"); if NULL,
#                  limbs are auto-detected
#   tol_abs        absolute discharge tolerance for flagging a gap (m³/s)
#   tol_rel        relative discharge tolerance (fraction of lower-limb end Q)
#
# Returns a data frame with one row per junction:
#   junction       junction index (1 = between limb 1 and 2, etc.)
#   limb_lower     limb ID of the lower limb
#   limb_upper     limb ID of the upper limb
#   stage_break    stage value at the junction
#   q_lower_end    last discharge of the lower limb
#   q_upper_start  first discharge of the upper limb
#   gap_abs        absolute gap  (q_upper_start - q_lower_end)
#   gap_rel        relative gap  (gap_abs / q_lower_end)
#   gap_flagged    logical: TRUE if gap exceeds either tolerance

detect_rc_gaps <- function(rc,
                           stage_col     = "stage",
                           discharge_col = "discharge",
                           limb_col      = "limb",
                           tol_abs       = 0.5,
                           tol_rel       = 0.02) {

  rc <- as.data.frame(rc)

  # Resolve limb column
  if (is.null(limb_col) || !limb_col %in% names(rc)) {
    message("'", limb_col, "' not found — auto-detecting limbs from discharge monotonicity.")
    rc[["limb_"]] <- .auto_limb(rc[[discharge_col]])
    limb_col <- "limb_"
  }

  limbs <- unique(rc[[limb_col]])

  if (length(limbs) < 2L) {
    message("Only one limb detected — no junctions to check.")
    return(invisible(NULL))
  }

  results <- vector("list", length(limbs) - 1L)

  for (j in seq_len(length(limbs) - 1L)) {
    lower_id <- limbs[j]
    upper_id <- limbs[j + 1L]

    lower_rows <- rc[rc[[limb_col]] == lower_id, ]
    upper_rows <- rc[rc[[limb_col]] == upper_id, ]

    # Sort each limb by stage to be safe
    lower_rows <- lower_rows[order(lower_rows[[stage_col]]), ]
    upper_rows <- upper_rows[order(upper_rows[[stage_col]]), ]

    q_low  <- lower_rows[[discharge_col]][nrow(lower_rows)]  # end of lower limb
    q_high <- upper_rows[[discharge_col]][1L]                # start of upper limb
    s_brk  <- lower_rows[[stage_col]][nrow(lower_rows)]

    gap_abs <- q_high - q_low
    gap_rel <- if (abs(q_low) > 1e-9) gap_abs / q_low else NA_real_

    results[[j]] <- data.frame(
      junction      = j,
      limb_lower    = as.character(lower_id),
      limb_upper    = as.character(upper_id),
      stage_break   = s_brk,
      q_lower_end   = q_low,
      q_upper_start = q_high,
      gap_abs       = gap_abs,
      gap_rel       = gap_rel,
      gap_flagged   = abs(gap_abs) > tol_abs | (!is.na(gap_rel) & abs(gap_rel) > tol_rel),
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, results)

  n_flagged <- sum(out$gap_flagged)
  message(sprintf("Checked %d junction(s): %d gap(s) flagged.", nrow(out), n_flagged))

  out
}


# ---------------------------------------------------------------------------
# resolve_rc_gaps()
# ---------------------------------------------------------------------------
#
# Closes discharge gaps at limb junctions using one of three strategies:
#
#   "interpolate"  (default)
#     Inserts a bridging row at the breakpoint stage in BOTH the lower and
#     upper limbs. The lower limb's bridging Q is linearly interpolated from
#     its two end points; similarly for the upper limb. The two bridging Qs
#     are then averaged so both limbs share the same value at the junction.
#     This preserves the shape of both limbs and merely pins their endpoints
#     to meet at a single agreed discharge.
#
#   "snap_to_lower"
#     Sets the first discharge of the upper limb equal to the last discharge
#     of the lower limb. Only the upper limb's starting point is moved.
#     Use when the lower limb is trusted (e.g. gauged) and the upper is
#     extrapolated.
#
#   "snap_to_upper"
#     Sets the last discharge of the lower limb equal to the first discharge
#     of the upper limb. Only the lower limb's endpoint is moved.
#     Use when the upper limb anchor (e.g. a flood frequency estimate) is
#     trusted.
#
# Arguments:
#   rc             data frame with rating curve data
#   stage_col      name of stage column          (default "stage")
#   discharge_col  name of discharge column      (default "discharge")
#   limb_col       name of limb-ID column        (default "limb")
#   method         one of "interpolate", "snap_to_lower", "snap_to_upper"
#   tol_abs        passed to detect_rc_gaps() — only flagged junctions are fixed
#   tol_rel        passed to detect_rc_gaps()
#
# Returns the corrected data frame (same columns, rows sorted stage-ascending).

resolve_rc_gaps <- function(rc,
                            stage_col     = "stage",
                            discharge_col = "discharge",
                            limb_col      = "limb",
                            method        = c("interpolate",
                                             "snap_to_lower",
                                             "snap_to_upper"),
                            tol_abs       = 0.5,
                            tol_rel       = 0.02) {

  method <- match.arg(method)
  rc     <- as.data.frame(rc)

  # Auto-add limb column if missing
  if (is.null(limb_col) || !limb_col %in% names(rc)) {
    message("'", limb_col, "' not found — auto-detecting limbs.")
    rc[["limb_"]] <- .auto_limb(rc[[discharge_col]])
    limb_col <- "limb_"
  }

  gaps <- detect_rc_gaps(rc,
                         stage_col     = stage_col,
                         discharge_col = discharge_col,
                         limb_col      = limb_col,
                         tol_abs       = tol_abs,
                         tol_rel       = tol_rel)

  if (is.null(gaps) || !any(gaps$gap_flagged)) {
    message("No gaps to resolve — returning input unchanged.")
    return(rc)
  }

  flagged <- gaps[gaps$gap_flagged, ]
  rc_out  <- rc  # work on a copy

  for (i in seq_len(nrow(flagged))) {
    jct       <- flagged[i, ]
    lower_id  <- jct$limb_lower
    upper_id  <- jct$limb_upper
    s_brk     <- jct$stage_break
    q_low_end <- jct$q_lower_end
    q_up_start <- jct$q_upper_start

    lower_idx <- which(rc_out[[limb_col]] == lower_id)
    upper_idx <- which(rc_out[[limb_col]] == upper_id)

    lower_rows <- rc_out[lower_idx, ]
    upper_rows <- rc_out[upper_idx, ]

    lower_rows <- lower_rows[order(lower_rows[[stage_col]]), ]
    upper_rows <- upper_rows[order(upper_rows[[stage_col]]), ]

    if (method == "interpolate") {
      # Build a bridging point for the lower limb at s_brk
      n_low  <- nrow(lower_rows)
      s_l1   <- lower_rows[[stage_col]][n_low - 1L]
      q_l1   <- lower_rows[[discharge_col]][n_low - 1L]
      q_l_bridge <- .interp_y(s_l1, q_l1, s_brk, q_low_end, s_brk)
      # (equals q_low_end by definition, but formula is robust for sub-step gaps)

      # Build a bridging point for the upper limb at s_brk
      s_u2   <- upper_rows[[stage_col]][2L]
      q_u2   <- upper_rows[[discharge_col]][2L]
      q_u_bridge <- .interp_y(s_brk, q_up_start, s_u2, q_u2, s_brk)

      # Agreed Q at the junction = midpoint of the two bridge estimates
      q_agreed <- (q_l_bridge + q_u_bridge) / 2.0

      message(sprintf(
        "Junction %d (limbs %s/%s, stage %.3f): Q gap %.2f -> %.2f | agreed Q = %.4f",
        jct$junction, lower_id, upper_id, s_brk,
        q_low_end, q_up_start, q_agreed
      ))

      # Patch: set the lower limb's last row Q to agreed value
      last_lower_in_out <- lower_idx[which.max(lower_rows[[stage_col]])]
      rc_out[[discharge_col]][last_lower_in_out] <- q_agreed

      # Patch: set the upper limb's first row Q to agreed value
      first_upper_in_out <- upper_idx[which.min(upper_rows[[stage_col]])]
      rc_out[[discharge_col]][first_upper_in_out] <- q_agreed

    } else if (method == "snap_to_lower") {
      message(sprintf(
        "Junction %d (limbs %s/%s, stage %.3f): snapping upper start %.4f -> %.4f",
        jct$junction, lower_id, upper_id, s_brk, q_up_start, q_low_end
      ))
      first_upper_in_out <- upper_idx[which.min(upper_rows[[stage_col]])]
      rc_out[[discharge_col]][first_upper_in_out] <- q_low_end

    } else {  # snap_to_upper
      message(sprintf(
        "Junction %d (limbs %s/%s, stage %.3f): snapping lower end %.4f -> %.4f",
        jct$junction, lower_id, upper_id, s_brk, q_low_end, q_up_start
      ))
      last_lower_in_out <- lower_idx[which.max(lower_rows[[stage_col]])]
      rc_out[[discharge_col]][last_lower_in_out] <- q_up_start
    }
  }

  # Return sorted by stage
  rc_out[order(rc_out[[stage_col]]), ]
}


# ---------------------------------------------------------------------------
# plot_rc_gaps()
# ---------------------------------------------------------------------------
#
# Diagnostic plot overlaying the original (dashed) and corrected (solid)
# rating curves, with gap junctions highlighted.
#
# Arguments:
#   rc_before      original data frame
#   rc_after       corrected data frame returned by resolve_rc_gaps()
#   stage_col      name of stage column          (default "stage")
#   discharge_col  name of discharge column      (default "discharge")
#   limb_col       name of limb-ID column        (default "limb")

plot_rc_gaps <- function(rc_before,
                         rc_after,
                         stage_col     = "stage",
                         discharge_col = "discharge",
                         limb_col      = "limb") {

  make_long <- function(rc, label) {
    rc <- as.data.frame(rc)
    if (!limb_col %in% names(rc)) rc[[limb_col]] <- 1L
    rc$version <- label
    rc$limb_f  <- factor(rc[[limb_col]])
    rc[, c(stage_col, discharge_col, "version", "limb_f")]
  }

  before_df <- make_long(rc_before, "Before")
  after_df  <- make_long(rc_after,  "After")
  combined  <- rbind(before_df, after_df)
  combined$version <- factor(combined$version, levels = c("Before", "After"))

  # Detect gap locations from the original
  gaps <- detect_rc_gaps(rc_before,
                         stage_col     = stage_col,
                         discharge_col = discharge_col,
                         limb_col      = limb_col)

  p <- ggplot(combined,
              aes(x = .data[[discharge_col]],
                  y = .data[[stage_col]],
                  colour = limb_f,
                  linetype = version,
                  linewidth = version)) +
    geom_line() +
    scale_linetype_manual(values  = c("Before" = "dashed", "After" = "solid"),
                          name    = NULL) +
    scale_linewidth_manual(values = c("Before" = 0.6, "After" = 1.2),
                           name   = NULL) +
    labs(
      title   = "Rating Curve — Gap Detection & Resolution",
      x       = "Discharge (m\u00b3/s)",
      y       = "Stage (m AOD)",
      colour  = "Limb",
      caption = "Dashed = original | Solid = corrected"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "grey80", fill = NA)
    )

  # Annotate each flagged gap junction
  if (!is.null(gaps) && any(gaps$gap_flagged)) {
    flagged <- gaps[gaps$gap_flagged, ]
    p <- p +
      geom_hline(data      = flagged,
                 aes(yintercept = stage_break),
                 colour    = "firebrick", linetype = "dotted",
                 linewidth = 0.5, inherit.aes = FALSE) +
      geom_label(data      = flagged,
                 aes(x     = (q_lower_end + q_upper_start) / 2,
                     y     = stage_break,
                     label = sprintf("Gap\n\u0394Q = %.1f m\u00b3/s", gap_abs)),
                 colour    = "firebrick", size = 3,
                 label.size = 0.2, fill = "white",
                 inherit.aes = FALSE)
  }

  print(p)
  invisible(p)
}


# ---------------------------------------------------------------------------
# Example usage
# ---------------------------------------------------------------------------

if (FALSE) {

  # Synthetic three-limb rating curve with deliberate gaps at the junctions
  set.seed(42)

  limb1 <- data.frame(
    stage     = seq(8.0, 10.0, by = 0.1),
    discharge = 12 * (seq(8.0, 10.0, by = 0.1) - 8.0)^1.65,
    limb      = 1L
  )

  # Limb 2 starts ~5 m³/s below where limb 1 ends (gap!)
  limb2_start_q <- tail(limb1$discharge, 1) - 5
  limb2 <- data.frame(
    stage     = seq(10.0, 11.2, by = 0.1),
    discharge = limb2_start_q + 30 * (seq(10.0, 11.2, by = 0.1) - 10.0)^1.4,
    limb      = 2L
  )

  # Limb 3 starts ~8 m³/s above where limb 2 ends (gap!)
  limb3_start_q <- tail(limb2$discharge, 1) + 8
  limb3 <- data.frame(
    stage     = seq(11.2, 12.0, by = 0.1),
    discharge = limb3_start_q + 60 * (seq(11.2, 12.0, by = 0.1) - 11.2)^1.3,
    limb      = 3L
  )

  rc_raw <- rbind(limb1, limb2, limb3)

  # 1. Detect gaps
  gap_report <- detect_rc_gaps(rc_raw)
  print(gap_report)

  # 2. Resolve using the default "interpolate" method
  rc_fixed_interp <- resolve_rc_gaps(rc_raw, method = "interpolate")

  # 3. Or snap the upper limb to the lower limb's endpoint
  rc_fixed_snap <- resolve_rc_gaps(rc_raw, method = "snap_to_lower")

  # 4. Diagnostic plot
  plot_rc_gaps(rc_raw, rc_fixed_interp)
}
