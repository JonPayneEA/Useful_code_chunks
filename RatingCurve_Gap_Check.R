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
# Dependencies: ggplot2 (base R otherwise)

library(ggplot2)


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Linear interpolation at a single point
#'
#' @param x1,y1 Coordinates of the first known point.
#' @param x2,y2 Coordinates of the second known point.
#' @param x_new x value at which to interpolate.
#' @return Interpolated y value at \code{x_new}.
#' @keywords internal
#' @noRd
.interp_y <- function(x1, y1, x2, y2, x_new) {
  y1 + (y2 - y1) * (x_new - x1) / (x2 - x1)
}


#' Auto-detect rating-curve limbs from discharge monotonicity breaks
#'
#' Assigns an integer limb ID to each row. A new limb is started whenever
#' discharge drops by more than \code{tol_frac} of the previous value,
#' indicating an independently-fitted segment beginning below the prior end.
#'
#' @param q    Numeric vector of discharge values (stage-ascending order).
#' @param tol_frac Fractional drop threshold that triggers a new limb
#'   (default \code{0.05}, i.e. a 5 \% fall in Q).
#' @return Integer vector of limb IDs, same length as \code{q}.
#' @keywords internal
#' @noRd
.auto_limb <- function(q, tol_frac = 0.05) {
  n    <- length(q)
  limb <- integer(n)
  limb[1L] <- 1L
  k <- 1L
  for (i in seq(2L, n)) {
    ref <- max(abs(q[i - 1L]), 1e-9)
    if ((q[i - 1L] - q[i]) / ref > tol_frac) {
      k <- k + 1L
    }
    limb[i] <- k
  }
  limb
}


# ---------------------------------------------------------------------------
# detect_rc_gaps()
# ---------------------------------------------------------------------------

#' Detect discharge gaps between rating-curve limbs
#'
#' @description
#' Scans every junction between consecutive limbs and measures the absolute
#' and relative discharge gap. A gap arises when the last discharge value of
#' one limb does not match the first discharge value of the next limb at the
#' shared breakpoint stage.
#'
#' @param rc A data frame containing the rating curve. Rows should be ordered
#'   by stage (ascending) within each limb, and limbs should be contiguous.
#' @param stage_col Character. Name of the stage (water level) column.
#'   Default \code{"stage"}.
#' @param discharge_col Character. Name of the discharge column.
#'   Default \code{"discharge"}.
#' @param limb_col Character or \code{NULL}. Name of the column that identifies
#'   each limb (integer or character values). If \code{NULL} or not present in
#'   \code{rc}, limbs are auto-detected from monotonicity breaks in discharge.
#'   Default \code{"limb"}.
#' @param tol_abs Numeric. Absolute discharge tolerance (same units as
#'   \code{discharge_col}) below which a gap is not flagged. Default \code{0.5}.
#' @param tol_rel Numeric. Relative discharge tolerance (fraction of the lower
#'   limb's end discharge) below which a gap is not flagged. Default \code{0.02}
#'   (2 \%).
#'
#' @return A data frame with one row per limb junction containing:
#'   \describe{
#'     \item{junction}{Junction index (1 = between limbs 1 and 2, etc.)}
#'     \item{limb_lower}{Limb ID of the lower limb.}
#'     \item{limb_upper}{Limb ID of the upper limb.}
#'     \item{stage_break}{Stage value at the junction (last stage of the lower limb).}
#'     \item{q_lower_end}{Discharge at the end of the lower limb.}
#'     \item{q_upper_start}{Discharge at the start of the upper limb.}
#'     \item{gap_abs}{Absolute gap: \code{q_upper_start - q_lower_end}.}
#'     \item{gap_rel}{Relative gap: \code{gap_abs / q_lower_end}.}
#'     \item{gap_flagged}{Logical; \code{TRUE} if the gap exceeds \code{tol_abs}
#'       or \code{tol_rel}.}
#'   }
#'   Returns \code{NULL} invisibly when fewer than two limbs are detected.
#'
#' @examples
#' limb1 <- data.frame(
#'   stage     = seq(8.0, 10.0, by = 0.2),
#'   discharge = 12 * (seq(8.0, 10.0, by = 0.2) - 8.0)^1.65,
#'   limb      = 1L
#' )
#' # Limb 2 starts 5 m3/s below where limb 1 ends (deliberate gap)
#' limb2 <- data.frame(
#'   stage     = seq(10.0, 11.5, by = 0.2),
#'   discharge = (tail(limb1$discharge, 1) - 5) +
#'               30 * (seq(10.0, 11.5, by = 0.2) - 10.0)^1.4,
#'   limb      = 2L
#' )
#' rc_raw <- rbind(limb1, limb2)
#'
#' detect_rc_gaps(rc_raw)
#' detect_rc_gaps(rc_raw, tol_abs = 1, tol_rel = 0.01)
detect_rc_gaps <- function(rc,
                           stage_col     = "stage",
                           discharge_col = "discharge",
                           limb_col      = "limb",
                           tol_abs       = 0.5,
                           tol_rel       = 0.02) {

  rc <- as.data.frame(rc)

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

    lower_rows <- lower_rows[order(lower_rows[[stage_col]]), ]
    upper_rows <- upper_rows[order(upper_rows[[stage_col]]), ]

    q_low  <- lower_rows[[discharge_col]][nrow(lower_rows)]
    q_high <- upper_rows[[discharge_col]][1L]
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
      gap_flagged   = abs(gap_abs) > tol_abs |
                      (!is.na(gap_rel) & abs(gap_rel) > tol_rel),
      stringsAsFactors = FALSE
    )
  }

  out       <- do.call(rbind, results)
  n_flagged <- sum(out$gap_flagged)
  message(sprintf("Checked %d junction(s): %d gap(s) flagged.", nrow(out), n_flagged))
  out
}


# ---------------------------------------------------------------------------
# resolve_rc_gaps()
# ---------------------------------------------------------------------------

#' Resolve discharge gaps between rating-curve limbs
#'
#' @description
#' Closes discharge discontinuities at limb junctions identified by
#' \code{\link{detect_rc_gaps}} using one of three strategies:
#'
#' \describe{
#'   \item{\code{"interpolate"} (default)}{
#'     Both limbs' endpoint discharges are moved to the midpoint of their
#'     respective bridge estimates at the breakpoint stage. This preserves
#'     the shape of both limbs while pinning them to a single agreed
#'     discharge at the junction.}
#'   \item{\code{"snap_to_lower"}}{
#'     The upper limb's first discharge is set equal to the lower limb's
#'     last discharge. Use when the lower (typically gauged) limb is
#'     authoritative.}
#'   \item{\code{"snap_to_upper"}}{
#'     The lower limb's last discharge is set equal to the upper limb's
#'     first discharge. Use when the upper limb anchor (e.g. a flood-
#'     frequency estimate) is authoritative.}
#' }
#'
#' Only junctions flagged by \code{detect_rc_gaps} (i.e. those exceeding
#' \code{tol_abs} or \code{tol_rel}) are modified.
#'
#' @param rc A data frame containing the rating curve.
#' @param stage_col Character. Name of the stage column. Default \code{"stage"}.
#' @param discharge_col Character. Name of the discharge column.
#'   Default \code{"discharge"}.
#' @param limb_col Character or \code{NULL}. Name of the limb-ID column.
#'   Auto-detected if absent. Default \code{"limb"}.
#' @param method Character. Resolution strategy: \code{"interpolate"},
#'   \code{"snap_to_lower"}, or \code{"snap_to_upper"}.
#'   Default \code{"interpolate"}.
#' @param tol_abs Numeric. Absolute discharge tolerance passed to
#'   \code{\link{detect_rc_gaps}}. Default \code{0.5}.
#' @param tol_rel Numeric. Relative discharge tolerance passed to
#'   \code{\link{detect_rc_gaps}}. Default \code{0.02}.
#'
#' @return The corrected data frame with the same columns as \code{rc}, sorted
#'   by \code{stage_col} ascending. If no gaps are flagged the input is
#'   returned unchanged.
#'
#' @seealso \code{\link{detect_rc_gaps}}, \code{\link{plot_rc_gaps}}
#'
#' @examples
#' limb1 <- data.frame(
#'   stage     = seq(8.0, 10.0, by = 0.2),
#'   discharge = 12 * (seq(8.0, 10.0, by = 0.2) - 8.0)^1.65,
#'   limb      = 1L
#' )
#' limb2 <- data.frame(
#'   stage     = seq(10.0, 11.5, by = 0.2),
#'   discharge = (tail(limb1$discharge, 1) - 5) +
#'               30 * (seq(10.0, 11.5, by = 0.2) - 10.0)^1.4,
#'   limb      = 2L
#' )
#' rc_raw <- rbind(limb1, limb2)
#'
#' # Default: average the two bridge estimates at the junction
#' rc_fixed <- resolve_rc_gaps(rc_raw, method = "interpolate")
#'
#' # Trust the gauged lower limb; shift only the upper limb start
#' rc_snapped <- resolve_rc_gaps(rc_raw, method = "snap_to_lower")
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
  rc_out  <- rc

  for (i in seq_len(nrow(flagged))) {
    jct        <- flagged[i, ]
    lower_id   <- jct$limb_lower
    upper_id   <- jct$limb_upper
    s_brk      <- jct$stage_break
    q_low_end  <- jct$q_lower_end
    q_up_start <- jct$q_upper_start

    lower_idx  <- which(rc_out[[limb_col]] == lower_id)
    upper_idx  <- which(rc_out[[limb_col]] == upper_id)

    lower_rows <- rc_out[lower_idx, ][order(rc_out[lower_idx, stage_col]), ]
    upper_rows <- rc_out[upper_idx, ][order(rc_out[upper_idx, stage_col]), ]

    last_lower_pos  <- lower_idx[which.max(rc_out[lower_idx, stage_col])]
    first_upper_pos <- upper_idx[which.min(rc_out[upper_idx, stage_col])]

    if (method == "interpolate") {
      # Bridge estimate from the lower limb's penultimate point
      n_low      <- nrow(lower_rows)
      s_l1       <- lower_rows[[stage_col]][n_low - 1L]
      q_l1       <- lower_rows[[discharge_col]][n_low - 1L]
      q_l_bridge <- .interp_y(s_l1, q_l1, s_brk, q_low_end, s_brk)

      # Bridge estimate from the upper limb's second point
      s_u2       <- upper_rows[[stage_col]][2L]
      q_u2       <- upper_rows[[discharge_col]][2L]
      q_u_bridge <- .interp_y(s_brk, q_up_start, s_u2, q_u2, s_brk)

      q_agreed <- (q_l_bridge + q_u_bridge) / 2.0

      message(sprintf(
        "Junction %d (limbs %s/%s, stage %.3f): gap %.2f -> %.2f | agreed Q = %.4f",
        jct$junction, lower_id, upper_id, s_brk, q_low_end, q_up_start, q_agreed
      ))

      rc_out[[discharge_col]][last_lower_pos]  <- q_agreed
      rc_out[[discharge_col]][first_upper_pos] <- q_agreed

    } else if (method == "snap_to_lower") {
      message(sprintf(
        "Junction %d (limbs %s/%s, stage %.3f): snapping upper start %.4f -> %.4f",
        jct$junction, lower_id, upper_id, s_brk, q_up_start, q_low_end
      ))
      rc_out[[discharge_col]][first_upper_pos] <- q_low_end

    } else {
      message(sprintf(
        "Junction %d (limbs %s/%s, stage %.3f): snapping lower end %.4f -> %.4f",
        jct$junction, lower_id, upper_id, s_brk, q_low_end, q_up_start
      ))
      rc_out[[discharge_col]][last_lower_pos] <- q_up_start
    }
  }

  rc_out[order(rc_out[[stage_col]]), ]
}


# ---------------------------------------------------------------------------
# plot_rc_gaps()
# ---------------------------------------------------------------------------

#' Diagnostic plot for rating-curve gap detection and resolution
#'
#' @description
#' Produces a ggplot2 figure overlaying the original (dashed) and corrected
#' (solid) rating curves. Works for any number of limbs.
#'
#' The original (Before) curve is drawn as one dashed line per limb, coloured
#' by limb ID, so gaps between independently-fitted segments are visible.
#' The corrected (After) curve is drawn as a single continuous grey line —
#' using one line object rather than one per limb — which guarantees there are
#' no rendering seams at limb boundaries regardless of how many limbs exist.
#'
#' Flagged gap junctions are marked with a dotted horizontal line; labels are
#' pinned to the right margin and staggered vertically so they never overlap
#' the curves or each other. A short segment connects each label back to its
#' junction stage line.
#'
#' @param rc_before Data frame. The original, uncorrected rating curve passed
#'   to \code{\link{resolve_rc_gaps}}.
#' @param rc_after Data frame. The corrected rating curve returned by
#'   \code{\link{resolve_rc_gaps}}.
#' @param stage_col Character. Name of the stage column. Default \code{"stage"}.
#' @param discharge_col Character. Name of the discharge column.
#'   Default \code{"discharge"}.
#' @param limb_col Character or \code{NULL}. Name of the limb-ID column.
#'   Defaults to \code{"limb"}; a single limb is assumed if the column is absent.
#'
#' @return A \code{ggplot} object (printed as a side-effect). Returned
#'   invisibly so it can be further modified or saved with
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @seealso \code{\link{detect_rc_gaps}}, \code{\link{resolve_rc_gaps}}
#'
#' @examples
#' limb1 <- data.frame(
#'   stage     = seq(8.0, 10.0, by = 0.2),
#'   discharge = 12 * (seq(8.0, 10.0, by = 0.2) - 8.0)^1.65,
#'   limb      = 1L
#' )
#' limb2 <- data.frame(
#'   stage     = seq(10.0, 11.5, by = 0.2),
#'   discharge = (tail(limb1$discharge, 1) - 5) +
#'               30 * (seq(10.0, 11.5, by = 0.2) - 10.0)^1.4,
#'   limb      = 2L
#' )
#' limb3 <- data.frame(
#'   stage     = seq(11.5, 12.5, by = 0.2),
#'   discharge = (tail(limb2$discharge, 1) + 8) +
#'               60 * (seq(11.5, 12.5, by = 0.2) - 11.5)^1.3,
#'   limb      = 3L
#' )
#' rc_raw   <- rbind(limb1, limb2, limb3)
#' rc_fixed <- resolve_rc_gaps(rc_raw)
#'
#' p <- plot_rc_gaps(rc_raw, rc_fixed)
#' # Save if needed:
#' # ggplot2::ggsave("rc_gap_check.png", p, width = 8, height = 6)
plot_rc_gaps <- function(rc_before,
                         rc_after,
                         stage_col     = "stage",
                         discharge_col = "discharge",
                         limb_col      = "limb") {

  # Standardise to fixed column names for ggplot2 aes() without extra deps
  make_plot_df <- function(rc, version_label) {
    rc <- as.data.frame(rc)
    if (!limb_col %in% names(rc)) rc[[limb_col]] <- 1L
    data.frame(
      stage_     = rc[[stage_col]],
      discharge_ = rc[[discharge_col]],
      limb_f     = factor(rc[[limb_col]]),
      version    = version_label,
      stringsAsFactors = FALSE
    )
  }

  before_df <- make_plot_df(rc_before, "Before")
  after_df  <- make_plot_df(rc_after,  "After")

  # Sort the After data by stage so geom_line connects points in the correct
  # order when all limbs are merged into one line object.
  after_df <- after_df[order(after_df$stage_), ]

  gaps <- detect_rc_gaps(rc_before,
                         stage_col     = stage_col,
                         discharge_col = discharge_col,
                         limb_col      = limb_col)

  p <- ggplot() +
    # Before: one dashed line per limb, coloured, so inter-limb gaps show
    geom_line(
      data      = before_df,
      aes(x = discharge_, y = stage_, colour = limb_f, group = limb_f),
      linetype  = "dashed", linewidth = 0.65
    ) +
    # After: ONE continuous line object (no limb grouping) — eliminates
    # rendering seams at limb boundaries for any number of limbs
    geom_line(
      data      = after_df,
      aes(x = discharge_, y = stage_),
      colour    = "grey20", linetype = "solid", linewidth = 1.2
    ) +
    scale_colour_discrete(name = "Limb") +
    labs(
      title   = "Rating Curve \u2014 Gap Detection & Resolution",
      x       = "Discharge (m\u00b3/s)",
      y       = paste0("Stage (", stage_col, ")"),
      caption = "Dashed (coloured by limb) = original  |  Solid grey = corrected"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(colour = "grey80", fill = NA)
    )

  if (!is.null(gaps) && any(gaps$gap_flagged)) {
    flagged <- gaps[gaps$gap_flagged, ]

    # ── Label positioning ────────────────────────────────────────────────────
    # Anchor all labels to the right margin so they never overlay the curves.
    # Stagger vertically when two junctions are too close together.
    q_max       <- max(after_df$discharge_, before_df$discharge_)
    stage_range <- diff(range(after_df$stage_, before_df$stage_))
    min_sep     <- stage_range * 0.07   # minimum vertical gap between labels

    # Sort by stage so the stagger pass moves upward through the plot
    flagged <- flagged[order(flagged$stage_break), ]

    label_y <- flagged$stage_break
    for (i in seq(2L, length(label_y))) {
      if (label_y[i] - label_y[i - 1L] < min_sep) {
        label_y[i] <- label_y[i - 1L] + min_sep
      }
    }

    label_df <- data.frame(
      x          = q_max,
      y          = label_y,
      y_junction = flagged$stage_break,
      label      = sprintf("Gap %d  \u0394Q = %.1f m\u00b3/s", flagged$junction, flagged$gap_abs),
      stringsAsFactors = FALSE
    )

    p <- p +
      # Dotted line across the full panel at each junction stage
      geom_hline(
        data        = flagged,
        aes(yintercept = stage_break),
        colour      = "firebrick", linetype = "dotted",
        linewidth   = 0.5, inherit.aes = FALSE
      ) +
      # Segment from right edge of panel down to the actual junction stage
      geom_segment(
        data        = label_df,
        aes(x = x, xend = x, y = y, yend = y_junction),
        colour      = "firebrick", linewidth = 0.4,
        linetype    = "solid", inherit.aes = FALSE
      ) +
      # Label pinned to the right margin, left-justified off the segment end
      geom_label(
        data        = label_df,
        aes(x = x, y = y, label = label),
        hjust       = 1, vjust = 0.5,
        colour      = "firebrick", size = 3,
        label.size  = 0.2, fill = "white", alpha = 0.9,
        inherit.aes = FALSE
      )
  }

  print(p)
  invisible(p)
}


# ---------------------------------------------------------------------------
# Full worked example (run manually)
# ---------------------------------------------------------------------------

if (FALSE) {

  # Three-limb rating curve with two deliberate gaps
  limb1 <- data.frame(
    stage     = seq(8.0, 10.0, by = 0.1),
    discharge = 12 * (seq(8.0, 10.0, by = 0.1) - 8.0)^1.65,
    limb      = 1L
  )

  # Gap 1: limb 2 starts 5 m³/s below where limb 1 ends
  limb2 <- data.frame(
    stage     = seq(10.0, 11.2, by = 0.1),
    discharge = (tail(limb1$discharge, 1) - 5) +
                30 * (seq(10.0, 11.2, by = 0.1) - 10.0)^1.4,
    limb      = 2L
  )

  # Gap 2: limb 3 starts 8 m³/s above where limb 2 ends
  limb3 <- data.frame(
    stage     = seq(11.2, 12.0, by = 0.1),
    discharge = (tail(limb2$discharge, 1) + 8) +
                60 * (seq(11.2, 12.0, by = 0.1) - 11.2)^1.3,
    limb      = 3L
  )

  rc_raw <- rbind(limb1, limb2, limb3)

  # 1. Inspect gaps
  gap_report <- detect_rc_gaps(rc_raw)
  print(gap_report)

  # 2a. Fix by averaging both limb endpoints at each junction
  rc_interp <- resolve_rc_gaps(rc_raw, method = "interpolate")

  # 2b. Fix by snapping upper limb to lower limb (lower limb trusted)
  rc_snap_lo <- resolve_rc_gaps(rc_raw, method = "snap_to_lower")

  # 2c. Fix by snapping lower limb to upper limb (upper anchor trusted)
  rc_snap_hi <- resolve_rc_gaps(rc_raw, method = "snap_to_upper")

  # 3. Visualise before vs after
  plot_rc_gaps(rc_raw, rc_interp)

  # Save the plot
  # p <- plot_rc_gaps(rc_raw, rc_interp)
  # ggplot2::ggsave("rc_gap_check.png", p, width = 9, height = 6, dpi = 150)
}
