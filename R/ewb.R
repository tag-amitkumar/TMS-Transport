# ==================================================================
# E-way bill Part-B — vehicle details, validity, and automatic renewal.
#
# An e-way bill has two halves. Part-A is the consignment: who is sending what
# to whom, against which invoice. Part-B is the transport: which vehicle is
# carrying it. Validity does not start when Part-A is filed — it starts when
# Part-B is entered, and it runs one day per 200 km, so a short lane is a
# 24-hour window.
#
# The problem this file exists to solve: when that window closes and the goods
# are still on the road, the bill is dead. A vehicle stopped at a check-post
# with a lapsed e-way bill is detained and the consignment is liable to
# penalty. The remedy in the rules is to enter Part-B again, which opens a
# fresh window — and that is a thing a human has to remember to do, at 2am, for
# a truck they are not looking at.
#
# So the app does it. ewb_autorenew() finds every bill whose validity has
# lapsed while its consignment is still moving and files a new Part-B against
# it. Each one is recorded as its own row in `ewaybill_partb`, never as an
# overwrite: the sequence of vehicle entries is the audit trail a GST officer
# would ask for, and collapsing it to a single "valid_to" would destroy the
# only evidence of why the bill is still alive.
#
# Nothing here calls the GST portal. Renewal is recorded locally, exactly like
# generation is — see DESIGN-REVIEW.md, gap #12.
# ==================================================================

# One day per 200 km, which is the statutory rule for regular cargo. A lane of
# 200 km or less therefore gets 24 hours, which is the common case and the one
# that bites.
ewb_validity_days <- function(km) {
  km <- suppressWarnings(as.numeric(km))
  if (length(km) != 1 || is.na(km) || km <= 0) km <- 200
  max(1, ceiling(km / 200))
}

# How often the renewal sweep runs while the app is up. A minute is far more
# often than needed for a 24-hour window, but it costs a filter over a small
# table and it means a demo does not have to wait.
EWB_RENEW_INTERVAL_MS <- 60 * 1000

# Past this many Part-B entries a consignment is not "in transit", it is stuck.
# Renewal continues — stranding a driver with a dead bill helps nobody — but
# the bill is flagged so somebody goes and looks at it.
EWB_RENEW_ALERT_AFTER <- 3

#' The distance behind a consignment, for the validity window.
.ewb_km <- function(cn_no) {
  cn <- get_consignments()
  c1 <- cn[cn$cn_no == cn_no, ]
  if (!nrow(c1)) return(200)
  tr <- get_trips()
  t <- tr[tr$trip_no == c1$trip_no[1], ]
  if (nrow(t) && !is.na(t$distance_km[1]) && t$distance_km[1] > 0) {
    return(t$distance_km[1])
  }
  bk <- get_bookings()
  b <- bk[bk$booking_no == c1$booking_no[1], ]
  if (nrow(b) && !is.na(b$distance_km[1]) && b$distance_km[1] > 0) {
    return(b$distance_km[1])
  }
  200
}

#' Every Part-B entry filed against a bill, oldest first.
ewb_partb <- function(ewb_no = NULL) {
  p <- store_typed("ewaybill_partb",
                   num      = "seq",
                   datetime = c("entered_dt", "valid_from", "valid_to"))
  if (!nrow(p)) return(p)
  if (!is.null(ewb_no)) p <- p[p$ewb_no %in% ewb_no, , drop = FALSE]
  p[order(p$ewb_no, p$seq), , drop = FALSE]
}

#' File a Part-B entry against a bill and open a fresh validity window.
#'
#' Returns the new validity end, invisibly. `mode` is "Auto" for the renewal
#' sweep and "Manual" when an operator enters it, because the two need to be
#' told apart when someone asks why a bill is still valid.
ewb_partb_add <- function(ewb_no, vehicle_id = NULL, reason = "Part-B entered",
                          mode = c("Manual", "Auto"), user_id = "system",
                          from = Sys.time()) {
  mode <- match.arg(mode)
  e <- get_ewaybills()
  row <- e[e$ewb_no == ewb_no, ]
  if (!nrow(row)) return(invisible(NULL))
  row <- row[1, ]

  if (is.null(vehicle_id) || !nzchar(vehicle_id %||% "")) vehicle_id <- row$vehicle_id
  days <- ewb_validity_days(.ewb_km(row$cn_no))
  valid_to <- from + days * 86400

  prior <- ewb_partb(ewb_no)
  seq_no <- if (nrow(prior)) max(prior$seq, na.rm = TRUE) + 1L else 1L

  v <- store_get("vehicles")
  store_insert("ewaybill_partb", list(
    partb_id   = next_id(store_get("ewaybill_partb")$partb_id, "PB-"),
    ewb_no     = ewb_no,
    seq        = seq_no,
    vehicle_id = vehicle_id,
    reg_no     = v$reg_no[match(vehicle_id, v$vehicle_id)] %||% "",
    entered_dt = format(from, "%Y-%m-%d %H:%M:%S"),
    valid_from = format(from, "%Y-%m-%d %H:%M:%S"),
    valid_to   = format(valid_to, "%Y-%m-%d %H:%M:%S"),
    reason     = reason,
    mode       = mode,
    entered_by = user_id
  ))

  # The bill itself always mirrors the latest Part-B, so every existing reader
  # of valid_to keeps working without knowing this table exists.
  store_update("ewaybills", list(ewb_no = ewb_no), list(
    vehicle_id = vehicle_id,
    valid_from = format(from, "%Y-%m-%d %H:%M:%S"),
    valid_to   = format(valid_to, "%Y-%m-%d %H:%M:%S"),
    part_b_seq = seq_no,
    status     = "Valid"
  ))
  invisible(valid_to)
}

#' Which consignments are still on the road.
#'
#' Delivered or cancelled goods do not need a live e-way bill, and renewing one
#' for them would keep a dead document alive for no reason.
.ewb_in_transit <- function() {
  cn <- get_consignments()
  if (!nrow(cn)) return(character(0))
  cn$cn_no[!cn$status %in% c("Delivered", "Cancelled", "Returned")]
}

#' Renew every lapsed bill whose consignment is still moving.
#'
#' Safe to call as often as you like: a bill it has just renewed has a validity
#' window in the future and will not match again.
#'
#' Returns a data frame of what was renewed, so a caller can tell the operator.
ewb_autorenew <- function(now = Sys.time(), user_id = "system") {
  e <- get_ewaybills()
  if (!nrow(e)) return(invisible(data.frame()))

  moving <- .ewb_in_transit()
  lapsed <- e[!is.na(e$valid_to) &
                as_dt(e$valid_to) <= now &
                e$cn_no %in% moving, , drop = FALSE]
  if (!nrow(lapsed)) return(invisible(data.frame()))

  out <- vector("list", nrow(lapsed))
  for (i in seq_len(nrow(lapsed))) {
    no <- lapsed$ewb_no[i]
    n_prior <- nrow(ewb_partb(no))
    ewb_partb_add(
      no,
      reason  = "Validity lapsed with the consignment still in transit",
      mode    = "Auto",
      user_id = user_id,
      from    = now
    )
    audit(user_id, "part-b-auto", "ewaybill",
          paste(no, "renewed, Part-B entry", n_prior + 1))
    out[[i]] <- data.frame(ewb_no = no, lr_no = lapsed$lr_no[i],
                           seq = n_prior + 1L, stringsAsFactors = FALSE)
  }
  invisible(do.call(rbind, out))
}
