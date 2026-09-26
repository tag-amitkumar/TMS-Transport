# ==================================================================
# Serviceability, freight rating and the e-way bill rule.
#
# Three decisions the booking screen needs and must not make for itself:
# which branch delivers a pincode, whether that delivery is ODA, and what the
# load should cost. They live here rather than in mod_bookings.R because the
# consignment note, the delivery branch's worklist and the rate-card screen
# all have to reach the same answer from the same inputs.
#
# Added for the change requests of 20 Sep 2026 — CR-07 to CR-12, CR-16 to
# CR-20. See CHANGE-REQUESTS-2026-09-20.md.
# ==================================================================

# ------------------------------------------------------------------
# Branch pincode master (CR-14, CR-20)
# ------------------------------------------------------------------

#' Every pincode a branch has been told it serves, with its delivery type.
branch_pincodes <- function() store_get("branch_pincodes")

#' The branches that have explicitly claimed a pincode.
#'
#' Returns zero rows for an unclaimed pincode, which is the signal to fall
#' back to the distance test rather than an error.
branch_for_pincode <- function(pincode) {
  p <- trimws(as.character(pincode %||% ""))
  if (!nzchar(p)) return(store_get("branch_pincodes")[0, , drop = FALSE])
  bp <- branch_pincodes()
  if (!nrow(bp)) return(bp)
  bp[as.character(bp$pincode) == p, , drop = FALSE]
}

#' Nearest branch to a pincode, by straight-line distance.
#'
#' Returns NULL when either end cannot be placed — an unknown pincode, or a
#' branch table with no coordinates. Callers treat NULL as "cannot classify"
#' and must not silently read it as Normal.
nearest_branch <- function(pincode) {
  loc <- geo_pin(pincode)
  if (is.null(loc) || is.na(loc$lat) || is.na(loc$lon)) return(NULL)

  br <- store_get("branches")
  if (!nrow(br)) return(NULL)
  br$lat <- as_num(br$lat); br$lon <- as_num(br$lon)
  br <- br[!is.na(br$lat) & !is.na(br$lon), , drop = FALSE]
  if (!nrow(br)) return(NULL)

  # Only a live branch can take a delivery; a closed one is still in the table
  # for historical bookings and must not be offered as the nearest.
  if ("status" %in% names(br)) {
    live <- br[br$status == "Active", , drop = FALSE]
    if (nrow(live)) br <- live
  }

  br$km <- geo_haversine(loc$lat, loc$lon, br$lat, br$lon)
  br <- br[order(br$km), , drop = FALSE]
  br[1, , drop = FALSE]
}

#' Resolve a destination pincode to a delivering branch and a delivery type.
#'
#' The single entry point for CR-09, CR-11, CR-12 and CR-20 — everything that
#' needs to know who delivers and whether it is ODA calls this, so the answer
#' cannot differ between the booking screen and the consignment note.
#'
#' Returns a list with:
#'   branch_id, branch_name  the delivering branch, "" when unresolved
#'   delivery_type           "Normal" | "ODA" | "Unserviceable"
#'   km                      straight-line distance, NA when unknown
#'   basis                   how it was decided, for display and for audit
#'
#' "Unserviceable" is distinct from ODA on purpose: ODA is a place we deliver
#' to for a surcharge, unserviceable is a pincode we could not place at all.
#' Collapsing the two would quote a surcharge for an address nobody can reach.
resolve_delivery <- function(pincode) {
  none <- list(branch_id = "", branch_name = "",
               delivery_type = "Unserviceable", km = NA_real_,
               basis = "PIN code not recognised")

  p <- trimws(as.character(pincode %||% ""))
  if (!nzchar(p)) return(none)

  # 1. An explicit entry in the branch master wins. Somebody decided this.
  claim <- branch_for_pincode(p)
  if (nrow(claim)) {
    # Two branches may both claim a pincode (D-05). Until a priority rule is
    # agreed, the nearer branch delivers, which is the answer an operator
    # would give anyway. Falls back to the first row when neither can be
    # placed geographically.
    if (nrow(claim) > 1) {
      nb <- nearest_branch(p)
      if (!is.null(nb) && nb$branch_id %in% claim$branch_id) {
        claim <- claim[claim$branch_id == nb$branch_id, , drop = FALSE]
      }
    }
    row <- claim[1, , drop = FALSE]
    br  <- store_get("branches")
    nm  <- br$name[match(row$branch_id, br$branch_id)]
    dt  <- row$delivery_type %||% "Normal"
    if (!dt %in% DELIVERY_TYPES) dt <- "Normal"
    return(list(
      branch_id = row$branch_id, branch_name = nm %||% row$branch_id,
      delivery_type = dt, km = NA_real_,
      basis = sprintf("Listed against %s as %s", nm %||% row$branch_id, dt)))
  }

  # 2. Nobody has classified it. Fall back to distance from the nearest branch.
  nb <- nearest_branch(p)
  if (is.null(nb)) return(none)

  km <- round(as.numeric(nb$km), 1)
  dt <- if (km <= ODA_RADIUS_KM) "Normal" else "ODA"
  list(branch_id = nb$branch_id, branch_name = nb$name,
       delivery_type = dt, km = km,
       basis = sprintf("%s km from %s (straight line), not in any branch list",
                       format(km, nsmall = 1), nb$name))
}

# ------------------------------------------------------------------
# Freight rating (CR-07, CR-08)
# ------------------------------------------------------------------

#' Rate per kg for a destination, in rupees.
#'
#' Looked up most specific first: an exact pincode rate, then the destination
#' branch's rate, then the standard fallback. Keyed this way because a rate
#' card is negotiated at whatever level the commercial team agreed it — some
#' lanes are priced to the pincode, most to the serving branch.
#'
#' Returns a list so the breakup can name which rule was applied; quoting a
#' figure without saying where it came from is how rate disputes start.
freight_rate <- function(dest_pincode, branch_id = "", dispatch = "LTL") {
  fr <- store_get("freight_rates")
  fallback <- list(rate = 0, basis = "No rate card entry — enter freight manually")
  if (!nrow(fr)) return(fallback)

  fr$rate_per_kg <- as_num(fr$rate_per_kg)
  fr <- fr[!is.na(fr$rate_per_kg), , drop = FALSE]
  if (!nrow(fr)) return(fallback)

  # Dispatch mode narrows first where the card distinguishes it; an entry with
  # a blank dispatch applies to both.
  if ("dispatch" %in% names(fr)) {
    keep <- fr$dispatch == dispatch | !nzchar(fr$dispatch %||% "")
    if (any(keep)) fr <- fr[keep, , drop = FALSE]
  }

  p <- trimws(as.character(dest_pincode %||% ""))
  if (nzchar(p) && "pincode" %in% names(fr)) {
    hit <- fr[as.character(fr$pincode) == p, , drop = FALSE]
    if (nrow(hit)) return(list(rate = hit$rate_per_kg[1],
                               basis = paste("PIN code rate for", p)))
  }
  if (nzchar(branch_id %||% "") && "branch_id" %in% names(fr)) {
    hit <- fr[fr$branch_id == branch_id & !nzchar(fr$pincode %||% ""), , drop = FALSE]
    if (nrow(hit)) {
      br <- store_get("branches")
      nm <- br$name[match(branch_id, br$branch_id)]
      return(list(rate = hit$rate_per_kg[1],
                  basis = paste("Branch rate —", nm %||% branch_id)))
    }
  }
  std <- fr[!nzchar(fr$pincode %||% "") & !nzchar(fr$branch_id %||% ""), , drop = FALSE]
  if (nrow(std)) return(list(rate = std$rate_per_kg[1], basis = "Standard rate"))
  fallback
}

#' ODA surcharge in rupees, read from settings so it is not compiled in.
oda_charge <- function() {
  v <- suppressWarnings(as.numeric(setting("oda_charge", "750")))
  if (is.na(v)) 750 else v
}

#' Minimum chargeable freight, so a 2 kg parcel does not bill at eight rupees.
freight_minimum <- function() {
  v <- suppressWarnings(as.numeric(setting("freight_minimum", "500")))
  if (is.na(v)) 500 else v
}

#' The freight breakup (CR-08).
#'
#' Returns one row per charge line plus the arithmetic, so the booking summary,
#' the consignment note and the invoice all print the same lines in the same
#' order instead of each re-deriving them.
freight_breakup <- function(weight_kg, dest_pincode, branch_id = "",
                            dispatch = "LTL", delivery_type = "Normal") {
  w <- suppressWarnings(as.numeric(weight_kg))
  if (is.na(w) || w < 0) w <- 0

  r    <- freight_rate(dest_pincode, branch_id, dispatch)
  base <- round(w * r$rate)
  minimum <- freight_minimum()

  # The minimum is applied as a visible top-up rather than by quietly raising
  # the base, so the customer can see why a light parcel costs what it does.
  topup <- if (base > 0 && base < minimum) minimum - base else 0
  oda   <- if (identical(delivery_type, "ODA")) oda_charge() else 0

  lines <- list(
    list(label = sprintf("Freight — %s kg @ %s/kg", format(round(w, 2)), inr(r$rate)),
         note = r$basis, amount = base),
    list(label = "Minimum charge top-up",
         note = paste("Minimum", inr(minimum), "per consignment"), amount = topup),
    list(label = "ODA surcharge",
         note = "Delivery outside the branch's ordinary beat", amount = oda)
  )
  lines <- Filter(function(l) l$amount > 0, lines)

  list(lines = lines, rate = r$rate, rate_basis = r$basis,
       base = base, minimum_topup = topup, oda = oda,
       freight = base + topup + oda)
}

# ------------------------------------------------------------------
# E-way bill requirement (CR-16, CR-17, CR-19)
# ------------------------------------------------------------------

#' Is this product type exempt from the e-way bill requirement? (CR-17)
#'
#' Annexure to Rule 138(14) exempts a specific list of goods whatever the
#' consignment is worth. The list is data, not code, so it can be corrected
#' without a release.
product_exempt <- function(product_type) {
  p <- trimws(as.character(product_type %||% ""))
  if (!nzchar(p)) return(FALSE)
  pt <- store_get("product_types")
  if (!nrow(pt) || !"name" %in% names(pt)) return(FALSE)
  row <- pt[pt$name == p, , drop = FALSE]
  if (!nrow(row)) return(FALSE)
  isTRUE(as.logical(row$ewb_exempt[1]))
}

#' Whether an e-way bill is required, and why (CR-16, CR-19).
#'
#' `invoice_total` is the sum across every invoice on the booking, not one
#' invoice — see the note on the thresholds in global.R.
ewb_required <- function(invoice_total, origin_state, dest_state,
                         product_type = "") {
  v <- suppressWarnings(as.numeric(invoice_total))
  if (is.na(v)) v <- 0

  interstate <- nzchar(origin_state %||% "") && nzchar(dest_state %||% "") &&
                !identical(origin_state, dest_state)
  limit <- if (interstate) EWB_THRESHOLD_INTERSTATE else EWB_THRESHOLD_INTRASTATE
  movement <- if (interstate) "interstate" else "intrastate"

  if (product_exempt(product_type)) {
    return(list(required = FALSE, limit = limit, movement = movement,
                total = v, exempt = TRUE,
                reason = sprintf("%s is exempt under Rule 138(14) whatever the value",
                                 product_type)))
  }

  req <- v >= limit
  list(required = req, limit = limit, movement = movement, total = v,
       exempt = FALSE,
       reason = if (req)
         sprintf("Invoice total %s is at or above the %s limit of %s",
                 inr(v), movement, inr(limit))
       else
         sprintf("Invoice total %s is below the %s limit of %s",
                 inr(v), movement, inr(limit)))
}

# ------------------------------------------------------------------
# Booking invoices (CR-15, CR-18)
# ------------------------------------------------------------------

#' Every invoice attached to a booking.
booking_invoices <- function(booking_no) {
  bi <- store_typed("booking_invoices", num = "invoice_value")
  if (!nrow(bi)) return(bi)
  bi[bi$booking_no == (booking_no %||% ""), , drop = FALSE]
}

#' Summed invoice value for a booking — the figure the threshold tests.
booking_invoice_total <- function(booking_no) {
  bi <- booking_invoices(booking_no)
  if (!nrow(bi)) return(0)
  sum(bi$invoice_value, na.rm = TRUE)
}
