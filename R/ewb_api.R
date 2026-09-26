# ==================================================================
# E-way bill API adapter (CR-21, CR-22).
#
# WHAT IS AND IS NOT PROVEN HERE
# ------------------------------
# The payload builders, the field mapping and the call flow below are written
# against the NIC e-way bill API specification and are exercised by the unit
# checks in tests/smoke.R. They have NOT been run against a live NIC or GSP
# endpoint, because no provider has been chosen yet — that is D-10 in
# CHANGE-REQUESTS-2026-09-20.md, and it needs credentials, a sandbox and
# approval before a single real call can be made.
#
# So this file is deliberately shaped as an adapter rather than a client:
# everything up to the HTTP boundary is real and testable, and the boundary
# itself is one function (.ewb_post) with one place to put the endpoint and
# the auth header. When the provider is chosen, that function and the
# integrations row are the only things that change.
#
# Until then ewb_api_configured() is FALSE, every entry point returns a
# not-configured result rather than erroring, and the screens fall back to
# manual entry exactly as they do today. An unconfigured integration must
# never block a booking — a truck cannot wait at the gate because a gateway
# has not been bought.
#
# WHY GENERATION IS NOT AUTOMATIC
# -------------------------------
# An e-way bill is a legal declaration by the consignor. Generating one
# silently from whatever is on the booking would put the carrier's name on a
# declaration nobody checked, and cancelling one is a 24-hour window. Every
# entry point here is called from an explicit operator action.
# ==================================================================

#' Provider configuration, read from the integrations table.
#'
#' Credentials never live in this file. They come from the environment, so a
#' token cannot end up in a commit — the same rule the GitHub sync follows.
ewb_api_config <- function() {
  ig <- store_get("integrations")
  row <- if (nrow(ig)) ig[grepl("e-way", tolower(ig$name)), , drop = FALSE] else ig[0, ]
  list(
    provider = if (nrow(row)) (row$provider[1] %||% "") else "",
    status   = if (nrow(row)) (row$status[1] %||% "Not configured") else "Not configured",
    base_url = Sys.getenv("TMS_EWB_BASE_URL", ""),
    username = Sys.getenv("TMS_EWB_USERNAME", ""),
    password = Sys.getenv("TMS_EWB_PASSWORD", ""),
    gstin    = Sys.getenv("TMS_EWB_GSTIN", setting("company_gstin", "")),
    client_id     = Sys.getenv("TMS_EWB_CLIENT_ID", ""),
    client_secret = Sys.getenv("TMS_EWB_CLIENT_SECRET", "")
  )
}

#' TRUE only when there is somewhere to send a request and something to sign
#' it with. Checked before every call.
ewb_api_configured <- function() {
  cfg <- ewb_api_config()
  nzchar(cfg$base_url) && nzchar(cfg$username) && nzchar(cfg$password)
}

.ewb_not_configured <- function(what) {
  list(ok = FALSE, configured = FALSE, ref = "", payload = NULL,
       message = paste0(
         what, " could not be sent: no e-way bill provider is configured. ",
         "Set TMS_EWB_BASE_URL, TMS_EWB_USERNAME and TMS_EWB_PASSWORD, and ",
         "record the provider on the Settings screen. Enter the number by ",
         "hand meanwhile."))
}

# ------------------------------------------------------------------
# Field mapping
# ------------------------------------------------------------------

#' A column that may not exist yet.
#'
#' Bookings written before the 20 Sep change requests have no consignee or
#' product-type column at all, and `b$consignee_name` on such a row is not
#' merely empty — it warns and returns NULL, which then propagates into the
#' payload as a missing field rather than a blank one.
col_or <- function(df, name, default = "") {
  if (!name %in% names(df)) return(default)
  v <- df[[name]]
  if (length(v) == 0 || is.na(v[1]) || !nzchar(as.character(v[1]))) return(default)
  as.character(v[1])
}

#' State code for the e-way bill payload.
#'
#' NIC identifies states by their GST numeric code, not by name, and the first
#' two digits of a GSTIN are exactly that code — so a party with a GSTIN
#' carries its own answer. The state name is the fallback for an unregistered
#' consignor, where it has to be looked up.
ewb_state_code <- function(gstin = "", state_name = "") {
  g <- gsub("[^0-9A-Za-z]", "", as.character(gstin %||% ""))
  if (nchar(g) >= 2 && grepl("^[0-9]{2}", g)) return(substr(g, 1, 2))
  codes <- c(
    "Jammu and Kashmir"="01","Himachal Pradesh"="02","Punjab"="03","Chandigarh"="04",
    "Uttarakhand"="05","Haryana"="06","Delhi"="07","Rajasthan"="08","Uttar Pradesh"="09",
    "Bihar"="10","Sikkim"="11","Arunachal Pradesh"="12","Nagaland"="13","Manipur"="14",
    "Mizoram"="15","Tripura"="16","Meghalaya"="17","Assam"="18","West Bengal"="19",
    "Jharkhand"="20","Odisha"="21","Chhattisgarh"="22","Madhya Pradesh"="23",
    "Gujarat"="24","Daman and Diu"="25","Dadra and Nagar Haveli"="26","Maharashtra"="27",
    "Karnataka"="29","Goa"="30","Lakshadweep"="31","Kerala"="32","Tamil Nadu"="33",
    "Puducherry"="34","Andaman and Nicobar Islands"="35","Telangana"="36",
    "Andhra Pradesh"="37","Ladakh"="38")
  unname(codes[as.character(state_name %||% "")] %||% "")
}

#' Transport mode code. 1 = road, which is all this carrier does; the others
#' are listed so the mapping is visible rather than a bare literal.
EWB_TRANS_MODE <- c(road = "1", rail = "2", air = "3", ship = "4")

#' Build the Part-A payload for one booking (CR-21).
#'
#' Part-A is the consignment: who is sending what to whom, and what it is
#' worth. It is everything except the vehicle, which is Part-B, and that split
#' is the whole reason a bill can be raised before a vehicle is allotted.
ewb_part_a_payload <- function(booking_no) {
  bk <- get_bookings()
  b  <- bk[bk$booking_no == booking_no, , drop = FALSE]
  if (!nrow(b)) return(NULL)
  b <- b[1, ]

  cl <- store_get("clients")
  cr <- cl[cl$client_id == b$client_id, , drop = FALSE]
  br <- store_get("branches")
  ob <- br[br$branch_id == b$branch_id, , drop = FALSE]

  inv <- booking_invoices(booking_no)
  # The value declared is the sum of the invoices on the consignment — the
  # same figure the threshold was tested against, so the bill and the decision
  # to raise it cannot disagree.
  total <- if (nrow(inv)) sum(as_num(inv$invoice_value), na.rm = TRUE) else 0
  first_inv <- if (nrow(inv)) inv[1, ] else NULL

  from_gstin <- if (nrow(ob)) (ob$gstin[1] %||% "") else ""
  to_gstin   <- if (nrow(cr)) (cr$gstin[1] %||% "") else ""

  list(
    supplyType    = "O",          # outward
    subSupplyType = "1",          # supply
    docType       = "INV",
    docNo         = if (!is.null(first_inv)) first_inv$invoice_no else b$booking_no,
    docDate       = format(as.Date(b$booking_date), "%d/%m/%Y"),

    fromGstin    = from_gstin,
    fromTrdName  = setting("company_name", BRAND$company),
    fromAddr1    = if (nrow(ob)) (ob$address[1] %||% "") else "",
    fromPlace    = b$origin_city,
    fromPincode  = as.integer(b$origin_pincode %||% 0),
    # act*StateCode is where the goods physically move from and to;
    # *StateCode is where the party is registered. NIC keeps them apart
    # because they routinely differ — a Maharashtra-registered customer takes
    # delivery in Delhi all the time — and deriving the physical one from the
    # GSTIN prefix (as this did at first) silently files every such movement
    # as intrastate against the wrong state. The registration code comes from
    # the GSTIN; the physical one comes from the address, never the GSTIN.
    actFromStateCode = ewb_state_code("", b$origin_state),
    fromStateCode    = ewb_state_code(from_gstin, b$origin_state),

    toGstin     = to_gstin,
    toTrdName   = col_or(b, "consignee_name",
                         if (nrow(cr)) cr$name[1] else ""),
    toAddr1     = col_or(b, "delivery_address"),
    toPlace     = b$dest_city,
    toPincode   = as.integer(b$dest_pincode %||% 0),
    actToStateCode = ewb_state_code("", b$dest_state),
    toStateCode    = ewb_state_code(to_gstin, b$dest_state),

    totalValue  = total,
    totInvValue = total,
    cgstValue   = 0, sgstValue = 0, igstValue = 0, cessValue = 0,

    transporterId   = from_gstin,
    transporterName = setting("company_name", BRAND$company),
    transMode       = unname(EWB_TRANS_MODE["road"]),
    transDistance   = as.character(round(as_num(b$distance_km) %||% 0)),
    transDocNo      = b$booking_no,
    transDocDate    = format(as.Date(b$booking_date), "%d/%m/%Y"),

    itemList = list(list(
      productName  = col_or(b, "product_type", b$material),
      productDesc  = b$material,
      quantity     = as_num(b$quantity) %||% 0,
      qtyUnit      = "NOS",
      taxableAmount = total
    ))
  )
}

#' Build the consolidated payload for a trip (CR-21).
#'
#' A consolidated bill is one document covering every consignment on one
#' vehicle for one trip. It does not replace the individual bills and carries
#' no value of its own — it exists so a checkpost can read one number instead
#' of thirty, which is why it is keyed on the trip and not on a booking.
ewb_consolidated_payload <- function(trip_no) {
  tr <- get_trips()
  t  <- tr[tr$trip_no == trip_no, , drop = FALSE]
  if (!nrow(t)) return(NULL)
  t <- t[1, ]

  cn <- get_consignments()
  cns <- cn[cn$trip_no == trip_no, , drop = FALSE]
  if (!nrow(cns)) return(NULL)

  eb <- store_get("ewaybills")
  bills <- eb$ewb_no[eb$cn_no %in% cns$cn_no]
  bills <- unique(bills[nzchar(bills)])
  if (!length(bills)) return(NULL)

  v  <- store_get("vehicles")
  br <- store_get("branches")
  ob <- br[br$branch_id == (t$branch_id %||% ""), , drop = FALSE]

  list(
    fromPlace    = t$route_from %||% "",
    fromState    = ewb_state_code(if (nrow(ob)) ob$gstin[1] else "",
                                  if (nrow(ob)) ob$state[1] else ""),
    vehicleNo    = gsub("[^A-Z0-9]", "",
                        toupper(v$reg_no[match(t$vehicle_id, v$vehicle_id)] %||% "")),
    transMode    = unname(EWB_TRANS_MODE["road"]),
    transDocNo   = trip_no,
    transDocDate = format(Sys.Date(), "%d/%m/%Y"),
    tripSheetEwbBills = lapply(bills, function(x) list(ewbNo = x))
  )
}

#' Build the Part-B / vehicle-update payload (CR-22).
#'
#' Filed when the vehicle changes — a breakdown, a transhipment at a hub — and
#' it is what keeps the bill valid for the rest of the journey. reasonCode 2
#' is transhipment, 1 is a breakdown; anything else goes in as "Others" with
#' the reason spelled out, because a checkpost reads the remark, not the code.
ewb_part_b_payload <- function(ewb_no, vehicle_reg, from_place, from_state_code,
                               reason = "Transhipment", reason_code = "2") {
  list(
    ewbNo        = as.character(ewb_no),
    vehicleNo    = gsub("[^A-Z0-9]", "", toupper(as.character(vehicle_reg %||% ""))),
    fromPlace    = from_place %||% "",
    fromState    = from_state_code %||% "",
    reasonCode   = reason_code,
    reasonRem    = substr(reason, 1, 50),
    transDocNo   = "",
    transDocDate = format(Sys.Date(), "%d/%m/%Y"),
    transMode    = unname(EWB_TRANS_MODE["road"])
  )
}

# ------------------------------------------------------------------
# The HTTP boundary — the only part that talks to a provider
# ------------------------------------------------------------------

#' POST a payload to the provider.
#'
#' Kept as one function so that choosing NIC directly or going through a GSP
#' changes this and nothing else. Every failure is caught and returned; an
#' e-way bill gateway being down must surface as a message next to a manual
#' entry box, never as a stack trace over a half-saved booking.
.ewb_post <- function(path, payload, cfg = ewb_api_config()) {
  if (!ewb_api_configured()) return(.ewb_not_configured("Request"))
  url <- paste0(sub("/+$", "", cfg$base_url), "/", sub("^/+", "", path))
  out <- tryCatch({
    resp <- httr::POST(
      url,
      httr::add_headers(
        "Content-Type" = "application/json",
        "username"     = cfg$username,
        "password"     = cfg$password,
        "gstin"        = cfg$gstin,
        "client-id"    = cfg$client_id,
        "client-secret"= cfg$client_secret),
      body = jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"),
      encode = "raw",
      httr::timeout(30))
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    body <- tryCatch(jsonlite::fromJSON(txt), error = function(e) list(raw = txt))
    list(ok = httr::status_code(resp) < 400, configured = TRUE,
         status = httr::status_code(resp), body = body, payload = payload,
         message = if (httr::status_code(resp) < 400) "Accepted by the provider"
                   else paste("Provider rejected the request:", txt))
  }, error = function(e) {
    list(ok = FALSE, configured = TRUE, status = NA, body = NULL, payload = payload,
         message = paste("Could not reach the e-way bill provider:",
                         conditionMessage(e)))
  })
  out
}

# ------------------------------------------------------------------
# Entry points
# ------------------------------------------------------------------

#' Generate Part-A for a booking (CR-21).
ewb_api_generate_part_a <- function(booking_no) {
  payload <- ewb_part_a_payload(booking_no)
  if (is.null(payload)) {
    return(list(ok = FALSE, configured = ewb_api_configured(), payload = NULL,
                message = paste("Nothing to send:", booking_no,
                                "has no booking record or no invoices.")))
  }
  if (!ewb_api_configured()) {
    r <- .ewb_not_configured("Part-A")
    r$payload <- payload   # returned so the screen can show what would be sent
    return(r)
  }
  .ewb_post("ewayapi/GenEwayBill", payload)
}

#' Generate a consolidated bill for a trip (CR-21).
ewb_api_generate_consolidated <- function(trip_no) {
  payload <- ewb_consolidated_payload(trip_no)
  if (is.null(payload)) {
    return(list(ok = FALSE, configured = ewb_api_configured(), payload = NULL,
                message = paste("Nothing to consolidate:", trip_no,
                                "has no consignments carrying e-way bills.")))
  }
  if (!ewb_api_configured()) {
    r <- .ewb_not_configured("Consolidated e-way bill"); r$payload <- payload; return(r)
  }
  .ewb_post("ewayapi/GenConsolidatedEwayBill", payload)
}

#' File Part-B against a bill after a vehicle change (CR-22).
#'
#' Writes locally whatever the provider does. The local record is what the
#' branch works from, and a bill whose vehicle changed at a hub at midnight
#' must not be lost because a gateway was unreachable — reconciliation is a
#' smaller problem than a missing movement record.
ewb_api_update_part_b <- function(ewb_no, vehicle_id, reason = "Vehicle changed",
                                  user_id = "system") {
  v <- store_get("vehicles")
  reg <- v$reg_no[match(vehicle_id, v$vehicle_id)] %||% ""

  eb <- store_get("ewaybills")
  row <- eb[eb$ewb_no == ewb_no, , drop = FALSE]
  cn <- get_consignments()
  c1 <- if (nrow(row)) cn[cn$cn_no == row$cn_no[1], , drop = FALSE] else cn[0, ]
  br <- store_get("branches")
  ob <- if (nrow(c1)) br[br$branch_id == c1$branch_id[1], , drop = FALSE] else br[0, ]

  payload <- ewb_part_b_payload(
    ewb_no, reg,
    from_place = if (nrow(c1)) c1$origin_city[1] else "",
    from_state_code = ewb_state_code(if (nrow(ob)) ob$gstin[1] else "",
                                     if (nrow(ob)) ob$state[1] else ""),
    reason = reason)

  local <- ewb_partb_add(ewb_no, vehicle_id = vehicle_id, reason = reason,
                         mode = "Manual", user_id = user_id)

  res <- if (!ewb_api_configured()) {
    r <- .ewb_not_configured("Part-B"); r$payload <- payload; r
  } else .ewb_post("ewayapi/VehEWB", payload)

  res$local_valid_to <- local
  res
}
