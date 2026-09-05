# ==================================================================
# India geography — pincode master, city master, lane estimation.
#
# Reference data, not application state. Nothing in the app writes here, so it
# deliberately bypasses store.R: no version signal, no reactivity, no CSV
# rewrite path. It is read once per process and cached for the life of it.
#
# Built by tools/build-pincodes.R from the GeoNames India postal export
# (CC BY 4.0). Rebuild when the source is refreshed; the app never regenerates
# these files at runtime.
#
#   pincodes.csv      19,238 rows — every pincode in India, with the city,
#                     state and coordinates it resolves to
#   cities.csv        630 rows — one per district-level city, with a
#                     representative pincode and a centroid
#   city_aliases.csv  the names people actually type (Bangalore, Noida,
#                     Gurugram) mapped onto the district they belong to
# ==================================================================

.geo <- new.env(parent = emptyenv())

# A function, not a constant. Shiny auto-sources everything in R/ before
# global.R is evaluated, so anything resolved at the top level here cannot see
# DATA_DIR yet — as a constant this failed the app at startup.
geo_dir <- function() file.path(DATA_DIR, "reference")

# Pincodes stay character throughout. They are identifiers, not quantities:
# arithmetic on one is never meaningful, and read as numeric they start
# rendering as 1.1e+05 the moment something formats a column of them. Only the
# coordinate and count columns are coerced.
.geo_read <- function(name, num = NULL) {
  if (!is.null(.geo[[name]])) return(.geo[[name]])
  p <- file.path(geo_dir(), paste0(name, ".csv"))
  if (!file.exists(p)) {
    warning("Geography reference missing: ", p, call. = FALSE)
    return(tibble::tibble())
  }
  df <- suppressWarnings(readr::read_csv(
    p, col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE))
  for (c in intersect(num, names(df))) df[[c]] <- as_num(df[[c]])
  .geo[[name]] <- df
  df
}

geo_pincodes <- function() .geo_read("pincodes", num = c("lat", "lon", "offices", "accuracy"))
geo_cities   <- function() .geo_read("cities",   num = c("lat", "lon", "pincodes"))
geo_aliases  <- function() .geo_read("city_aliases")

#' Every state and union territory, alphabetically.
geo_states <- function() sort(unique(geo_cities()$state))

#' Look up one pincode. NULL when it is not six digits or not in the master.
geo_pin <- function(pincode) {
  p <- trimws(as.character(pincode %||% ""))
  if (!grepl("^[1-9][0-9]{5}$", p)) return(NULL)
  df <- geo_pincodes()
  r <- df[df$pincode == p, ]
  if (nrow(r)) r[1, ] else NULL
}

#' Look up one city by its exact name. NULL when unknown.
geo_city <- function(city) {
  c0 <- trimws(as.character(city %||% ""))
  if (!nzchar(c0)) return(NULL)
  df <- geo_cities()
  r <- df[df$city == c0, ]
  if (nrow(r)) return(r[1, ])
  # Fall back through the alias table so a booking that was saved as
  # "Bangalore" still resolves after the city master moved to "Bengaluru".
  a <- geo_aliases()
  hit <- a$city[tolower(a$alias) == tolower(c0)]
  if (!length(hit)) return(NULL)
  r <- df[df$city == hit[1], ]
  if (nrow(r)) r[1, ] else NULL
}

#' Choices for a city selector: "Nagpur, Maharashtra" -> "Nagpur".
#'
#' Aliases ride in the label rather than in a separate lookup, because
#' selectize searches the label. That is what makes typing "Noida" find
#' Gautam Buddha Nagar and "Bangalore" find Bengaluru — the district names
#' nobody outside the revenue department uses.
geo_city_choices <- function() {
  if (!is.null(.geo$choices)) return(.geo$choices)
  ct <- geo_cities()
  if (!nrow(ct)) return(character(0))
  a  <- geo_aliases()
  extra <- vapply(ct$city, function(c0) {
    al <- a$alias[a$city == c0]
    if (length(al)) paste0(" (", paste(al, collapse = ", "), ")") else ""
  }, character(1), USE.NAMES = FALSE)
  lab <- paste0(ct$city, ", ", ct$state, extra)
  ord <- order(ct$city)
  .geo$choices <- setNames(ct$city[ord], lab[ord])
  .geo$choices
}

#' The pincodes belonging to one city, as selector choices.
#'
#' Labelled with the locality so a Mumbai clerk picking among 89 codes sees
#' "400001 · Mumbai G.P.O." rather than eighty-nine bare numbers.
geo_city_pin_choices <- function(city, limit = 400) {
  df <- geo_pincodes()
  r <- df[df$city == (city %||% ""), ]
  if (!nrow(r)) return(character(0))
  r <- r[order(r$pincode), ]
  if (nrow(r) > limit) r <- r[seq_len(limit), ]
  setNames(r$pincode, paste0(r$pincode, " · ", r$locality))
}

# ------------------------------------------------------------------
# Lane estimation
# ------------------------------------------------------------------

# Great-circle distance in km.
geo_haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  p1 <- lat1 * pi / 180
  p2 <- lat2 * pi / 180
  dp <- (lat2 - lat1) * pi / 180
  dl <- (lon2 - lon1) * pi / 180
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# Road distance is longer than the straight line by a circuity factor. 1.20 was
# calibrated against sixteen published NH distances spanning 150 to 1,670 km
# (Mumbai-Pune through Kolkata-Chennai): mean absolute error 3.4%, worst case
# 10.3%. Good enough to quote an indicative lane, not good enough to bill on —
# which is why an estimated lane is always labelled as one.
ROAD_FACTOR <- 1.20

# Long-haul trucking in India averages a little over 400 km in a driving day
# once loading, checkposts and rest are counted. 425 reproduces the transit
# days on the hand-mapped lanes in the route master.
KM_PER_DAY <- 425

#' Distance and transit promise for a lane.
#'
#' The PIN codes are the lane, and the city names are a label for it. Passing
#' the PINs is what makes a local booking work at all: a cartage job from
#' Nagpur 440001 to Nagpur 440016 has the same city at both ends, and resolving
#' by city alone would call that a zero-kilometre trip to itself. Where a PIN
#' is not supplied the city centroid stands in.
#'
#' The hand-mapped route master wins wherever it has an entry — those numbers
#' are commercial commitments, negotiated per lane, and an estimate must never
#' silently replace one. It is consulted on the exact PIN pair first, then on
#' the city pair, so a lane priced city-to-city still applies while a lane
#' priced between two specific PINs can override it.
#'
#' Returns NULL when neither end resolves, so callers can tell "no answer"
#' apart from "estimated answer".
geo_lane <- function(from_city, to_city, from_pin = NULL, to_pin = NULL) {
  fp <- geo_pin(from_pin)
  tp <- geo_pin(to_pin)
  a  <- if (!is.null(fp)) fp else geo_city(from_city)
  b  <- if (!is.null(tp)) tp else geo_city(to_city)
  if (is.null(a) || is.null(b)) return(NULL)

  is_local <- identical(a$city, b$city)
  path <- if (is_local) paste0(a$city, " · ", a$pincode, " → ", b$pincode)
          else paste(a$city, "→", b$city)

  rt <- store_get("pincode_routes")
  if (nrow(rt)) {
    hit <- rt[rt$src_pincode == a$pincode & rt$dst_pincode == b$pincode, ]
    # Only fall back to the city pair for a genuine inter-city lane. A
    # city-to-city mapping says nothing about a delivery that starts and ends
    # inside that city, and applying its 1,035 km to a cross-town run would be
    # wrong by two orders of magnitude.
    if (!nrow(hit) && !is_local) {
      hit <- rt[rt$src_city == a$city & rt$dst_city == b$city, ]
    }
    if (nrow(hit)) {
      h <- hit[1, ]
      km_mapped <- as_num(h$distance_km)
      return(list(
        km       = km_mapped,
        km_known = !is.na(km_mapped),
        days     = as_num(h$transit_days),
        mapped   = TRUE,
        local    = is_local,
        path     = h$route_path,
        branches = h$branch_mapping,
        status   = h$availability
      ))
    }
  }

  km <- geo_haversine(a$lat, a$lon, b$lat, b$lon) * ROAD_FACTOR

  # Whether that number means anything.
  #
  # A quarter of Indian pincodes are graded 1 by GeoNames: the locality could
  # not be placed, so it carries its city's coordinate instead. Over a trunk
  # lane that is harmless — being a few km off inside Mumbai does not move a
  # 1,400 km figure — and the calibration bears that out at 3.4% mean error.
  #
  # Inside one city it is fatal. Chembur and Malad are 15 km apart and both
  # carry the same fallback point, so the honest answer for a cross-town lane
  # between two ungraded PINs is "not known", not "0 km". Quoting zero would
  # price a real cartage job at nothing.
  # A city row carries no accuracy grade; treat that as ungraded rather than
  # letting NA propagate into the && below, where it would error.
  fine <- function(x) isTRUE(as_num(x$accuracy) >= 3)
  km_known <- !is_local ||
    (!(a$lat == b$lat && a$lon == b$lon) && fine(a) && fine(b))

  list(
    km       = if (km_known) round(km) else NA_real_,
    km_known = km_known,
    days     = if (km_known) max(1, ceiling(km / KM_PER_DAY)) else 1,
    mapped   = FALSE,
    local    = is_local,
    path     = path,
    branches = NA_character_,
    status   = "Available"
  )
}

#' The distance to persist on a row — blank when it is not known.
#'
#' `%||%` does not help here: an unknown distance is NA, not NULL, and
#' as.character(NA) writes the literal string "NA" into the CSV, which then
#' reads back as a value rather than a gap.
geo_lane_km <- function(ln) {
  if (is.null(ln) || !isTRUE(ln$km_known) || is.na(ln$km)) return("")
  ln$km
}
