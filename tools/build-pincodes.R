# ==================================================================
# Build the India pincode / city reference master.
#
# Run on a developer machine, not on the server:
#
#   Rscript tools/build-pincodes.R path/to/IN.txt
#
# Source: GeoNames postal-code export for India
# (https://download.geonames.org/export/zip/IN.zip), licensed CC BY 4.0.
# It is preferred over the various scraped India Post CSVs because it is
# maintained, complete (every row carries a district, a state and coordinates),
# and — the part that matters here — geocoded. Coordinates let the booking form
# quote a distance and a transit promise for a lane nobody has mapped by hand,
# which with 19k pincodes in play is nearly every lane.
#
# Writes three files under data/reference/. They are reference data, not
# application state: nothing in the app ever writes to them, so they are
# deliberately kept out of TABLES and out of the store.
# ==================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
src  <- if (length(args)) args[1] else "IN.txt"
if (!file.exists(src)) stop("GeoNames extract not found: ", src)

out_dir <- file.path("data", "reference")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("Reading ", src)
raw <- read_tsv(
  src,
  col_names = c("country", "pincode", "place", "state", "state_code",
                "district", "district_code", "admin3", "admin3_code",
                "lat", "lon", "accuracy"),
  col_types = cols(.default = col_character(),
                   lat = col_double(), lon = col_double()),
  progress = FALSE
)
message("  ", format(nrow(raw), big.mark = ","), " post-office rows")

# ------------------------------------------------------------------
# Corrections to the source
# ------------------------------------------------------------------

# Ladakh was separated from Jammu & Kashmir as its own union territory on
# 31 October 2019. GeoNames still files Leh and Kargil under J&K, which would
# leave the state list one UT short and print the wrong state on a Leh LR.
raw$state[raw$district %in% c("Leh", "Kargil")] <- "Ladakh"

# Delhi is nine revenue districts (Central, East, New Delhi and so on). For
# freight it is one city — nobody books to "North West Delhi" — so the
# districts collapse while the pincode still resolves the exact area.
raw$district[raw$state == "Delhi"] <- "Delhi"

# ------------------------------------------------------------------
# One row per pincode
# ------------------------------------------------------------------
#
# A pincode is not always inside one district (201301 straddles Ghaziabad and
# Gautam Buddha Nagar), so the city is the district most of its post offices
# sit in. Coordinates are the *median* of those offices rather than the mean:
# 744301 in the Nicobars carries offices 190 km apart including one clear
# outlier, and a mean would split the difference and land in the sea.

modal <- function(x) {
  t <- sort(table(x), decreasing = TRUE)
  names(t)[1]
}

# Representative locality: prefer an office whose name carries the city name
# ("Mohan Nagar (Nagpur)"), otherwise the first alphabetically, which in
# practice lands on the head office ("Noida H.O" ahead of "Noida Sector 12").
pick_locality <- function(places, city) {
  hit <- places[grepl(city, places, fixed = TRUE)]
  sort(if (length(hit)) hit else places)[1]
}

message("Collapsing to one row per pincode")
pins <- raw %>%
  group_by(pincode) %>%
  summarise(
    city     = modal(district),
    state    = modal(state),
    lat      = round(median(lat, na.rm = TRUE), 4),
    lon      = round(median(lon, na.rm = TRUE), 4),
    offices  = n(),
    # GeoNames grades each row: 1 means it could not place the locality and
    # substituted a city-level point, 3 and 4 mean a real fix. Roughly 10% of
    # India is grade 1, concentrated in the metros — every Mumbai and Bengaluru
    # pincode without a fix lands on the same city coordinate. Carrying the
    # best grade available lets the app tell an inter-city distance (where a
    # city-level point is fine) from a cross-town one (where it is useless).
    accuracy = max(as.integer(accuracy), na.rm = TRUE),
    locality = pick_locality(place, modal(district)),
    .groups  = "drop"
  ) %>%
  arrange(pincode) %>%
  select(pincode, locality, city, state, lat, lon, offices, accuracy)

# ------------------------------------------------------------------
# One row per city
# ------------------------------------------------------------------
#
# The representative pincode is the lowest one in the city: in the India Post
# numbering a city head office almost always holds the lowest code in its range
# (Nagpur 440001, Mumbai 400001, Jaipur 302001).

message("Deriving cities")
cities <- pins %>%
  group_by(city, state) %>%
  summarise(
    pincode  = min(pincode),
    lat      = round(median(lat), 4),
    lon      = round(median(lon), 4),
    pincodes = n(),
    .groups  = "drop"
  ) %>%
  arrange(state, city)

# City names are used as a bare key on the booking (origin_city), which only
# works if they are unique nationally. GeoNames already disambiguates the
# collisions — Raigarh(MH) against Raigarh in Chhattisgarh, Aurangabad(BH)
# against Aurangabad in Maharashtra — but that is a property of the source, not
# a promise, so fail the build rather than ship an ambiguous lane.
dupes <- cities$city[duplicated(cities$city)]
if (length(dupes)) {
  stop("City names are not unique across states: ",
       paste(unique(dupes), collapse = ", "))
}

# ------------------------------------------------------------------
# Aliases
# ------------------------------------------------------------------
#
# What a clerk types is not always what the district is called. Renamed metros
# (Bangalore/Bengaluru), satellite towns that are not their own district (Noida
# sits in Gautam Buddha Nagar) and the twin-city shorthands all have to
# resolve, or the search box looks broken for the busiest lanes in the country.
# Hand-written and deliberately short: every entry is a name in real use.
alias <- tibble::tribble(
  ~alias,          ~city,
  "Bangalore",     "Bengaluru",
  "Bombay",        "Mumbai",
  "Calcutta",      "Kolkata",
  "Madras",        "Chennai",
  "Poona",         "Pune",
  "Trivandrum",    "Thiruvananthapuram",
  "Cochin",        "Ernakulam",
  "Kochi",         "Ernakulam",
  "Calicut",       "Kozhikode",
  "Mysore",        "Mysuru",
  "Mangalore",     "Dakshina Kannada",
  "Belgaum",       "Belagavi",
  "Hubli",         "Dharwad",
  "Gulbarga",      "Kalaburagi",
  "Baroda",        "Vadodara",
  "Vizag",         "Visakhapatnam",
  "Puducherry",    "Pondicherry",
  "Trichy",        "Tiruchirappalli",
  "Thoothukkudi",  "Tuticorin",
  "Ooty",          "Nilgiris",
  "New Delhi",     "Delhi",
  "Noida",         "Gautam Buddha Nagar",
  "Greater Noida", "Gautam Buddha Nagar",
  "Gurugram",      "Gurgaon",
  "Navi Mumbai",   "Thane",
  "Bhiwandi",      "Thane",
  "Secunderabad",  "Hyderabad",
  "Jamshedpur",    "East Singhbhum",
  "Tatanagar",     "East Singhbhum",
  "Prayagraj",     "Allahabad",
  "Guwahati",      "Kamrup",
  "Simla",         "Shimla",
  "Panaji",        "North Goa",
  "Panjim",        "North Goa"
)

# An alias pointing at a city that does not exist is worse than no alias: the
# search finds it, the clerk picks it, and the booking carries a city the
# pincode master has never heard of. Drop the ones this GeoNames vintage does
# not support rather than failing the build, and say which.
unknown <- setdiff(alias$city, cities$city)
if (length(unknown)) {
  message("  skipping aliases for unknown cities: ",
          paste(unknown, collapse = ", "))
  alias <- alias[!alias$city %in% unknown, ]
}
alias <- alias %>%
  left_join(cities %>% select(city, state), by = "city") %>%
  filter(alias != city) %>%
  arrange(alias)

# ------------------------------------------------------------------

write_csv(pins,   file.path(out_dir, "pincodes.csv"),     na = "")
write_csv(cities, file.path(out_dir, "cities.csv"),       na = "")
write_csv(alias,  file.path(out_dir, "city_aliases.csv"), na = "")

kb <- function(f) round(file.size(file.path(out_dir, f)) / 1024)
message("\nWrote data/reference/")
message("  pincodes.csv     ", format(nrow(pins), big.mark = ","),
        " pincodes (", kb("pincodes.csv"), " KB)")
message("  cities.csv       ", nrow(cities), " cities across ",
        dplyr::n_distinct(cities$state), " states and union territories")
message("  city_aliases.csv ", nrow(alias), " alternate names")
