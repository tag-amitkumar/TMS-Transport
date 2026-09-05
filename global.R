# ==================================================================
# MoveWing Logistics Pvt. Ltd. — Transport Management System
# Global configuration, libraries, storage paths and shared helpers.
# ==================================================================

# Pin to India Standard Time. Hosted containers (shinyapps.io, Docker) almost
# always run UTC, which is 5h30m behind IST. Left alone, a dispatch stamped at
# 09:00 IST records as 03:30, and anything between midnight and 05:30 IST files
# against the *previous* date — which silently corrupts attendance days,
# e-way-bill validity windows and daily booking counts. Setting TZ here makes
# Sys.time()/Sys.Date() report IST wherever the app runs, so local dev and the
# deployed instance agree.
Sys.setenv(TZ = "Asia/Kolkata")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(scales)
  library(DT)
  library(plotly)
  library(shinyWidgets)
  library(bcrypt)
  library(uuid)
})

# leaflet backs a single screen (Live GPS). Loading it with library() would make
# a missing install crash the whole app at startup, so it is referenced
# namespaced (leaflet::) and guarded by this flag instead — every other screen
# still works if the package is absent.
HAS_LEAFLET <- requireNamespace("leaflet", quietly = TRUE)

# Basemap tiles.
#
# The deck's map is a pale Positron-style basemap, but both CARTO and Stamen now
# gate their tiles behind an API key and stamp "API KEY REQUIRED" across
# unauthenticated requests. OpenStreetMap's standard tiles are keyless, so those
# are used and desaturated in CSS (.leaflet-tile-pane filter in styles.css) to
# get the same muted look without a credential.
CARTO_LIGHT <- "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
CARTO_ATTR  <- "&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors"

# ------------------------------------------------------------------
# Brand
#
# One place for the operator's identity. Every screen, printed document and
# generated email address reads from here, so rebranding is this block and a
# logo file — not a search across thirty modules.
# ------------------------------------------------------------------
BRAND <- list(
  company = "MoveWing Logistics Pvt. Ltd.",
  # The company name without the legal suffix. Used where the full form is too
  # long to sit well — the sidebar rail, the login card, a document header.
  short   = "MoveWing",
  unit    = "Logistics Pvt. Ltd.",
  # The system, as distinct from the company that runs it.
  product = "Transport Management System",
  domain  = "movewinglogistics.in",
  # Lives in www/. A square, transparent-background lock-up. The app falls back
  # to a drawn monogram when it is absent, so a fresh clone still looks
  # finished rather than showing a broken image.
  logo    = "movewing_logo.png",
  navy    = "#12275C",
  orange  = "#E9631A"
)

BRAND$title <- paste(BRAND$short, "—", BRAND$product)

#' The square mark — an "MW" monogram in the brand colours.
#'
#' Used wherever the surface is dark or the space is small: the sidebar rail,
#' a small inline badge, a favicon-sized slot. The artwork is a square lock-up
#' whose lettering is dark navy — legible on paper and on a white card, and
#' invisible against the navy rail — so the rail keeps the monogram whether or
#' not the file exists.
brand_mark <- function(size = 38) {
  # nowrap and a conservative type size: "MW" is two wide glyphs, and at the
  # ratio a single letter would take they broke onto two lines inside the tile.
  div(class = "tms-brand-tile",
      style = sprintf(paste0("width:%1$dpx;height:%1$dpx;border-radius:%2$dpx;",
                             "background:%3$s;font-size:%4$.2fpx;font-weight:800;",
                             "letter-spacing:-.04em;white-space:nowrap;line-height:1;"),
                      size, max(6, round(size / 4.2)), BRAND$navy, size * 0.34),
      HTML(sprintf('<span style="color:#fff;">M</span><span style="color:%s;">W</span>',
                   BRAND$orange)))
}

#' The full logo lock-up, for white surfaces — the login card, printed
#' documents, anything that goes to a customer.
#'
#' Falls back to the mark when the artwork is absent, because it is a
#' binary asset that cannot live in this file: without the fallback a fresh
#' clone would render a broken-image icon on the login screen and on every
#' lorry receipt.
brand_logo <- function(height = 56, alt = BRAND$company) {
  p <- file.path("www", BRAND$logo)
  if (file.exists(p)) {
    return(tags$img(src = BRAND$logo, alt = alt, height = paste0(height, "px"),
                    style = "display:block;width:auto;max-width:100%;"))
  }
  brand_mark(height)
}

#' TRUE when the real artwork is in place.
#'
#' Layout differs: the artwork already contains the company name, so printing
#' the wordmark beside it would say everything twice.
brand_has_logo <- function() file.exists(file.path("www", BRAND$logo))

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------
DATA_DIR <- "data"

# ------------------------------------------------------------------
# Seed profile
#
#   "minimal" — two logins (Super Admin + Operations) and one sample record per
#               entity. For handing someone a clean system to put their own data
#               into. This is the profile this branch ships.
#   "demo"    — the full generated dataset (18 branches, 342 bookings, 214
#               clients) that exercises the app at realistic volume.
#
# Override at runtime with TMS_SEED_PROFILE. Changing it only affects a *fresh*
# seed: R/seed.R writes data/ once, and the app reads whatever is there.
# ------------------------------------------------------------------
SEED_PROFILE <- Sys.getenv("TMS_SEED_PROFILE", "minimal")

# Roles offered as one-click previews on the login screen.
#
# Every chip needs something behind it — an active account for an internal role,
# or a master record for Customer/Vendor — otherwise clicking it just produces
# an error. The minimal profile has exactly two accounts, so it offers exactly
# two chips.
DEMO_ROLES <- if (identical(SEED_PROFILE, "minimal")) {
  c("Super Admin", "Operations Manager")
} else {
  c("Super Admin", "Branch Admin", "Dispatcher", "Accountant",
    "HR Manager", "Vendor", "Customer")
}

# Receipt/POD photos come off phone cameras, which routinely exceed Shiny's
# 5 MB default request cap.
options(shiny.maxRequestSize = 15 * 1024^2)

# Every persisted table. store.R reads/writes exclusively through this map, so
# renaming a file or moving to a database means editing one list, not 31 modules.
TABLES <- c(
  "branches", "users", "roles", "permissions",
  "clients", "vendors", "vendor_documents",
  "vehicles", "employees", "drivers", "pincode_routes",
  "bookings", "trips", "consignments", "consignment_events",
  "ewaybills", "ewaybill_partb", "pods", "gps_pings",
  "complaints", "invoices", "payments", "vendor_payments",
  "attendance", "leave_types", "leave_requests", "payroll",
  "settings", "notification_events", "integrations", "audit_log"
)

tbl_path <- function(name) file.path(DATA_DIR, paste0(name, ".csv"))

# ------------------------------------------------------------------
# Domain vocabulary
#
# Resolved from the design deck, which used several competing labels for the
# same thing. Decisions are recorded in DESIGN-REVIEW.md; these constants are
# the single source of truth the app actually runs on.
# ------------------------------------------------------------------

# Deck showed BK-30241, BKG-4482, BKG-4471 and BKG-4490 across five screens for
# what is plainly one entity. Standardised on a single BKG- series.
PREFIX <- list(
  booking       = "BKG-",
  consignment   = "CN-",
  lorry_receipt = "LR-",
  trip          = "TRP-",
  invoice       = "INV-",
  complaint     = "CMP-",
  payment       = "PAY-",
  employee      = "EMP-",
  client        = "CUST-",
  vendor        = "VND-"
)

# Booking lifecycle. Cargo Moto's kanban is a *view* over these same records
# grouped by allocation stage, not a separate booking type — the deck's
# 13-vs-342 count mismatch was a mockup artifact.
BOOKING_STATUS <- c("Draft", "Confirmed", "Vehicle Allocated", "In Transit",
                    "Delivered", "Closed", "Cancelled")

CONSIGNMENT_STATUS <- c("In Prep", "Dispatched", "In Transit", "At Hub",
                        "Out for Delivery", "Delivered")

VEHICLE_STATUS   <- c("Available", "Allocated", "In Transit", "Maintenance", "Inactive")
DRIVER_STATUS    <- c("On trip", "Available", "On leave")
POD_STATUS       <- c("Pending", "Uploaded", "Verified", "Approved")
EWB_STATUS       <- c("Valid", "Expiring Soon", "Expired")
COMPLAINT_STATUS <- c("New", "Assigned", "In Progress", "Resolved")
INVOICE_STATUS   <- c("Draft", "Sent", "Partially paid", "Paid", "Overdue")
TRIP_STATUS      <- c("Planned", "Loading", "Running", "At Hub", "Completed")

# Ten access levels, exactly as drawn on the Security & Roles matrix.
ROLE_LEVELS <- c("Super Admin", "Branch Admin", "Operations Manager",
                 "Booking Executive", "Dispatcher", "Accountant",
                 "HR Manager", "Driver", "Vendor", "Customer")

# GTA freight under Indian GST can sit on reverse charge (recipient pays) or
# forward charge (carrier pays). The deck showed RCM 5%, FCM 12% and FCM 18%
# with only the hint "RCM flagged per client configuration", so the rate is
# stored per client rather than inferred — see clients$gst_mode / gst_pct.
GST_MODES <- c("RCM", "FCM")

# ------------------------------------------------------------------
# Freight payment terms
#
# Who settles the freight, and when. This is not the same thing as GST
# treatment: a To-Pay consignment can still be reverse charge. It rides on the
# LR because the driver has to know whether to collect at the door.
#
#   Paid    — consignor settled it at booking; nothing to collect on delivery.
#   To Pay  — consignee pays before the goods are released.
#   Credit  — booked to the customer's account and billed on the monthly cycle.
#   TBB     — To Be Billed: the customer holds their account at another branch,
#             so the paperwork travels with the load but the invoice is raised
#             by that branch against their existing account.
# ------------------------------------------------------------------
PAYMENT_MODES <- c("Paid", "To Pay", "Credit", "TBB")

PAYMENT_LABEL <- c(
  "Paid"   = "Paid (100% at booking)",
  "To Pay" = "To Pay (collect from consignee)",
  "Credit" = "Credit (monthly billing)",
  "TBB"    = "TBB (bill at another branch)"
)

# The colour each term carries on the LR and in every list. Money already in
# hand reads green; money to be chased on the doorstep reads amber.
PAYMENT_COLOUR <- c("Paid" = "green", "To Pay" = "orange",
                    "Credit" = "blue", "TBB" = "purple")

# How a booking got into the system. Manual entries are the ones written on a
# paper LR book while TMS was unreachable and keyed in afterwards — they carry
# the original paper reference and the time the load was actually accepted, so
# the audit trail does not silently claim the booking was taken when it was
# merely typed up.
ENTRY_MODES <- c("Online", "Manual")

# ------------------------------------------------------------------
# Formatting helpers
#
# Indian numbering (lakh/crore) is used throughout the deck's KPI cards, and
# base R's big.mark cannot produce the 2,2,3 grouping, so it is done by hand.
# ------------------------------------------------------------------

# 1234567 -> "12,34,567"
inr_group <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    neg <- v < 0
    s <- formatC(abs(round(v)), format = "d")
    if (nchar(s) > 3) {
      last3 <- substring(s, nchar(s) - 2)
      rest  <- substring(s, 1, nchar(s) - 3)
      # Group the remaining digits in pairs, right to left.
      rest  <- gsub("(?<=.)(?=(?:..)+$)", ",", rest, perl = TRUE)
      s <- paste0(rest, ",", last3)
    }
    paste0(if (neg) "-" else "", s)
  }, character(1), USE.NAMES = FALSE)
}

inr <- function(x) paste0("₹", inr_group(x))

# Compact lakh/crore form for stat cards: 14200000 -> "₹1.42 Cr"
inr_compact <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  vapply(x, function(v) {
    if (is.na(v)) return("—")
    a <- abs(v)
    sgn <- if (v < 0) "-" else ""
    if (a >= 1e7)      sprintf("%s₹%s Cr", sgn, format(round(a / 1e7, 2), nsmall = 2))
    else if (a >= 1e5) sprintf("%s₹%s L",  sgn, format(round(a / 1e5, 1), nsmall = 1))
    else if (a >= 1e3) sprintf("%s₹%s K",  sgn, format(round(a / 1e3, 0)))
    else               sprintf("%s₹%s",    sgn, round(a))
  }, character(1), USE.NAMES = FALSE)
}

# Deck's date convention: DD-MMM-YYYY, shown short ("08 Aug") in dense tables.
fmt_date <- function(d, short = FALSE) {
  d <- suppressWarnings(as.Date(d))
  ifelse(is.na(d), "—", format(d, if (short) "%d %b" else "%d %b %Y"))
}

fmt_dt <- function(x) {
  x <- suppressWarnings(as.POSIXct(x, tz = "Asia/Kolkata"))
  ifelse(is.na(x), "—", format(x, "%d %b %H:%M"))
}

fmt_wt <- function(t) {
  t <- suppressWarnings(as.numeric(t))
  ifelse(is.na(t), "—", paste0(format(round(t, 1), nsmall = 1), " T"))
}

# Blank-safe coalesce: treats "", NA and NULL alike, which flat-file reads
# produce interchangeably depending on whether a column was ever populated.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && (is.na(a) || identical(a, ""))) return(b)
  a
}

# Days until a date, negative once past. Drives every expiry badge in the app
# (licence, insurance, fitness, permit, PUC, e-way bill).
days_to <- function(d) as.numeric(as.Date(d) - Sys.Date())

# Expiry severity shared by vehicle documents, driver licences and e-way bills
# so one threshold change updates every screen at once.
expiry_state <- function(d, warn = 30) {
  n <- days_to(d)
  dplyr::case_when(
    is.na(n)  ~ "none",
    n < 0     ~ "danger",
    n <= warn ~ "warn",
    TRUE      ~ "ok"
  )
}

# Next id in a prefixed series, e.g. next_id(bookings$booking_no, "BKG-") on
# c("BKG-4471","BKG-4482") -> "BKG-4483". Flat-file storage has no sequences,
# so ids are derived from the data on each insert.
next_id <- function(existing, prefix, width = 4) {
  nums <- suppressWarnings(as.integer(sub(paste0("^", prefix), "", existing)))
  nums <- nums[!is.na(nums)]
  n <- if (length(nums)) max(nums) + 1L else 1L
  paste0(prefix, formatC(n, width = width, flag = "0", format = "d"))
}

# ------------------------------------------------------------------
# References that get read off a screen and typed into another system
# ------------------------------------------------------------------

# An e-way bill number is read aloud at a check-post, copied onto a paper LR
# and keyed into another system, usually in a hurry and often in bad light.
# Every character with a lookalike is a transcription error waiting to happen,
# so the alphabet drops them:
#
#   0 / O       indistinguishable in most sans-serif faces
#   1 / I / L   the same problem, and worse in handwriting
#
# What is left is 2-9 and 23 letters — 31 characters, so a 12-character
# reference still spans 7.9 x 10^17 combinations. No separators either: a hyphen or a space invites "was
# that a dash?" and gets dropped or doubled on re-entry.
REF_ALPHABET <- c(as.character(2:9), setdiff(LETTERS, c("O", "I", "L")))

#' A hand-transcribable reference of `n` characters, unique against `existing`.
#'
#' Deliberately not the 12-digit numeric format the GST portal issues. Nothing
#' in this app talks to the GSP (see DESIGN-REVIEW.md, gap #12), and a
#' reference that cannot be mistaken for a real government e-way bill number is
#' the honest way to say so.
safe_ref <- function(n = 12, existing = character(0)) {
  for (i in seq_len(50)) {
    r <- paste(sample(REF_ALPHABET, n, replace = TRUE), collapse = "")
    if (!r %in% existing) return(r)
  }
  # 31^12 makes this unreachable in practice. Falling through silently and
  # handing back a duplicate would not be.
  stop("Could not allocate a unique reference after 50 attempts")
}

# Start of the rolling window used by the "recent activity" KPI cards.
#
# Month-to-date was the obvious choice and the wrong one: every such card reads
# zero on the 1st of a month, which operators read as a broken feed rather than
# as a fresh period. A rolling 30-day window always has content. Genuinely
# monthly things — the payroll run, the deliveries-per-month chart, the
# attendance Month tab — still use calendar months.
since_30d <- function() Sys.Date() - 30
