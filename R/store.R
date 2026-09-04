# ==================================================================
# Storage layer — flat CSV files behind a narrow read/write API.
#
# Everything in the app goes through store_get() / store_set() / store_insert()
# / store_update(). No module touches read_csv() or a file path directly.
#
# Why that matters here: flat files were chosen for zero-setup portability, but
# they give no transactions and no referential integrity. Keeping the access
# surface this thin means swapping in SQLite or Postgres later is a rewrite of
# this one file, not of 31 screen modules. The function signatures are
# deliberately database-shaped (a table name, a filter, a named list of values)
# rather than file-shaped for exactly that reason.
# ==================================================================

# In-memory cache of every table.
#
# Re-reading CSVs on each reactive invalidation would make list screens crawl
# once bookings/consignments run to a few thousand rows. Tables are read once on
# first access and held here; writes update the cache and the file together, so
# readers never see a stale value within a session.
.store <- new.env(parent = emptyenv())

# Reactivity bridge.
#
# The cache above is a plain environment, so nothing downstream can tell when
# it changes. Screens re-read the store on every render, but a render only
# happens when a *reactive* dependency invalidates — so a row written by a
# handler sat in the file while the table on screen kept showing the old data.
# Creating a booking looked like it had failed.
#
# Every mutating call bumps this counter, and store_get() takes a dependency on
# it whenever it is called inside a reactive context. One global counter rather
# than one per table: writes are rare, tables are small, and a booking insert
# legitimately changes what the dashboard, the kanban and the consignment
# register should show.
.store_version <- NULL

.store_bump <- function() {
  if (is.null(.store_version)) return(invisible(NULL))
  shiny::isolate(.store_version(.store_version() + 1))
  invisible(NULL)
}

#' Create the version signal. Called once from app.R, after Shiny is loaded.
store_init_reactivity <- function() {
  if (is.null(.store_version)) .store_version <<- shiny::reactiveVal(0L)
  invisible(TRUE)
}

# Reading with an explicit column spec would mean maintaining 30 schemas by
# hand. Instead everything is read as character and coerced at the point of use
# — flat files have no types anyway, and readr's guesser is the main source of
# "column changed type when the data changed" bugs (an all-numeric id column
# guessed as double, then rendered as 4.47e3).
.read_tbl <- function(name) {
  p <- tbl_path(name)
  if (!file.exists(p)) return(tibble::tibble())
  suppressWarnings(
    readr::read_csv(p, col_types = readr::cols(.default = readr::col_character()),
                    progress = FALSE)
  )
}

.write_tbl <- function(name, df) {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
  readr::write_csv(df, tbl_path(name), na = "")
  invisible(TRUE)
}

#' Read a whole table (cached).
#'
#' Inside a reactive context this also takes a dependency on the store version,
#' so a screen re-renders when anything is written. Outside one — tests, the
#' seed, startup — it is an ordinary read.
store_get <- function(name) {
  if (!is.null(.store_version) && !is.null(shiny::getDefaultReactiveDomain())) {
    .store_version()
  }
  if (is.null(.store[[name]])) .store[[name]] <- .read_tbl(name)
  .store[[name]]
}

#' Replace a whole table, in cache and on disk.
store_set <- function(name, df) {
  .store[[name]] <- df
  .write_tbl(name, df)
  .store_bump()
  invisible(df)
}

#' Drop the cache so the next read comes off disk.
#'
#' Needed because the deployed app may have its data files replaced underneath
#' it (a git pull, a re-seed), and because tests want a clean slate.
store_refresh <- function(name = NULL) {
  if (is.null(name)) {
    rm(list = ls(.store), envir = .store)
  } else {
    if (!is.null(.store[[name]])) rm(list = name, envir = .store)
  }
  # Dropping the cache changes what every reader will see next, so it counts as
  # a mutation for reactivity purposes.
  .store_bump()
  invisible(TRUE)
}

#' Append one row supplied as a named list.
#'
#' Columns absent from `values` are filled with "" rather than NA so the CSV
#' round-trips to the same shape — readr reads an empty field back as NA for
#' some columns and "" for others depending on what else is in the column,
#' and that inconsistency leaks into every downstream filter.
store_insert <- function(name, values) {
  df <- store_get(name)
  row <- tibble::as_tibble_row(lapply(values, function(v) as.character(v %||% "")))
  if (nrow(df)) {
    missing_cols <- setdiff(names(df), names(row))
    for (m in missing_cols) row[[m]] <- ""
    extra_cols <- setdiff(names(row), names(df))
    for (e in extra_cols) df[[e]] <- ""
    row <- row[, names(df), drop = FALSE]
  }
  store_set(name, bind_rows(df, row))
}

#' Update rows matching `where` (a named list of equality tests).
store_update <- function(name, where, values) {
  df <- store_get(name)
  if (!nrow(df)) return(invisible(df))
  hit <- rep(TRUE, nrow(df))
  for (k in names(where)) {
    if (!k %in% names(df)) return(invisible(df))
    hit <- hit & (df[[k]] == as.character(where[[k]]))
  }
  hit[is.na(hit)] <- FALSE
  if (!any(hit)) return(invisible(df))
  for (k in names(values)) {
    if (!k %in% names(df)) df[[k]] <- ""
    df[[k]][hit] <- as.character(values[[k]] %||% "")
  }
  store_set(name, df)
}

#' Delete rows matching `where`.
store_delete <- function(name, where) {
  df <- store_get(name)
  if (!nrow(df)) return(invisible(df))
  hit <- rep(TRUE, nrow(df))
  for (k in names(where)) hit <- hit & (df[[k]] == as.character(where[[k]]))
  hit[is.na(hit)] <- FALSE
  store_set(name, df[!hit, , drop = FALSE])
}

# ------------------------------------------------------------------
# Typed accessors
#
# Tables are stored as all-character (see .read_tbl). These wrappers coerce the
# numeric/date columns a screen actually needs, so modules can do arithmetic
# without scattering as.numeric() through every pipeline.
# ------------------------------------------------------------------

as_num <- function(x) suppressWarnings(as.numeric(x))
as_dt  <- function(x) suppressWarnings(as.POSIXct(x, tz = "Asia/Kolkata"))
as_dte <- function(x) suppressWarnings(as.Date(x))

#' Read a table with named columns coerced to numeric / Date / datetime.
#'
#' Coercion runs even when the table has no rows. Returning early on an empty
#' frame left declared-numeric columns as character, and an empty table is a
#' perfectly ordinary state — nothing settled yet, or everything paid off — so
#' the first `sum()` downstream failed with "invalid 'type' (character) of
#' argument". An empty numeric column costs nothing; an empty character column
#' pretending to be numeric costs a crashed screen.
store_typed <- function(name, num = NULL, date = NULL, datetime = NULL) {
  df <- store_get(name)
  for (c in intersect(num, names(df)))      df[[c]] <- as_num(df[[c]])
  for (c in intersect(date, names(df)))     df[[c]] <- as_dte(df[[c]])
  for (c in intersect(datetime, names(df))) df[[c]] <- as_dt(df[[c]])
  df
}

# Convenience readers for the tables whose numeric columns are used on more than
# one screen. Defined once here so a column rename does not have to be chased
# through every module that happens to need the money fields.
get_bookings <- function() {
  store_typed("bookings",
              num  = c("weight_t", "quantity", "freight", "gst_pct",
                       "gst_amount", "insurance_amt", "declared_value", "total"),
              date = "booking_date")
}

get_consignments <- function() {
  store_typed("consignments",
              num  = c("weight_t", "freight"),
              date = c("dispatch_date", "expected_delivery", "delivered_date"))
}

get_trips <- function() {
  store_typed("trips",
              num      = c("distance_km", "border_allowance", "food_allowance", "advance"),
              datetime = c("dispatch_dt", "eta"))
}

get_vehicles <- function() {
  store_typed("vehicles",
              num  = "capacity_t",
              date = c("insurance_expiry", "fitness_expiry",
                       "permit_expiry", "puc_expiry"))
}

get_drivers <- function() {
  store_typed("drivers",
              num  = c("trips_lifetime", "on_time_pct"),
              date = "licence_expiry")
}

get_employees <- function() {
  store_typed("employees", num = "salary", date = "joined")
}

get_invoices <- function() {
  store_typed("invoices",
              num  = c("amount", "gst_pct", "gst_amount", "total", "paid_amount"),
              date = c("invoice_date", "due_date"))
}

get_payments <- function() {
  store_typed("payments", num = "amount", date = "date")
}

get_vendor_payments <- function() {
  store_typed("vendor_payments", num = "amount", date = "due_date")
}

get_clients <- function() {
  store_typed("clients", num = c("credit_limit", "gst_pct"), date = "since")
}

get_ewaybills <- function() {
  store_typed("ewaybills", datetime = c("valid_from", "valid_to"))
}

get_complaints <- function() {
  store_typed("complaints",
              num  = "progress_pct",
              date = c("raised_dt", "target_dt", "resolved_dt"))
}

get_gps <- function() {
  store_typed("gps_pings", num = c("lat", "lon", "speed"), datetime = "ts")
}

get_payroll <- function() {
  store_typed("payroll",
              num = c("basic", "hra", "da", "incentive", "ot", "trip_allowance",
                      "advances", "pf", "esi", "pt", "net"))
}

get_pods <- function() store_typed("pods", datetime = "upload_dt")

get_attendance <- function() store_typed("attendance", date = "date")

# ------------------------------------------------------------------
# Settings — stored as a key/value table rather than columns so the Settings
# screen can add a preference without a schema migration.
# ------------------------------------------------------------------

setting <- function(key, default = "") {
  s <- store_get("settings")
  if (!nrow(s) || !"key" %in% names(s)) return(default)
  v <- s$value[s$key == key]
  if (!length(v)) default else (v[1] %||% default)
}

setting_set <- function(key, value) {
  s <- store_get("settings")
  if (nrow(s) && key %in% s$key) {
    store_update("settings", list(key = key), list(value = value))
  } else {
    store_insert("settings", list(key = key, value = value))
  }
}

# ------------------------------------------------------------------
# Audit log
#
# The Security & Roles screen advertises an audit log, and Settings claims
# "Changes are audit-logged with user + timestamp". Every mutating action in the
# app routes through here so that claim is actually true.
# ------------------------------------------------------------------
audit <- function(user_id, action, module, detail = "") {
  store_insert("audit_log", list(
    ts      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    user_id = user_id %||% "system",
    action  = action,
    module  = module,
    detail  = detail
  ))
}
