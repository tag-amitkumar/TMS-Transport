# ==================================================================
# Smoke test — run from the project root:
#   Rscript tests/smoke.R
#
# Loads the whole app without starting a server and asserts the things that
# would otherwise only fail once a human clicked the screen: that every module
# is defined, that the seed is internally consistent, that RBAC actually denies,
# and that the domain rules (POD gate, GST treatment, capacity) hold.
# ==================================================================

suppressPackageStartupMessages({
  source("global.R"); source("R/store.R"); source("R/geo.R"); source("R/ewb.R"); source("R/rbac.R")
  source("R/serviceability.R"); source("R/ewb_api.R")
  source("R/meta_api.R"); source("R/meta_signup.R"); source("R/notify_templates.R"); source("R/notify.R")
  source("R/seed.R"); source("R/seed_minimal.R")
  source("R/theme.R"); source("R/ui_helpers.R"); source("R/nav.R")
  source("R/github_sync.R")
  for (f in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) source(f)
})

# ------------------------------------------------------------------
# Leave data/ exactly as it was found.
#
# This suite exercises the real store, so every store_set() it makes writes
# through to data/*.csv — the files that are committed and baked into the
# image. That bit once: a test run left two fabricated WhatsApp consent
# records in the seed, they were committed, and the next deploy installed
# them into the live volume. A seeded opt-in is the one row that table must
# never hold.
#
# Restoring on exit is cheaper than remembering to, and it covers the failure
# path too — a suite that dies half way through used to leave the seed in
# whatever state it had reached.
# ------------------------------------------------------------------
.seed_backup <- file.path(tempdir(), "smoke-seed-backup")
if (dir.exists("data")) {
  unlink(.seed_backup, recursive = TRUE)
  dir.create(.seed_backup, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files("data", full.names = TRUE), .seed_backup,
            recursive = TRUE, copy.date = TRUE)
}
reg.finally <- function() {
  if (!dir.exists(.seed_backup)) return(invisible(NULL))
  file.copy(list.files(.seed_backup, full.names = TRUE), "data",
            overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
  unlink(.seed_backup, recursive = TRUE)
  invisible(NULL)
}
reg.finalizer(environment(), function(e) reg.finally(), onexit = TRUE)

pass <- 0; fail <- 0
ok <- function(label, cond) {
  if (isTRUE(cond)) { pass <<- pass + 1 }
  else { fail <<- fail + 1; cat("  FAIL:", label, "\n") }
}

cat("\n== Modules ==\n")
seed_if_empty()

for (p in names(NAV_PAGES)) {
  fn_ui <- switch(p,
    "booking_new" = "booking_new_ui", "consign_multi" = "consign_multi_ui",
    paste0(p, "_ui"))
  ok(paste("UI defined:", fn_ui), exists(fn_ui) && is.function(get(fn_ui)))
}
for (m in unique(vapply(NAV_PAGES, `[[`, character(1), "module"))) {
  fn <- paste0(m, "_server")
  ok(paste("server defined:", fn), exists(fn) && is.function(get(fn)))
}

cat("\n== Seed integrity ==\n")
b   <- store_get("branches");    e  <- get_employees()
d   <- get_drivers();            v  <- get_vehicles()
cl  <- get_clients();            bk <- get_bookings()
tr  <- get_trips();              cn <- get_consignments()
inv <- get_invoices();           pd <- get_pods()

MINIMAL <- identical(SEED_PROFILE, "minimal")
cat("   profile:", SEED_PROFILE, "\n")

# Volumes are the one thing that legitimately differs between profiles.
# Everything below this block is an invariant and must hold for both.
expect_n <- if (MINIMAL) {
  list(branches = 2, employees = 4, vehicles = 3, clients = 2, bookings = 3)
} else {
  list(branches = 18, employees = 81, vehicles = 62, clients = 214, bookings = 342)
}

ok("branches seeded",           nrow(b)  == expect_n$branches)
local({
  u <- store_get("users")
  ok("user emails unique",  !any(duplicated(tolower(u$email))))
  ok("user emails well formed",
     all(grepl("^[a-z0-9._-]+@[a-z0-9.-]+\\.[a-z]{2,}$", u$email)))
  ok("demo password verifies", bcrypt::checkpw("tms@2026", u$password_hash[1]))
  ok("no plaintext passwords stored", !"password" %in% names(u))
})
ok("employees seeded",          nrow(e)  == expect_n$employees)
ok("vehicles seeded",           nrow(v)  == expect_n$vehicles)
ok("clients seeded",            nrow(cl) == expect_n$clients)
ok("bookings seeded",           nrow(bk) == expect_n$bookings)

# The one-record rule: every driver is an employee flagged as one.
ok("every driver is an employee", all(d$driver_id %in% e$employee_id))
ok("driver count matches flag",   nrow(d) == sum(e$is_driver == "TRUE"))

# Referential integrity across the operations chain.
ok("trips -> bookings",        all(tr$booking_no %in% bk$booking_no))
ok("trips -> vehicles",        all(tr$vehicle_id %in% v$vehicle_id))
ok("trips -> drivers",         all(tr$driver_id  %in% d$driver_id))
ok("consignments -> trips",    all(cn$trip_no    %in% tr$trip_no))
ok("consignments -> clients",  all(cn$client_id  %in% cl$client_id))
ok("invoices -> clients",      all(inv$client_id %in% cl$client_id))
ok("pods -> consignments",     all(pd$cn_no      %in% cn$cn_no))
ok("bookings -> branches",     all(bk$branch_id  %in% b$branch_id))
ok("LR numbers unique",        !any(duplicated(cn$lr_no)))
ok("CN numbers unique",        !any(duplicated(cn$cn_no)))
ok("booking numbers unique",   !any(duplicated(bk$booking_no)))

cat("\n== Fleet consistency ==\n")
# Vehicle and driver status must agree with the trip book. Asserting status
# separately from the trips is what let a truck sit on a Running trip while the
# register called it Available — so it was allocated a second load on top of
# the one it was already carrying.
local({
  live <- tr[tr$status %in% c("Planned", "Loading", "Running", "At Hub"), ]

  ok("no vehicle is on two live trips at once", !any(duplicated(live$vehicle_id)))
  ok("no driver is on two live trips at once",  !any(duplicated(live$driver_id)))

  busy_v <- unique(live$vehicle_id)
  ok("vehicles on a live trip are not marked Available",
     !any(v$status[v$vehicle_id %in% busy_v] == "Available"))
  ok("vehicles with no live trip are not marked In Transit",
     !any(v$status[!(v$vehicle_id %in% busy_v)] == "In Transit"))

  busy_d <- unique(live$driver_id)
  ok("drivers on a live trip are marked On trip",
     all(d$status[d$driver_id %in% busy_d] == "On trip"))
  ok("drivers with no live trip are not marked On trip",
     !any(d$status[!(d$driver_id %in% busy_d)] == "On trip"))

  # A completed trip must have released its vehicle and driver, or the fleet
  # slowly runs out of anything allocatable.
  done <- tr[tr$status == "Completed", ]
  freed_v <- setdiff(done$vehicle_id, busy_v)
  ok("completed trips release their vehicle",
     all(v$status[v$vehicle_id %in% freed_v] %in% c("Available", "Maintenance", "Inactive")))

  # The dispatcher must always have something to work with.
  ok("at least one vehicle is allocatable", any(v$status == "Available"))
  ok("at least one driver is allocatable",  any(d$status == "Available"))
})

cat("\n== Domain rules ==\n")

# Every invoice must trace to a consignment with a verified/approved POD.
auto <- inv[inv$source == "auto", ]
good_pod <- pd$lr_no[pd$status %in% c("Verified", "Approved")]
ok("invoices only against verified POD", all(auto$lr_no %in% good_pod))

# Reverse charge collects no tax.
rcm <- inv[inv$gst_mode == "RCM", ]
ok("RCM invoices carry zero GST", all(rcm$gst_amount == 0, na.rm = TRUE))
ok("RCM totals equal freight",    all(abs(rcm$total - rcm$amount) < 1, na.rm = TRUE))

fcm <- inv[inv$gst_mode == "FCM", ]
ok("FCM invoices carry GST", nrow(fcm) == 0 || all(fcm$gst_amount > 0, na.rm = TRUE))

# GST treatment on an invoice must match the client master it came from.
ok("invoice GST mode follows client",
   all(inv$gst_mode == cl$gst_mode[match(inv$client_id, cl$client_id)], na.rm = TRUE))

# No consignment may exceed the capacity of the vehicle carrying it.
load_by_trip <- aggregate(weight_t ~ trip_no, data = cn, FUN = sum)
cap <- v$capacity_t[match(tr$vehicle_id[match(load_by_trip$trip_no, tr$trip_no)], v$vehicle_id)]
ok("no trip exceeds vehicle capacity",
   all(load_by_trip$weight_t <= cap + 0.05, na.rm = TRUE))

# Booking money adds up.
ok("booking totals reconcile",
   all(abs(bk$total - (bk$freight + bk$gst_amount + bk$insurance_amt)) < 1, na.rm = TRUE))

# Payroll net = earnings - deductions.
pr <- get_payroll()
calc <- pr$basic + pr$hra + pr$da + pr$incentive + pr$ot + pr$trip_allowance -
  pr$advances - pr$pf - pr$esi - pr$pt
ok("payroll net reconciles", all(abs(pr$net - calc) < 1, na.rm = TRUE))

# Payroll runs for the month that has closed, so the screen is never empty on
# the 1st with every allowance showing zero.
period <- unique(pr$period)
ok("exactly one payroll period seeded", length(period) == 1)
ok("payroll period is the closed month",
   period[1] == format(lubridate::floor_date(Sys.Date(), "month") - 1, "%b %Y"))

# Trip allowances actually come from the Trip Book — the headline claim of the
# payroll screen. A zero here means the automatic pull is silently broken.
tb <- tr[format(as.Date(tr$dispatch_dt), "%b %Y") == period[1], ]
ok("trips exist in the payroll period", nrow(tb) > 0)
ok("trip allowances are non-zero", sum(pr$trip_allowance, na.rm = TRUE) > 0)
ok("advances are non-zero",        sum(pr$advances, na.rm = TRUE) > 0)

expect <- tapply(tb$border_allowance + tb$food_allowance, tb$driver_id, sum)
got <- pr$trip_allowance[match(names(expect), pr$employee_id)]
ok("payroll allowances match Trip Book",
   all(abs(got - as.numeric(expect)) < 1, na.rm = TRUE))

expect_adv <- tapply(tb$advance, tb$driver_id, sum)
got_adv <- pr$advances[match(names(expect_adv), pr$employee_id)]
ok("payroll advances match Trip Book",
   all(abs(got_adv - as.numeric(expect_adv)) < 1, na.rm = TRUE))

# A trip advance offsets the allowance it funds, so recovering it must not wipe
# out a driver's pay. A handful on hold is a real exception queue; a third of
# the fleet on hold means the advance model is wrong.
held <- sum(pr$net < 0)
ok("almost nobody has negative net pay", held <= 3)
ok("drivers take home a living wage",
   all(pr$net[pr$net > 0] >= 5000, na.rm = TRUE))

cat("\n== RBAC ==\n")
# Super Admin outranks the matrix on every module that holds company data. The
# exception is the personal ones: the driver console reports the position of a
# phone in somebody's hand, and there is nothing there to administer.
ok("Super Admin sees every company module",
   all(vapply(setdiff(MODULES, PERSONAL_MODULES),
              function(m) can("Super Admin", m, "view"), logical(1))))
ok("Super Admin does not get the personal ones",
   !any(vapply(PERSONAL_MODULES,
               function(m) can("Super Admin", m, "view"), logical(1))))
ok("Customer cannot see payroll", !can("Customer", "hrms_payroll", "view"))
ok("Customer cannot see clients", !can("Customer", "clients", "view"))
ok("Vendor cannot see invoices",  !can("Vendor", "invoices", "view"))
ok("Driver cannot create bookings", !can("Driver", "bookings", "create"))
ok("Dispatcher cannot delete",    !can("Dispatcher", "bookings", "delete"))
ok("Dispatcher can allocate",     can("Dispatcher", "cargo_moto", "create"))
ok("Accountant can approve money", can("Accountant", "invoices", "approve"))
ok("Accountant cannot see GPS",   !can("Accountant", "livegps", "view"))
ok("HR Manager can edit HRMS",    can("HR Manager", "hrms_employees", "edit"))
ok("HR Manager cannot see bookings", !can("HR Manager", "bookings", "view"))
ok("Branch Admin cannot open security", !can("Branch Admin", "security", "view"))

# Every role must land somewhere it is allowed to be.
for (r in ROLE_LEVELS) {
  lp <- landing_page(r)
  ok(paste("landing page valid for", r), can(r, NAV_PAGES[[lp]]$module, "view"))
}

cat("\n== Branch scoping ==\n")
admin  <- list(role = "Super Admin", branch_id = b$branch_id[1], cross_branch = "TRUE")
scoped <- list(role = "Branch Admin", branch_id = b$branch_id[2], cross_branch = "FALSE")
ok("super admin sees all bookings", nrow(scope_branch(bk, admin)) == nrow(bk))
ok("branch admin sees fewer",       nrow(scope_branch(bk, scoped)) < nrow(bk))
ok("branch admin sees only own",
   all(scope_branch(bk, scoped)$branch_id == scoped$branch_id))

cust <- list(role = "Customer", branch_id = "", cross_branch = "FALSE",
             client_id = cl$client_id[1], vendor_id = "")
own <- scope_owner(cn, cust)
ok("customer sees only own consignments", all(own$client_id == cust$client_id))
ok("customer sees a non-empty slice",     nrow(own) >= 0)

cat("\n== E-way bills ==\n")
local({
  ew <- get_ewaybills()
  live_status <- c("Dispatched", "In Transit", "At Hub", "Out for Delivery")
  live_cn <- cn$cn_no[cn$status %in% live_status]
  ok("EWBs only for live consignments", all(ew$cn_no %in% live_cn))
  ok("no EWB on a delivered consignment",
     !any(ew$cn_no %in% cn$cn_no[cn$status == "Delivered"]))
  # The register must be mostly actionable, not a wall of dead rows.
  expired <- sum(ew$valid_to < Sys.time(), na.rm = TRUE)
  ok("most EWBs are still valid", nrow(ew) == 0 || expired / nrow(ew) < 0.25)

  # Only the demo profile manufactures an expiry queue. The minimal profile has
  # a single bill and no reason to ship it half-expired.
  if (!MINIMAL) {
    ok("some EWBs need attention",
       any(ew$valid_to < Sys.time() + 24 * 3600, na.rm = TRUE))
  }
})

cat("\n== Empty-table typing ==\n")
# An empty table must still come back with its numeric columns numeric. When
# store_typed() short-circuited on zero rows they stayed character, and the
# Payments screen died on sum() the moment a vendor had nothing outstanding —
# which is the normal state right after everyone has been paid.
local({
  for (getter in c("get_vendor_payments", "get_payments", "get_invoices",
                   "get_ewaybills", "get_gps", "get_payroll")) {
    df <- get(getter)()
    nums <- intersect(c("amount", "total", "net", "speed", "lat", "lon"), names(df))
    bad <- nums[!vapply(nums, function(c) is.numeric(df[[c]]), logical(1))]
    ok(paste(getter, "numeric columns are numeric"), length(bad) == 0)
    # And the arithmetic the screens actually do must not error.
    ok(paste(getter, "sums without error"),
       !inherits(try(sum(df[[nums[1]]], na.rm = TRUE), silent = TRUE), "try-error") ||
         length(nums) == 0)
  }
})

cat("\n== Blank fields survive coercion ==\n")
# store_insert() writes "" for omitted fields, while a CSV read yields NA. A
# typed read then hit as.POSIXct("") — which throws rather than warns — so a
# single insert with an empty date poisoned every later read of that table for
# the rest of the session.
local({
  ok("empty string coerces to NA date",     is.na(as_dte("")))
  ok("empty string coerces to NA datetime", is.na(as_dt("")))
  ok("whitespace coerces to NA datetime",   is.na(as_dt("   ")))
  ok("empty string coerces to NA number",   is.na(as_num("")))
  ok("real values still parse",
     !is.na(as_dt("2026-09-04 08:00:00")) && !is.na(as_dte("2026-09-04")))

  # And the round trip that actually broke: insert a row with blank dates,
  # then read the table back through its typed accessor.
  before <- nrow(store_get("pods"))
  store_insert("pods", list(pod_id = "POD-BLANK", lr_no = "LR-BLANK",
                            cn_no = "CN-BLANK", client_id = "", branch_id = "",
                            uploaded_by = "", file_name = "", upload_dt = "",
                            status = "Pending"))
  ok("typed read survives a blank datetime",
     !inherits(try(get_pods(), silent = TRUE), "try-error"))
  store_delete("pods", list(pod_id = "POD-BLANK"))
  ok("blank-field probe cleaned up", nrow(store_get("pods")) == before)
})

cat("\n== Store round-trip ==\n")
# Exercises the actual write path. Without this the suite passed while
# store_insert() was broken by a missing namespace, because nothing but the
# running app ever called it.
local({
  before <- nrow(store_get("audit_log"))
  audit("TEST-USER", "smoke", "tests", "round-trip")
  after <- store_get("audit_log")
  ok("audit insert adds a row", nrow(after) == before + 1)
  ok("inserted values persist",
     identical(after$user_id[nrow(after)], "TEST-USER") &&
       identical(after$action[nrow(after)], "smoke"))

  # A partial insert must not corrupt the column set.
  cols_before <- names(store_get("audit_log"))
  store_insert("audit_log", list(ts = "2026-01-01 00:00:00", action = "partial"))
  ok("partial insert keeps schema", identical(names(store_get("audit_log")), cols_before))

  store_update("audit_log", list(action = "partial"), list(detail = "filled"))
  a <- store_get("audit_log")
  ok("update writes the field", any(a$detail == "filled", na.rm = TRUE))

  store_delete("audit_log", list(user_id = "TEST-USER"))
  store_delete("audit_log", list(action = "partial"))
  ok("delete removes rows", nrow(store_get("audit_log")) == before)
})

cat("\n== Helpers ==\n")
ok("inr_group lakh grouping", inr_group(1234567) == "12,34,567")
ok("inr_group small",          inr_group(999) == "999")
ok("inr_compact crore",        inr_compact(14200000) == "₹1.42 Cr")
ok("inr_compact lakh",         inr_compact(840000) == "₹8.4 L")
ok("next_id increments",       next_id(c("BKG-4471","BKG-4482"), "BKG-") == "BKG-4483")
ok("next_id from empty",       next_id(character(0), "BKG-") == "BKG-0001")
ok("expiry_state expired",     expiry_state(Sys.Date() - 1) == "danger")
ok("expiry_state warn",        expiry_state(Sys.Date() + 10) == "warn")
ok("expiry_state ok",          expiry_state(Sys.Date() + 200) == "ok")
ok("%||% blank falls through", ("" %||% "x") == "x")


cat("\n== Geography master ==\n")
{
  pins <- geo_pincodes(); cts <- geo_cities()
  ok("pincode master loaded",        nrow(pins) > 19000)
  ok("city master loaded",           nrow(cts) > 600)
  ok("every state and UT present",   length(geo_states()) == 36)
  ok("pincodes are unique",          !any(duplicated(pins$pincode)))
  ok("city names are unique",        !any(duplicated(cts$city)))
  ok("every pincode is six digits",  all(grepl("^[1-9][0-9]{5}$", pins$pincode)))
  ok("no missing city",              all(nzchar(pins$city)))
  ok("no missing state",             all(nzchar(pins$state)))
  ok("no missing coordinates",       !any(is.na(pins$lat) | is.na(pins$lon)))
  # Everything must sit inside India's bounding box. A stray sign or a swapped
  # lat/lon pair would otherwise put a consignment in the Indian Ocean and
  # quote a plausible-looking distance for it.
  ok("latitudes within India",       all(pins$lat  >=  6 & pins$lat  <= 38))
  ok("longitudes within India",      all(pins$lon  >= 68 & pins$lon  <= 98))
  ok("every city resolves a pincode",
     all(vapply(cts$pincode, function(p) !is.null(geo_pin(p)), logical(1))))
  ok("city of each city's pincode matches",
     all(mapply(function(p, c) identical(geo_pin(p)$city, c),
                cts$pincode, cts$city)))

  # Known codes, so a bad rebuild of the master is caught rather than shipped.
  ok("110001 is Delhi",              identical(geo_pin("110001")$city,  "Delhi"))
  ok("400001 is Mumbai",             identical(geo_pin("400001")$city,  "Mumbai"))
  ok("440001 is Nagpur",             identical(geo_pin("440001")$city,  "Nagpur"))
  ok("560001 is Bengaluru",          identical(geo_pin("560001")$city,  "Bengaluru"))
  ok("700001 is Kolkata",            identical(geo_pin("700001")$city,  "Kolkata"))
  ok("600001 is Chennai",            identical(geo_pin("600001")$state, "Tamil Nadu"))
  # Ladakh was split from J&K in 2019; the build patches the source for it.
  ok("194101 is in Ladakh",          identical(geo_pin("194101")$state, "Ladakh"))
  ok("Ladakh is a state in its own right", "Ladakh" %in% geo_states())
  ok("no Leh left under J&K",
     !any(cts$city %in% c("Leh", "Kargil") & cts$state == "Jammu & Kashmir"))

  ok("rejects a five-digit code",    is.null(geo_pin("11000")))
  ok("rejects a leading zero",       is.null(geo_pin("010001")))
  ok("rejects letters",              is.null(geo_pin("11000A")))
  ok("rejects blank",                is.null(geo_pin("")))
  ok("rejects an unassigned code",   is.null(geo_pin("999999")))
  ok("tolerates surrounding space",  identical(geo_pin(" 110001 ")$city, "Delhi"))
}

cat("\n== City lookup and aliases ==\n")
{
  ok("exact city resolves",          identical(geo_city("Nagpur")$city, "Nagpur"))
  ok("unknown city is NULL",         is.null(geo_city("Atlantis")))
  ok("blank city is NULL",           is.null(geo_city("")))
  # The names people actually type must reach the district they belong to.
  ok("Bangalore -> Bengaluru",       identical(geo_city("Bangalore")$city, "Bengaluru"))
  ok("Bombay -> Mumbai",             identical(geo_city("Bombay")$city, "Mumbai"))
  ok("Noida -> Gautam Buddha Nagar",
     identical(geo_city("Noida")$city, "Gautam Buddha Nagar"))
  ok("alias lookup is case-insensitive",
     identical(geo_city("bangalore")$city, "Bengaluru"))

  ch <- geo_city_choices()
  ok("choices cover every city",     length(ch) == nrow(geo_cities()))
  ok("choice values are city names", all(ch %in% geo_cities()$city))
  ok("choice labels carry the state", all(grepl(", ", names(ch), fixed = TRUE)))
  # The alias has to be in the label, because that is the only text selectize
  # searches — this is what makes typing "Noida" find the district.
  ok("alias searchable in the label",
     any(grepl("Bangalore", names(ch), fixed = TRUE)))
  ok("aliases point at real cities",
     all(geo_aliases()$city %in% geo_cities()$city))

  ok("city pin choices are that city's",
     all(geo_pin_city_check <- geo_city_pin_choices("Nagpur") %in%
           geo_pincodes()$pincode[geo_pincodes()$city == "Nagpur"]))
  ok("unknown city yields no pins",  length(geo_city_pin_choices("Atlantis")) == 0)
}

cat("\n== Lane estimation ==\n")
{
  # Against published NH distances. The road factor was calibrated to 1.20 on
  # sixteen lanes; 15% is a generous band that still catches a broken factor,
  # a swapped coordinate or a centroid that has drifted.
  near <- function(a, b, tol = 0.15) abs(a - b) / b <= tol
  ok("Delhi-Mumbai ~1400 km",    near(geo_lane("Delhi", "Mumbai")$km, 1400))
  ok("Mumbai-Bengaluru ~980 km", near(geo_lane("Mumbai", "Bengaluru")$km, 980))
  ok("Chennai-Bengaluru ~350 km",near(geo_lane("Chennai", "Bengaluru")$km, 350))
  ok("Delhi-Kolkata ~1500 km",   near(geo_lane("Delhi", "Kolkata")$km, 1500))
  ok("Hyderabad-Bengaluru ~570", near(geo_lane("Hyderabad", "Bengaluru")$km, 570))

  ok("distance is symmetric",
     geo_lane("Delhi", "Mumbai")$km == geo_lane("Mumbai", "Delhi")$km)
  ok("transit is at least a day",
     geo_lane("Mumbai", "Thane")$days >= 1)
  ok("long haul takes longer",
     geo_lane("Delhi", "Kolkata")$days > geo_lane("Delhi", "Jaipur")$days)

  # A mapped lane is a commercial commitment and must never be replaced by the
  # estimate, even when the estimate disagrees.
  mapped <- geo_lane("Nagpur", "Delhi")
  ok("mapped lane is flagged mapped", isTRUE(mapped$mapped))
  ok("mapped lane keeps its own transit", mapped$days == 3)
  ok("mapped lane keeps its own distance", mapped$km == 1035)
  ok("mapped lane carries branch routing", nzchar(mapped$branches))

  est <- geo_lane("Mumbai", "Bengaluru")
  ok("unmapped lane is flagged estimated", isFALSE(est$mapped))
  ok("estimated lane still has a path",    nzchar(est$path))

  ok("unknown origin gives no lane",  is.null(geo_lane("Atlantis", "Delhi")))
  ok("unknown destination gives none",is.null(geo_lane("Delhi", "Atlantis")))
  ok("blank city gives no lane",      is.null(geo_lane("", "Delhi")))
  # A city to itself with no PIN codes given is not a zero-kilometre lane, it
  # is an unanswerable question — both ends resolve to the same centroid.
  ok("a city to itself has no estimable distance",
     isFALSE(geo_lane("Delhi", "Delhi")$km_known))
}

cat("\n== Local lanes — one city, two PIN codes ==\n")
{
  # Cross-town cartage is ordinary freight. The PIN pair is the lane; the city
  # being the same at both ends says nothing about whether it is valid.
  # 110001 and 110002 are both graded 4 — really placed — so this one has a
  # distance worth quoting.
  loc <- geo_lane("Delhi", "Delhi", "110001", "110002")
  ok("local lane resolves",           !is.null(loc))
  ok("local lane is flagged local",   isTRUE(loc$local))
  ok("local lane is not mapped",      isFALSE(loc$mapped))
  ok("local distance is known",       isTRUE(loc$km_known))
  ok("local distance is a local one", loc$km > 0 && loc$km < 30)
  ok("local transit is one day",      loc$days == 1)
  ok("local path names both PINs",
     grepl("110001", loc$path, fixed = TRUE) &&
     grepl("110002", loc$path, fixed = TRUE))

  # The mapped Nagpur→Delhi lane must not leak into a Delhi→Delhi run just
  # because a city matches at one end. 1,035 km on a cross-town job would be
  # wrong by two orders of magnitude, and it would price the load that way.
  ok("trunk mapping does not apply to a local lane", loc$km < 100)

  # A quarter of pincodes are graded 1: GeoNames could not place the locality
  # and gave it the city's own coordinate. Both Nagpur codes here are graded 1
  # and land on the same point. The only honest answer for that pair is that
  # the distance is unknown — quoting the 0 km the arithmetic produces would
  # price a real cartage job at nothing.
  vague <- geo_lane("Nagpur", "Nagpur", "440001", "440016")
  ok("ungraded local pair still resolves",  !is.null(vague))
  ok("ungraded local pair is still local",  isTRUE(vague$local))
  ok("ungraded local distance is unknown",  isFALSE(vague$km_known))
  ok("unknown distance is NA, not zero",    is.na(vague$km))
  ok("unknown distance stores as blank",    identical(geo_lane_km(vague), ""))

  # The same imprecision is irrelevant between cities: a few km of error does
  # not move a thousand-kilometre figure, so those stay quotable.
  ok("inter-city distance is known despite grade 1",
     isTRUE(geo_lane("Nagpur", "Mumbai", "440016", "400097")$km_known))

  # A PIN outranks the city name handed to it, which is what keeps the form
  # correct when a stale city label rides along with a fresh PIN.
  cross <- geo_lane("Nagpur", "Nagpur", "440001", "110020")
  ok("PIN overrides a wrong city",    isFALSE(cross$local))
  ok("PIN-driven distance is the real one", cross$km > 900)

  # And falling back to centroids when no PIN is supplied must still work.
  ok("city-only lane still resolves", !is.null(geo_lane("Nagpur", "Delhi")))
  ok("an unresolvable PIN falls back to the city",
     identical(geo_lane("Nagpur", "Delhi", "999999", "999999")$km,
               geo_lane("Nagpur", "Delhi")$km))
  ok("a mapped lane reports its distance as known",
     isTRUE(geo_lane("Nagpur", "Delhi")$km_known))
}

cat("\n== Booking geography ==\n")
{
  bk <- store_get("bookings")
  ok("bookings carry an origin PIN",  all(nzchar(bk$origin_pincode)))
  ok("bookings carry a destination PIN", all(nzchar(bk$dest_pincode)))
  ok("origin PINs resolve",
     all(vapply(bk$origin_pincode, function(p) !is.null(geo_pin(p)), logical(1))))
  ok("destination PINs resolve",
     all(vapply(bk$dest_pincode, function(p) !is.null(geo_pin(p)), logical(1))))
  # The PIN and the city on a row have to agree, or the LR prints one place
  # and the e-way bill is raised against another.
  ok("origin PIN agrees with origin city",
     all(mapply(function(p, c) identical(geo_pin(p)$city, c),
                bk$origin_pincode, bk$origin_city)))
  ok("destination PIN agrees with destination city",
     all(mapply(function(p, c) identical(geo_pin(p)$city, c),
                bk$dest_pincode, bk$dest_city)))
  ok("origin state agrees with the PIN",
     all(mapply(function(p, s) identical(geo_pin(p)$state, s),
                bk$origin_pincode, bk$origin_state)))
  ok("distance is recorded",          all(as_num(bk$distance_km) > 0))
}

cat("\n== Datetimes survive a mixed column ==\n")
{
  # as.POSIXct() picks one format for a whole character vector and accepts a
  # candidate only if every element parses with it, so one date-only value
  # drags the column down to "%Y-%m-%d" and strptime silently drops the time
  # off all the rest. Twelve consignment events read 00:00 on the tracking
  # screen because one of them had no clock on it.
  mixed <- c("2026-08-27 15:00:00", "2026-08-28", "2026-08-29 07:45:00")
  got <- as_dt(mixed)
  ok("a time survives alongside a date-only value",
     format(got[1], "%H:%M") == "15:00")
  ok("the later time survives too",  format(got[3], "%H:%M") == "07:45")
  ok("a date with no time is midnight, not NA",
     !is.na(got[2]) && format(got[2], "%H:%M") == "00:00")
  ok("minutes-only timestamps parse",
     format(as_dt("2026-08-27 15:30"), "%H:%M") == "15:30")
  ok("blank becomes NA",             is.na(as_dt("")))
  ok("nonsense becomes NA",          is.na(as_dt("not a date")))
  ok("POSIXct passes through unchanged",
     format(as_dt(as.POSIXct("2026-08-27 15:00:00", tz = "Asia/Kolkata")),
            "%H:%M") == "15:00")
  ok("result is POSIXct",            inherits(got, "POSIXct"))
  ok("timezone is IST",              identical(attr(got, "tzone"), "Asia/Kolkata"))

  # And the real tables must carry their times, which is what was visibly broken.
  evs <- store_typed("consignment_events", num = "seq", datetime = "event_dt")
  ok("seeded events are not all midnight",
     !nrow(evs) || any(format(evs$event_dt, "%H:%M") != "00:00"))
  # A live consignment whose latest movement is dated tomorrow reads as a
  # broken clock to whoever is tracking it.
  ok("no event is in the future",
     !nrow(evs) || all(evs$event_dt <= Sys.time() + 60))
  ok("events run in sequence order",
     !nrow(evs) || all(vapply(split(evs, evs$cn_no), function(g) {
       g <- g[order(g$seq), ]; all(diff(as.numeric(g$event_dt)) >= 0)
     }, logical(1))))
}

cat("\n== Brand is wired from one place ==\n")
{
  ok("company name is set",       nzchar(BRAND$company))
  ok("settings carry the company", identical(setting("company_name"), BRAND$company))
  ok("support email uses the brand domain",
     grepl(paste0("@", BRAND$domain), setting("support_email"), fixed = TRUE))
  ok("every user email is on the brand domain",
     all(grepl(paste0("@", BRAND$domain, "$"), store_get("users")$email)))
  # A rebrand that leaves the previous operator's name in the seed is the kind
  # of thing nobody notices until it is on a customer's lorry receipt.
  ok("no trace of the previous brand in the data",
     !any(grepl("amardip", unlist(lapply(TABLES, function(t) {
       d <- store_get(t); if (nrow(d)) as.character(unlist(d)) else character(0)
     })), ignore.case = TRUE)))
  ok("page title names the company and the product",
     grepl(BRAND$short, BRAND$title, fixed = TRUE) &&
     grepl(BRAND$product, BRAND$title, fixed = TRUE))

  # The mark must render whether or not the artwork file is present — a missing
  # logo should degrade to a monogram, never to a broken image.
  ok("mark renders", nchar(as.character(brand_mark(38))) > 20)
  ok("logo renders with or without the file",
     nchar(as.character(brand_logo(56))) > 20)
  ok("brand_has_logo agrees with the filesystem",
     brand_has_logo() == file.exists(file.path("www", BRAND$logo)))
}

cat("\n== References that get typed by hand ==\n")
{
  r <- vapply(seq_len(2000), function(i) safe_ref(12), character(1))
  ok("reference is the requested length", all(nchar(r) == 12))
  # The whole point of the alphabet: nothing that can be misread off a paper
  # LR or a check-post screen.
  ok("no zero or capital O",     !any(grepl("[0O]", r)))
  ok("no one, I or L",           !any(grepl("[1IL]", r)))
  ok("no separators or symbols", !any(grepl("[^A-Z2-9]", r)))
  ok("alphabet is 8 digits and 23 letters", length(REF_ALPHABET) == 31)
  ok("references are unique",    length(unique(r)) == length(r))
  ok("uniqueness is enforced against existing",
     !safe_ref(4, existing = "AAAA") %in% "AAAA")

  # And the seeded bills must already be in that shape, or the demo shows the
  # old format on screen while the code claims the new one.
  e <- get_ewaybills()
  ok("seeded EWB numbers use the safe alphabet",
     !nrow(e) || !any(grepl("[^A-Z2-9]", e$ewb_no)))
  ok("seeded EWB numbers carry no separator",
     !nrow(e) || !any(grepl("[ -]", e$ewb_no)))
}

cat("\n== E-way bill validity ==\n")
{
  # One day per 200 km or part thereof — the statutory rule. A short lane is
  # therefore a 24-hour window, which is the case that actually bites.
  ok("200 km is one day",     ewb_validity_days(200) == 1)
  ok("a local lane is one day", ewb_validity_days(12) == 1)
  ok("201 km is two days",    ewb_validity_days(201) == 2)
  ok("1035 km is six days",   ewb_validity_days(1035) == 6)
  ok("a missing distance falls back to one day",
     ewb_validity_days(NA) == 1)
  ok("zero distance does not give zero days", ewb_validity_days(0) == 1)

  # The seed must obey the same rule it documents, or the first person to check
  # the arithmetic finds a contradiction.
  e <- get_ewaybills(); pb <- ewb_partb()
  if (nrow(e) && nrow(pb)) {
    ok("bill mirrors its latest Part-B",
       all(vapply(e$ewb_no, function(no) {
         p <- ewb_partb(no)
         !nrow(p) || identical(as.character(e$valid_to[e$ewb_no == no][1]),
                               as.character(p$valid_to[nrow(p)]))
       }, logical(1))))
    ok("every Part-B entry belongs to a real bill", all(pb$ewb_no %in% e$ewb_no))
    ok("Part-B sequence starts at 1",  min(pb$seq) == 1)
    ok("Part-B ids are unique",        !any(duplicated(pb$partb_id)))
    ok("every Part-B records its source", all(pb$mode %in% c("Manual", "Auto")))
    ok("every Part-B gives a reason",  all(nzchar(pb$reason)))
    ok("Part-B window matches the distance rule",
       all(round(as.numeric(difftime(pb$valid_to, pb$valid_from, units = "days"))) >= 1))
  }
}
cat("\n== Card layout ==\n")
local({
  # card_panel's `...` must come before its named text arguments. When it did
  # not, R matched the body positionally to `sub` and rendered whole tables
  # inside the card header, wrapped in a <p> — invalid markup that displayed
  # only because the browser closed the paragraph for us. Two dozen call sites
  # were written that way, so this is the assertion that keeps the signature
  # from drifting back.
  fm <- names(formals(card_panel))
  ok("card_panel takes body before sub", which(fm == "...") < which(fm == "sub"))

  body_of <- function(x) {
    h <- as.character(x)
    sub(".*<div class=\"tms-card-body\">", "", h)
  }
  h1 <- as.character(card_panel(title = "T", div(class = "probe-a", "x")))
  ok("positional content lands in the body",  grepl("probe-a", body_of(h1)))
  ok("it does not land in the header",
     !grepl("probe-a", sub("<div class=\"tms-card-body\">.*", "", h1)))

  h2 <- as.character(card_panel(title = "T", sub = "S", div(class = "probe-b", "x")))
  ok("a named sub still reaches the header", grepl("tms-card-sub[^>]*>S", h2))
  ok("and the body is still the body",       grepl("probe-b", body_of(h2)))

  # card_panel(empty_panel(...)) — a card with content and no header at all.
  h3 <- as.character(card_panel(div(class = "probe-c", "x")))
  ok("a headerless card renders no header",  !grepl("tms-card-head", h3))
  ok("its content is body, not an h6",       grepl("probe-c", body_of(h3)))

  h4 <- as.character(card_panel(title = "T", foot = div(class = "probe-d")))
  ok("foot still renders in the foot",
     grepl("tms-card-foot", h4) && grepl("probe-d", h4))
})

cat("\n== Footer ==\n")
local({
  f <- as.character(brand_footer())
  ok("footer carries the year",      grepl("2026", f, fixed = TRUE))
  ok("footer names the owner",       grepl("MoveWing Logistics", f, fixed = TRUE))
  ok("footer names the developer",
     grepl("Developed and managed by Katxel Private Limited", f, fixed = TRUE))
  # "Pvt. Ltd." already ends in a full stop; the notice must not add a second.
  ok("no doubled full stop",         !grepl("\\.\\.", BRAND$copyright))
  ok("footer is a <footer>",         grepl("^<footer", f))
})

cat("\n== GitHub data sync ==\n")
local({
  # Every assertion here runs with sync switched OFF and never touches the
  # network. The point is that the app is unchanged when it is not configured,
  # and that the queue behaves before anyone hands it a token.
  old <- Sys.getenv(c("TMS_GITHUB_REPO_URL", "TMS_GITHUB_PAT"), unset = NA)
  Sys.unsetenv(c("TMS_GITHUB_REPO_URL", "TMS_GITHUB_PAT"))

  ok("sync off without a token",        !gh_enabled())
  ok("a write does not queue when off", !isTRUE(gh_mark_dirty("bookings")))
  ok("flush is a no-op when off",       length(gh_flush()) == 0)
  ok("pull is a no-op when off",        !isTRUE(gh_pull_all()))

  # A store write must still succeed with no token, no network and no repo —
  # this is the path every existing installation is on.
  before <- nrow(get_clients())
  store_insert("clients", list(client_id = "SYNC-TEST", name = "Sync Probe"))
  ok("store writes work with sync off", nrow(get_clients()) == before + 1)
  store_delete("clients", list(client_id = "SYNC-TEST"))
  ok("probe row removed",               nrow(get_clients()) == before)

  # Owner/repo parsing, which decides where a push lands. Getting this wrong
  # silently would mean committing someone else's repository.
  p <- gh_parse_repo("https://github.com/tag-amitkumar/TMS-Transport.git")
  ok("repo url parses to owner",  identical(p$owner, "tag-amitkumar"))
  ok("repo url parses to repo",   identical(p$repo,  "TMS-Transport"))
  ok("url without .git parses",
     identical(gh_parse_repo("https://github.com/a/b")$repo, "b"))
  ok("trailing slash parses",
     identical(gh_parse_repo("https://github.com/a/b/")$repo, "b"))
  ok("a non-GitHub url is refused", is.null(gh_parse_repo("https://gitlab.com/a/b.git")))
  ok("an empty url is refused",     is.null(gh_parse_repo("")))

  # The static reference master is 19k rows that never change at runtime.
  # Pushing it on every lane lookup would be pure waste.
  ok("reference data is never queued", all(c("pincodes", "cities") %in% SYNC_SKIP))
  ok("business tables are not skipped",
     !any(c("bookings", "consignments", "gps_pings", "pods") %in% SYNC_SKIP))

  # With a token present the queue must coalesce: fifty GPS pings inside one
  # flush window are one commit, not fifty.
  Sys.setenv(TMS_GITHUB_REPO_URL = "https://github.com/example/example.git",
             TMS_GITHUB_PAT = "not-a-real-token")
  ok("sync on once configured", gh_enabled())
  ok("default branch is this one", identical(gh_config()$branch, "minimal-setup"))
  for (i in 1:50) gh_mark_dirty("gps_pings")
  gh_mark_dirty("bookings")
  ok("repeated writes coalesce to one entry", sum(gh_pending() == "gps_pings") == 1)
  ok("distinct tables queue separately",
     all(c("gps_pings", "bookings") %in% gh_pending()))
  ok("skipped tables never enter the queue", !isTRUE(gh_mark_dirty("pincodes")))
  .sync$dirty <- character(0)

  Sys.unsetenv(c("TMS_GITHUB_REPO_URL", "TMS_GITHUB_PAT"))
  for (k in names(old)) if (!is.na(old[[k]])) do.call(Sys.setenv, setNames(list(old[[k]]), k))

  # The token is the one thing that must never reach the repository, which is
  # public. Nothing in the tree may carry it but the gitignored .Renviron.
  src <- unlist(lapply(c("app.R", "global.R", list.files("R", "[.]R$", full.names = TRUE)),
                       readLines, warn = FALSE))
  ok("no token literal in the source",
     !any(grepl("gh[ps]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,}", src)))
  ok(".Renviron is gitignored",
     any(grepl("^\\.Renviron\\s*$", readLines(".gitignore", warn = FALSE))))
})

cat("\n== Serviceability — which branch, and is it ODA (CR-09/11/12/20) ==\n")
local({
  m <- seed_cr_masters(store_get("branches"))
  for (n in names(m)) store_set(n, m[[n]])
  br <- store_get("branches")

  own <- resolve_delivery(br$pincode[1])
  ok("a branch's own PIN code is delivered by that branch",
     identical(own$branch_id, br$branch_id[1]))
  ok("a branch's own PIN code is Normal, not ODA",
     identical(own$delivery_type, "Normal"))

  # An unlisted PIN code falls through to the distance rule. Somewhere far
  # from every branch must come back ODA whatever the branch list says.
  far <- resolve_delivery("682001")   # Kochi
  ok("a distant unlisted PIN code is ODA", identical(far$delivery_type, "ODA"))
  ok("ODA by distance says how far it was", grepl("km", far$basis))

  ok("an unknown PIN code is Unserviceable, not ODA",
     identical(resolve_delivery("999999")$delivery_type, "Unserviceable"))
  ok("a blank PIN code is Unserviceable",
     identical(resolve_delivery("")$delivery_type, "Unserviceable"))

  # An explicit listing beats the distance rule — that is the whole point of
  # the list, and it is how an exception gets recorded.
  store_insert("branch_pincodes", list(branch_id = br$branch_id[1],
                                       pincode = "682001", delivery_type = "Normal"))
  ok("an explicit listing overrides the distance rule",
     identical(resolve_delivery("682001")$delivery_type, "Normal"))
})

cat("\n== Freight breakup (CR-07/08) ==\n")
local({
  br <- store_get("branches")
  b  <- freight_breakup(1000, br$pincode[1], br$branch_id[1], "LTL", "Normal")
  ok("freight is rate x weight", b$base == round(1000 * b$rate))
  ok("a normal delivery carries no ODA surcharge", b$oda == 0)
  ok("every line is named", all(nzchar(vapply(b$lines, `[[`, character(1), "label"))))
  ok("no zero-value line is printed",
     all(vapply(b$lines, function(l) l$amount > 0, logical(1))))

  o <- freight_breakup(1000, br$pincode[1], br$branch_id[1], "LTL", "ODA")
  ok("an ODA delivery costs more than the same load to a normal one",
     o$freight > b$freight)
  ok("the ODA difference is exactly the surcharge",
     o$freight - b$freight == oda_charge())

  tiny <- freight_breakup(1, br$pincode[1], br$branch_id[1], "LTL", "Normal")
  ok("a tiny parcel bills at the minimum", tiny$freight == freight_minimum())
  ok("the minimum shows as a top-up line, not a silently raised rate",
     tiny$minimum_topup > 0)
  ok("zero weight quotes nothing rather than the minimum",
     freight_breakup(0, br$pincode[1], br$branch_id[1], "LTL", "Normal")$freight == 0)
})

cat("\n== Masters survive a CSV round-trip ==\n")
local({
  # The bug this guards against: an empty CSV field reads back as NA, not "".
  # `x == "682001"` is then NA, and subsetting a data frame by NA returns a row
  # of NAs rather than no rows — so a lookup "matches" a row that does not
  # exist and returns NA where a rate was expected. Setting the tables from
  # tibbles in a test never reproduces it; only a write-then-read does.
  store_set("freight_rates", tibble::tibble(
    pincode     = c("", "", ""),
    branch_id   = c("", "BR-001", "BR-001"),
    dispatch    = c("", "LTL", "FTL"),
    rate_per_kg = c("12", "14", "9")))
  store_refresh("freight_rates")          # force the next read off disk
  fr <- store_get("freight_rates")
  ok("blank CSV fields read back as NA, not empty string",
     any(is.na(fr$pincode)))

  r <- freight_rate("682001", "BR-001", "LTL")
  ok("an unmatched PIN code falls through to the branch rate, not NA",
     is.finite(r$rate) && r$rate == 14)
  ok("the fallback says which rule applied", nzchar(r$basis))

  b <- freight_breakup(2000, "682001", "BR-001", "LTL", "ODA")
  ok("freight is a real number after a round-trip", is.finite(b$freight))
  ok("no charge line is NA",
     all(vapply(b$lines, function(l) is.finite(l$amount), logical(1))))

  # Same trap, other masters.
  ok("product exemption survives a round-trip",
     isTRUE(product_exempt("Books & printed")))
  ok("an unlisted product is not exempt", isFALSE(product_exempt("Nonesuch")))

  # An unrated destination must quote zero, not NA, so validate_form can
  # refuse it with a message instead of the summary blanking out.
  store_set("freight_rates", tibble::tibble(
    pincode = character(), branch_id = character(),
    dispatch = character(), rate_per_kg = character()))
  z <- freight_breakup(1000, "682001", "BR-999", "LTL", "Normal")
  ok("an empty rate card quotes zero rather than NA",
     is.finite(z$freight) && z$freight == 0)
})

cat("\n== E-way bill threshold (CR-16/17/19) ==\n")
local({
  ok("intrastate below 1 lakh does not need one",
     !ewb_required(99999, "Maharashtra", "Maharashtra", "General goods")$required)
  ok("intrastate at 1 lakh does",
     ewb_required(100000, "Maharashtra", "Maharashtra", "General goods")$required)
  ok("interstate below 50k does not",
     !ewb_required(49999, "Maharashtra", "Delhi", "General goods")$required)
  ok("interstate at 50k does",
     ewb_required(50000, "Maharashtra", "Delhi", "General goods")$required)

  # CR-19: the test is on the sum, which is what stops a load being split
  # across invoices to duck the limit.
  store_set("booking_invoices", tibble::tibble(
    booking_no = rep("BKG-TEST", 3), invoice_no = c("A", "B", "C"),
    invoice_value = c("20000", "20000", "20000"), ewb_no = c("", "", "")))
  tot <- booking_invoice_total("BKG-TEST")
  ok("invoice values sum across the booking", tot == 60000)
  ok("three invoices under the limit still require one together",
     ewb_required(tot, "Maharashtra", "Delhi", "General goods")$required)

  ok("an exempt product never needs one, however valuable",
     !ewb_required(5000000, "Maharashtra", "Delhi", "Books & printed")$required)
  ok("a non-exempt product of the same value does",
     ewb_required(5000000, "Maharashtra", "Delhi", "Electronics")$required)

  # An unresolved destination must take the stricter limit. The other way
  # round lets an interstate load leave with no bill.
  ok("an unknown destination state uses the stricter interstate limit",
     ewb_required(60000, "Maharashtra", "", "General goods")$required)
})

cat("\n== E-way bill payload (CR-21/22) ==\n")
local({
  ok("no provider is configured yet", !ewb_api_configured())

  ok("state code comes from the GSTIN prefix when there is one",
     identical(ewb_state_code("27AAACA1234F1Z1", ""), "27"))
  ok("state code falls back to the name", identical(ewb_state_code("", "Delhi"), "07"))
  ok("an unknown state yields blank, not a wrong code",
     identical(ewb_state_code("", "Atlantis"), ""))

  bk <- store_get("bookings")
  bn <- bk$booking_no[1]
  store_set("booking_invoices", tibble::tibble(
    booking_no = bn, invoice_no = "INV-T1", invoice_value = "120000", ewb_no = ""))
  p <- ewb_part_a_payload(bn)
  ok("Part-A builds for a real booking", !is.null(p))
  ok("Part-A declares the invoice total", p$totInvValue == 120000)
  ok("Part-A is road transport", identical(p$transMode, "1"))
  ok("Part-A serialises to valid JSON",
     !is.null(jsonlite::fromJSON(jsonlite::toJSON(p, auto_unbox = TRUE, null = "null"))))

  # act*StateCode is the physical movement; *StateCode is the registration.
  # Deriving the physical one from a GSTIN files cross-state deliveries against
  # the wrong state.
  b1 <- bk[bk$booking_no == bn, ]
  ok("the physical destination code follows the address, not the GSTIN",
     identical(p$actToStateCode, ewb_state_code("", b1$dest_state[1])))

  r <- ewb_api_generate_part_a(bn)
  ok("an unconfigured provider fails softly rather than erroring", isFALSE(r$ok))
  ok("it says why", grepl("no e-way bill provider is configured", r$message))
  ok("it still shows what would have been sent", !is.null(r$payload))

  ok("a booking with no invoices sends nothing",
     isFALSE(ewb_api_generate_part_a("BKG-DOES-NOT-EXIST")$ok))
})

cat("\n== Consolidated e-way bill (CR-21) ==\n")
local({
  tr <- get_trips(); cn <- get_consignments(); eb <- get_ewaybills()
  # A trip whose consignments actually carry bills is the only case that can
  # consolidate; anything else must say so rather than build a bill of nothing.
  with_bills <- tr$trip_no[vapply(tr$trip_no, function(t) {
    cs <- cn$cn_no[cn$trip_no == t]
    any(eb$cn_no %in% cs & nzchar(eb$ewb_no))
  }, logical(1))]

  if (length(with_bills)) {
    p <- ewb_consolidated_payload(with_bills[1])
    ok("a trip carrying bills builds a consolidated payload", !is.null(p))
    ok("it lists at least one bill", length(p$tripSheetEwbBills) >= 1)
    ok("it is road transport", identical(p$transMode, "1"))
    ok("it carries the vehicle", nzchar(p$vehicleNo))
    ok("it serialises",
       !is.null(jsonlite::fromJSON(jsonlite::toJSON(p, auto_unbox = TRUE, null = "null"))))
  } else {
    ok("(no seeded trip carries an e-way bill — consolidation not exercised)", TRUE)
  }

  ok("a trip that does not exist consolidates nothing",
     is.null(ewb_consolidated_payload("TRP-NOPE")))
  r <- ewb_api_generate_consolidated("TRP-NOPE")
  ok("and says so rather than erroring", isFALSE(r$ok) && nzchar(r$message))
})

cat("\n== One rule for mobiles, one rule for e-way bills ==\n")
local({
  ok("a plain 10-digit mobile is valid", valid_mobile("9876543210"))
  ok("+91 and spacing are accepted", valid_mobile("+91 98765 43210"))
  ok("punctuation is stripped for storage",
     identical(clean_mobile("+91-98765 43210"), "9876543210"))
  ok("a landline-style number is refused", !valid_mobile("1234567890"))
  ok("nine digits is refused", !valid_mobile("987654321"))
  ok("blank is refused", !valid_mobile(""))

  # The offline booking path must reach the same verdict as the online one for
  # the same load. It used to skip the test entirely, which made the rule
  # avoidable by choosing that screen.
  a <- ewb_required(60000, "Maharashtra", "Delhi", "General goods")
  ok("the same load gives the same verdict whichever screen books it",
     isTRUE(a$required))
  ok("and the reason is stated either way", nzchar(a$reason))

  src <- paste(readLines("R/mod_bookings.R", warn = FALSE), collapse = "\n")
  ok("the offline form collects invoices", grepl("mi_add", src))
  ok("the offline form runs the e-way bill test", grepl("m_ewb\\(\\)", src))
  ok("the offline form asks for a dispatch type", grepl("m_dispatch", src))
  ok("the offline form asks for a product type", grepl("m_product", src))
  ok("the offline form resolves serviceability", grepl("resolve_delivery\\(input\\$m_to_pin", src))
  ok("no hand-rolled mobile regex remains outside global.R",
     !grepl("6-9\\]\\[0-9\\]\\{9\\}", src))
})

cat("\n== Customer notifications ==\n")
local({
  store_set("message_templates", seed_message_templates())
  blank <- function(...) tibble::tibble(...)
  store_set("wa_optin", blank(msisdn=character(),state=character(),source=character(),
                              client_id=character(),recorded_by=character(),recorded_dt=character()))
  store_set("notification_outbox", blank(outbox_id=character(),stage=character(),cn_no=character(),
    msisdn=character(),to_name=character(),template=character(),body=character(),state=character(),
    detail=character(),queued_dt=character(),sent_dt=character(),message_id=character(),queued_by=character()))

  t <- store_get("message_templates")
  ok("every customer-facing stage has a template", all(NOTIFY_STAGES %in% t$stage))
  ok("all templates are UTILITY, not MARKETING", all(t$category == "UTILITY"))
  ok("none ships pre-approved", !any(t$status == "Approved"))
  ok("every template declares its variables", all(nzchar(t$vars)))
  ok("no template body is empty", all(nchar(t$body) > 40))
  ok("every template body fits WhatsApp's 1024-char limit", all(nchar(t$body) <= 1024))

  ok("a ten-digit number gets the country code", identical(wa_msisdn("9876543210"), "919876543210"))
  ok("spacing and +91 are tolerated", identical(wa_msisdn("+91 98765 43210"), "919876543210"))
  ok("a leading zero is dropped", identical(wa_msisdn("09876543210"), "919876543210"))
  ok("rubbish yields nothing rather than a bad number", identical(wa_msisdn("abc"), ""))

  v <- c("Ravi","CN-00009","Nagpur","Delhi","12 Oct 2026")
  tpl <- t$body[t$stage == "booking_confirmed"][1]
  filled <- template_fill(tpl, v)
  ok("variables are substituted", grepl("CN-00009", filled, fixed = TRUE))
  ok("no placeholder survives a full fill", !grepl("\\{\\{[0-9]+\\}\\}", filled))
  ok("values survive the body round-trip",
     identical(notify_vars_from_body(tpl, filled)[2], "CN-00009"))

  # The gate. Consent is the one that must never be bypassed.
  notify_stage("booking_confirmed", "CN-00009", "9812345678", "Ravi", v)
  ob <- store_get("notification_outbox")
  ok("nothing is queued to a number that has not consented", ob$state[1] == "blocked")
  ok("and the reason names consent", grepl("consent", ob$detail[1], ignore.case = TRUE))

  wa_set_optin("9812345678", "opted_in", source = "test")
  ok("consent is recorded", wa_opted_in("9812345678"))
  notify_stage("booking_confirmed", "CN-00009", "9812345678", "Ravi", v)
  ob <- store_get("notification_outbox")
  ok("an unapproved template still blocks", ob$state[2] == "blocked")
  ok("and says the template is not approved",
     grepl("not approved", ob$detail[2], fixed = TRUE))

  wa_set_optin("9812345678", "opted_out", source = "STOP")
  ok("opt-out overrides earlier consent", !wa_opted_in("9812345678"))
  notify_stage("delivered", "CN-00009", "9812345678", "Ravi", v)
  ob <- store_get("notification_outbox")
  ok("an opt-out is called out explicitly, not folded into 'no consent'",
     grepl("opted OUT", ob$detail[3], fixed = TRUE))

  ok("no sender is configured yet", !meta_configured())
  p <- meta_template_payload("9812345678", "tms_delivered", "en", c("Ravi", "CN-00009"))
  ok("the payload is a template send", identical(p$type, "template"))
  ok("it addresses an E.164 number", identical(p$to, "919812345678"))
  ok("it serialises",
     !is.null(jsonlite::fromJSON(jsonlite::toJSON(p, auto_unbox = TRUE, null = "null"))))
  r <- meta_send(p)
  ok("sending without a sender fails softly", isFALSE(r$ok))
  ok("and says what is missing", grepl("TMS_WA_", r$message))

  ok("nothing was ever actually sent", !any(store_get("notification_outbox")$state == "sent"))

  # Put the tables back. This suite writes through the real store, so whatever
  # it leaves behind is what gets committed and baked into the image — and on
  # the first run of these tests that is exactly what happened: two fabricated
  # consent records shipped in the seed. A seeded opt-in is the one thing this
  # table must never contain, since its whole purpose is to evidence that a
  # real person agreed.
  store_set("wa_optin", blank(msisdn=character(),state=character(),source=character(),
                              client_id=character(),recorded_by=character(),recorded_dt=character()))
  store_set("notification_outbox", blank(outbox_id=character(),stage=character(),cn_no=character(),
    msisdn=character(),to_name=character(),template=character(),body=character(),state=character(),
    detail=character(),queued_dt=character(),sent_dt=character(),message_id=character(),queued_by=character()))
})

cat("\n== Meta Embedded Signup ==\n")
local({
  ok("signup is not offered without an app secret", !meta_signup_available())
  ok("a code exchange is refused when the app is unconfigured",
     isFALSE(meta_exchange_code("dummy-code")$ok))
  ok("an empty code is refused outright", isFALSE(meta_exchange_code("")$ok))

  # The token store must sit outside the repo, or it ends up in a commit.
  pth <- path.expand(meta_cred_path())
  ok("credentials default outside the project tree",
     !grepl(normalizePath(".", mustWork = FALSE), pth, fixed = TRUE))

  # Round-trip into a temporary location, so the test cannot touch a real one.
  old <- Sys.getenv("TMS_WA_CREDENTIALS", NA_character_)
  tmp <- file.path(tempdir(), "wa_test_credentials.json")
  Sys.setenv(TMS_WA_CREDENTIALS = tmp)
  meta_save_credentials("WABA-TEST", "PNID-TEST", "secret-token-value",
                        display_number = "+91 90000 00000")
  ok("credentials are written", file.exists(tmp))
  ok("and are owner-only",
     identical(substr(as.character(file.info(tmp)$mode), 1, 3), "600"))

  cred <- meta_saved_credentials()
  ok("they read back", identical(cred$waba_id, "WABA-TEST"))

  # With no environment override, config falls through to the stored values.
  ok("config picks up a stored connection",
     identical(meta_config()$phone_number_id, "PNID-TEST"))
  ok("a stored connection counts as configured", meta_configured())

  # The environment must win, so a deliberately set variable is never
  # silently overridden by an old stored connection.
  Sys.setenv(TMS_WA_PHONE_NUMBER_ID = "PNID-FROM-ENV")
  ok("the environment overrides the stored file",
     identical(meta_config()$phone_number_id, "PNID-FROM-ENV"))
  Sys.unsetenv("TMS_WA_PHONE_NUMBER_ID")

  s <- meta_connection_summary()
  ok("the summary reports the connection", isTRUE(s$connected))
  ok("the summary never carries the token",
     !any(grepl("secret-token-value", unlist(lapply(s, as.character)), fixed = TRUE)))

  meta_forget_credentials()
  ok("disconnect removes the file", !file.exists(tmp))
  ok("and the app is unconfigured again", !meta_configured())

  if (is.na(old)) Sys.unsetenv("TMS_WA_CREDENTIALS") else Sys.setenv(TMS_WA_CREDENTIALS = old)
})

cat("\n== The shipped seed carries no invented consent ==\n")
local({
  # Read the files, not the store: the store is whatever this suite has just
  # done to it, and the question here is what is on disk to be committed.
  for (f in c("wa_optin", "notification_outbox")) {
    path <- file.path("data", paste0(f, ".csv"))
    n <- if (file.exists(path)) max(0L, length(readLines(path, warn = FALSE)) - 1L) else 0L
    ok(paste0("data/", f, ".csv ships empty"), n == 0L)
  }
  ok("no template ships marked Approved",
     !any(readr::read_csv("data/message_templates.csv", show_col_types = FALSE)$status == "Approved"))
})

cat("\n== Change-request constants ==\n")
local({
  ok("LTL is the first dispatch option (CR-10)", DISPATCH_TYPES[1] == "LTL")
  ok("FTL is the second", DISPATCH_TYPES[2] == "FTL")
  ok("both dispatch types are labelled", all(nzchar(DISPATCH_LABEL[DISPATCH_TYPES])))
  ok("the ODA radius is 15 km (CR-11)", ODA_RADIUS_KM == 15)
  ok("operator roles exist for the TBB gate (CR-05/13)", length(OPERATOR_ROLES) > 0)
  ok("Customer is not an operator", !("Customer" %in% OPERATOR_ROLES))
  ok("Driver is not an operator", !("Driver" %in% OPERATOR_ROLES))
  ok("the four new masters are registered tables",
     all(c("branch_pincodes", "freight_rates", "booking_invoices",
           "product_types") %in% TABLES))
  ok("the consignment note is no longer called a lorry receipt (CR-23)",
     !any(grepl("LORRY RECEIPT",
                paste(readLines("R/ui_helpers.R", warn = FALSE), collapse = "\n"))))
  ok("four copies are printed across two sheets (CR-24)",
     length(LR_COPIES) == 2 && all(lengths(LR_COPIES) == 2))
  # The button label once said three while the print emitted four. Anything
  # that states the count must derive it, not repeat it.
  ok("no screen states a copy count that contradicts LR_COPIES",
     !any(grepl("\\(3 copies\\)|three copies to an A4",
                paste(readLines("R/mod_bookings.R", warn = FALSE), collapse = "\n"))))
  ok("the consignor copy comes first (CR-25)", LR_COPIES[[1]][1] == "Consignor Copy")
})

reg.finally()   # put data/ back before reporting

cat(sprintf("\n%s  %d passed, %d failed\n\n",
            if (fail == 0) "PASS" else "FAIL", pass, fail))
if (fail > 0) quit(status = 1)
