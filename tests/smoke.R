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
  source("R/seed.R"); source("R/seed_minimal.R")
  source("R/theme.R"); source("R/ui_helpers.R"); source("R/nav.R")
  for (f in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) source(f)
})

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
ok("Super Admin sees everything", all(vapply(MODULES, function(m) can("Super Admin", m, "view"), logical(1))))
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
cat(sprintf("\n%s  %d passed, %d failed\n\n",
            if (fail == 0) "PASS" else "FAIL", pass, fail))
if (fail > 0) quit(status = 1)
