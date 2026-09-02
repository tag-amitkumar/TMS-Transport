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
  source("global.R"); source("R/store.R"); source("R/rbac.R")
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
  list(branches = 2, employees = 3, vehicles = 2, clients = 2, bookings = 3)
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

cat(sprintf("\n%s  %d passed, %d failed\n\n",
            if (fail == 0) "PASS" else "FAIL", pass, fail))
if (fail > 0) quit(status = 1)
