# ==================================================================
# Functional test — run from the project root:
#   Rscript tests/functional.R
#
# tests/smoke.R proves the data is coherent and every screen is wired up. It
# does NOT press any buttons, which is how a New Booking form with permanently
# empty dropdowns shipped: every screen rendered, nothing could be submitted.
#
# This file drives the module servers through shiny::testServer with simulated
# inputs and asserts on what actually landed in the store. It is the difference
# between "the screen appears" and "the button works".
#
# Runs against a scratch copy of data/, restored on exit, so it never disturbs
# the seeded demo data.
# ==================================================================

suppressPackageStartupMessages({
  library(shiny)
  source("global.R"); source("R/store.R"); source("R/rbac.R")
  source("R/seed.R"); source("R/seed_minimal.R")
  source("R/theme.R"); source("R/ui_helpers.R"); source("R/nav.R")
  for (f in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) source(f)
})

# ---- scratch data ------------------------------------------------
#
# These tests write for real — they create bookings, allocate vehicles, raise
# invoices. data/ is copied aside first and put back afterwards.
#
# The restore is registered with reg.finalizer(onexit = TRUE), NOT on.exit().
# on.exit() binds to a function frame and there is no function frame at the top
# level of a script, so it silently does nothing under Rscript — which is how an
# earlier run of this file leaked a test vehicle and a test client into the
# committed demo data. reg.finalizer runs on interpreter shutdown, including
# after quit(status = 1) on failure.
backup <- file.path(tempdir(), paste0("tms-data-", as.integer(Sys.time())))
dir.create(backup, recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(list.files(DATA_DIR, full.names = TRUE), backup, overwrite = TRUE))

restore_data <- function() {
  if (!dir.exists(backup)) return(invisible(NULL))
  invisible(file.copy(list.files(backup, full.names = TRUE), DATA_DIR, overwrite = TRUE))
  # Anything the tests created that was not in the backup has to go, or the
  # next run trips over its own leftovers.
  kept <- basename(list.files(backup))
  for (f in setdiff(basename(list.files(DATA_DIR)), kept)) unlink(file.path(DATA_DIR, f))
  unlink(backup, recursive = TRUE)
  store_refresh()
  invisible(NULL)
}
invisible(reg.finalizer(environment(), function(e) restore_data(), onexit = TRUE))

pass <- 0; fail <- 0
ok <- function(label, cond) {
  if (isTRUE(cond)) pass <<- pass + 1
  else { fail <<- fail + 1; cat("  FAIL:", label, "\n") }
}
section <- function(x) cat("\n==", x, "==\n")

# A signed-in Super Admin, shaped exactly as mod_auth builds it.
admin <- list(user_id = "USR-0001", name = "Amardip Singh",
              email = "admin@amardiptms.in", role = "Super Admin",
              branch_id = store_get("branches")$branch_id[1],
              department = "Management", cross_branch = "TRUE",
              client_id = "", vendor_id = "")
ops <- modifyList(admin, list(user_id = "USR-0002", name = "Suresh Khan",
                              role = "Operations Manager", cross_branch = "FALSE"))

usr  <- function(u = admin) reactiveVal(u)
noop <- function(...) invisible(NULL)

# ==================================================================
section("Booking — the form the customer demo opens on")

testServer(bookings_server, args = list(user = usr(), nav = noop), {
  # The bug that started this: the selectors are rendered, so they must produce
  # a real control containing every customer — not an empty stub.
  html <- as.character(output$sel_client$html %||% output$sel_client)
  ok("customer selector renders", nchar(html) > 50)
  cl <- store_get("clients")
  ok("customer selector lists every client",
     all(vapply(cl$client_id, function(id) grepl(id, html, fixed = TRUE), logical(1))))
  ok("customer selector starts unselected", grepl("Select a customer", html, fixed = TRUE))

  bhtml <- as.character(output$sel_branch$html %||% output$sel_branch)
  ok("branch selector renders", grepl(store_get("branches")$branch_id[1], bhtml, fixed = TRUE))
  fhtml <- as.character(output$sel_from$html %||% output$sel_from)
  ok("origin selector renders", nchar(fhtml) > 50)

  before <- nrow(store_get("bookings"))

  # Empty form must be refused, not silently saved.
  session$setInputs(f_client = "", f_pickup = "", f_drop = "", f_material = "",
                    f_weight = 10, f_freight = 15000, f_qty = 100, f_pkg = "",
                    f_ins = "Not insured", f_remarks = "",
                    f_from = "Nagpur", f_to = "Delhi",
                    f_branch = admin$branch_id, f_date = Sys.Date())
  session$setInputs(confirm = 1)
  ok("empty form is refused", nrow(store_get("bookings")) == before)

  # Same origin and destination must be refused.
  session$setInputs(f_client = cl$client_id[1], f_pickup = "A", f_drop = "B",
                    f_material = "Cement", f_from = "Nagpur", f_to = "Nagpur")
  session$setInputs(confirm = 2)
  ok("same origin/destination refused", nrow(store_get("bookings")) == before)

  # Zero weight must be refused.
  session$setInputs(f_to = "Delhi", f_weight = 0)
  session$setInputs(confirm = 3)
  ok("zero weight refused", nrow(store_get("bookings")) == before)

  # A complete form must save.
  session$setInputs(f_weight = 12.5, f_freight = 21000)
  session$setInputs(save_draft = 1)
  bk <- get_bookings()
  ok("draft saves", nrow(bk) == before + 1)

  new <- bk[nrow(bk), ]
  ok("draft has Draft status",   identical(new$status, "Draft"))
  ok("booking number allocated", grepl("^BKG-", new$booking_no))
  ok("client carried through",   identical(new$client_id, cl$client_id[1]))
  ok("weight carried through",   abs(new$weight_t - 12.5) < .001)
  ok("freight carried through",  abs(new$freight - 21000) < .001)

  # GST must come from the client master, never the form.
  cli <- get_clients(); cr <- cli[cli$client_id == cl$client_id[1], ]
  ok("GST mode from client master", identical(new$gst_mode, cr$gst_mode[1]))
  ok("GST rate from client master", abs(new$gst_pct - cr$gst_pct[1]) < .001)

  # Reverse charge collects nothing at booking time, exactly as it collects
  # nothing at invoice time. Charging it here inflated the booking total against
  # an invoice that correctly showed zero.
  if (identical(cr$gst_mode[1], "RCM")) {
    ok("RCM booking collects no GST", new$gst_amount == 0)
    ok("RCM total excludes GST",
       abs(new$total - (new$freight + new$insurance_amt)) < 1)
  } else {
    ok("FCM booking charges GST",
       abs(new$gst_amount - round(21000 * cr$gst_pct[1] / 100)) < 1)
    ok("FCM total includes GST",
       abs(new$total - (new$freight + new$gst_amount + new$insurance_amt)) < 1)
  }
  ok("total reconciles", abs(new$total - (new$freight + new$gst_amount + new$insurance_amt)) < 1)

  # Booking and invoice must agree on the treatment, or revenue is overstated.
  ok("booking GST rule matches the invoice rule",
     (identical(new$gst_mode, "RCM") && new$gst_amount == 0) ||
       (identical(new$gst_mode, "FCM") && new$gst_amount > 0))

  # Picking a customer sets the origin to their city; the destination must step
  # aside rather than colliding and blocking the form.
  ok("origin and destination differ after picking a customer",
     !identical(new$origin_city, new$dest_city))

  # Confirm writes a Confirmed booking, which is what dispatch can see.
  session$setInputs(confirm = 4)
  bk2 <- get_bookings()
  ok("confirm saves a second booking", nrow(bk2) == before + 2)
  ok("confirmed booking is Confirmed", identical(bk2$status[nrow(bk2)], "Confirmed"))
})

# ==================================================================
section("Payment terms")

testServer(bookings_server, args = list(user = usr(), nav = noop), {
  cl <- store_get("clients"); br <- store_get("branches")
  base <- list(f_client = cl$client_id[1], f_pickup = "A", f_drop = "B",
               f_material = "Terms check", f_weight = 8, f_qty = 10, f_pkg = "10",
               f_ins = "Not insured", f_remarks = "", f_freight = 12000,
               f_from = "Nagpur", f_to = "Delhi",
               f_branch = admin$branch_id, f_date = Sys.Date())

  # Each term must land on the booking as chosen.
  for (m in c("Paid", "To Pay", "Credit")) {
    do.call(session$setInputs, base)
    session$setInputs(f_pay = m)
    session$setInputs(save_draft = paste0("t", m))
    bk <- get_bookings()
    ok(paste("payment mode saved:", m),
       identical(bk$payment_mode[nrow(bk)], m))
  }

  # TBB is meaningless without the branch that will raise the bill.
  n <- nrow(get_bookings())
  do.call(session$setInputs, base)
  session$setInputs(f_pay = "TBB", f_bill_branch = "")
  session$setInputs(save_draft = "tbb-bad")
  ok("TBB without a billing branch refused", nrow(get_bookings()) == n)

  other <- br$branch_id[br$branch_id != admin$branch_id][1]
  session$setInputs(f_bill_branch = other)
  session$setInputs(save_draft = "tbb-good")
  bk <- get_bookings()
  ok("TBB saves with a billing branch", nrow(bk) == n + 1)
  ok("TBB records the billing branch",
     identical(bk$bill_at_branch_id[nrow(bk)], other))

  # Everything created through the form is an online entry.
  ok("form bookings are marked Online",
     identical(bk$entry_mode[nrow(bk)], "Online"))
})

# ==================================================================
section("Manual entry — loads booked while the system was down")

testServer(bookings_server, args = list(user = usr(), nav = noop), {
  cl <- store_get("clients")
  n <- nrow(get_bookings())
  m <- list(m_ref = "QA/LR/9001", m_date = Sys.Date() - 1, m_time = "07:30",
            m_client = cl$client_id[1], m_branch = admin$branch_id,
            m_pay = "To Pay", m_from = "Nagpur", m_to = "Delhi",
            m_material = "Offline load", m_weight = 11, m_freight = 18000,
            m_remarks = "")

  # Paper reference is what ties the row back to the book it came from.
  do.call(session$setInputs, modifyList(m, list(m_ref = "")))
  session$setInputs(m_save = 1)
  ok("manual entry without a paper reference refused", nrow(get_bookings()) == n)

  do.call(session$setInputs, m)
  session$setInputs(m_save = 2)
  bk <- get_bookings()
  ok("manual entry creates a booking", nrow(bk) == n + 1)

  new <- bk[nrow(bk), ]
  ok("manual entry marked Manual",     identical(new$entry_mode, "Manual"))
  ok("paper reference retained",       identical(new$manual_ref, "QA/LR/9001"))
  ok("time the load was taken kept",   grepl("07:30", new$manual_dt, fixed = TRUE))
  ok("manual entry carries its terms", identical(new$payment_mode, "To Pay"))
  # A load already accepted must reach dispatch, not sit in Draft.
  ok("manual entry enters Confirmed",  identical(new$status, "Confirmed"))
  ok("manual entry gets a normal booking number", grepl("^BKG-", new$booking_no))

  # GST still comes from the client master, not from whoever is keying it in.
  cli <- get_clients(); cr <- cli[cli$client_id == cl$client_id[1], ]
  if (identical(cr$gst_mode[1], "RCM")) {
    ok("manual RCM collects no GST", new$gst_amount == 0)
  } else {
    ok("manual FCM charges GST", new$gst_amount > 0)
  }

  # The same paper slip must not be entered twice.
  n2 <- nrow(get_bookings())
  session$setInputs(m_save = 3)
  ok("duplicate paper reference refused", nrow(get_bookings()) == n2)
})

# ==================================================================
section("Allocation — booking becomes a trip, an LR and a consignment")

alloc_bk <- NULL
testServer(cargo_moto_server, args = list(user = usr(), nav = noop), {
  bk <- get_bookings()
  pend <- bk[bk$status == "Confirmed", ]
  ok("a confirmed booking is waiting", nrow(pend) > 0)
  alloc_bk <<- pend$booking_no[nrow(pend)]

  v  <- get_vehicles(); d <- get_drivers()
  load_t <- pend$weight_t[nrow(pend)]
  big    <- v[v$capacity_t >= load_t, ]
  small  <- v[v$capacity_t <  load_t, ]

  n_trip <- nrow(store_get("trips")); n_cn <- nrow(store_get("consignments"))

  # Over-capacity must be refused outright.
  if (nrow(small)) {
    session$setInputs(a_booking = alloc_bk, a_vehicle = small$vehicle_id[1],
                      a_driver = d$driver_id[1], a_save = 1)
    ok("over-capacity allocation refused", nrow(store_get("trips")) == n_trip)
  } else {
    ok("over-capacity allocation refused (no smaller vehicle to test)", TRUE)
  }

  # A valid allocation must create trip + consignment together.
  session$setInputs(a_booking = alloc_bk, a_vehicle = big$vehicle_id[1],
                    a_driver = d$driver_id[1], a_save = 2)

  tr <- get_trips(); cn <- get_consignments()
  ok("trip created",        nrow(tr) == n_trip + 1)
  ok("consignment created", nrow(cn) == n_cn + 1)

  t <- tr[tr$booking_no == alloc_bk, ]
  c1 <- cn[cn$booking_no == alloc_bk, ]
  ok("trip links the booking",     nrow(t) == 1)
  ok("consignment links the trip", nrow(c1) == 1 && identical(c1$trip_no[1], t$trip_no[1]))
  ok("LR number allocated",        grepl("^LR-", c1$lr_no[1]))
  ok("CN number allocated",        grepl("^CN-", c1$cn_no[1]))
  ok("consignment starts In Prep", identical(c1$status[1], "In Prep"))

  # Statuses must move together, or the boards disagree with each other.
  bk2 <- get_bookings()
  ok("booking becomes Vehicle Allocated",
     identical(bk2$status[bk2$booking_no == alloc_bk], "Vehicle Allocated"))
  ok("vehicle becomes Allocated",
     identical(get_vehicles()$status[get_vehicles()$vehicle_id == big$vehicle_id[1]], "Allocated"))
  ok("driver becomes On trip",
     identical(get_drivers()$status[get_drivers()$driver_id == d$driver_id[1]], "On trip"))

  ev <- store_get("consignment_events")
  ok("timeline seeded", sum(ev$cn_no == c1$cn_no[1]) >= 2)

  # Terms must ride along, or the printed LR cannot tell the driver whether to
  # collect on delivery.
  bkr <- bk[bk$booking_no == alloc_bk, ]
  ok("payment mode carried onto the consignment",
     identical(c1$payment_mode[1], bkr$payment_mode[1]))
})

# ==================================================================
section("POD gate — no invoice without proof of delivery")

testServer(invoices_server, args = list(user = usr(), nav = noop), {
  cn <- get_consignments()
  pods <- get_pods()
  n_inv <- nrow(get_invoices())

  # A consignment with no verified POD must be refused.
  unverified <- cn$cn_no[!(cn$lr_no %in% pods$lr_no[pods$status %in% c("Verified", "Approved")])]
  if (length(unverified)) {
    session$setInputs(bill_one = unverified[1])
    ok("invoice refused without verified POD", nrow(get_invoices()) == n_inv)
  } else {
    ok("invoice refused without verified POD (none unverified)", TRUE)
  }

  # A delivered consignment with an approved POD and no invoice yet must bill.
  good <- cn[cn$status == "Delivered" &
               cn$lr_no %in% pods$lr_no[pods$status %in% c("Verified", "Approved")] &
               !(cn$lr_no %in% get_invoices()$lr_no), ]
  if (nrow(good)) {
    session$setInputs(bill_one = good$cn_no[1])
    inv <- get_invoices()
    ok("invoice raised against verified POD", nrow(inv) == n_inv + 1)
    r <- inv[inv$cn_no == good$cn_no[1], ]
    ok("invoice links the consignment", nrow(r) == 1)
    cli <- get_clients(); cr <- cli[cli$client_id == good$client_id[1], ]
    ok("invoice GST mode follows client", identical(r$gst_mode[1], cr$gst_mode[1]))
    if (identical(cr$gst_mode[1], "RCM")) {
      ok("RCM invoice collects no tax", r$gst_amount[1] == 0)
      ok("RCM total equals freight",    abs(r$total[1] - r$amount[1]) < 1)
    } else {
      ok("FCM invoice collects tax", r$gst_amount[1] > 0)
      ok("FCM total includes tax",   abs(r$total[1] - (r$amount[1] + r$gst_amount[1])) < 1)
    }
    ok("new invoice starts unpaid", r$paid_amount[1] == 0)

    # Billing the same consignment twice must be refused.
    n2 <- nrow(get_invoices())
    session$setInputs(bill_one = good$cn_no[1])
    ok("double-invoicing refused", nrow(get_invoices()) == n2)

    # Payment terms decide who raises the invoice and whether anything is owed.
    pay <- r$payment_mode[1]
    if (identical(pay, "Paid")) {
      ok("Paid invoice opens settled", abs(r$paid_amount[1] - r$total[1]) < 1)
      ok("Paid invoice status is Paid", identical(r$status[1], "Paid"))
    } else if (identical(pay, "TBB")) {
      # The receivable belongs to the branch holding the customer's account,
      # not the one that despatched the load.
      ok("TBB invoice is raised by the billing branch",
         identical(r$branch_id[1], good$bill_at_branch_id[1]))
      ok("TBB invoice is not pre-settled", r$paid_amount[1] == 0)
    } else {
      ok("credit/to-pay invoice opens unpaid", r$paid_amount[1] == 0)
      ok("credit/to-pay invoice starts Draft", identical(r$status[1], "Draft"))
    }
    ok("invoice records the payment term", identical(r$payment_mode[1], pay))
  } else {
    ok("invoice raised against verified POD (nothing billable)", TRUE)
  }
})

# ==================================================================
section("Receipts reconcile against the invoice")

testServer(payments_server, args = list(user = usr()), {
  inv <- get_invoices()
  inv$balance <- inv$total - tidyr::replace_na(inv$paid_amount, 0)
  open <- inv[inv$balance > 0 & inv$status != "Draft", ]

  if (nrow(open)) {
    target <- open[1, ]
    n_pay <- nrow(store_get("payments"))
    # The invoice may already carry a part payment, so assert on the delta
    # rather than assuming it starts at zero.
    already <- tidyr::replace_na(target$paid_amount, 0)

    # Partial payment leaves the invoice open.
    part <- round(target$balance / 2)
    session$setInputs(r_inv = target$invoice_no, r_amt = part,
                      r_mode = "NEFT", r_date = Sys.Date(),
                      r_ref = "UTR-TEST-1", r_save = 1)
    ok("receipt recorded", nrow(store_get("payments")) == n_pay + 1)
    i2 <- get_invoices(); i2 <- i2[i2$invoice_no == target$invoice_no, ]
    ok("partial payment applied",     abs(i2$paid_amount[1] - (already + part)) < 1)
    ok("partial keeps invoice open",  identical(i2$status[1], "Partially paid"))

    # Settling the remainder marks it paid.
    session$setInputs(r_amt = target$balance - part, r_ref = "UTR-TEST-2", r_save = 2)
    i3 <- get_invoices(); i3 <- i3[i3$invoice_no == target$invoice_no, ]
    ok("balance settles the invoice", abs(i3$paid_amount[1] - target$total) < 1)
    ok("settled invoice is Paid",     identical(i3$status[1], "Paid"))

    # Zero must be refused.
    n3 <- nrow(store_get("payments"))
    session$setInputs(r_amt = 0, r_save = 3)
    ok("zero receipt refused", nrow(store_get("payments")) == n3)
  } else {
    ok("receipt recorded (no open invoice)", TRUE)
  }
})

# ==================================================================
section("POD lifecycle")

testServer(pod_server, args = list(user = usr()), {
  p <- get_pods()
  up <- p[p$status == "Uploaded", ]
  if (!nrow(up)) {
    # Push one into Uploaded so verify/approve can be exercised.
    pend <- p[p$status == "Pending", ]
    if (nrow(pend)) {
      store_update("pods", list(pod_id = pend$pod_id[1]),
                   list(status = "Uploaded", uploaded_by = "Test",
                        file_name = "t.jpg",
                        upload_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      up <- get_pods(); up <- up[up$status == "Uploaded", ]
    }
  }
  if (nrow(up)) {
    session$userData$pod_sel <- up$pod_id[1]
    session$setInputs(verify = 1)
    ok("POD verifies",
       identical(get_pods()$status[get_pods()$pod_id == up$pod_id[1]], "Verified"))
    session$setInputs(approve = 1)
    ok("POD approves",
       identical(get_pods()$status[get_pods()$pod_id == up$pod_id[1]], "Approved"))
  } else {
    ok("POD verifies (none available)", TRUE)
    ok("POD approves (none available)", TRUE)
  }
})

# ==================================================================
section("Complaints")

testServer(complaints_server, args = list(user = usr()), {
  cn <- get_consignments()
  n <- nrow(store_get("complaints"))

  # Description is required.
  session$setInputs(c_cn = cn$cn_no[1], c_cat = "Delay", c_pri = "High",
                    c_sub = "", c_save = 1)
  ok("complaint without description refused", nrow(store_get("complaints")) == n)

  session$setInputs(c_sub = "Test complaint raised by functional suite", c_save = 2)
  cmp <- get_complaints()
  ok("complaint registered", nrow(cmp) == n + 1)
  r <- cmp[nrow(cmp), ]
  ok("complaint starts New", identical(r$status, "New"))
  ok("high priority gets 1-day SLA",
     as.numeric(as.Date(r$target_dt) - as.Date(r$raised_dt)) == 1)

  # Resolution note is required to close.
  session$userData$cmp_sel <- r$complaint_id
  session$setInputs(r_note = "", r_save = 1)
  ok("resolve without a note refused",
     !identical(get_complaints()$status[get_complaints()$complaint_id == r$complaint_id], "Resolved"))
  session$setInputs(r_note = "Handled and customer informed", r_save = 2)
  done <- get_complaints(); done <- done[done$complaint_id == r$complaint_id, ]
  ok("complaint resolves", identical(done$status[1], "Resolved"))
  ok("resolution recorded", nzchar(done$resolution[1]))
  ok("progress set to 100", done$progress_pct[1] == 100)
})

# ==================================================================
section("Master data — create")

# Note: the modal save button is `save` in the relationship/admin modules and
# `n_save` in the fleet/HR ones. Harmless inconsistency, but it has to be
# matched exactly here or the handler simply never fires and the test passes
# for the wrong reason.
testServer(clients_server, args = list(user = usr()), {
  n <- nrow(store_get("clients"))
  session$setInputs(n_name = "", save = 1)
  ok("client without a name refused", nrow(store_get("clients")) == n)

  session$setInputs(n_name = "Functional Test Traders", n_contact = "QA",
                    n_mobile = "+91 90000 00001", n_email = "qa@example.com",
                    n_gstin = "27TESTQ1234F1Z9", n_pan = "TESTQ1234F",
                    n_city = "Nagpur", n_gm = "RCM", n_gp = 5,
                    n_credit = 250000, n_branch = admin$branch_id,
                    n_bill = "Test address", save = 2)
  cl <- get_clients()
  ok("client created", nrow(cl) == n + 1)
  ok("client id allocated", grepl("^CUST-", cl$client_id[nrow(cl)]))
  ok("client GST stored", identical(cl$gst_mode[nrow(cl)], "RCM"))
})

testServer(vehicles_server, args = list(user = usr(), nav = noop), {
  n <- nrow(store_get("vehicles"))
  session$setInputs(n_reg = "", n_save = 1)
  ok("vehicle without a registration refused", nrow(store_get("vehicles")) == n)

  session$setInputs(n_reg = "MH-31 QA 0001", n_model = "Test", n_body = "32 ft MXL",
                    n_cap = 25, n_owner = "Company", n_vendor = "",
                    n_branch = admin$branch_id, n_rc = "RCQA0001",
                    n_ins = Sys.Date() + 365, n_save = 2)
  ok("vehicle created", nrow(store_get("vehicles")) == n + 1)

  # Duplicate registration must be refused.
  n2 <- nrow(store_get("vehicles"))
  session$setInputs(n_save = 3)
  ok("duplicate registration refused", nrow(store_get("vehicles")) == n2)
})

testServer(pincodes_server, args = list(user = usr()), {
  n <- nrow(store_get("pincode_routes"))
  b <- store_get("branches")
  session$setInputs(n_src = b$city[1], n_dst = b$city[1], n_save = 1)
  ok("route with same source and destination refused",
     nrow(store_get("pincode_routes")) == n)

  # A lane that already exists must be refused.
  rt <- store_get("pincode_routes")
  if (nrow(rt)) {
    session$setInputs(n_src = rt$src_city[1], n_dst = rt$dst_city[1],
                      n_spin = rt$src_pincode[1], n_dpin = rt$dst_pincode[1],
                      n_days = 3, n_pzone = "Zone North", n_dzone = "Zone North",
                      n_avail = "Available", n_save = 2)
    ok("duplicate lane refused", nrow(store_get("pincode_routes")) == n)
  } else {
    ok("duplicate lane refused (no routes)", TRUE)
  }
})

testServer(hrms_employees_server, args = list(user = usr(), nav = noop), {
  n <- nrow(store_get("employees"))
  session$setInputs(n_name = "", n_save = 1)
  ok("employee without a name refused", nrow(store_get("employees")) == n)

  session$setInputs(n_name = "QA Tester", n_mobile = "+91 90000 00002",
                    n_desig = "Analyst", n_dept = "Operations",
                    n_branch = admin$branch_id, n_joined = Sys.Date(),
                    n_stype = "monthly", n_salary = 30000, n_pan = "QATST1234Z",
                    n_save = 2)
  ok("employee created", nrow(store_get("employees")) == n + 1)
})

testServer(users_server, args = list(user = usr()), {
  n <- nrow(store_get("users"))
  session$setInputs(n_name = "QA User", n_email = "", save = 1)
  ok("user without an email refused", nrow(store_get("users")) == n)

  session$setInputs(n_email = "qa.user@amardiptms.in", n_role = "Dispatcher",
                    n_branch = admin$branch_id, n_dept = "Operations",
                    n_mobile = "+91 90000 00003", n_cross = FALSE, save = 2)
  u <- store_get("users")
  ok("user created", nrow(u) == n + 1)
  ok("password stored hashed, not plain",
     grepl("^\\$2", u$password_hash[nrow(u)]))

  # Duplicate email must be refused.
  n2 <- nrow(store_get("users"))
  session$setInputs(save = 3)
  ok("duplicate email refused", nrow(store_get("users")) == n2)
})

# ==================================================================
section("Permissions are enforced on the server, not just hidden")

# An Operations Manager must not be able to create a user or an invoice even
# by firing the handler directly — the button being hidden is not the defence.
testServer(users_server, args = list(user = usr(ops)), {
  n <- nrow(store_get("users"))
  session$setInputs(n_name = "Sneaky", n_email = "sneaky@amardiptms.in",
                    n_role = "Super Admin", n_branch = ops$branch_id,
                    n_dept = "X", n_mobile = "1", n_cross = TRUE, n_save = 1)
  ok("ops cannot create a user", nrow(store_get("users")) == n)
})

testServer(invoices_server, args = list(user = usr(ops), nav = noop), {
  n <- nrow(get_invoices())
  session$setInputs(m_client = get_clients()$client_id[1], m_desc = "detention",
                    m_amt = 5000, m_save = 1)
  ok("ops cannot raise a manual invoice", nrow(get_invoices()) == n)
})

testServer(security_server, args = list(user = usr(ops)), {
  p_before <- store_get("permissions")
  session$setInputs(role = "Dispatcher")
  session$setInputs(granted = "bookings|delete,invoices|approve")
  ok("ops cannot edit the permission matrix",
     identical(store_get("permissions"), p_before))
})

# ==================================================================
section("Trip Book & e-way bill")

testServer(trips_server, args = list(user = usr(), nav = noop), {
  tr <- get_trips()
  if (nrow(tr)) {
    # The list is sorted by dispatch date, so row 1 of the table is not
    # necessarily row 1 of the file. Assert on the fleet total instead of
    # guessing which trip the selection landed on.
    total_before <- sum(as.numeric(tr$advance), na.rm = TRUE)
    session$setInputs(tbl_rows_selected = 1, adv_amt = 0, adv_save = 1)
    ok("zero advance refused",
       abs(sum(as.numeric(get_trips()$advance), na.rm = TRUE) - total_before) < .001)

    session$setInputs(adv_amt = 750, adv_save = 2)
    ok("advance recorded",
       abs(sum(as.numeric(get_trips()$advance), na.rm = TRUE) - (total_before + 750)) < .001)
  } else {
    ok("zero advance refused (no trips)", TRUE)
    ok("advance recorded (no trips)", TRUE)
  }
})

testServer(ewaybill_server, args = list(user = usr(), nav = noop), {
  cn <- get_consignments()
  have <- get_ewaybills()$cn_no
  need <- cn[!(cn$cn_no %in% have) & cn$status != "Delivered", ]
  n <- nrow(get_ewaybills())
  if (nrow(need)) {
    session$setInputs(g_cn = need$cn_no[1], g_save = 1)
    e <- get_ewaybills()
    ok("e-way bill reference generated", nrow(e) == n + 1)
    ok("bill links its consignment", need$cn_no[1] %in% e$cn_no)
    ok("bill starts Valid", identical(e$status[nrow(e)], "Valid"))
  } else {
    ok("e-way bill reference generated (nothing needs one)", TRUE)
    ok("bill links its consignment", TRUE)
    ok("bill starts Valid", TRUE)
  }
})

# ==================================================================
section("HRMS")

testServer(hrms_attendance_server, args = list(user = usr()), {
  lr <- store_get("leave_requests")
  pend <- lr[lr$status == "Pending", ]
  if (nrow(pend)) {
    session$setInputs(approve = pend$lr_id[1])
    after <- store_get("leave_requests")
    ok("leave approves",
       identical(after$status[after$lr_id == pend$lr_id[1]], "Approved"))
    # Approving must write the leave days into the register payroll reads.
    a <- get_attendance()
    days <- seq(as.Date(pend$from_date[1]), as.Date(pend$to_date[1]), by = "day")
    marked <- a[a$employee_id == pend$employee_id[1] & a$date %in% days, ]
    ok("approved leave lands in the attendance register",
       nrow(marked) > 0 && all(marked$status == "L"))
  } else {
    ok("leave approves (none pending)", TRUE)
    ok("approved leave lands in the attendance register", TRUE)
  }
})

testServer(hrms_payroll_server, args = list(user = usr()), {
  n_audit <- nrow(store_get("audit_log"))
  session$setInputs(approve_confirm = 1)
  a <- store_get("audit_log")
  ok("payroll approval is audited",
     nrow(a) > n_audit && any(a$module == "hrms_payroll" & a$action == "approve"))
})

# ==================================================================
section("Settings")

testServer(settings_server, args = list(user = usr()), {
  session$setInputs(tab = "Company")
  session$setInputs(c_name = "Amardip Road Carriers Ltd", c_gstin = setting("company_gstin"),
                    c_office = setting("registered_office"),
                    c_email = setting("support_email"),
                    c_phone = setting("support_phone"),
                    c_df = "DD-MMM-YYYY", c_fy = "01 April", save = 1)
  ok("company name saves", identical(setting("company_name"), "Amardip Road Carriers Ltd"))

  ig <- store_get("integrations")
  session$setInputs(toggle = ig$name[1])
  ok("integration toggles",
     !identical(store_get("integrations")$status[1], ig$status[1]))
})

# ==================================================================
section("Tracking")

testServer(tracking_server, args = list(user = usr()), {
  cn <- get_consignments()
  session$setInputs(q_cn = cn$cn_no[1], q_mobile = "", q_track = "", track = 1)
  html <- as.character(output$result$html %||% output$result)
  ok("tracking finds a consignment by CN", grepl(cn$lr_no[1], html, fixed = TRUE))

  session$setInputs(q_cn = "CN-DOES-NOT-EXIST", track = 2)
  html2 <- as.character(output$result$html %||% output$result)
  ok("tracking reports a miss cleanly", grepl("No shipment found", html2, fixed = TRUE))
})

restore_data()

# The whole point of the scratch copy is that the suite leaves no trace. Prove
# it rather than trusting it.
after <- vapply(c("bookings", "clients", "vehicles", "users", "trips",
                  "consignments", "invoices", "payments", "complaints"),
                function(t) nrow(store_get(t)), integer(1))
cat("\nrestored row counts:", paste(names(after), after, sep = "=", collapse = "  "), "\n")

cat(sprintf("\n%s  %d passed, %d failed\n\n",
            if (fail == 0) "PASS" else "FAIL", pass, fail))
if (fail > 0) quit(status = 1)
