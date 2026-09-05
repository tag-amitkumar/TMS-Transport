# ==================================================================
# Minimal seed — two logins, one sample record per entity.
#
# For handing someone a clean system to enter their own data into, rather than
# the full generated dataset in seed.R. Selected by SEED_PROFILE = "minimal".
#
# Two deliberate departures from a literal "one of everything":
#
#  1. TWO bookings, not one. A single booking can only be in one state, and the
#     app's screens split across the lifecycle — an in-transit load is what
#     gives Live GPS and the e-way bill register something to show, while a
#     delivered-and-paid one is what gives POD, Invoices and Payments something
#     to show. One of each means every screen has exactly one row instead of
#     half the app looking broken.
#
#  2. TWO vehicles, drivers and clients. Allocation and the booking form are
#     both pick-from-a-list actions; with a single option there is nothing to
#     demonstrate and no way to see the capacity check reject a choice.
#
# Everything else is a single record. Reference data that is configuration
# rather than content — roles, the permission matrix, leave types, notification
# routing, integrations — is carried over in full, because those are features of
# the app, not sample data.
# ==================================================================

build_seed_minimal <- function() {

  TODAY <- Sys.Date()
  NOW   <- Sys.time()

  # Anchor the completed job inside the last closed month so the payroll run
  # always has trip allowances to pull, whatever day this is seeded on.
  month_start  <- lubridate::floor_date(TODAY, "month")
  done_dispatch <- month_start - 5
  done_deliver  <- month_start - 3
  period        <- format(month_start - 1, "%b %Y")

  pw <- bcrypt::hashpw("tms@2026")
  out <- list()

  # ---------------- Branches ----------------
  # Two, because a consignment needs an origin and a destination.
  out$branches <- tibble::tribble(
    ~branch_id, ~code,     ~name,          ~address,                      ~city,     ~state,        ~pincode, ~lat,   ~lon,   ~is_depot, ~manager,        ~contact,             ~gstin,                ~since,        ~status,
    "BR-001",   "BR-NGP",  "Nagpur (HQ)",  "Plot 14, MIDC, Hingna Rd",    "Nagpur",  "Maharashtra", "440001", 21.146, 79.088, "TRUE",    "Amardip Singh", "+91 98230 11223",    "27AAACA1234F1Z1",     "2016-04-01",  "Active",
    "BR-002",   "BR-DEL",  "Delhi",        "Okhla Industrial Area, Ph-2", "Delhi",   "Delhi",       "110001", 28.644, 77.216, "TRUE",    "Vikram Chauhan","+91 98110 44567",    "07AAACA1234F1Z2",     "2018-07-01",  "Active"
  )

  # ---------------- Users — the two logins ----------------
  out$users <- tibble::tribble(
    ~user_id,   ~name,           ~email,                         ~role,                ~branch_id, ~department,  ~mobile,            ~cross_branch,
    "USR-0001", "Amardip Singh", "admin@amardiptms.in",          "Super Admin",        "BR-001",   "Management", "+91 98230 11223",  "TRUE",
    "USR-0002", "Suresh Khan",   "operations@amardiptms.in",     "Operations Manager", "BR-001",   "Operations", "+91 98901 22345",  "FALSE"
  ) |>
    dplyr::mutate(password_hash = pw, client_id = "", vendor_id = "", status = "Active") |>
    dplyr::select(user_id, name, email, password_hash, role, branch_id,
                  department, mobile, cross_branch, client_id, vendor_id, status)

  # ---------------- Employees ----------------
  # Two drivers plus the operations coordinator, who is the same person as the
  # operations login — the app treats a driver as an employee with a licence,
  # and payroll reads this table, so the login needs a record here too.
  # Three drivers, not two. With only as many drivers as there are live trips
  # the fleet is fully committed the moment the seed loads, the allocation
  # dialog opens on empty lists, and the dispatcher's main action cannot be
  # demonstrated at all. One spare keeps the board workable.
  out$employees <- tibble::tribble(
    ~employee_id, ~name,           ~mobile,           ~designation,      ~department,  ~branch_id, ~joined,      ~pan,          ~salary_type, ~salary, ~is_driver,
    "EMP-0001",   "Ramesh Yadav",  "+91 98600 11223", "Driver",          "Fleet",      "BR-001",   "2022-03-14", "AKZPY1122M",  "per_trip",   18000,   "TRUE",
    "EMP-0002",   "Prakash Singh", "+91 98111 77880", "Driver",          "Fleet",      "BR-001",   "2023-01-09", "BLMPS3344Q",  "per_trip",   18000,   "TRUE",
    "EMP-0004",   "Vikas Dubey",   "+91 90040 12987", "Driver",          "Fleet",      "BR-001",   "2023-06-21", "DRTPD7788S",  "per_trip",   18000,   "TRUE",
    "EMP-0003",   "Suresh Khan",   "+91 98901 22345", "Ops Coordinator", "Operations", "BR-001",   "2021-11-02", "CNQPK5566R",  "monthly",    32000,   "FALSE"
  ) |>
    dplyr::mutate(
      aadhaar     = paste0("xxxx xxxx ", c("8814", "4471", "5590", "9026")),
      bank_masked = paste0(c("SBI", "HDFC", "Axis", "ICICI"), " ···· ",
                           c("4471", "8829", "6620", "3310")),
      docs_status = "complete",
      status      = "Active"
    ) |>
    dplyr::select(employee_id, name, mobile, designation, department, branch_id,
                  joined, aadhaar, pan, bank_masked, salary_type, salary,
                  is_driver, docs_status, status)

  # ---------------- Vehicles ----------------
  out$vehicles <- tibble::tribble(
    ~vehicle_id, ~reg_no,          ~model,               ~body,        ~capacity_t, ~owner_type, ~vendor_id,  ~driver_id,  ~branch_id, ~rc_number,
    "VEH-001",   "MH-31 GH 6612",  "Tata Signa 3523.TK", "32 ft MXL",  25,          "Company",   "",          "EMP-0001",  "BR-001",   "MH31201845612",
    "VEH-002",   "MH-31 KT 2210",  "Eicher Pro 6028",    "28 ft",      18,          "Vendor",    "VND-0001",  "EMP-0002",  "BR-001",   "MH31199033421",
    "VEH-003",   "MH-31 AB 4590",  "Tata LPT 1618",      "24 ft",      16,          "Company",   "",          "EMP-0004",  "BR-001",   "MH31215567890"
  ) |>
    dplyr::mutate(
      insurance_expiry = as.character(TODAY + c(240, 95, 310)),
      fitness_expiry   = as.character(TODAY + c(180, 60, 400)),
      permit_expiry    = as.character(TODAY + c(700, 520, 880)),
      # One document deliberately close to expiry, so the amber badge and the
      # allocation warning are visible without having to manufacture one.
      puc_expiry       = as.character(TODAY + c(150, 18, 260)),
      service_due_km   = c(120000, 90000, 140000),
      joined_fleet     = as.character(TODAY - c(900, 480, 260)),
      # Placeholder. Real status is derived from the trips further down —
      # hardcoding it here is what put a truck on a Running trip while the
      # register called it Available, so it got allocated a second load.
      status           = "Available"
    )

  # ---------------- Drivers ----------------
  out$drivers <- tibble::tribble(
    ~driver_id, ~name,           ~mobile,           ~licence_no,        ~licence_class, ~depot_branch_id, ~assigned_vehicle_id, ~emergency_contact, ~trips_lifetime, ~on_time_pct,
    "EMP-0001", "Ramesh Yadav",  "+91 98600 11223", "MH-31 0022214",    "HGV",          "BR-001",         "VEH-001",            "+91 98600 99887",  1,               100,
    "EMP-0002", "Prakash Singh", "+91 98111 77880", "MP-09 0033391",    "HMV",          "BR-001",         "VEH-002",            "+91 98111 66554",  1,               100,
    "EMP-0004", "Vikas Dubey",   "+91 90040 12987", "UP-32 0044482",    "HGV",          "BR-001",         "VEH-003",            "+91 90040 55221",  0,               100
  ) |>
    dplyr::mutate(
      licence_expiry = as.character(TODAY + c(420, 25, 610)),  # one expiring soon
      status         = "Available"                             # derived below
    )

  # ---------------- Clients ----------------
  out$clients <- tibble::tribble(
    ~client_id,  ~name,               ~contact_person, ~mobile,            ~email,                    ~gstin,             ~pan,          ~city,    ~state,        ~gst_mode, ~gst_pct, ~credit_limit, ~branch_id,
    "CUST-0001", "Shree Cement Ltd",  "Rahul Mehta",   "+91 98230 44112",  "rahul@example.com",       "27AAACS1234F1Z2",  "AAACS1234F",  "Nagpur", "Maharashtra", "RCM",     5,        1000000,       "BR-001",
    "CUST-0002", "BuildMart Traders", "Sanjay Gupta",  "+91 98100 33245",  "sanjay@example.com",      "07AABCB9922K1Z8",  "AABCB9922K",  "Delhi",  "Delhi",       "FCM",     18,       300000,        "BR-002"
  ) |>
    dplyr::mutate(
      billing_address  = c("Plot Gate 2, Hingna Rd, Nagpur", "Okhla Phase 2, New Delhi"),
      pickup_address   = c("Plant Gate 2, Hingna Rd, Nagpur", "Okhla Depot, New Delhi"),
      delivery_address = c("Okhla Depot, New Delhi", "MIDC Warehouse, Nagpur"),
      since            = as.character(TODAY - c(900, 400)),
      status           = "Active"
    ) |>
    dplyr::select(client_id, name, contact_person, mobile, email, gstin, pan, city,
                  state, billing_address, pickup_address, delivery_address,
                  credit_limit, gst_mode, gst_pct, branch_id, since, status)

  # ---------------- Vendor ----------------
  out$vendors <- tibble::tibble(
    vendor_id = "VND-0001", name = "Raghunath Transport Co",
    owner_name = "Raghunath Deshmukh", mobile = "+91 98901 22345",
    gstin = "27AABFR2211M1ZP", pan = "AABFR2211M", city = "Nagpur",
    branch_id = "BR-001", payment_terms = "15-day credit",
    bank_name = "HDFC Bank, Sitabuldi", account_no = "50100234567810",
    ifsc = "HDFC0000234", since = as.character(TODAY - 1200), status = "Active"
  )

  out$vendor_documents <- tibble::tibble(
    vendor_id = "VND-0001",
    doc_type  = c("Vendor Agreement", "PAN Card", "GST Certificate"),
    status    = c("Uploaded", "Uploaded", "Pending")
  )

  # ---------------- Routes ----------------
  # Both directions, so a return load can be quoted.
  out$pincode_routes <- tibble::tribble(
    ~route_id, ~src_pincode, ~src_city, ~dst_pincode, ~dst_city, ~distance_km, ~transit_days,
    "RT-0001", "440001",     "Nagpur",  "110001",     "Delhi",   1035,         3,
    "RT-0002", "110001",     "Delhi",   "440001",     "Nagpur",  1035,         3
  ) |>
    dplyr::mutate(
      route_path     = paste(src_city, "→", dst_city),
      branch_mapping = paste(c("Nagpur (HQ)", "Delhi"), "→", c("Delhi", "Nagpur (HQ)")),
      pickup_zone    = c("Zone Central", "Zone North"),
      delivery_zone  = c("Zone North", "Zone Central"),
      availability   = "Available"
    )

  # ---------------- Bookings ----------------
  # Three, one per stage of the lifecycle, so every operational screen opens
  # with something on it:
  #   BKG-0001  Delivered and paid  → POD, Invoices, Payments
  #   BKG-0002  In transit          → Live GPS, e-way bill, tracking
  #   BKG-0003  Confirmed, no truck → Cargo Moto's Unassigned lane, and gives
  #                                   Allocate Cargo something to act on.
  #                                   Without it the dispatcher's main action
  #                                   opens on "nothing awaiting allocation".
  out$bookings <- tibble::tribble(
    ~booking_no, ~booking_date,                   ~client_id,  ~origin_city, ~dest_city, ~material,                     ~weight_t, ~quantity, ~freight, ~status,
    "BKG-0001",  as.character(done_dispatch - 1), "CUST-0001", "Nagpur",     "Delhi",    "Cement bags — OPC 53 grade",  24.0,      480,       27600,    "Delivered",
    "BKG-0002",  as.character(TODAY - 2),         "CUST-0002", "Delhi",      "Nagpur",   "Construction hardware",       9.5,       120,       11600,    "In Transit",
    "BKG-0003",  as.character(TODAY),             "CUST-0001", "Nagpur",     "Delhi",    "Steel coils",                 15.0,      300,       22400,    "Confirmed"
  ) |>
    dplyr::mutate(
      branch_id        = c("BR-001", "BR-002", "BR-001"),
      pickup_address   = c("Plant Gate 2, Hingna Rd, Nagpur", "Okhla Depot, New Delhi",
                           "Plant Gate 2, Hingna Rd, Nagpur"),
      delivery_address = c("Okhla Depot, New Delhi", "MIDC Warehouse, Nagpur",
                           "Okhla Depot, New Delhi"),
      packages         = c("480 bags", "120 pkgs", "300 coils"),
      insurance        = c("Insured", "Not insured", "Not insured"),
      declared_value   = c(864000, 0, 0),
      gst_mode         = c("RCM", "FCM", "RCM"),
      gst_pct          = c(5, 18, 5),
      # Reverse charge collects no tax: the recipient discharges it.
      gst_amount       = c(0, round(11600 * 0.18), 0),
      insurance_amt    = c(350, 0, 0),
      total            = freight + gst_amount + insurance_amt,
      remarks          = c("Handle with care · fragile packaging", "", "Awaiting vehicle"),
      booking_user_id  = "USR-0002",
      priority         = "Normal",

      # One of each term that can be shown standing still. "Paid" is left for
      # the operator to create live, since a new booking is the natural place
      # to demonstrate it.
      #   Credit → the ordinary monthly-billing flow, already invoiced
      #   To Pay → driver collects on delivery, so it shouts on the LR
      #   TBB    → customer's account sits at Delhi, so Delhi raises the bill
      payment_mode      = c("Credit", "To Pay", "TBB"),
      bill_at_branch_id = c("", "", "BR-002"),

      # BKG-0002 was written in the paper LR book while TMS was unreachable and
      # keyed in afterwards, so it carries the original reference and the time
      # the load was actually accepted — not the time someone typed it up.
      entry_mode = c("Online", "Manual", "Online"),
      manual_ref = c("", "DEL/LR/4471", ""),
      manual_dt  = c("", paste(TODAY - 2, "07:40:00"), ""),

      # Geography is a PIN pair resolved through the same master the booking
      # form uses — the industrial pincodes at each end, not the head-office
      # ones, since that is where the goods actually move between. The distance
      # is the figure quoted at booking time, frozen onto the row.
      origin_pincode = c("440016", "110020", "440016"),
      dest_pincode   = c("110020", "440016", "110020"),
      origin_state   = c("Maharashtra", "Delhi", "Maharashtra"),
      dest_state     = c("Delhi", "Maharashtra", "Delhi"),
      distance_km    = 1035
    ) |>
    dplyr::select(booking_no, booking_date, branch_id, client_id, pickup_address,
                  delivery_address, origin_city, dest_city,
                  origin_pincode, dest_pincode, origin_state, dest_state,
                  distance_km, material, weight_t,
                  quantity, packages, insurance, declared_value, freight, gst_mode,
                  gst_pct, gst_amount, insurance_amt, total, remarks,
                  booking_user_id, priority, payment_mode, bill_at_branch_id,
                  entry_mode, manual_ref, manual_dt, status)

  # ---------------- Trips ----------------
  out$trips <- tibble::tribble(
    ~trip_no,   ~booking_no, ~vehicle_id, ~driver_id, ~vendor_id, ~branch_id, ~route_from, ~route_to, ~status,
    "TRP-0001", "BKG-0001",  "VEH-001",   "EMP-0001", "",         "BR-001",   "Nagpur",    "Delhi",   "Completed",
    "TRP-0002", "BKG-0002",  "VEH-002",   "EMP-0002", "VND-0001", "BR-002",   "Delhi",     "Nagpur",  "Running"
  ) |>
    dplyr::mutate(
      distance_km      = 1035,
      dispatch_dt      = c(paste(done_dispatch, "06:10:00"), paste(TODAY - 1, "18:00:00")),
      eta              = c(paste(done_deliver, "14:30:00"),  paste(TODAY + 2, "09:00:00")),
      border_allowance = 1000,
      food_allowance   = 400,
      # Tracks the allowance it funds, so recovering it never wipes out a wage.
      advance          = c(1600, 1500)
    )

  # Fleet status is DERIVED from the trips, never asserted alongside them.
  # Stating it twice is how a truck ended up marked Available while it was out
  # on a Running trip — and then got allocated a second load on top of the one
  # it was already carrying.
  live <- out$trips[out$trips$status %in% c("Planned", "Loading", "Running", "At Hub"), ]
  out$vehicles$status <- ifelse(
    out$vehicles$vehicle_id %in% live$vehicle_id[live$status %in% c("Running", "At Hub")],
    "In Transit",
    ifelse(out$vehicles$vehicle_id %in% live$vehicle_id, "Allocated", "Available"))
  out$drivers$status <- ifelse(out$drivers$driver_id %in% live$driver_id,
                               "On trip", "Available")

  # ---------------- Consignments ----------------
  out$consignments <- tibble::tribble(
    ~cn_no,     ~lr_no,     ~booking_no, ~trip_no,   ~client_id,  ~vehicle_id, ~driver_id, ~branch_id, ~origin_city, ~dest_city, ~weight_t, ~freight, ~status,
    "CN-00001", "LR-0001",  "BKG-0001",  "TRP-0001", "CUST-0001", "VEH-001",   "EMP-0001", "BR-001",   "Nagpur",     "Delhi",    24.0,      27600,    "Delivered",
    "CN-00002", "LR-0002",  "BKG-0002",  "TRP-0002", "CUST-0002", "VEH-002",   "EMP-0002", "BR-002",   "Delhi",      "Nagpur",   9.5,       11600,    "In Transit"
  ) |>
    dplyr::mutate(
      origin_addr       = c("Plant Gate 2, Hingna Rd, Nagpur", "Okhla Depot, New Delhi"),
      dest_addr         = c("Okhla Depot, New Delhi", "MIDC Warehouse, Nagpur"),
      dispatch_date     = as.character(c(done_dispatch, TODAY - 1)),
      expected_delivery = as.character(c(done_deliver, TODAY + 2)),
      delivered_date    = c(as.character(done_deliver), ""),
      parent_cn_no      = "",
      # Payment terms travel with the load. The driver reads them off the LR to
      # know whether to collect before releasing the goods, so they are copied
      # onto the consignment rather than looked up through the booking.
      payment_mode      = c("Credit", "To Pay"),
      bill_at_branch_id = c("", "")
    )

  # Timeline: the delivered one has run the full course, the live one is midway.
  steps <- c("Booking Created", "Vehicle Allocated", "Picked Up", "In Transit",
             "At Hub", "Out For Delivery", "Delivered", "POD Uploaded")
  ev <- function(cn, n, start) {
    tibble::tibble(
      cn_no    = cn,
      seq      = seq_len(n),
      event    = steps[seq_len(n)],
      detail   = "",
      event_dt = as.character(as.POSIXct(start, tz = "Asia/Kolkata") +
                                cumsum(rep(9, n)) * 3600)
    )
  }
  out$consignment_events <- dplyr::bind_rows(
    ev("CN-00001", 8, paste(done_dispatch, "06:00:00")),
    ev("CN-00002", 4, paste(TODAY - 1, "18:00:00"))
  )

  # ---------------- E-way bill ----------------
  # Only the consignment actually on the road carries one; a delivered
  # consignment's bill is spent and drops out of the register.
  out$ewaybills <- tibble::tibble(
    ewb_no = "7213 4456 8890", invoice_no = "BM/26-27/0002",
    cn_no = "CN-00002", lr_no = "LR-0002",
    gstin = "07AABCB9922K1Z8", vehicle_id = "VEH-002",
    transporter_id = "27AAACA1234F1ZT",
    valid_from = paste(TODAY - 1, "18:00:00"),
    valid_to   = format(NOW + 2 * 86400, "%Y-%m-%d %H:%M:%S"),
    status     = "Valid"
  )

  # ---------------- POD ----------------
  out$pods <- tibble::tribble(
    ~pod_id,    ~lr_no,    ~cn_no,     ~client_id,  ~branch_id, ~uploaded_by,   ~file_name,      ~status,
    "POD-0001", "LR-0001", "CN-00001", "CUST-0001", "BR-001",   "Ramesh Yadav", "POD_0001.jpg",  "Approved",
    "POD-0002", "LR-0002", "CN-00002", "CUST-0002", "BR-002",   "",             "",              "Pending"
  ) |>
    dplyr::mutate(upload_dt = c(paste(done_deliver, "17:05:00"), ""))

  # ---------------- Live GPS ----------------
  # One ping for the vehicle on the road, placed midway down the corridor.
  out$gps_pings <- tibble::tibble(
    vehicle_id = "VEH-002", trip_no = "TRP-0002",
    lat = 24.85, lon = 78.10, speed = 51, heading = "S",
    ignition = "ON", idle_min = 12,
    ts = format(NOW - 240, "%Y-%m-%d %H:%M:%S"),
    status = "Running"
  )

  # ---------------- Invoice & receipt ----------------
  # Raised against the delivered consignment, whose POD is approved — the gate
  # the app enforces. RCM, so no tax is collected and the total is the freight.
  # Left PARTIALLY paid on purpose. A fully settled book makes every money
  # figure on the app read zero — Customer Outstanding, Top Outstanding
  # Clients, the client 360° balance — and leaves Record Receipt with no
  # invoice to act on, so the reconciliation behaviour cannot be shown at all.
  # ₹15,000 received against ₹27,600 leaves a live balance to settle.
  out$invoices <- tibble::tibble(
    invoice_no = "INV-0001", invoice_date = as.character(done_deliver),
    client_id = "CUST-0001", branch_id = "BR-001",
    lr_no = "LR-0001", cn_no = "CN-00001",
    amount = 27600, gst_mode = "RCM", gst_pct = 5, gst_amount = 0,
    total = 27600, paid_amount = 15000,
    due_date = as.character(done_deliver + 14),
    source = "auto", payment_mode = "Credit", status = "Partially paid"
  )

  out$payments <- tibble::tibble(
    payment_id = "RCP-0001", date = as.character(done_deliver + 6),
    client_id = "CUST-0001", invoice_no = "INV-0001",
    mode = "NEFT", reference = "UTR7712880", amount = 15000,
    received_by = "Amardip Singh"
  )

  # The vendor's vehicle is still out, so nothing is settleable yet.
  out$vendor_payments <- tibble::tibble(
    vp_id = character(), vendor_id = character(), trips = character(),
    amount = character(), due_date = character(), status = character(),
    approved_by = character()
  )

  # ---------------- Complaint ----------------
  out$complaints <- tibble::tibble(
    complaint_id = "CMP-0001", client_id = "CUST-0001",
    lr_no = "LR-0001", cn_no = "CN-00001", branch_id = "BR-001",
    category = "POD Issue", priority = "Medium",
    subject = "POD copy not received by consignee",
    raised_dt = as.character(TODAY - 1), target_dt = as.character(TODAY + 1),
    resolved_dt = "", assigned_to = "USR-0002", progress_pct = 40,
    resolution = "", status = "In Progress"
  )

  # ---------------- HRMS ----------------
  out$leave_types <- tibble::tibble(
    code = c("CL", "SL", "EL"),
    name = c("Casual (CL)", "Sick (SL)", "Earned (EL)"),
    annual_quota = c(8, 7, 15)
  )

  # Fourteen days of register, with the driver's duty days derived from the
  # trips rather than punched — the rule the attendance screen documents.
  days <- seq(TODAY - 13, TODAY, by = "day")
  trip_days <- tibble::tibble(
    employee_id = c("EMP-0001", "EMP-0002"),
    d = as.Date(c(done_dispatch, TODAY - 1)),
    trip_no = c("TRP-0001", "TRP-0002")
  )
  out$attendance <- tidyr::crossing(employee_id = out$employees$employee_id, date = days) |>
    dplyr::left_join(trip_days, by = c("employee_id", "date" = "d")) |>
    dplyr::mutate(
      status = dplyr::case_when(
        !is.na(trip_no)                 ~ "T",
        lubridate::wday(date) == 1      ~ "off",
        TRUE                            ~ "P"
      ),
      leave_type = "",
      trip_no    = tidyr::replace_na(trip_no, ""),
      date       = as.character(date)
    ) |>
    dplyr::select(employee_id, date, status, trip_no, leave_type)

  out$leave_requests <- tibble::tibble(
    lr_id = "LVR-0001", employee_id = "EMP-0002", leave_type = "CL",
    from_date = as.character(TODAY + 4), to_date = as.character(TODAY + 5),
    days = 2, reason = "family function",
    applied_dt = as.character(TODAY - 1), status = "Pending"
  )

  # Payroll for the closed month, allowances pulled from trips in that window.
  tb <- out$trips |>
    dplyr::filter(format(as.Date(dispatch_dt), "%b %Y") == period) |>
    dplyr::group_by(driver_id) |>
    dplyr::summarise(allow = sum(border_allowance + food_allowance),
                     adv = sum(advance), .groups = "drop")

  out$payroll <- out$employees |>
    dplyr::left_join(tb, by = c("employee_id" = "driver_id")) |>
    dplyr::mutate(
      allow = tidyr::replace_na(allow, 0),
      adv   = tidyr::replace_na(adv, 0),
      basic = round(salary * .5), hra = round(salary * .3),
      da    = salary - round(salary * .5) - round(salary * .3),
      incentive = 0, ot = 0,
      trip_allowance = allow, advances = adv,
      pf  = round(basic * .12),
      esi = ifelse(salary <= 21000, round(salary * .0075), 0),
      pt  = 200,
      net = basic + hra + da + incentive + ot + trip_allowance - advances - pf - esi - pt,
      period = period,
      status = ifelse(net < 0, "On hold", "Ready")
    ) |>
    dplyr::select(period, employee_id, basic, hra, da, incentive, ot,
                  trip_allowance, advances, pf, esi, pt, net, status)

  # ---------------- Roles, permissions, configuration ----------------
  # Carried over in full: these are features of the app, not sample data. The
  # Security screen would be meaningless with two roles in its matrix.
  out$roles <- tibble::tibble(
    role = ROLE_LEVELS,
    description = c(
      "Full system access, all branches",
      "Full access scoped to own branch",
      "Bookings, consignments, fleet ops",
      "Creates & edits bookings, LRs",
      "Vehicle allocation & dispatch",
      "Invoices, payments, GST, payroll",
      "Employees, attendance, leave, payroll",
      "Mobile app · trip & POD updates",
      "Assigned trips, POD upload, payments",
      "Self-service portal · own shipments only"
    )
  )
  out$permissions <- DEFAULT_PERMISSIONS

  out$settings <- tibble::tribble(
    ~key,                    ~value,
    "company_name",          "Amardip Road Carriers",
    "company_gstin",         "27AAACA1234F1Z1",
    "registered_office",     "Plot 14, MIDC, Hingna Rd, Nagpur 440016",
    "support_email",         "support@amardiptms.in",
    "support_phone",         "+91 98230 11223",
    "base_currency",         "INR",
    "timezone",              "Asia/Kolkata",
    "date_format",           "DD-MMM-YYYY",
    "fy_start",              "01 April",
    "retain_gps_months",     "18",
    "retain_docs_months",    "36",
    "retain_finance_years",  "8",
    "retain_notif_months",   "12"
  )

  out$notification_events <- tibble::tribble(
    ~event,                        ~email, ~sms, ~whatsapp, ~inapp,
    "Booking Created",              1, 1, 1, 1,
    "Vehicle Assigned",             0, 1, 1, 1,
    "Shipment Dispatched",          1, 1, 1, 1,
    "Shipment Delivered",           1, 1, 1, 1,
    "POD Uploaded",                 1, 0, 1, 1,
    "E-Way Bill Expiry",            1, 1, 0, 1,
    "Vehicle Maintenance Reminder", 1, 0, 0, 1,
    "Complaint Updates",            1, 0, 1, 1,
    "Payroll Generated",            1, 0, 0, 1,
    "Leave Approval",               1, 0, 0, 1
  )

  out$integrations <- tibble::tribble(
    ~name,                    ~provider,                           ~status,
    "WhatsApp Business API",  "Meta Cloud API via Gupshup",        "Not configured",
    "Transactional Email",    "SendGrid",                          "Not configured",
    "SMS Gateway",            "MSG91",                             "Not configured",
    "GST / e-Way Bill (GSP)", "GSTR-1/3B · e-way generate/cancel", "Not configured",
    "GPS / Telematics",       "OEM feeds — live tracking",         "Not configured",
    "Payment Gateway",        "Razorpay — customer bill payments", "Not configured"
  )

  out$audit_log <- tibble::tibble(
    ts = character(), user_id = character(),
    action = character(), module = character(), detail = character()
  )

  out
}
