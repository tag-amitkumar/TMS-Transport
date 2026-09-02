# ==================================================================
# Synthetic seed data.
#
# Generates one internally consistent dataset for the whole app. Run with
#   Rscript R/seed.R
# or let app.R call seed_if_empty() on first start.
#
# Two things this file is deliberate about:
#
# 1. THE NUMBERS RECONCILE. The design deck's screens contradicted each other
#    (156 vs 62 vs 52 vehicles; 18 vs 9 branches; 81 employees but "142/150
#    drivers" in HR reports). Those were mockup artifacts. Here one set of
#    entities is generated and every count on every screen is derived from it,
#    so the dashboard, the vehicle master and the GPS feed always agree.
#
# 2. EVERYTHING IS FICTIONAL. This repo is public. Names, GSTINs, PANs, bank
#    accounts, Aadhaar fragments and phone numbers below are generated from
#    fixed patterns and belong to no real person or company. Phone numbers use
#    the +91 98xxx reserved-looking block; GSTINs are structurally shaped but
#    carry an invalid checksum on purpose so they cannot be mistaken for live
#    registrations.
# ==================================================================

# Fixed seed keeps the committed CSVs reproducible — regenerating should
# produce the same data, so a re-seed shows up as an empty diff rather than
# 3,000 changed rows.
set.seed(20260901)

TODAY <- Sys.Date()
NOW   <- Sys.time()

# ------------------------------------------------------------------
# Reference vocabulary
# ------------------------------------------------------------------

FIRST <- c("Amardip","Vikram","Suresh","Rohit","Poonam","Neha","Devendra","Kavita",
           "Ramesh","Prakash","Vikas","Anil","Manoj","Gopal","Santosh","Rajesh",
           "Deepak","Ganesh","Jitendra","Meena","Amit","Priya","Mahesh","Pooja",
           "Sunita","Arun","Kiran","Nitin","Sanjay","Alok","Kishor","Jignesh",
           "Rahul","Arif","Sameer","Raghunath","Jaspreet","Ravi","Suraj","Snehal",
           "Farida","Imran","Lata","Nandini","Yogesh","Bhavna","Tarun","Shalini")

LAST  <- c("Singh","Chauhan","Khan","Kulkarni","Jain","Agrawal","Sahu","Sharma",
           "Yadav","Dubey","Verma","Jha","Kale","Pawar","More","Chavan","Rathod",
           "Rao","Kulkarni","Nair","Kamble","Patil","Deshmukh","Sai","Malhotra",
           "Wankhede","Mehta","Gupta","Sinha","Patel","Shaikh","Joshi","Bhosale")

# City, state, pincode, and whether the branch is a routing hub/depot. Nine of
# the eighteen are depots — which is what the Pin Code Mapping screen counts,
# reconciling its "9 branches" against the branch master's 18.
CITIES <- tibble::tribble(
  ~city,        ~state,           ~pin,     ~depot, ~lat,   ~lon,
  "Nagpur",     "Maharashtra",    "440001", TRUE,   21.146, 79.088,
  "Delhi",      "Delhi",          "110001", TRUE,   28.644, 77.216,
  "Mumbai",     "Maharashtra",    "400001", TRUE,   19.076, 72.877,
  "Pune",       "Maharashtra",    "411001", TRUE,   18.520, 73.856,
  "Indore",     "Madhya Pradesh", "452001", TRUE,   22.720, 75.857,
  "Raipur",     "Chhattisgarh",   "492001", TRUE,   21.251, 81.630,
  "Jaipur",     "Rajasthan",      "302001", TRUE,   26.912, 75.787,
  "Surat",      "Gujarat",        "395001", TRUE,   21.170, 72.831,
  "Kanpur",     "Uttar Pradesh",  "208001", TRUE,   26.449, 80.331,
  "Nashik",     "Maharashtra",    "422001", FALSE,  19.997, 73.789,
  "Bhopal",     "Madhya Pradesh", "462001", FALSE,  23.259, 77.412,
  "Lucknow",    "Uttar Pradesh",  "226001", FALSE,  26.846, 80.946,
  "Ahmedabad",  "Gujarat",        "380001", FALSE,  23.022, 72.571,
  "Hyderabad",  "Telangana",      "500001", FALSE,  17.385, 78.486,
  "Kolkata",    "West Bengal",    "700001", FALSE,  22.573, 88.364,
  "Bengaluru",  "Karnataka",      "560001", FALSE,  12.972, 77.594,
  "Gurugram",   "Haryana",        "122001", FALSE,  28.459, 77.027,
  "Morbi",      "Gujarat",        "363641", FALSE,  22.812, 70.837
)

MATERIALS <- tibble::tribble(
  ~material,                  ~unit,   ~dens,
  "Cement bags — OPC 53 grade","bags",  1.00,
  "Steel coils",               "coils", 1.20,
  "TMT bars",                  "bundles",1.10,
  "FMCG cartons",              "pkgs",  0.55,
  "Agro produce",              "bags",  0.70,
  "Polymer granules",          "bags",  0.80,
  "Construction hardware",     "pkgs",  0.90,
  "Ceramic tiles",             "boxes", 1.05,
  "Copper wire coils",         "coils", 1.15,
  "Packaged goods",            "pkgs",  0.60,
  "PVC granules",              "bags",  0.75,
  "Textile bales",             "bales", 0.50
)

VEHICLE_TYPES <- tibble::tribble(
  ~body,           ~cap,  ~model,
  "32 ft MXL",     25,    "Tata Signa 3523.TK",
  "32 ft SXL",     25,    "Ashok Leyland 3520",
  "Trailer 40 ft", 30,    "BharatBenz 4023",
  "28 ft",         18,    "Eicher Pro 6028",
  "24 ft",         16,    "Tata LPT 1618",
  "19 ft",         10,    "Eicher Pro 2114",
  "22 ft Taurus",  21,    "Ashok Leyland 2820"
)

STATE_RTO <- c("Maharashtra"="MH","Delhi"="DL","Madhya Pradesh"="MP",
               "Chhattisgarh"="CG","Rajasthan"="RJ","Gujarat"="GJ",
               "Uttar Pradesh"="UP","Telangana"="TS","West Bengal"="WB",
               "Karnataka"="KA","Haryana"="HR","Nagaland"="NL")

STATE_GST <- c("Maharashtra"="27","Delhi"="07","Madhya Pradesh"="23",
               "Chhattisgarh"="22","Rajasthan"="08","Gujarat"="24",
               "Uttar Pradesh"="09","Telangana"="36","West Bengal"="19",
               "Karnataka"="29","Haryana"="06","Nagaland"="13")

# ------------------------------------------------------------------
# Generators
#
# Structurally plausible, deliberately invalid identifiers — see the header
# note. The GSTIN check digit is fixed at "Z" + a filler rather than computed.
# ------------------------------------------------------------------

gen_name   <- function(n) paste(sample(FIRST, n, TRUE), sample(LAST, n, TRUE))
gen_mobile <- function(n) paste0("+91 9", sprintf("%04d", sample(8000:8999, n, TRUE)),
                                 " ", sprintf("%05d", sample(10000:99999, n, TRUE)))
gen_pan    <- function(n) paste0(
  replicate(n, paste0(sample(LETTERS, 5, TRUE), collapse = "")),
  sprintf("%04d", sample(1000:9999, n, TRUE)),
  sample(LETTERS, n, TRUE))
gen_gstin  <- function(state, pan) paste0(STATE_GST[state] %||% "27", pan, "1ZX")
gen_aadhaar<- function(n) paste0("xxxx xxxx ", sprintf("%04d", sample(1000:9999, n, TRUE)))
gen_ifsc   <- function(n) paste0(sample(c("HDFC","ICIC","SBIN","AXIS","KKBK"), n, TRUE),
                                 "0", sprintf("%06d", sample(100000:999999, n, TRUE)))
gen_acct   <- function(n) sprintf("%014.0f", sample(10000000000000:99999999999999, n, TRUE))

pick <- function(x, n, ...) sample(x, n, replace = TRUE, ...)

# ==================================================================
# Build
# ==================================================================

build_seed <- function() {

  out <- list()

  # ---------------- Branches (18) ----------------
  nb <- nrow(CITIES)
  branch_mgr <- gen_name(nb)
  branch_mgr[1] <- "Amardip Singh"
  branches <- tibble::tibble(
    branch_id  = sprintf("BR-%03d", seq_len(nb)),
    code       = paste0("BR-", toupper(substr(CITIES$city, 1, 3))),
    name       = ifelse(seq_len(nb) == 1, paste0(CITIES$city, " (HQ)"), CITIES$city),
    address    = paste0(c("Plot 14, MIDC, Hingna Rd","Okhla Industrial Area, Ph-2",
                          "Bhiwandi Logistics Park","Chakan MIDC Phase 2",
                          "Sanwer Rd Transport Nagar","Urla Industrial Area",
                          "Sitapura Industrial Area","Sachin GIDC",
                          "Transport Nagar, Dadanagar","Satpur MIDC",
                          "Govindpura Industrial Area","Amausi Industrial Area",
                          "Naroda GIDC","Jeedimetla Ph-3","Howrah Transport Hub",
                          "Peenya Industrial Area","Sector 34 Logistics Park",
                          "Ceramic Zone, Lakhdhirpur Rd")),
    city       = CITIES$city,
    state      = CITIES$state,
    pincode    = CITIES$pin,
    lat        = CITIES$lat,
    lon        = CITIES$lon,
    is_depot   = CITIES$depot,
    manager    = branch_mgr,
    contact    = gen_mobile(nb),
    gstin      = paste0(STATE_GST[CITIES$state], "AAACA1234F1Z", seq_len(nb) %% 10),
    since      = as.character(seq(as.Date("2016-01-01"), by = "5 months", length.out = nb)),
    # One inactive branch, mirroring the deck's Jaipur row.
    status     = ifelse(seq_len(nb) == 7, "Inactive", "Active")
  )
  out$branches <- branches
  bid <- branches$branch_id

  # ---------------- Employees (81) ----------------
  # 48 drivers + 33 office/ops/mechanics, which is what makes the HRMS
  # Employees filter chips (Office 15 / Operations 12 / Mechanics 6 / Drivers 48)
  # add up to the 81 header count.
  n_drv <- 48L; n_off <- 15L; n_ops <- 12L; n_mec <- 6L
  n_emp <- n_drv + n_off + n_ops + n_mec

  emp_role <- c(rep("Driver", n_drv), rep("Office", n_off),
                rep("Operations", n_ops), rep("Mechanic", n_mec))

  desig <- ifelse(emp_role == "Driver", "Driver",
           ifelse(emp_role == "Mechanic", "Mechanic",
           ifelse(emp_role == "Operations",
                  pick(c("Ops Coordinator","Dispatcher","Booking Executive"), n_emp),
                  pick(c("Sales Executive","Accountant","HR Executive","Admin Executive"), n_emp))))

  dept <- ifelse(emp_role == "Driver", "Fleet",
          ifelse(emp_role == "Mechanic", "Maintenance",
          ifelse(emp_role == "Operations", "Operations",
                 ifelse(desig == "Accountant", "Accounts",
                        ifelse(desig == "HR Executive", "Human Resources", "Sales")))))

  emp_names <- gen_name(n_emp)
  # Pin the handful of people the deck names, so the seeded app opens on the
  # same records the mockups show.
  emp_names[1:8] <- c("Ramesh Yadav","Suresh Khan","Prakash Singh","Vikas Dubey",
                      "Anil Verma","Manoj Jha","Gopal Kale","Santosh Pawar")
  emp_names[n_drv + 1] <- "Amit Kulkarni"
  emp_names[n_drv + 2] <- "Priya Joshi"
  emp_names[n_drv + 3] <- "Meena Rao"
  emp_names[n_drv + n_off + 1] <- "Pooja Nair"
  emp_names[n_drv + n_off + n_ops + 1] <- "Mahesh Kamble"

  desig[n_drv + 1] <- "Sales Executive"; dept[n_drv + 1] <- "Sales"
  desig[n_drv + 2] <- "Sales Executive"; dept[n_drv + 2] <- "Sales"
  desig[n_drv + 3] <- "Accountant";      dept[n_drv + 3] <- "Accounts"
  desig[n_drv + n_off + 1] <- "Ops Coordinator"; dept[n_drv + n_off + 1] <- "Operations"

  emp_branch <- pick(bid, n_emp, prob = c(.22, .12, .10, .08, .07, .07, .05, .05,
                                          .05, .04, .03, .03, .02, .02, .02, .01, .01, .01))
  emp_pan <- gen_pan(n_emp)

  employees <- tibble::tibble(
    employee_id = sprintf("EMP-%04d", seq_len(n_emp)),
    name        = emp_names,
    mobile      = gen_mobile(n_emp),
    designation = desig,
    department  = dept,
    branch_id   = emp_branch,
    joined      = as.character(TODAY - sample(200:2000, n_emp, TRUE)),
    aadhaar     = gen_aadhaar(n_emp),
    pan         = emp_pan,
    bank_masked = paste0(pick(c("SBI","HDFC","ICICI","Axis"), n_emp), " ···· ",
                         sprintf("%04d", sample(1000:9999, n_emp, TRUE))),
    # Drivers are paid per trip plus allowances; everyone else monthly. This is
    # what makes the payroll screen's "TRIP ALLOW. (AUTO)" column meaningful.
    salary_type = ifelse(emp_role == "Driver", "per_trip", "monthly"),
    salary      = ifelse(emp_role == "Driver", 18000,
                  ifelse(emp_role == "Mechanic", 22000,
                         sample(seq(24000, 42000, 2000), n_emp, TRUE))),
    is_driver   = ifelse(emp_role == "Driver", "TRUE", "FALSE"),
    docs_status = pick(c("complete","complete","complete","complete",
                         "ESIC card missing","PAN pending"), n_emp),
    status      = "Active"
  )

  # ---------------- Vehicles (62) ----------------
  n_veh <- 62L
  vt <- VEHICLE_TYPES[pick(seq_len(nrow(VEHICLE_TYPES)), n_veh), ]
  veh_branch <- pick(bid, n_veh, prob = c(.20,.12,.10,.09,.08,.08,.05,.05,.05,.04,.03,.03,.02,.02,.02,.01,.01,.00))
  veh_state  <- branches$state[match(veh_branch, branches$branch_id)]

  reg_no <- paste0(
    STATE_RTO[veh_state], "-", sprintf("%02d", sample(1:45, n_veh, TRUE)), " ",
    replicate(n_veh, paste0(sample(LETTERS, 2, TRUE), collapse = "")), " ",
    sprintf("%04d", sample(1000:9999, n_veh, TRUE))
  )
  reg_no <- make.unique(reg_no, sep = "")

  # 26 of 62 are vendor-owned, matching the deck's mix of "Company owned" and
  # "Vendor — <name>" rows on the vehicle master.
  owner_type <- c(rep("Company", 36), rep("Vendor", 26))[sample(n_veh)]

  vehicles <- tibble::tibble(
    vehicle_id       = sprintf("VEH-%03d", seq_len(n_veh)),
    reg_no           = reg_no,
    model            = vt$model,
    body             = vt$body,
    capacity_t       = vt$cap,
    owner_type       = owner_type,
    vendor_id        = "",            # filled after vendors exist
    driver_id        = "",            # filled after driver assignment
    branch_id        = veh_branch,
    rc_number        = paste0(gsub("[^A-Z0-9]", "", reg_no), sprintf("%04d", sample(1000:9999, n_veh, TRUE))),
    insurance_expiry = as.character(TODAY + sample(-20:500, n_veh, TRUE)),
    fitness_expiry   = as.character(TODAY + sample(-15:600, n_veh, TRUE)),
    permit_expiry    = as.character(TODAY + sample(60:1200, n_veh, TRUE)),
    puc_expiry       = as.character(TODAY + sample(-10:400, n_veh, TRUE)),
    service_due_km   = sample(seq(60000, 200000, 10000), n_veh, TRUE),
    joined_fleet     = as.character(TODAY - sample(300:2200, n_veh, TRUE)),
    status           = "Available"
  )

  # ---------------- Drivers (48) — an extension of employees ----------------
  # The deck's driver panel says "ONE RECORD, EVERYWHERE": the driver master is
  # not a second people table, it is the licence/depot/performance extension of
  # the employee record. driver_id therefore *is* the employee_id.
  drv_emp <- employees[employees$is_driver == "TRUE", ]
  drivers <- tibble::tibble(
    driver_id        = drv_emp$employee_id,
    name             = drv_emp$name,
    mobile           = drv_emp$mobile,
    licence_no       = paste0(STATE_RTO[branches$state[match(drv_emp$branch_id, bid)]], "-",
                              sprintf("%02d", sample(1:40, n_drv, TRUE)), " ",
                              sprintf("%07d", sample(10000:9999999, n_drv, TRUE))),
    licence_class    = pick(c("HGV","HMV","HTV"), n_drv),
    licence_expiry   = as.character(TODAY + sample(-5:1500, n_drv, TRUE)),
    depot_branch_id  = drv_emp$branch_id,
    assigned_vehicle_id = "",
    emergency_contact= gen_mobile(n_drv),
    trips_lifetime   = sample(40:230, n_drv, TRUE),
    on_time_pct      = sample(78:98, n_drv, TRUE),
    status           = "Available"
  )

  # Give each driver a vehicle from their own depot where one is free — this is
  # what makes the vehicle master's DRIVER column and the driver master's
  # ASSIGNED VEHICLE column agree instead of pointing at each other randomly.
  free <- vehicles$vehicle_id
  for (i in seq_len(nrow(drivers))) {
    cand <- vehicles$vehicle_id[vehicles$branch_id == drivers$depot_branch_id[i] &
                                  vehicles$vehicle_id %in% free]
    if (!length(cand)) cand <- free
    if (!length(cand)) break
    v <- cand[1]
    drivers$assigned_vehicle_id[i] <- v
    vehicles$driver_id[vehicles$vehicle_id == v] <- drivers$driver_id[i]
    free <- setdiff(free, v)
  }

  # ---------------- Clients (214) ----------------
  n_cli <- 214L
  cli_stub <- c("Shree Cement","Vardhman Steels","Om Agro Traders","Kalpana Polymers",
                "Deccan FMCG Distributors","BuildMart Traders","Sunrise Ceramics",
                "Metro Wires","Suresh Industries","Om Steel & Alloys","Bhavani Traders",
                "Kalyani Textiles","Vardhman Cement","Raghav Agro","Sagar Chemicals",
                "Nova Packaging","Pinnacle Hardware","Everest Minerals","Trimurti Foods",
                "Ashoka Plastics","Konark Ceramics","Riddhi Metals","Sankalp Agro",
                "Gokul Dairy Supplies","Vishwas Engineering")
  cli_suffix <- c("Ltd","Pvt Ltd","& Co","Industries","Corporation","Enterprises")
  cli_names <- make.unique(
    paste(pick(cli_stub, n_cli), pick(cli_suffix, n_cli)), sep = " "
  )
  cli_names[1:8] <- c("Shree Cement Ltd","Vardhman Steels","Om Agro Traders",
                      "Kalpana Polymers","Deccan FMCG Distributors","BuildMart Traders",
                      "Sunrise Ceramics","Metro Wires Pvt Ltd")

  cli_city_idx <- pick(seq_len(nb), n_cli)
  cli_pan <- gen_pan(n_cli)

  # GST treatment is stored per client, not inferred. The deck showed RCM 5%,
  # FCM 12% and FCM 18% with only the hint "RCM flagged per client
  # configuration" — most GTA freight sits on reverse charge at 5%.
  gst_mode <- pick(c("RCM","RCM","RCM","RCM","FCM"), n_cli)
  gst_pct  <- ifelse(gst_mode == "RCM", 5, pick(c(12, 18), n_cli))

  clients <- tibble::tibble(
    client_id     = sprintf("CUST-%04d", seq_len(n_cli)),
    name          = cli_names,
    contact_person= gen_name(n_cli),
    mobile        = gen_mobile(n_cli),
    email         = paste0(tolower(gsub("[^a-z]", "", tolower(substr(cli_names, 1, 10)))),
                           "@example.com"),
    gstin         = gen_gstin(CITIES$state[cli_city_idx], cli_pan),
    pan           = cli_pan,
    city          = CITIES$city[cli_city_idx],
    state         = CITIES$state[cli_city_idx],
    billing_address = paste0("Plot ", sample(2:90, n_cli, TRUE), ", ",
                             pick(c("MIDC","GIDC","Industrial Area","Transport Nagar"), n_cli),
                             ", ", CITIES$city[cli_city_idx]),
    pickup_address  = paste0("Gate ", sample(1:6, n_cli, TRUE), ", ",
                             CITIES$city[cli_city_idx]),
    delivery_address= paste0(pick(c("Depot","Warehouse","Godown"), n_cli), " ",
                             sample(1:9, n_cli, TRUE), ", ",
                             CITIES$city[pick(seq_len(nb), n_cli)]),
    credit_limit  = sample(c(1,2,3,4,5,10), n_cli, TRUE) * 1e5,
    gst_mode      = gst_mode,
    gst_pct       = gst_pct,
    branch_id     = bid[cli_city_idx],
    since         = as.character(TODAY - sample(200:2400, n_cli, TRUE)),
    status        = "Active"
  )

  # ---------------- Vendors (36) ----------------
  n_ven <- 36L
  ven_stub <- c("Raghunath Transport","Sai Motors Fleet","Highway Carriers",
                "Malhotra Trucking","Ganesh Roadways","Bharat Fleet Owners",
                "Nagpur Freight Carriers","Om Roadlines","Shivam Logistics",
                "Deccan Fleet Services","National Carriers","Krishna Transport")
  ven_names <- make.unique(paste(pick(ven_stub, n_ven),
                                 pick(c("Co","Pvt Ltd","Assoc.","Services"), n_ven)), sep = " ")
  ven_names[1:7] <- c("Raghunath Transport Co","Sai Motors Fleet Services",
                      "Highway Carriers Pvt Ltd","Malhotra Trucking Co",
                      "Ganesh Roadways","Bharat Fleet Owners Assoc.",
                      "Nagpur Freight Carriers")
  ven_city_idx <- pick(seq_len(nb), n_ven)
  ven_pan <- gen_pan(n_ven)

  vendors <- tibble::tibble(
    vendor_id     = sprintf("VND-%04d", seq_len(n_ven)),
    name          = ven_names,
    owner_name    = gen_name(n_ven),
    mobile        = gen_mobile(n_ven),
    gstin         = gen_gstin(CITIES$state[ven_city_idx], ven_pan),
    pan           = ven_pan,
    city          = CITIES$city[ven_city_idx],
    branch_id     = bid[ven_city_idx],
    payment_terms = pick(c("15-day credit","30-day credit","Advance + 7-day","To-pay"), n_ven),
    bank_name     = pick(c("HDFC Bank","ICICI Bank","State Bank of India","Axis Bank"), n_ven),
    account_no    = gen_acct(n_ven),
    ifsc          = gen_ifsc(n_ven),
    since         = as.character(TODAY - sample(400:2600, n_ven, TRUE)),
    status        = "Active"
  )

  # Attach vendor-owned vehicles to actual vendors.
  vend_rows <- which(vehicles$owner_type == "Vendor")
  vehicles$vendor_id[vend_rows] <- pick(vendors$vendor_id, length(vend_rows))

  vendor_documents <- tidyr::crossing(
    vendor_id = vendors$vendor_id,
    doc_type  = c("Vendor Agreement", "PAN Card", "GST Certificate")
  )
  vendor_documents$status <- pick(c("Uploaded","Uploaded","Uploaded","Pending"),
                                  nrow(vendor_documents))

  out$employees <- employees
  out$drivers   <- drivers
  out$vehicles  <- vehicles
  out$clients   <- clients
  out$vendors   <- vendors
  out$vendor_documents <- vendor_documents

  # ---------------- Users (148) ----------------
  # Internal logins only. Driver/Vendor/Customer portal identities are derived
  # from their own masters at sign-in rather than duplicated here.
  n_usr <- 148L
  usr_role <- c("Super Admin",
                rep("Branch Admin", 17),
                rep("Operations Manager", 24),
                rep("Booking Executive", 41),
                rep("Dispatcher", 19),
                rep("Accountant", 11),
                rep("HR Manager", 4),
                rep("Booking Executive", n_usr - 117))
  usr_role <- usr_role[seq_len(n_usr)]

  usr_names <- gen_name(n_usr)
  usr_names[1] <- "Amardip Singh"
  usr_names[2:8] <- c("Vikram Chauhan","Rohit Kulkarni","Neha Agrawal","Devendra Sahu",
                      "Kavita Sharma","Suresh Khan","Poonam Jain")

  usr_branch <- c(bid[1], pick(bid, n_usr - 1))
  handle <- tolower(gsub("[^a-z ]", "", tolower(usr_names)))
  handle <- gsub(" ", ".", handle)
  # De-duplicate the local part, not the finished address — appending to the
  # whole string lands the counter after the TLD and yields "…@domain.in1".
  email  <- paste0(make.unique(handle, sep = "."), "@amardiptms.in")

  users <- tibble::tibble(
    user_id      = sprintf("USR-%04d", seq_len(n_usr)),
    name         = usr_names,
    email        = email,
    # All demo accounts share one password, hashed not stored in the clear.
    # README documents it; this is seed data for a public demo, not a secret.
    password_hash= bcrypt::hashpw("tms@2026"),
    role         = usr_role,
    branch_id    = usr_branch,
    department   = ifelse(usr_role == "Accountant", "Finance",
                   ifelse(usr_role == "HR Manager", "Human Resources",
                   ifelse(usr_role == "Super Admin", "Management", "Operations"))),
    mobile       = gen_mobile(n_usr),
    # Only the Super Admin sees across branches by default.
    cross_branch = ifelse(usr_role == "Super Admin", "TRUE", "FALSE"),
    client_id    = "",
    vendor_id    = "",
    status       = c("Active", pick(c("Active","Active","Active","Active","Suspended"), n_usr - 1))
  )
  users$status[1] <- "Active"
  out$users <- users

  # ---------------- Roles & permissions ----------------
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

  # ---------------- Pin code routes (312) ----------------
  depots <- branches[branches$is_depot, ]
  n_rt <- 312L
  src_i <- pick(seq_len(nb), n_rt); dst_i <- pick(seq_len(nb), n_rt)
  keep  <- src_i != dst_i
  src_i <- src_i[keep]; dst_i <- dst_i[keep]; n_rt <- length(src_i)

  # Transit days from great-circle distance at a 450 km/day road average — this
  # is what makes the "avg 2.8 days" headline fall out of the data rather than
  # being a hard-coded number.
  hav <- function(a, b, c, d) {
    r <- 6371; p <- pi / 180
    2 * r * asin(sqrt(sin((c - a) * p / 2)^2 + cos(a * p) * cos(c * p) * sin((d - b) * p / 2)^2))
  }
  dist_km <- hav(CITIES$lat[src_i], CITIES$lon[src_i], CITIES$lat[dst_i], CITIES$lon[dst_i])
  transit <- pmax(1, ceiling(dist_km / 450))

  avail <- ifelse(transit <= 3, "Available", ifelse(transit <= 5, "Limited", "Not Serviceable"))

  pincode_routes <- tibble::tibble(
    route_id      = sprintf("RT-%04d", seq_len(n_rt)),
    src_pincode   = CITIES$pin[src_i],
    src_city      = CITIES$city[src_i],
    dst_pincode   = CITIES$pin[dst_i],
    dst_city      = CITIES$city[dst_i],
    distance_km   = round(dist_km),
    transit_days  = transit,
    route_path    = paste(CITIES$city[src_i], "→", CITIES$city[dst_i]),
    branch_mapping= paste(branches$name[src_i], "→", branches$name[dst_i]),
    pickup_zone   = paste("Zone", pick(c("North","South","East","West","Central"), n_rt)),
    delivery_zone = paste("Zone", pick(c("North","South","East","West","Central"), n_rt)),
    availability  = avail
  )
  out$pincode_routes <- pincode_routes

  # ---------------- Bookings (342 this cycle) ----------------
  n_bk <- 342L
  bk_cli  <- pick(clients$client_id, n_bk)
  bk_ci   <- match(clients$city[match(bk_cli, clients$client_id)], CITIES$city)
  bk_di   <- pick(seq_len(nb), n_bk)
  bk_di[bk_di == bk_ci] <- ((bk_di[bk_di == bk_ci]) %% nb) + 1L

  mat_i   <- pick(seq_len(nrow(MATERIALS)), n_bk)
  # Capped at the largest body in the fleet — a booking heavier than any truck
  # could never be allocated, so seeding one would only produce a stuck row.
  weight  <- round(pmin(runif(n_bk, 4, 26) * MATERIALS$dens[mat_i],
                        max(VEHICLE_TYPES$cap)), 1)
  # Road freight is not linear in distance — there is a fixed cost to putting a
  # truck on the road, a per-tonne handling component, and only then a
  # per-tonne-km line haul. A flat rate-per-km with a floor collapses most short
  # hauls onto the floor and makes every invoice the same number.
  bk_dist <- hav(CITIES$lat[bk_ci], CITIES$lon[bk_ci], CITIES$lat[bk_di], CITIES$lon[bk_di])
  freight <- round((2500 + weight * 400 + weight * bk_dist * 0.85) / 100) * 100

  bk_gm  <- clients$gst_mode[match(bk_cli, clients$client_id)]
  bk_gp  <- clients$gst_pct [match(bk_cli, clients$client_id)]
  insured<- runif(n_bk) < .45
  ins_amt<- ifelse(insured, round(freight * .0125 / 10) * 10, 0)

  # Age bookings backwards from today; older ones are further along the
  # lifecycle, which is what makes the status mix look like a live pipeline.
  age <- sample(0:60, n_bk, TRUE, prob = rev(seq_len(61))^.6)
  status <- dplyr::case_when(
    age == 0            ~ pick(c("Draft","Confirmed"), n_bk),
    age <= 2            ~ pick(c("Confirmed","Vehicle Allocated"), n_bk),
    age <= 6            ~ pick(c("Vehicle Allocated","In Transit"), n_bk),
    age <= 12           ~ pick(c("In Transit","Delivered"), n_bk),
    TRUE                ~ pick(c("Delivered","Closed","Closed"), n_bk)
  )
  status[sample(n_bk, 9)] <- "Cancelled"

  bookings <- tibble::tibble(
    booking_no    = sprintf("BKG-%04d", 4200 + seq_len(n_bk)),
    booking_date  = as.character(TODAY - age),
    branch_id     = bid[bk_ci],
    client_id     = bk_cli,
    pickup_address  = clients$pickup_address[match(bk_cli, clients$client_id)],
    delivery_address= paste0(pick(c("Depot","Warehouse","Godown"), n_bk), ", ", CITIES$city[bk_di]),
    origin_city   = CITIES$city[bk_ci],
    dest_city     = CITIES$city[bk_di],
    material      = MATERIALS$material[mat_i],
    weight_t      = weight,
    quantity      = round(weight * pick(c(18, 20, 24, 30), n_bk)),
    packages      = paste(round(weight * pick(c(18, 20, 24, 30), n_bk)), MATERIALS$unit[mat_i]),
    insurance     = ifelse(insured, "Insured", "Not insured"),
    declared_value= ifelse(insured, round(freight * 30 / 1000) * 1000, 0),
    freight       = freight,
    gst_mode      = bk_gm,
    gst_pct       = bk_gp,
    gst_amount    = round(freight * bk_gp / 100),
    insurance_amt = ins_amt,
    total         = freight + round(freight * bk_gp / 100) + ins_amt,
    remarks       = pick(c("", "", "Handle with care · fragile packaging",
                           "Deliver before 18:00", "Tarpaulin required"), n_bk),
    booking_user_id = pick(users$user_id[users$role %in% c("Booking Executive","Dispatcher")], n_bk),
    priority      = pick(c("Normal","Normal","Normal","Priority"), n_bk),
    status        = status
  )
  out$bookings <- bookings

  # ---------------- Trips ----------------
  # The Trip Book: referenced across payroll, vendor settlement, consolidation
  # and GPS in the deck but never drawn. Modelled as a first-class entity here
  # because it is what links a vehicle+driver to one or more consignments and
  # carries the allowances payroll pulls.
  moving <- bookings[bookings$status %in% c("Vehicle Allocated","In Transit","Delivered","Closed"), ]
  n_trip <- nrow(moving)

  # Assign a vehicle that can actually carry the load. Picking at random would
  # seed trips whose consignment weight exceeds the truck's rated capacity —
  # which is exactly the condition the allocation screen refuses to create, so
  # the seed must not manufacture it either.
  trip_veh <- vapply(moving$weight_t, function(w) {
    fit <- vehicles$vehicle_id[vehicles$capacity_t >= w]
    if (!length(fit)) fit <- vehicles$vehicle_id[which.max(vehicles$capacity_t)]
    sample(fit, 1)
  }, character(1), USE.NAMES = FALSE)

  trip_drv <- vehicles$driver_id[match(trip_veh, vehicles$vehicle_id)]
  # A vehicle with no assigned driver still needs one for the trip.
  blank <- !nzchar(trip_drv)
  trip_drv[blank] <- pick(drivers$driver_id, sum(blank))

  trip_age <- TODAY - as.Date(moving$booking_date)
  trip_status <- dplyr::case_when(
    moving$status == "Vehicle Allocated" ~ "Planned",
    moving$status == "In Transit"        ~ pick(c("Running","At Hub"), n_trip),
    TRUE                                 ~ "Completed"
  )

  trips <- tibble::tibble(
    trip_no    = sprintf("TRP-%04d", 2100 + seq_len(n_trip)),
    booking_no = moving$booking_no,
    vehicle_id = trip_veh,
    driver_id  = trip_drv,
    vendor_id  = vehicles$vendor_id[match(trip_veh, vehicles$vehicle_id)],
    branch_id  = moving$branch_id,
    route_from = moving$origin_city,
    route_to   = moving$dest_city,
    distance_km= round(hav(CITIES$lat[match(moving$origin_city, CITIES$city)],
                           CITIES$lon[match(moving$origin_city, CITIES$city)],
                           CITIES$lat[match(moving$dest_city,   CITIES$city)],
                           CITIES$lon[match(moving$dest_city,   CITIES$city)])),
    dispatch_dt= as.character(as.POSIXct(moving$booking_date, tz = "Asia/Kolkata") +
                                sample(6:20, n_trip, TRUE) * 3600),
    eta        = as.character(as.POSIXct(moving$booking_date, tz = "Asia/Kolkata") +
                                (1 + sample(1:4, n_trip, TRUE)) * 86400),
    # Fixed per-trip allowances quoted on the payroll screen: border ₹1,000,
    # food ₹400. These are what the payroll run pulls automatically.
    border_allowance = 1000,
    food_allowance   = 400,
    # A trip advance is the float a driver draws to cover that trip's tolls,
    # fuel and food — so it tracks the allowance it offsets, give or take.
    # Sampling it independently (₹2k–₹12k a trip) made a fortnight of driving
    # owe more than a month's pay and put two-thirds of the fleet on hold.
    advance          = round((1000 + 400) * runif(n_trip, 0.85, 1.25) / 50) * 50,
    status     = trip_status
  )
  out$trips <- trips

  # ---------------- Consignments (LR) ----------------
  cn <- tibble::tibble(
    cn_no       = sprintf("CN-%05d", 90500 + seq_len(n_trip)),
    lr_no       = sprintf("LR-%04d", 8500 + seq_len(n_trip)),
    booking_no  = moving$booking_no,
    trip_no     = trips$trip_no,
    client_id   = moving$client_id,
    vehicle_id  = trips$vehicle_id,
    driver_id   = trips$driver_id,
    branch_id   = moving$branch_id,
    origin_city = moving$origin_city,
    dest_city   = moving$dest_city,
    origin_addr = moving$pickup_address,
    dest_addr   = moving$delivery_address,
    weight_t    = moving$weight_t,
    freight     = moving$freight,
    dispatch_date     = as.character(as.Date(trips$dispatch_dt)),
    expected_delivery = as.character(as.Date(trips$eta)),
    delivered_date    = ifelse(moving$status %in% c("Delivered","Closed"),
                               as.character(as.Date(trips$eta)), ""),
    parent_cn_no= "",
    status      = dplyr::case_when(
      moving$status == "Vehicle Allocated" ~ "In Prep",
      moving$status == "In Transit"        ~ pick(c("Dispatched","In Transit","At Hub","Out for Delivery"), n_trip),
      TRUE                                 ~ "Delivered"
    )
  )
  out$consignments <- cn

  # Status timeline rows for the tracking screen.
  ev_steps <- c("Booking Created","Vehicle Allocated","Picked Up","In Transit",
                "At Hub","Out For Delivery","Delivered","POD Uploaded")
  reached <- match(cn$status, c("In Prep","Dispatched","In Transit","At Hub",
                                "Out for Delivery","Delivered"))
  reached <- ifelse(is.na(reached), 1, pmin(reached + 2, length(ev_steps)))

  out$consignment_events <- purrr::map_dfr(seq_len(nrow(cn)), function(i) {
    k <- reached[i]
    tibble::tibble(
      cn_no    = cn$cn_no[i],
      seq      = seq_len(k),
      event    = ev_steps[seq_len(k)],
      detail   = "",
      event_dt = as.character(as.POSIXct(cn$dispatch_date[i], tz = "Asia/Kolkata") +
                                cumsum(sample(3:20, k, TRUE)) * 3600)
    )
  })

  # ---------------- E-way bills ----------------
  #
  # Only consignments actually on the road carry a live e-way bill. A delivered
  # consignment's bill is spent — keeping it in the register would fill the
  # screen with hundreds of "Expired" rows that nobody can act on and bury the
  # handful that genuinely need renewing before a check-post stops the vehicle.
  live <- cn$status %in% c("Dispatched", "In Transit", "At Hub", "Out for Delivery")
  ewb_src <- cn[live, ]
  n_ewb <- nrow(ewb_src)

  valid_from <- as.POSIXct(ewb_src$dispatch_date, tz = "Asia/Kolkata")
  # Validity is distance-based (24h per ~200 km) and gets extended in practice,
  # so the window is anchored to now rather than to dispatch. A few land in the
  # past or inside 24h, which is what the expiry queue exists to surface.
  valid_to   <- NOW + sample(c(-2, -1, 0, 1, 1, 2, 2, 3, 3, 4, 5),
                             n_ewb, TRUE, prob = c(1, 1, 2, 3, 3, 4, 4, 4, 4, 3, 2)) * 86400 +
    runif(n_ewb, -6, 6) * 3600

  out$ewaybills <- tibble::tibble(
    ewb_no     = paste(sprintf("%04d", sample(7211:7299, n_ewb, TRUE)),
                       sprintf("%04d", sample(1000:9999, n_ewb, TRUE)),
                       sprintf("%04d", sample(1000:9999, n_ewb, TRUE))),
    invoice_no = paste0(toupper(substr(gsub("[^A-Za-z]", "", clients$name[match(ewb_src$client_id, clients$client_id)]), 1, 2)),
                        "/26-27/", sprintf("%04d", sample(1000:9999, n_ewb, TRUE))),
    cn_no      = ewb_src$cn_no,
    lr_no      = ewb_src$lr_no,
    gstin      = clients$gstin[match(ewb_src$client_id, clients$client_id)],
    vehicle_id = ewb_src$vehicle_id,
    transporter_id = paste0(substr(branches$gstin[1], 1, 13), "ZT"),
    valid_from = as.character(valid_from),
    valid_to   = as.character(valid_to),
    status     = dplyr::case_when(
      valid_to < NOW                     ~ "Expired",
      valid_to < NOW + 24 * 3600         ~ "Expiring Soon",
      TRUE                               ~ "Valid"
    )
  )

  # ---------------- POD ----------------
  pod_src <- cn[cn$status %in% c("Out for Delivery","Delivered"), ]
  n_pod <- nrow(pod_src)
  pod_status <- ifelse(pod_src$status == "Out for Delivery", "Pending",
                       pick(c("Uploaded","Verified","Approved","Approved"), n_pod))
  out$pods <- tibble::tibble(
    pod_id     = sprintf("POD-%04d", seq_len(n_pod)),
    lr_no      = pod_src$lr_no,
    cn_no      = pod_src$cn_no,
    client_id  = pod_src$client_id,
    branch_id  = pod_src$branch_id,
    uploaded_by= ifelse(pod_status == "Pending", "",
                        drivers$name[match(pod_src$driver_id, drivers$driver_id)]),
    file_name  = ifelse(pod_status == "Pending", "",
                        paste0("POD_", sub("LR-", "", pod_src$lr_no),
                               pick(c(".jpg",".jpg",".pdf"), n_pod))),
    upload_dt  = ifelse(pod_status == "Pending", "",
                        as.character(as.POSIXct(pod_src$expected_delivery, tz = "Asia/Kolkata") +
                                       sample(1:12, n_pod, TRUE) * 3600)),
    status     = pod_status
  )

  # ---------------- GPS pings ----------------
  # One current position per vehicle that is out on a running trip, jittered off
  # the great-circle midpoint of its route so the map looks like a live fleet.
  run <- trips[trips$status %in% c("Running","At Hub"), ]
  run <- run[!duplicated(run$vehicle_id), ]
  n_g <- nrow(run)
  fi <- match(run$route_from, CITIES$city); ti <- match(run$route_to, CITIES$city)
  frac <- runif(n_g, .15, .85)
  gstat <- pick(c("Running","Running","Running","Running","Halted","Alert"), n_g)

  out$gps_pings <- tibble::tibble(
    vehicle_id = run$vehicle_id,
    trip_no    = run$trip_no,
    lat        = round(CITIES$lat[fi] + (CITIES$lat[ti] - CITIES$lat[fi]) * frac + rnorm(n_g, 0, .12), 4),
    lon        = round(CITIES$lon[fi] + (CITIES$lon[ti] - CITIES$lon[fi]) * frac + rnorm(n_g, 0, .12), 4),
    speed      = ifelse(gstat == "Halted", 0, sample(28:68, n_g, TRUE)),
    heading    = pick(c("N","NE","E","SE","S","SW","W","NW"), n_g),
    ignition   = ifelse(gstat == "Halted", "OFF", "ON"),
    idle_min   = ifelse(gstat == "Halted", sample(15:180, n_g, TRUE), sample(0:40, n_g, TRUE)),
    ts         = as.character(NOW - sample(20:600, n_g, TRUE)),
    status     = gstat
  )

  # Vehicle status derives from trips, so the fleet donut on the dashboard and
  # the vehicle master's filter chips cannot disagree.
  vehicles$status <- "Available"
  vehicles$status[vehicles$vehicle_id %in% trips$vehicle_id[trips$status == "Planned"]]  <- "Allocated"
  vehicles$status[vehicles$vehicle_id %in% run$vehicle_id]                                <- "In Transit"
  maint <- sample(setdiff(vehicles$vehicle_id, c(run$vehicle_id)), 6)
  vehicles$status[vehicles$vehicle_id %in% maint] <- "Maintenance"
  inact <- sample(setdiff(vehicles$vehicle_id, c(run$vehicle_id, maint)), 3)
  vehicles$status[vehicles$vehicle_id %in% inact] <- "Inactive"
  out$vehicles <- vehicles

  drivers$status <- "Available"
  drivers$status[drivers$driver_id %in% run$driver_id] <- "On trip"
  on_leave <- sample(setdiff(drivers$driver_id, run$driver_id), 4)
  drivers$status[drivers$driver_id %in% on_leave] <- "On leave"
  out$drivers <- drivers

  # ---------------- Complaints ----------------
  n_cmp <- 34L
  cmp_cn <- cn[sample(nrow(cn), n_cmp), ]
  cmp_stat <- pick(c("New","Assigned","In Progress","Resolved","Resolved"), n_cmp)
  raised <- TODAY - sample(0:25, n_cmp, TRUE)

  out$complaints <- tibble::tibble(
    complaint_id = sprintf("CMP-%04d", 3280 + seq_len(n_cmp)),
    client_id    = cmp_cn$client_id,
    lr_no        = cmp_cn$lr_no,
    cn_no        = cmp_cn$cn_no,
    branch_id    = cmp_cn$branch_id,
    category     = pick(c("Delay","Damaged Goods","POD Issue","Billing Issue",
                          "Driver Complaint","Wrong Delivery","Service Complaint"), n_cmp),
    priority     = pick(c("High","Medium","Medium","Low"), n_cmp),
    subject      = pick(c("Shipment delayed beyond committed ETA",
                          "Bags found torn on arrival",
                          "POD copy not received by consignee",
                          "Billing amount mismatch on invoice",
                          "Driver misbehaviour reported",
                          "Wrong consignee delivery address used",
                          "Repeated pickup delay",
                          "Short quantity received",
                          "E-way bill mismatch flagged at checkpost"), n_cmp),
    raised_dt    = as.character(raised),
    target_dt    = as.character(raised + 2),
    resolved_dt  = ifelse(cmp_stat == "Resolved", as.character(raised + sample(1:3, n_cmp, TRUE)), ""),
    assigned_to  = ifelse(cmp_stat == "New", "", pick(users$user_id[users$role != "Super Admin"], n_cmp)),
    progress_pct = dplyr::case_when(cmp_stat == "New" ~ 0, cmp_stat == "Assigned" ~ 15,
                                    cmp_stat == "In Progress" ~ sample(35:80, n_cmp, TRUE),
                                    TRUE ~ 100),
    resolution   = ifelse(cmp_stat == "Resolved", "Customer notified · closed", ""),
    status       = cmp_stat
  )

  # ---------------- Invoices ----------------
  # Auto-raised from delivered consignments that have an approved POD, which is
  # the rule the deck states: "Invoices generate automatically from delivered
  # consignments (POD required)". Six delivered-with-POD are left unbilled so
  # the "Unbilled Delivered" queue on the invoice screen has content.
  approved_pods <- out$pods$lr_no[out$pods$status %in% c("Verified","Approved")]
  billable <- cn[cn$lr_no %in% approved_pods, ]
  unbilled <- billable[seq_len(min(6, nrow(billable))), ]
  billed   <- billable[-seq_len(min(6, nrow(billable))), ]
  n_inv <- nrow(billed)

  inv_gm <- clients$gst_mode[match(billed$client_id, clients$client_id)]
  inv_gp <- clients$gst_pct [match(billed$client_id, clients$client_id)]
  inv_dt <- as.Date(billed$delivered_date)
  inv_dt[is.na(inv_dt)] <- TODAY
  due    <- inv_dt + 14

  # RCM invoices carry no tax collected by the carrier — the recipient pays it.
  gst_amt <- ifelse(inv_gm == "RCM", 0, round(billed$freight * inv_gp / 100))
  total   <- billed$freight + gst_amt

  inv_status <- dplyr::case_when(
    due < TODAY - 7  ~ pick(c("Paid","Paid","Overdue"), n_inv),
    due < TODAY      ~ pick(c("Paid","Partially paid","Overdue"), n_inv),
    TRUE             ~ pick(c("Sent","Sent","Draft"), n_inv)
  )
  paid_amt <- dplyr::case_when(
    inv_status == "Paid"           ~ total,
    inv_status == "Partially paid" ~ round(total * runif(n_inv, .3, .7) / 100) * 100,
    TRUE                           ~ 0
  )

  invoices <- tibble::tibble(
    invoice_no  = sprintf("INV-%04d", 2000 + seq_len(n_inv)),
    invoice_date= as.character(inv_dt),
    client_id   = billed$client_id,
    branch_id   = billed$branch_id,
    lr_no       = billed$lr_no,
    cn_no       = billed$cn_no,
    amount      = billed$freight,
    gst_mode    = inv_gm,
    gst_pct     = inv_gp,
    gst_amount  = gst_amt,
    total       = total,
    paid_amount = paid_amt,
    due_date    = as.character(due),
    source      = "auto",
    status      = inv_status
  )
  out$invoices <- invoices

  # ---------------- Payments ----------------
  paid_inv <- invoices[invoices$paid_amount > 0, ]
  n_pay <- nrow(paid_inv)
  out$payments <- tibble::tibble(
    payment_id  = sprintf("RCP-%04d", seq_len(n_pay)),
    date        = as.character(as.Date(paid_inv$due_date) - sample(0:10, n_pay, TRUE)),
    client_id   = paid_inv$client_id,
    invoice_no  = paid_inv$invoice_no,
    mode        = pick(c("NEFT","NEFT","RTGS","UPI","Cheque","Cash"), n_pay),
    reference   = paste0("UTR", sprintf("%07d", sample(1000000:9999999, n_pay, TRUE))),
    amount      = paid_inv$paid_amount,
    received_by = pick(users$name[users$role == "Accountant"], n_pay)
  )

  # ---------------- Vendor payments ----------------
  vtrips <- trips[nzchar(trips$vendor_id) & trips$status == "Completed", ]
  vagg <- vtrips |>
    dplyr::group_by(vendor_id) |>
    dplyr::summarise(trips = dplyr::n(),
                     amount = sum(round(distance_km * 42)), .groups = "drop")
  out$vendor_payments <- tibble::tibble(
    vp_id     = sprintf("PAY-%04d", 2200 + seq_len(nrow(vagg))),
    vendor_id = vagg$vendor_id,
    trips     = vagg$trips,
    amount    = vagg$amount,
    due_date  = as.character(TODAY + sample(1:20, nrow(vagg), TRUE)),
    # Payments above ₹50,000 need Branch Admin sign-off, per the deck's callout.
    status    = ifelse(vagg$amount > 50000, "Awaiting approval", "Approved"),
    approved_by = ""
  )

  # ---------------- HRMS: leave types, attendance, leave requests ----------------
  out$leave_types <- tibble::tibble(
    code = c("CL","SL","EL"),
    name = c("Casual (CL)","Sick (SL)","Earned (EL)"),
    annual_quota = c(8, 7, 15)
  )

  # Last 30 days of attendance. Driver duty days are derived from trips rather
  # than punched — the deck is explicit that "no separate punch needed while on
  # the road", and that trip-derived duty is what feeds payroll.
  days <- seq(TODAY - 29, TODAY, by = "day")
  trip_days <- trips |>
    dplyr::mutate(d = as.Date(dispatch_dt)) |>
    dplyr::select(driver_id, d, trip_no)

  att <- tidyr::crossing(employee_id = employees$employee_id, date = days) |>
    dplyr::left_join(trip_days, by = c("employee_id" = "driver_id", "date" = "d")) |>
    dplyr::mutate(
      dow = lubridate::wday(date),
      status = dplyr::case_when(
        !is.na(trip_no) ~ "T",                       # on trip (auto)
        dow == 1        ~ "off",                     # Sunday
        TRUE            ~ sample(c("P","P","P","P","P","P","P","P","P","A","L"),
                                 dplyr::n(), TRUE)
      ),
      leave_type = ifelse(status == "L", sample(c("CL","SL","EL"), dplyr::n(), TRUE), "")
    ) |>
    dplyr::select(employee_id, date, status, trip_no, leave_type) |>
    dplyr::mutate(date = as.character(date), trip_no = tidyr::replace_na(trip_no, ""))
  out$attendance <- att

  n_lv <- 26L
  lv_emp <- pick(employees$employee_id, n_lv)
  lv_from <- TODAY + sample(-12:14, n_lv, TRUE)
  lv_days <- sample(1:4, n_lv, TRUE)
  out$leave_requests <- tibble::tibble(
    lr_id      = sprintf("LVR-%04d", seq_len(n_lv)),
    employee_id= lv_emp,
    leave_type = pick(c("CL","SL","EL"), n_lv),
    from_date  = as.character(lv_from),
    to_date    = as.character(lv_from + lv_days - 1),
    days       = lv_days,
    reason     = pick(c("family function","village visit","medical","personal work",
                        "regularisation · doctor note attached"), n_lv),
    applied_dt = as.character(lv_from - sample(1:8, n_lv, TRUE)),
    status     = ifelse(lv_from > TODAY, pick(c("Pending","Pending","Approved"), n_lv),
                        pick(c("Approved","Approved","Rejected"), n_lv))
  )

  # ---------------- Payroll (last completed month) ----------------
  #
  # Payroll is run for the month that has finished, not the one in progress —
  # attendance has to be closed and trip allowances totalled before anything can
  # be paid. Seeding the current month would also leave the screen empty on the
  # 1st, with every driver showing a zero allowance, which reads as the
  # automatic Trip Book pull being broken rather than as a new period.
  period_end <- lubridate::floor_date(TODAY, "month") - 1
  period <- format(period_end, "%b %Y")

  # Trip allowance is summed from the Trip Book, not typed in. This is the
  # "AUTOMATIC PULL" the payroll screen describes.
  trip_allow <- trips |>
    dplyr::filter(format(as.Date(dispatch_dt), "%b %Y") == period) |>
    dplyr::group_by(driver_id) |>
    dplyr::summarise(allow = sum(border_allowance + food_allowance),
                     adv   = sum(advance), n = dplyr::n(), .groups = "drop")

  pr <- employees |>
    dplyr::left_join(trip_allow, by = c("employee_id" = "driver_id")) |>
    dplyr::mutate(
      allow = tidyr::replace_na(allow, 0),
      adv   = tidyr::replace_na(adv, 0),
      basic = round(salary * .5),
      hra   = round(salary * .3),
      da    = salary - round(salary * .5) - round(salary * .3),
      incentive = ifelse(department == "Sales", sample(c(0, 3000, 4500, 6000), dplyr::n(), TRUE), 0),
      ot        = ifelse(department == "Maintenance", sample(c(0, 800, 1200), dplyr::n(), TRUE), 0),
      trip_allowance = allow,
      advances  = adv,
      # Statutory: PF 12% of basic, ESI 0.75% where gross <= 21k, PT flat ₹200.
      pf  = round(basic * .12),
      esi = ifelse(salary <= 21000, round(salary * .0075), 0),
      pt  = 200,
      net = basic + hra + da + incentive + ot + trip_allowance - advances - pf - esi - pt,
      status = ifelse(net < 0, "On hold", "Ready")
    )

  out$payroll <- tibble::tibble(
    period = period, employee_id = pr$employee_id,
    basic = pr$basic, hra = pr$hra, da = pr$da,
    incentive = pr$incentive, ot = pr$ot,
    trip_allowance = pr$trip_allowance, advances = pr$advances,
    pf = pr$pf, esi = pr$esi, pt = pr$pt, net = pr$net,
    status = pr$status
  )

  # ---------------- Settings ----------------
  out$settings <- tibble::tribble(
    ~key, ~value,
    "company_name",    "Amardip Road Carriers",
    "company_gstin",   branches$gstin[1],
    "registered_office","Plot 14, MIDC, Hingna Rd, Nagpur 440016",
    "support_email",   "support@amardiptms.in",
    "support_phone",   "+91 98230 11223",
    "base_currency",   "INR",
    "timezone",        "Asia/Kolkata",
    "date_format",     "DD-MMM-YYYY",
    "fy_start",        "01 April",
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

  # Integration status is stored, not live. Nothing here holds a credential —
  # the app ships with every connector in a disconnected/stub state and the
  # Settings screen only records intent. See DESIGN-REVIEW.md.
  out$integrations <- tibble::tribble(
    ~name,                 ~provider,                        ~status,
    "WhatsApp Business API","Meta Cloud API via Gupshup",     "Not configured",
    "Transactional Email",  "SendGrid",                       "Not configured",
    "SMS Gateway",          "MSG91",                          "Not configured",
    "GST / e-Way Bill (GSP)","GSTR-1/3B · e-way generate/cancel","Not configured",
    "GPS / Telematics",     "OEM feeds — live tracking",      "Not configured",
    "Payment Gateway",      "Razorpay — customer bill payments","Not configured"
  )

  out$audit_log <- tibble::tibble(
    ts = character(), user_id = character(),
    action = character(), module = character(), detail = character()
  )

  out
}

#' Write the seed to data/*.csv.
#'
#' `profile` selects which dataset to generate — see SEED_PROFILE in global.R.
#' Both profiles produce the same tables with the same columns, so nothing
#' downstream has to know which one it is looking at.
seed_write <- function(overwrite = TRUE, profile = SEED_PROFILE) {
  if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
  d <- if (identical(profile, "minimal")) build_seed_minimal() else build_seed()
  for (nm in names(d)) {
    p <- tbl_path(nm)
    if (!overwrite && file.exists(p)) next
    readr::write_csv(d[[nm]], p, na = "")
  }
  store_refresh()
  invisible(vapply(d, nrow, integer(1)))
}

#' Seed only if the data directory has no bookings yet.
seed_if_empty <- function() {
  if (!file.exists(tbl_path("bookings"))) {
    message("No data found — generating seed…")
    print(seed_write())
  }
  invisible(TRUE)
}

# Allow `Rscript R/seed.R` from the project root.
if (sys.nframe() == 0 && !interactive()) {
  source("global.R")
  source("R/store.R")
  source("R/rbac.R")
  source("R/seed_minimal.R")
  cat("Seed profile:", SEED_PROFILE, "\n")
  counts <- seed_write()
  cat("\nSeeded tables:\n")
  print(counts)
}
