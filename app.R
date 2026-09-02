# ==================================================================
# TMS — Transport Management System
# Logistics ERP: bookings · consignments · fleet · compliance ·
# accounts · HRMS · portals
#
# Entry point. Sources the foundation, seeds on first run, then wires the
# auth gate to the shell router.
# ==================================================================

source("global.R")
source("R/store.R")
source("R/rbac.R")
source("R/seed.R")
source("R/seed_minimal.R")
source("R/theme.R")
source("R/ui_helpers.R")
source("R/nav.R")

# A fresh clone ships without data/ populated, and hosted runtimes get an
# ephemeral filesystem on restart. Generating the seed on demand means the app
# always starts into a working state rather than an empty one.
seed_if_empty()

# Screen modules.
for (f in list.files("R", pattern = "^mod_.*\\.R$", full.names = TRUE)) source(f)

# ------------------------------------------------------------------
# UI
# ------------------------------------------------------------------

# bslib themes attach to the page, not to a bare tagList, so the shell is
# assembled through fluidPage to keep the Bootstrap bundle wired up.
ui <- fluidPage(
  theme = app_theme(),
  tags$head(
    tags$title("TMS — Transport Management System"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Strip fluidPage's gutters — the shell manages its own spacing.
    tags$style(HTML(".container-fluid{padding:0;max-width:none;}"))
  ),
  uiOutput("app_root")
)

# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------

server <- function(input, output, session) {

  user <- auth_server("auth")
  page <- reactiveVal("dashboard")

  # Land each role somewhere it is actually allowed to be.
  observeEvent(user(), {
    req(user())
    page(landing_page(user()$role))
  })

  # ---------------- Router ----------------

  observeEvent(input$nav_go, {
    req(user())
    target <- input$nav_go
    meta <- NAV_PAGES[[target]]
    if (is.null(meta)) return()
    # Server-side check: a forged nav event must not reach a screen the role
    # cannot view.
    if (!can(user()$role, meta$module, "view")) {
      showNotification("You do not have access to that screen.", type = "error")
      return()
    }
    page(target)
  })

  observeEvent(input$sign_out, {
    audit(user()$user_id %||% "?", "sign-out", "auth")
    session$reload()
  })

  # ---------------- Root: login gate or shell ----------------

  output$app_root <- renderUI({
    if (is.null(user())) return(auth_ui("auth"))

    u <- user()
    div(
      class = "tms-shell",

      # ---- sidebar ----
      div(
        class = "tms-sidebar",
        div(
          class = "tms-brand",
          div(class = "tms-brand-tile", "T"),
          div(div(class = "tms-brand-name", "TMS"),
              div(class = "tms-brand-sub", "Logistics ERP"))
        ),
        uiOutput("sidebar_nav"),
        div(
          class = "tms-userchip",
          avatar(u$name),
          div(style = "min-width:0;",
              div(style = "color:#fff;font-size:.8125rem;font-weight:600;
                           overflow:hidden;text-overflow:ellipsis;white-space:nowrap;", u$name),
              div(style = "color:#7F93AB;font-size:.6875rem;", u$role)),
          tags$span(
            style = "margin-left:auto;cursor:pointer;color:#7F93AB;",
            title = "Sign out",
            onclick = "Shiny.setInputValue('sign_out', Math.random(), {priority:'event'})",
            fontawesome::fa("right-from-bracket", height = "0.9em", fill = "currentColor")
          )
        )
      ),

      # ---- main column ----
      div(
        class = "tms-main",
        uiOutput("topbar"),
        div(class = "tms-page", uiOutput("page_body"))
      )
    )
  })

  output$sidebar_nav <- renderUI({
    req(user())
    render_nav(user()$role, page())
  })

  # styles.css hides the sidebar below 992px. Shiny suspends outputs inside
  # hidden elements and does not revisit that decision when a CSS media query
  # later reveals them, so on a narrow window the navigation stayed empty even
  # after the window was widened. Computing it regardless costs nothing and
  # makes the rail appear the moment there is room for it.
  outputOptions(output, "sidebar_nav", suspendWhenHidden = FALSE)

  output$topbar <- renderUI({
    req(user())
    u <- user()
    meta <- NAV_PAGES[[page()]] %||% list(title = "TMS", crumb = c("Home"))

    crumb <- meta$crumb
    div(
      class = "tms-topbar",
      div(
        div(class = "tms-crumb",
            HTML(paste0(
              paste(head(crumb, -1), collapse = " / "),
              if (length(crumb) > 1) " / " else "",
              "<b>", tail(crumb, 1), "</b>"
            ))),
        h1(class = "tms-title", meta$title)
      ),

      div(
        class = "tms-search",
        fontawesome::fa("magnifying-glass"),
        tags$input(id = "global_search", type = "text",
                   placeholder = "Search bookings, LRs, vehicles, clients…")
      ),
      div(class = "tms-iconbtn", title = "Help", fontawesome::fa("circle-question")),
      div(class = "tms-iconbtn", title = "Notifications",
          fontawesome::fa("bell"), span(class = "dot")),
      div(
        class = "d-flex align-items-center gap-2 ps-2",
        avatar(u$name),
        div(div(style = "font-size:.8125rem;font-weight:600;color:#12263F;", u$name),
            div(class = "tiny muted", u$role))
      )
    )
  })

  # Each screen's UI is rendered on demand. Module *servers* are all started
  # once below, so their observers exist regardless of which page is showing —
  # outputs simply do not evaluate while their page is off-screen.
  output$page_body <- renderUI({
    req(user())
    p <- page()
    switch(p,
      "dashboard"       = dashboard_ui("dashboard"),
      "branches"        = branches_ui("branches"),
      "users"           = users_ui("users"),
      "clients"         = clients_ui("clients"),
      "vendors"         = vendors_ui("vendors"),
      "bookings"        = bookings_ui("bookings"),
      "booking_new"     = booking_new_ui("bookings"),
      "cargo_moto"      = cargo_moto_ui("cargo_moto"),
      "consignments"    = consignments_ui("consignments"),
      "consign_multi"   = consign_multi_ui("consignments"),
      "trips"           = trips_ui("trips"),
      "ewaybill"        = ewaybill_ui("ewaybill"),
      "pod"             = pod_ui("pod"),
      "tracking"        = tracking_ui("tracking"),
      "livegps"         = livegps_ui("livegps"),
      "vehicles"        = vehicles_ui("vehicles"),
      "drivers"         = drivers_ui("drivers"),
      "pincodes"        = pincodes_ui("pincodes"),
      "complaints"      = complaints_ui("complaints"),
      "invoices"        = invoices_ui("invoices"),
      "payments"        = payments_ui("payments"),
      "hrms_employees"  = hrms_employees_ui("hrms_employees"),
      "hrms_attendance" = hrms_attendance_ui("hrms_attendance"),
      "hrms_payroll"    = hrms_payroll_ui("hrms_payroll"),
      "reports"         = reports_ui("reports"),
      "portal_branch"   = portal_branch_ui("portal_branch"),
      "portal_customer" = portal_customer_ui("portal_customer"),
      "portal_vendor"   = portal_vendor_ui("portal_vendor"),
      "security"        = security_ui("security"),
      "settings"        = settings_ui("settings"),
      div(class = "muted", "Screen not found.")
    )
  })

  # ---------------- Module servers ----------------
  # `nav` lets a module route elsewhere (e.g. "Open live tracking" on the
  # vehicle panel jumps to the GPS screen).
  nav <- function(to) page(to)

  dashboard_server("dashboard", user, nav)
  branches_server("branches", user)
  users_server("users", user)
  clients_server("clients", user)
  vendors_server("vendors", user)
  bookings_server("bookings", user, nav)
  cargo_moto_server("cargo_moto", user, nav)
  consignments_server("consignments", user, nav)
  trips_server("trips", user, nav)
  ewaybill_server("ewaybill", user, nav)
  pod_server("pod", user)
  tracking_server("tracking", user)
  livegps_server("livegps", user, nav)
  vehicles_server("vehicles", user, nav)
  drivers_server("drivers", user, nav)
  pincodes_server("pincodes", user)
  complaints_server("complaints", user)
  invoices_server("invoices", user, nav)
  payments_server("payments", user)
  hrms_employees_server("hrms_employees", user, nav)
  hrms_attendance_server("hrms_attendance", user)
  hrms_payroll_server("hrms_payroll", user)
  reports_server("reports", user)
  portal_branch_server("portal_branch", user, nav)
  portal_customer_server("portal_customer", user, nav)
  portal_vendor_server("portal_vendor", user)
  security_server("security", user)
  settings_server("settings", user)
}

shinyApp(ui, server)
