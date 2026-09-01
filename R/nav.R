# ==================================================================
# Sidebar navigation — grouped exactly as the design deck draws it.
#
# One declarative tree drives three things: the rail itself, the breadcrumb on
# each page, and which module server the router shows. Adding a screen means
# adding one entry here plus its mod_*.R file.
#
# `module` maps to an entry in MODULES (rbac.R). Items whose module the signed-in
# role cannot view are dropped from the rail, and a group with no surviving
# children disappears with them.
# ==================================================================

NAV <- list(
  list(caption = "Main", items = list(
    list(id = "dashboard", label = "Dashboard", icon = "house", module = "dashboard",
         crumb = c("Home", "Dashboard"), title = "Dashboard")
  )),

  list(caption = "Administration", items = list(
    list(id = "branches", label = "Branches", icon = "building", module = "branches",
         crumb = c("Administration", "Branches"), title = "Branches"),
    list(id = "users", label = "Users", icon = "users", module = "users",
         crumb = c("Administration", "Users"), title = "Users")
  )),

  list(caption = "Relationships", items = list(
    list(id = "clients", label = "Clients", icon = "book", module = "clients",
         crumb = c("Relationships", "Clients"), title = "Clients"),
    list(id = "vendors", label = "Vendors", icon = "briefcase", module = "vendors",
         crumb = c("Relationships", "Vendors"), title = "Vendors")
  )),

  list(caption = "Operations", items = list(
    list(id = "bookings", label = "Cargo Bookings", icon = "box", module = "bookings",
         crumb = c("Operations", "Cargo Bookings"), title = "Cargo Bookings",
         children = list(
           list(id = "bookings",     label = "All Bookings", title = "Cargo Bookings",
                crumb = c("Operations", "Cargo Bookings", "All Bookings")),
           list(id = "booking_new",  label = "New Booking",  title = "New Booking",
                module = "bookings",
                crumb = c("Operations", "Cargo Bookings", "New Booking"))
         )),
    list(id = "cargo_moto", label = "Cargo Moto", icon = "truck-fast", module = "cargo_moto",
         crumb = c("Operations", "Cargo Moto"), title = "Cargo Moto Management"),
    list(id = "consignments", label = "Consignments", icon = "table-cells", module = "consignments",
         crumb = c("Operations", "Consignments"), title = "Consignments (LR)",
         children = list(
           list(id = "consignments",  label = "Consignment (LR)", title = "Consignments (LR)",
                crumb = c("Operations", "Consignments", "Consignment (LR)")),
           list(id = "consign_multi", label = "Multiple Consignment", module = "consignments",
                title = "Multiple Consignment Management",
                crumb = c("Operations", "Consignments", "Multiple Consignment"))
         )),
    # Not in the original deck. Added because trips are referenced by payroll,
    # vendor settlement, consolidation and GPS with no screen to manage them —
    # see DESIGN-REVIEW.md, gap #3.
    list(id = "trips", label = "Trip Book", icon = "route", module = "trips",
         crumb = c("Operations", "Trip Book"), title = "Trip Book"),
    list(id = "ewaybill", label = "E-Way Bill", icon = "file-lines", module = "ewaybill",
         crumb = c("Compliance", "E-Way Bill"), title = "E-Way Bill (EWB) Management"),
    list(id = "pod", label = "POD", icon = "circle-check", module = "pod",
         crumb = c("Operations", "POD"), title = "POD Management"),
    list(id = "tracking", label = "Shipment Tracking", icon = "location-crosshairs", module = "tracking",
         crumb = c("Operations", "Shipment Tracking"), title = "Shipment Tracking"),
    list(id = "livegps", label = "Live GPS", icon = "location-dot", module = "livegps",
         crumb = c("Operations", "Live GPS"), title = "Live Vehicle GPS Tracking")
  )),

  list(caption = "Fleet & People", items = list(
    list(id = "vehicles", label = "Fleet & Drivers", icon = "truck", module = "vehicles",
         crumb = c("Fleet & People", "Vehicles"), title = "Vehicles",
         children = list(
           list(id = "vehicles", label = "Vehicles", title = "Vehicles",
                crumb = c("Fleet & People", "Vehicles")),
           list(id = "drivers",  label = "Drivers", module = "drivers", title = "Drivers",
                crumb = c("Fleet & People", "Drivers")),
           list(id = "pincodes", label = "Pin Code Mapping", module = "pincodes",
                title = "Pin Code Mapping",
                crumb = c("Fleet & People", "Pin Code Mapping"))
         ))
  )),

  list(caption = "Support", items = list(
    list(id = "complaints", label = "Complaints", icon = "triangle-exclamation", module = "complaints",
         crumb = c("Support", "Complaints"), title = "Complaints")
  )),

  list(caption = "Accounts", items = list(
    list(id = "invoices", label = "Accounts", icon = "file-invoice", module = "invoices",
         crumb = c("Accounts", "Invoices & Billing"), title = "Invoices & Billing",
         children = list(
           list(id = "invoices", label = "Invoices & Billing", title = "Invoices & Billing",
                crumb = c("Accounts", "Invoices & Billing")),
           list(id = "payments", label = "Payments & Ledger", module = "payments",
                title = "Payments & Ledger",
                crumb = c("Accounts", "Payments & Ledger"))
         ))
  )),

  list(caption = "HRMS", items = list(
    list(id = "hrms_employees", label = "HRMS", icon = "user-group", module = "hrms_employees",
         crumb = c("HRMS", "Employees"), title = "HRMS — Employees",
         children = list(
           list(id = "hrms_employees", label = "Employees", title = "HRMS — Employees",
                crumb = c("HRMS", "Employees")),
           list(id = "hrms_attendance", label = "Attendance & Leave", module = "hrms_attendance",
                title = "HRMS — Attendance & Leave",
                crumb = c("HRMS", "Attendance & Leave")),
           list(id = "hrms_payroll", label = "Payroll", module = "hrms_payroll",
                title = "HRMS — Payroll", crumb = c("HRMS", "Payroll"))
         ))
  )),

  list(caption = "Insights", items = list(
    list(id = "reports", label = "Reports & Analytics", icon = "chart-column", module = "reports",
         crumb = c("Insights", "Reports & Analytics"), title = "Reports & Analytics"),
    list(id = "portal_branch", label = "Portals", icon = "table-columns", module = "portal_branch",
         crumb = c("Portals", "Branch Dashboard"), title = "Branch Dashboard",
         children = list(
           list(id = "portal_branch", label = "Branch Dashboard", title = "Branch Dashboard",
                crumb = c("Portals", "Branch Dashboard")),
           list(id = "portal_customer", label = "Customer Dashboard", module = "portal_customer",
                title = "Customer Dashboard", crumb = c("Portals", "Customer Dashboard")),
           list(id = "portal_vendor", label = "Vendor Dashboard", module = "portal_vendor",
                title = "Vendor Dashboard", crumb = c("Portals", "Vendor Dashboard"))
         ))
  )),

  list(caption = "System", items = list(
    list(id = "security", label = "Security & Roles", icon = "shield-halved", module = "security",
         crumb = c("System", "Security & Roles"), title = "Security & Roles"),
    list(id = "settings", label = "Settings", icon = "sliders", module = "settings",
         crumb = c("System", "Settings"), title = "Settings")
  ))
)

#' Flatten NAV into a page_id -> metadata lookup.
#'
#' Parent entries that carry children contribute their first child as the
#' landing page, so clicking the group header lands somewhere real.
NAV_PAGES <- local({
  pages <- list()
  for (grp in NAV) {
    for (it in grp$items) {
      if (!is.null(it$children)) {
        for (ch in it$children) {
          pages[[ch$id]] <- list(
            id = ch$id, title = ch$title, crumb = ch$crumb,
            module = ch$module %||% it$module, parent = it$id, group = grp$caption
          )
        }
      } else {
        pages[[it$id]] <- list(
          id = it$id, title = it$title, crumb = it$crumb,
          module = it$module, parent = NA_character_, group = grp$caption
        )
      }
    }
  }
  pages
})

#' Every page id a role may reach, used to pick a safe landing page at sign-in.
allowed_pages <- function(role) {
  ids <- names(NAV_PAGES)
  ids[vapply(ids, function(p) can(role, NAV_PAGES[[p]]$module, "view"), logical(1))]
}

#' Default landing page for a role.
#'
#' Portal roles land on their own dashboard rather than the internal one, which
#' is what the deck's Customer/Vendor screens imply.
landing_page <- function(role) {
  pref <- switch(role,
    "Customer" = "portal_customer",
    "Vendor"   = "portal_vendor",
    "Driver"   = "tracking",
    "dashboard")
  ok <- allowed_pages(role)
  if (pref %in% ok) pref else (ok[1] %||% "dashboard")
}

#' Render the sidebar for a role, marking `active`.
#'
#' Groups collapse to nothing when the role can see none of their children, so
#' an Accountant does not get an empty "Fleet & People" caption.
render_nav <- function(role, active) {
  active_parent <- NAV_PAGES[[active]]$parent %||% NA_character_

  groups <- lapply(NAV, function(grp) {
    items <- Filter(Negate(is.null), lapply(grp$items, function(it) {

      if (!is.null(it$children)) {
        kids <- Filter(function(ch) can(role, ch$module %||% it$module, "view"), it$children)
        if (!length(kids)) return(NULL)

        # Expand the group only while one of its children is the current page,
        # matching the deck where exactly one group is open at a time.
        expanded <- identical(active_parent, it$id) || active %in% vapply(kids, `[[`, character(1), "id")

        return(tagList(
          div(
            class = paste0("tms-nav-item", if (expanded) " active"),
            onclick = sprintf("Shiny.setInputValue('nav_go','%s',{priority:'event'})", kids[[1]]$id),
            span(class = "ico", fontawesome::fa(it$icon, height = "0.95em", fill = "currentColor")),
            span(it$label),
            span(class = "caret", fontawesome::fa(if (expanded) "chevron-down" else "chevron-right",
                                                  height = "0.7em", fill = "currentColor"))
          ),
          if (expanded) lapply(kids, function(ch) {
            div(
              class = paste0("tms-nav-sub", if (identical(ch$id, active)) " active"),
              onclick = sprintf("Shiny.setInputValue('nav_go','%s',{priority:'event'})", ch$id),
              ch$label
            )
          })
        ))
      }

      if (!can(role, it$module, "view")) return(NULL)
      div(
        class = paste0("tms-nav-item", if (identical(it$id, active)) " active"),
        onclick = sprintf("Shiny.setInputValue('nav_go','%s',{priority:'event'})", it$id),
        span(class = "ico", fontawesome::fa(it$icon, height = "0.95em", fill = "currentColor")),
        span(it$label)
      )
    }))

    if (!length(items)) return(NULL)
    tagList(div(class = "tms-nav-caption", grp$caption), items)
  })

  div(class = "tms-nav", Filter(Negate(is.null), groups))
}
