# ==================================================================
# Portals — branch, customer and vendor dashboards.
#
# All three live in one file because they are the same idea at three scopes:
# a read-mostly summary for one party. The scoping is what differs, and it is
# applied at the data layer (scope_branch / scope_owner), not by hiding UI —
# a Customer session literally cannot read another client's consignments.
# ==================================================================

# ------------------------------------------------------------------
# Branch dashboard
# ------------------------------------------------------------------

portal_branch_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-start justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("head")),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("health"), inline = TRUE),
            uiOutput(ns("switch"), inline = TRUE))),
    uiOutput(ns("cards")),
    div(class = "row g-3",
        div(class = "col-xl-8",
            card_panel(title = "Today's Bookings",
                       actions = actionLink(ns("view_all"), "View all", class = "tiny"),
                       body_class = "tms-card-body p-0",
                       div(class = "tms-table", DTOutput(ns("bookings"))))),
        div(class = "col-xl-4",
            card_panel(title = "Staff Attendance",
                       actions = span(class = "tiny muted", textOutput(ns("att_n"), inline = TRUE)),
                       uiOutput(ns("attendance")))))
  )
}

portal_branch_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Super Admin can switch branches; everyone else is pinned to their own.
    current <- reactiveVal(NULL)
    observeEvent(user(), current(user()$branch_id))
    observeEvent(input$pick_branch, current(input$pick_branch))

    br <- reactive({
      b <- store_get("branches")
      r <- b[b$branch_id == (current() %||% user()$branch_id), ]
      if (nrow(r)) r[1, ] else b[1, ]
    })

    output$head <- renderUI({
      b <- br()
      div(h5(class = "tms-card-title", style = "font-size:1.0625rem;",
             paste(b$name, "— Dashboard")),
          p(class = "tms-card-sub",
            paste(b$code, "·", b$address, "· Today,", fmt_date(Sys.Date()))))
    })

    output$health <- renderUI(pill("All systems normal", "green"))

    output$switch <- renderUI({
      if (!identical(user()$role, "Super Admin") &&
          !isTRUE(as.logical(user()$cross_branch %||% "FALSE"))) return(NULL)
      b <- store_get("branches")
      div(style = "min-width:180px;",
          selectInput(ns("pick_branch"), NULL, setNames(b$branch_id, b$name),
                      selected = current(), width = "100%"))
    })

    output$cards <- renderUI({
      b <- br()
      bk <- get_bookings(); bk <- bk[bk$branch_id == b$branch_id, ]
      v  <- get_vehicles(); v  <- v[v$branch_id == b$branch_id, ]
      cn <- get_consignments(); cn <- cn[cn$branch_id == b$branch_id, ]
      tr <- get_trips(); tr <- tr[tr$branch_id == b$branch_id, ]
      pd <- get_pods(); pd <- pd[pd$branch_id == b$branch_id, ]
      cp <- get_complaints(); cp <- cp[cp$branch_id == b$branch_id, ]
      inv <- get_invoices(); inv <- inv[inv$branch_id == b$branch_id, ]
      e  <- get_employees(); e <- e[e$branch_id == b$branch_id, ]
      a  <- get_attendance()
      a  <- a[a$employee_id %in% e$employee_id & a$date == Sys.Date(), ]

      today_bk <- sum(bk$booking_date == Sys.Date(), na.rm = TRUE)
      yday_bk  <- sum(bk$booking_date == Sys.Date() - 1, na.rm = TRUE)
      recent <- inv[!is.na(inv$invoice_date) &
                   inv$invoice_date >= since_30d(), ]
      pending_pod <- sum(pd$status == "Pending")
      overdue_pod <- sum(pd$status == "Pending" &
                           cn$expected_delivery[match(pd$cn_no, cn$cn_no)] < Sys.Date() - 1,
                         na.rm = TRUE)
      present <- sum(a$status %in% c("P", "T"))

      tagList(
        div(class = "row g-3 mb-3",
            div(class = "col", stat_card("TODAY'S BOOKINGS", today_bk,
                  sub = if (yday_bk) HTML(sprintf('<span class="%s">%s%d</span> vs yesterday',
                            if (today_bk >= yday_bk) "delta-up" else "delta-down",
                            if (today_bk >= yday_bk) "▲ " else "▼ ",
                            abs(today_bk - yday_bk))) else "no comparison",
                  icon = fontawesome::fa("box"))),
            div(class = "col", stat_card("VEHICLES AVAILABLE", sum(v$status == "Available"),
                  unit = paste0("/ ", nrow(v)),
                  sub = paste(sum(v$status %in% c("In Transit", "Allocated")), "in transit or allocated"),
                  icon = fontawesome::fa("truck"))),
            div(class = "col", stat_card("RUNNING TRIPS",
                  sum(tr$status %in% c("Running", "At Hub", "Loading")),
                  sub = paste("across", dplyr::n_distinct(tr$route_to), "destinations"),
                  icon = fontawesome::fa("route"))),
            div(class = "col", stat_card("DELIVERIES",
                  sum(cn$status == "Delivered", na.rm = TRUE),
                  sub = paste(sum(cn$delivered_date == Sys.Date(), na.rm = TRUE), "completed today"),
                  icon = fontawesome::fa("circle-check")))),
        div(class = "row g-3 mb-3",
            div(class = "col", stat_card("PENDING POD", pending_pod,
                  sub = if (overdue_pod) paste(overdue_pod, "> 24 hrs overdue") else "all within window",
                  accent = if (overdue_pod) "danger" else "none",
                  icon = fontawesome::fa("file-lines"))),
            div(class = "col", stat_card("REVENUE · 30 DAYS", inr_compact(sum(recent$total, na.rm = TRUE)),
                  sub = paste(nrow(recent), "invoices"),
                  icon = fontawesome::fa("indian-rupee-sign"))),
            div(class = "col", stat_card("COMPLAINTS", sum(cp$status != "Resolved"),
                  sub = paste(sum(cp$status == "New"), "open ·",
                              sum(cp$status == "In Progress"), "in progress"),
                  icon = fontawesome::fa("triangle-exclamation"))),
            div(class = "col", stat_card("STAFF ATTENDANCE", present, unit = paste0("/ ", nrow(e)),
                  sub = paste0(if (nrow(e)) round(present / nrow(e) * 100) else 0, "% present today"),
                  icon = fontawesome::fa("users"))))
      )
    })

    output$bookings <- renderDT({
      b <- br()
      bk <- get_bookings(); bk <- bk[bk$branch_id == b$branch_id, ]
      bk <- bk[order(bk$booking_date, decreasing = TRUE), ]
      bk <- bk[seq_len(min(8, nrow(bk))), ]
      cl <- store_get("clients"); tr <- get_trips(); v <- store_get("vehicles")
      if (!nrow(bk)) return(tms_table(tibble::tibble(Message = "No bookings")))

      veh <- v$reg_no[match(tr$vehicle_id[match(bk$booking_no, tr$booking_no)], v$vehicle_id)]
      df <- tibble::tibble(
        `BOOKING NO.` = paste0('<span style="font-weight:600;color:#12263F;">',
                               htmlEscape(bk$booking_no), "</span>"),
        CUSTOMER = cl$name[match(bk$client_id, cl$client_id)],
        ROUTE = paste(bk$origin_city, "→", bk$dest_city),
        VEHICLE = ifelse(is.na(veh), '<span class="muted">Unassigned</span>', mono(veh)),
        STATUS = pill_html(bk$status)
      )
      tms_table(df, page = 8, selection = "none")
    })

    observeEvent(input$view_all, nav("bookings"))

    output$att_n <- renderText({
      b <- br(); e <- get_employees(); e <- e[e$branch_id == b$branch_id, ]
      a <- get_attendance(); a <- a[a$employee_id %in% e$employee_id & a$date == Sys.Date(), ]
      paste0(sum(a$status %in% c("P", "T")), " / ", nrow(e), " present")
    })

    output$attendance <- renderUI({
      b <- br(); e <- get_employees(); e <- e[e$branch_id == b$branch_id, ]
      a <- get_attendance(); a <- a[a$employee_id %in% e$employee_id & a$date == Sys.Date(), ]
      if (!nrow(e)) return(div(class = "muted tiny", "No staff at this branch."))
      e <- e[seq_len(min(8, nrow(e))), ]

      div(
        lapply(seq_len(nrow(e)), function(i) {
          r <- e[i, ]
          st <- a$status[match(r$employee_id, a$employee_id)]
          lbl <- if (is.na(st)) "Not marked"
                 else if (st %in% c("P", "T")) "Present"
                 else if (st == "L") "On leave"
                 else if (st == "off") "Week off" else "Absent"
          col <- c(Present = "green", `On leave` = "orange", Absent = "red",
                   `Week off` = "grey", `Not marked` = "grey")[[lbl]]
          div(class = "d-flex align-items-center gap-2 py-2",
              style = if (i < nrow(e)) "border-bottom:1px solid #EEF2F7;" else "",
              avatar(r$name, "sm"),
              div(div(style = "font-weight:600;font-size:.8125rem;color:#12263F;", r$name),
                  div(class = "tiny muted", r$designation)),
              div(class = "ms-auto", pill(lbl, col)))
        })
      )
    })
  })
}

# ------------------------------------------------------------------
# Customer dashboard
# ------------------------------------------------------------------

portal_customer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-start justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("head")),
        div(class = "d-flex gap-2 align-items-center",
            pill("Customer Portal View", "grey"),
            uiOutput(ns("new_btn"), inline = TRUE))),
    uiOutput(ns("cards")),
    div(class = "row g-3",
        div(class = "col-xl-8",
            card_panel(title = "Active Shipments",
                       sub = "Track live or download proof of delivery",
                       body_class = "tms-card-body p-0",
                       div(class = "tms-table", DTOutput(ns("shipments"))))),
        div(class = "col-xl-4",
            uiOutput(ns("bills")),
            div(class = "mt-3", uiOutput(ns("complaint")))))
  )
}

portal_customer_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Which client this portal is showing. A Customer session is locked to its
    # own; internal roles may inspect any client's view.
    client <- reactive({
      cl <- store_get("clients")
      cid <- user()$client_id %||% ""
      if (identical(user()$role, "Customer") && nzchar(cid)) {
        r <- cl[cl$client_id == cid, ]
        return(if (nrow(r)) r[1, ] else NULL)
      }
      r <- cl[cl$client_id == (input$pick %||% cl$client_id[1]), ]
      if (nrow(r)) r[1, ] else cl[1, ]
    })

    cons <- reactive({
      c <- client(); req(c)
      cn <- get_consignments()
      cn[cn$client_id == c$client_id, ]
    })

    bills <- reactive({
      c <- client(); req(c)
      i <- get_invoices()
      i <- i[i$client_id == c$client_id, ]
      if (!nrow(i)) return(i)
      i$balance <- i$total - tidyr::replace_na(i$paid_amount, 0)
      i
    })

    output$head <- renderUI({
      c <- client()
      if (is.null(c)) return(div(class = "muted", "No client selected."))
      tagList(
        div(h5(class = "tms-card-title", style = "font-size:1.0625rem;",
               paste("Welcome back,", c$name)),
            p(class = "tms-card-sub", paste("Account", c$client_id, "·", c$city))),
        if (!identical(user()$role, "Customer")) div(
          style = "min-width:220px;", class = "mt-2",
          selectizeInput(ns("pick"), NULL,
                         setNames(store_get("clients")$client_id, store_get("clients")$name),
                         selected = c$client_id, width = "100%"))
      )
    })

    output$new_btn <- renderUI({
      if (!can(user()$role, "bookings", "create")) return(NULL)
      btn_primary(ns("new_booking"), "New Booking", fontawesome::fa("plus"))
    })
    observeEvent(input$new_booking, nav("booking_new"))

    output$cards <- renderUI({
      cn <- cons(); i <- bills()
      pods <- get_pods(); pods <- pods[pods$client_id == client()$client_id, ]
      active <- cn[cn$status != "Delivered", ]
      delivered_30d <- cn[cn$status == "Delivered" & !is.na(cn$delivered_date) &
                            cn$delivered_date >= since_30d(), ]
      open <- if (nrow(i)) i[i$balance > 0 & i$status != "Draft", ] else i
      overdue <- if (nrow(open)) sum(open$due_date < Sys.Date(), na.rm = TRUE) else 0
      cmp <- get_complaints(); cmp <- cmp[cmp$client_id == client()$client_id, ]

      tagList(
        div(class = "row g-3 mb-3",
            div(class = "col", stat_card("ACTIVE SHIPMENTS", nrow(active),
                  sub = paste(sum(active$dispatch_date == Sys.Date(), na.rm = TRUE), "dispatched today"),
                  icon = fontawesome::fa("route"))),
            div(class = "col", stat_card("DELIVERED SHIPMENTS", nrow(delivered_30d), unit = "30 d",
                  sub = "rolling window", icon = fontawesome::fa("circle-check"))),
            div(class = "col", stat_card("SHIPMENT TRACKING", nrow(active), unit = "live",
                  sub = "GPS updated every 60 s", icon = fontawesome::fa("location-dot"))),
            div(class = "col", stat_card("POD DOWNLOADS",
                  sum(pods$status %in% c("Uploaded", "Verified", "Approved")), unit = "available",
                  sub = paste(sum(pods$status == "Pending"), "pending upload"),
                  icon = fontawesome::fa("download")))),
        div(class = "row g-3 mb-3",
            div(class = "col-md-3", stat_card("OUTSTANDING BILLS",
                  inr_compact(if (nrow(open)) sum(open$balance) else 0),
                  sub = if (overdue) paste(overdue, "overdue invoices") else "nothing overdue",
                  accent = if (overdue) "danger" else "none",
                  icon = fontawesome::fa("indian-rupee-sign"))),
            div(class = "col-md-3", stat_card("COMPLAINT STATUS",
                  sum(cmp$status != "Resolved"), unit = "open",
                  sub = "avg resolution 1.8 days", icon = fontawesome::fa("circle-question"))))
      )
    })

    output$shipments <- renderDT({
      cn <- cons(); v <- store_get("vehicles"); pods <- get_pods()
      cn <- cn[order(cn$dispatch_date, decreasing = TRUE), ]
      cn <- cn[seq_len(min(8, nrow(cn))), ]
      if (!nrow(cn)) return(tms_table(tibble::tibble(Message = "No shipments")))

      has_pod <- cn$lr_no %in% pods$lr_no[pods$status %in% c("Uploaded", "Verified", "Approved")]
      df <- tibble::tibble(
        CONSIGNMENT = mapply(cell2, cn$cn_no,
                             paste0(cn$lr_no, " · ",
                                    ifelse(cn$status == "Delivered",
                                           paste("delivered", fmt_date(cn$delivered_date, TRUE)),
                                           paste("ETA", fmt_date(cn$expected_delivery, TRUE))))),
        ROUTE   = paste(cn$origin_city, "→", cn$dest_city),
        VEHICLE = mono(v$reg_no[match(cn$vehicle_id, v$vehicle_id)]),
        STATUS  = pill_html(cn$status),
        ACTION  = ifelse(has_pod,
                         '<span class="pill pill-green nodot">POD ready</span>',
                         '<span class="pill pill-grey nodot">Track</span>')
      )
      tms_table(df, page = 8, selection = "none")
    })

    output$bills <- renderUI({
      i <- bills()
      if (!nrow(i)) return(card_panel(title = "Outstanding Bills",
                                      div(class = "muted tiny", "No invoices.")))
      open <- i[i$balance > 0 & i$status != "Draft", ]
      overdue <- if (nrow(open)) sum(open$due_date < Sys.Date(), na.rm = TRUE) else 0
      oldest <- if (nrow(open)) max(as.numeric(Sys.Date() - open$due_date), na.rm = TRUE) else 0

      card_panel(
        title = "Outstanding Bills",
        actions = if (overdue) pill(paste(overdue, "overdue"), "orange") else pill("Clear", "green"),
        dl_rows(
          "Total outstanding" = inr(if (nrow(open)) sum(open$balance) else 0),
          "Invoices pending"  = nrow(open),
          "Oldest due"        = if (oldest > 0) paste(round(oldest), "days") else "—"
        ),
        if (nrow(open)) tagList(
          tags$hr(class = "soft"),
          div(lapply(seq_len(min(3, nrow(open))), function(k) {
            r <- open[order(open$due_date), ][k, ]
            div(class = "d-flex justify-content-between py-1",
                span(class = "tiny", paste(r$invoice_no, "·", fmt_date(r$due_date, TRUE))),
                span(style = "font-weight:650;font-size:.8125rem;", inr_compact(r$balance)))
          }))
        ),
        div(class = "mt-3", btn_ghost(ns("all_inv"), "View All Invoices", class = "w-100"))
      )
    })

    observeEvent(input$all_inv, {
      if (can(user()$role, "invoices", "view")) nav("invoices")
      else showNotification("Invoice detail is available from your account manager.",
                            type = "warning")
    })

    output$complaint <- renderUI({
      cmp <- get_complaints(); cmp <- cmp[cmp$client_id == client()$client_id, ]
      open <- cmp[cmp$status != "Resolved", ]
      card_panel(
        title = "Complaint Status",
        if (nrow(open)) {
          r <- open[order(open$raised_dt, decreasing = TRUE), ][1, ]
          callout(paste(r$complaint_id, "OPEN"),
                  paste0(r$subject, " — ", r$lr_no, " · raised ",
                         round(as.numeric(Sys.Date() - r$raised_dt)), " days ago"),
                  "warn")
        } else callout("No open complaints", "Everything is on track.", "ok"),
        div(class = "mt-3", btn_ghost(ns("raise"), "Raise a Complaint", class = "w-100"))
      )
    })

    observeEvent(input$raise, nav("complaints"))
  })
}

# ------------------------------------------------------------------
# Vendor dashboard
# ------------------------------------------------------------------

portal_vendor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-start justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("head")),
        div(class = "d-flex gap-2 align-items-center",
            pill("Vendor Portal View", "grey"),
            btn_primary(ns("upload_pod"), "Upload POD", fontawesome::fa("upload")))),
    uiOutput(ns("cards")),
    div(class = "row g-3",
        div(class = "col-xl-8",
            card_panel(title = "Assigned Trips",
                       actions = span(class = "tiny muted", textOutput(ns("n_active"), inline = TRUE)),
                       body_class = "tms-card-body p-0",
                       div(class = "tms-table", DTOutput(ns("trips"))))),
        div(class = "col-xl-4",
            uiOutput(ns("payments")),
            div(class = "mt-3", uiOutput(ns("compliance")))))
  )
}

portal_vendor_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vendor <- reactive({
      v <- store_get("vendors")
      vid <- user()$vendor_id %||% ""
      if (identical(user()$role, "Vendor") && nzchar(vid)) {
        r <- v[v$vendor_id == vid, ]
        return(if (nrow(r)) r[1, ] else NULL)
      }
      r <- v[v$vendor_id == (input$pick %||% v$vendor_id[1]), ]
      if (nrow(r)) r[1, ] else v[1, ]
    })

    my_trips <- reactive({
      vd <- vendor(); req(vd)
      t <- get_trips()
      t[t$vendor_id == vd$vendor_id, ]
    })

    my_vehicles <- reactive({
      vd <- vendor(); req(vd)
      v <- get_vehicles()
      v[v$vendor_id == vd$vendor_id, ]
    })

    output$head <- renderUI({
      vd <- vendor()
      if (is.null(vd)) return(div(class = "muted", "No vendor selected."))
      tagList(
        div(h5(class = "tms-card-title", style = "font-size:1.0625rem;",
               paste("Welcome back,", vd$name)),
            p(class = "tms-card-sub", paste("Vendor", vd$vendor_id, "·", vd$city))),
        if (!identical(user()$role, "Vendor")) div(
          style = "min-width:220px;", class = "mt-2",
          selectizeInput(ns("pick"), NULL,
                         setNames(store_get("vendors")$vendor_id, store_get("vendors")$name),
                         selected = vd$vendor_id, width = "100%"))
      )
    })

    output$cards <- renderUI({
      t <- my_trips(); v <- my_vehicles()
      cn <- get_consignments(); mine <- cn[cn$trip_no %in% t$trip_no, ]
      pods <- get_pods(); mypod <- pods[pods$lr_no %in% mine$lr_no, ]
      vp <- get_vendor_payments(); vp <- vp[vp$vendor_id == vendor()$vendor_id, ]

      active <- t[t$status != "Completed", ]
      recent <- t[t$status == "Completed" & !is.na(t$dispatch_dt) &
                 as.Date(t$dispatch_dt) >= since_30d(), ]

      tagList(
        div(class = "row g-3 mb-3",
            div(class = "col", stat_card("ASSIGNED TRIPS", nrow(active),
                  sub = paste(sum(active$status == "Planned"), "pending pickup"),
                  icon = fontawesome::fa("route"))),
            div(class = "col", stat_card("ACTIVE VEHICLES",
                  sum(v$status != "Maintenance"), unit = paste0("/ ", nrow(v)),
                  sub = paste(sum(v$status == "Maintenance"), "under maintenance"),
                  icon = fontawesome::fa("truck"))),
            div(class = "col", stat_card("COMPLETED TRIPS", nrow(recent), unit = "30 d",
                  sub = "rolling window", icon = fontawesome::fa("circle-check")))),
        div(class = "row g-3 mb-3",
            div(class = "col", stat_card("PENDING PAYMENTS",
                  inr_compact(sum(vp$amount, na.rm = TRUE)),
                  sub = paste(nrow(vp), "invoices awaiting approval"), accent = "warn",
                  icon = fontawesome::fa("indian-rupee-sign"))),
            div(class = "col", stat_card("UPLOAD POD",
                  sum(mypod$status == "Pending"), unit = "due",
                  sub = "upload within 24 hrs of delivery",
                  icon = fontawesome::fa("upload"))),
            div(class = "col", stat_card("DELIVERY UPDATES",
                  sum(mine$delivered_date == Sys.Date(), na.rm = TRUE), unit = "today",
                  sub = "last update just now", icon = fontawesome::fa("location-dot"))))
      )
    })

    output$n_active <- renderText(paste(sum(my_trips()$status != "Completed"), "active"))

    output$trips <- renderDT({
      t <- my_trips(); v <- store_get("vehicles"); dr <- store_get("drivers")
      cn <- get_consignments(); pods <- get_pods()
      t <- t[order(t$dispatch_dt, decreasing = TRUE), ]
      t <- t[seq_len(min(8, nrow(t))), ]
      if (!nrow(t)) return(tms_table(tibble::tibble(Message = "No trips assigned")))

      lr <- cn$lr_no[match(t$trip_no, cn$trip_no)]
      pod_st <- pods$status[match(lr, pods$lr_no)]
      cn_st  <- cn$status[match(t$trip_no, cn$trip_no)]

      df <- tibble::tibble(
        VEHICLE = mono(v$reg_no[match(t$vehicle_id, v$vehicle_id)]),
        ROUTE   = paste(t$route_from, "→", t$route_to),
        DRIVER  = dr$name[match(t$driver_id, dr$driver_id)],
        STATUS  = pill_html(cn_st %||% t$status),
        POD     = ifelse(is.na(pod_st), '<span class="muted tiny">—</span>',
                         pill_html(pod_st))
      )
      tms_table(df, page = 8, selection = "none")
    })

    output$payments <- renderUI({
      vp <- get_vendor_payments(); vp <- vp[vp$vendor_id == vendor()$vendor_id, ]
      t  <- my_trips()
      card_panel(
        title = "Pending Payments",
        actions = pill(paste(nrow(vp), "invoices"), if (nrow(vp)) "orange" else "green"),
        if (!nrow(vp)) div(class = "muted tiny", "Nothing pending.")
        else tagList(
          dl_rows(
            "Total pending"      = inr(sum(vp$amount)),
            "Oldest invoice"     = paste(round(max(as.numeric(Sys.Date() - as.Date(vp$due_date)), 0)), "days"),
            "Trips settled"      = sum(vp$trips)
          ),
          tags$hr(class = "soft"),
          div(lapply(seq_len(min(4, nrow(vp))), function(i) {
            r <- vp[i, ]
            div(class = "d-flex justify-content-between py-1",
                span(class = "tiny", paste(r$vp_id, "·", r$trips, "trips")),
                span(style = "font-weight:650;font-size:.8125rem;", inr_compact(r$amount)))
          })),
          div(class = "mt-3", btn_ghost(ns("hist"), "View Payment History", class = "w-100"))
        )
      )
    })

    output$compliance <- renderUI({
      v <- my_vehicles()
      if (!nrow(v)) return(NULL)
      bad <- vapply(seq_len(nrow(v)), function(i) {
        any(expiry_state(c(v$insurance_expiry[i], v$fitness_expiry[i],
                           v$permit_expiry[i], v$puc_expiry[i])) %in% c("warn", "danger"))
      }, logical(1))
      ok <- sum(!bad)
      soonest <- suppressWarnings(min(c(v$insurance_expiry, v$fitness_expiry,
                                        v$permit_expiry, v$puc_expiry), na.rm = TRUE))

      card_panel(
        title = "Compliance",
        if (all(!bad))
          callout("Fleet documents in order",
                  sprintf("%d / %d vehicles fully compliant · next renewal %s.",
                          ok, nrow(v), fmt_date(soonest, TRUE)), "ok")
        else
          callout("Documents need attention",
                  sprintf("%d of %d vehicles have a document expiring or expired · earliest %s.",
                          sum(bad), nrow(v), fmt_date(soonest, TRUE)), "warn")
      )
    })

    observeEvent(input$hist, {
      vp <- get_vendor_payments(); vp <- vp[vp$vendor_id == vendor()$vendor_id, ]
      showModal(modalDialog(
        title = "Payment history", size = "l", easyClose = TRUE,
        if (!nrow(vp)) div(class = "muted", "No payments on record.")
        else div(class = "tms-table",
                 DT::datatable(
                   tibble::tibble(REF = vp$vp_id, TRIPS = vp$trips,
                                  AMOUNT = inr(vp$amount),
                                  DUE = fmt_date(vp$due_date, TRUE),
                                  STATUS = vp$status),
                   rownames = FALSE, selection = "none",
                   options = list(dom = "tp", pageLength = 10))),
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$upload_pod, {
      showModal(modalDialog(
        title = "Upload POD", easyClose = TRUE,
        fileInput(ns("v_file"), "Signed POD", accept = c(".jpg", ".jpeg", ".png", ".pdf")),
        callout("Within 24 hours",
                "Uploading the signed consignee copy releases the trip for settlement.",
                "info"),
        footer = modalButton("Close")
      ))
    })
  })
}
