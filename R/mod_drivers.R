# ==================================================================
# Drivers.
#
# The deck's own note on this screen — "ONE RECORD, EVERYWHERE … powers trip
# assignment (licence checks), attendance and HRMS payroll settlement" — is the
# design decision this module implements: `drivers` is not a second people
# table, it is the licence/depot/performance extension of an employee record,
# joined on driver_id == employee_id. See DESIGN-REVIEW.md, gap #6.
# ==================================================================

drivers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("lic_warn"), inline = TRUE),
            uiOutput(ns("add_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl"))),
                 foot = "Driver master carries licence & Aadhaar details with expiry alerts, assigned vehicle and attendance/performance history"),
      uiOutput(ns("panel"))
    )
  )
}

drivers_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")

    # Join the employee record in once; every panel below reads this.
    all_rows <- reactive({
      d <- get_drivers()
      e <- get_employees()
      d <- scope_branch(d, user(), col = "depot_branch_id")
      if (!nrow(d)) return(d)
      i <- match(d$driver_id, e$employee_id)
      d$branch_id <- e$branch_id[i]
      d$joined    <- e$joined[i]
      d$aadhaar   <- e$aadhaar[i]
      d$salary    <- e$salary[i]
      d
    })

    lic_due <- reactive({
      d <- all_rows()
      if (!nrow(d)) return(0L)
      sum(expiry_state(d$licence_expiry) %in% c("warn", "danger"))
    })

    output$chips <- renderUI({
      d <- all_rows()
      items  <- setNames(c("All", DRIVER_STATUS), c("All", DRIVER_STATUS))
      counts <- as.list(c(nrow(d), vapply(DRIVER_STATUS, function(s) sum(d$status == s), integer(1))))
      names(counts) <- c("All", DRIVER_STATUS)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$lic_warn <- renderUI({
      n <- lic_due()
      if (!n) return(NULL)
      div(class = "chip", style = "border-color:#EFA31D;color:#B3701A;",
          span(style = "width:6px;height:6px;border-radius:50%;background:#EFA31D;"),
          sprintf("%d licence%s expiring ≤ 30 d", n, if (n == 1) "" else "s"))
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "drivers", "create")) return(NULL)
      btn_primary(ns("add"), "Add Driver", fontawesome::fa("plus"))
    })

    rows <- reactive({
      d <- all_rows()
      if (!identical(filt(), "All")) d <- d[d$status == filt(), ]
      d
    })

    output$tbl <- renderDT({
      d <- rows(); v <- store_get("vehicles"); b <- store_get("branches")
      df <- tibble::tibble(
        DRIVER = mapply(function(n, m, dep) paste0(
          '<div class="d-flex align-items-center gap-2">', avatar_html(n),
          '<div><div style="font-weight:600;color:#12275C;">', htmlEscape(n),
          '</div><div style="font-size:.6875rem;color:#64748B;">', htmlEscape(m),
          " · ", htmlEscape(dep), " depot</div></div></div>"),
          d$name, d$mobile, b$city[match(d$depot_branch_id, b$branch_id)]),
        LICENCE = mono(d$licence_no),
        EXPIRY  = expiry_badge(d$licence_expiry, warn = 60),
        `ASSIGNED VEHICLE` = ifelse(nzchar(d$assigned_vehicle_id),
                                    mono(v$reg_no[match(d$assigned_vehicle_id, v$vehicle_id)]), "—"),
        TRIPS = d$trips_lifetime,
        `ON-TIME %` = paste0(d$on_time_pct, "%"),
        STATUS = pill_html(d$status)
      )
      tms_table(df, page = 12, align = align_right(4:5))
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    output$panel <- renderUI({
      d <- sel()
      if (is.null(d)) return(card_panel(empty_panel("Select a driver for the 360° view")))
      v <- store_get("vehicles"); b <- store_get("branches")
      tr <- get_trips(); cur <- tr[tr$driver_id == d$driver_id & tr$status != "Completed", ]

      # Attendance is trip-derived for drivers — no punch while on the road.
      att <- get_attendance()
      att <- att[att$employee_id == d$driver_id &
                   att$date >= since_30d(), ]
      present <- sum(att$status %in% c("P", "T"))
      workdays <- sum(att$status != "off")

      qtr_trips <- tr[tr$driver_id == d$driver_id &
                        !is.na(tr$dispatch_dt) &
                        as.Date(tr$dispatch_dt) >= lubridate::floor_date(Sys.Date(), "quarter"), ]

      lic_st <- expiry_state(d$licence_expiry, 60)

      card_panel(
        div(class = "d-flex align-items-start gap-3 mb-3",
            avatar(d$name, "lg"),
            div(h6(class = "tms-card-title", paste(d$name, "— 360° view")),
                p(class = "tms-card-sub",
                  paste("Driver since", format(as.Date(d$joined), "%b %Y"), "·",
                        b$city[match(d$depot_branch_id, b$branch_id)], "depot ·",
                        d$trips_lifetime, "trips lifetime"))),
            div(class = "ms-auto", pill(d$status))),

        dl_rows(
          "Driving licence" = HTML(paste0('<span class="mono">', d$licence_no,
                                          "</span> · ", d$licence_class)),
          "Licence expiry"  = HTML(sprintf('<span class="pill pill-%s">%s</span>',
                                           c(none = "grey", danger = "red",
                                             warn = "orange", ok = "green")[lic_st],
                                           fmt_date(d$licence_expiry))),
          "Aadhaar"         = HTML(paste0('<span class="mono">', d$aadhaar, "</span> · ✓ verified")),
          "Contact"         = span(class = "mono", d$mobile),
          "Emergency contact" = span(class = "mono", d$emergency_contact),
          "Assigned vehicle"= if (nzchar(d$assigned_vehicle_id))
                                span(class = "mono", v$reg_no[match(d$assigned_vehicle_id, v$vehicle_id)])
                              else "—",
          "Current trip"    = if (nrow(cur))
                                paste0(cur$trip_no[1], " · ", cur$route_from[1], " → ", cur$route_to[1])
                              else "—"
        ),

        tags$hr(class = "soft"),
        div(class = "form-section", "Performance history"),
        div(
          div(class = "d-flex justify-content-between tiny mb-1",
              span("On-time delivery"), span(style = "font-weight:650;", paste0(d$on_time_pct, "%"))),
          progress_bar(d$on_time_pct, "green"),
          div(class = "d-flex justify-content-between tiny mb-1 mt-3",
              span("Trips completed (this qtr)"),
              span(style = "font-weight:650;",
                   paste0(sum(qtr_trips$status == "Completed"), " / ", nrow(qtr_trips)))),
          progress_bar(if (nrow(qtr_trips)) sum(qtr_trips$status == "Completed") / nrow(qtr_trips) * 100 else 0, "blue"),
          div(class = "d-flex justify-content-between tiny mb-1 mt-3",
              span("Attendance (30 d)"),
              span(style = "font-weight:650;", paste0(present, " / ", workdays))),
          progress_bar(if (workdays) present / workdays * 100 else 0, "amber")
        ),

        div(class = "mt-3",
            callout("One record, everywhere",
                    "This master powers trip assignment (licence checks), attendance and HRMS payroll settlement.",
                    "info", fontawesome::fa("circle-info"))),

        div(class = "d-flex gap-2 mt-3",
            btn_ghost(ns("open_emp"), "Open employee record", class = "flex-fill"),
            if (nrow(cur)) btn_dark(ns("open_trip"), "View trip", class = "flex-fill"))
      )
    })

    observeEvent(input$open_emp,  nav("hrms_employees"))
    observeEvent(input$open_trip, nav("trips"))

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "drivers", "create")) return()
      # A driver must exist as an employee first — that is the whole point of
      # the one-record rule. Offer only employees who are not yet drivers.
      e <- get_employees()
      d <- get_drivers()
      cand <- e[!(e$employee_id %in% d$driver_id), ]
      b <- store_get("branches")
      showModal(modalDialog(
        title = "Add driver", size = "l", easyClose = TRUE,
        callout("Drivers extend an employee record",
                "Pick an existing employee. If the person is not on the payroll yet, create them under HRMS → Employees first.",
                "info"),
        div(class = "row g-3 mt-1",
            div(class = "col-12", selectInput(ns("n_emp"), "Employee",
                                              setNames(cand$employee_id,
                                                       paste0(cand$name, " · ", cand$designation)))),
            div(class = "col-6", textInput(ns("n_lic"), "Licence no.")),
            div(class = "col-3", selectInput(ns("n_class"), "Class", c("HGV", "HMV", "HTV"))),
            div(class = "col-3", dateInput(ns("n_exp"), "Expiry", value = Sys.Date() + 1095)),
            div(class = "col-6", selectInput(ns("n_depot"), "Depot",
                                             setNames(b$branch_id, b$name))),
            div(class = "col-6", textInput(ns("n_emg"), "Emergency contact"))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("n_save"), "Add driver"))
      ))
    })

    observeEvent(input$n_save, {
      if (!require_perm(session, user()$role, "drivers", "create")) return()
      req(input$n_emp)
      if (!nzchar(input$n_lic %||% "")) {
        showNotification("Licence number is required.", type = "error"); return()
      }
      e <- get_employees(); r <- e[e$employee_id == input$n_emp, ]
      req(nrow(r)); r <- r[1, ]

      store_insert("drivers", list(
        driver_id = r$employee_id, name = r$name, mobile = r$mobile,
        licence_no = input$n_lic, licence_class = input$n_class,
        licence_expiry = as.character(input$n_exp),
        depot_branch_id = input$n_depot, assigned_vehicle_id = "",
        emergency_contact = input$n_emg,
        trips_lifetime = 0, on_time_pct = 100, status = "Available"
      ))
      # Keep the employee record consistent with its new driver extension.
      store_update("employees", list(employee_id = r$employee_id),
                   list(is_driver = "TRUE", designation = "Driver", department = "Fleet"))
      audit(user()$user_id, "create", "drivers", r$employee_id)
      removeModal()
      showNotification(paste(r$name, "added to the driver master."), type = "message")
    })
  })
}
