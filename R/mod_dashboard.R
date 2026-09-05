# ==================================================================
# Dashboard — the Super Admin landing screen from the deck.
#
# Every figure here is derived from the same tables the detail screens read, so
# the headline counts and the drill-downs cannot drift apart. Branch-scoped
# users see their own slice: the cards recompute rather than showing group
# totals a Branch Admin has no business seeing.
# ==================================================================

dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),

    div(
      class = "row g-3",
      div(
        class = "col-xl-8",
        card_panel(
          title = "Live Fleet Snapshot",
          sub = textOutput(ns("gps_sub"), inline = TRUE),
          actions = tagList(
            uiOutput(ns("gps_health"), inline = TRUE),
            btn_ghost(ns("open_gps"), "Open Live GPS", class = "btn-tms-sm")
          ),
          body_class = "tms-card-body p-0",
          if (HAS_LEAFLET) leaflet::leafletOutput(ns("map"), height = 400)
          else div(class = "p-4", callout("Map unavailable",
                   "Install the leaflet package to render the fleet map.", "warn"))
        )
      ),
      div(
        class = "col-xl-4",
        card_panel(
          title = "Fleet Status",
          actions = span(class = "tiny muted", textOutput(ns("fleet_total"), inline = TRUE)),
          div(class = "row align-items-center g-2",
              div(class = "col-5", plotlyOutput(ns("donut"), height = 150)),
              div(class = "col-7", uiOutput(ns("fleet_legend"))))
        ),
        div(class = "mt-3",
            card_panel(
              title = "Latest Complaints",
              actions = actionLink(ns("all_complaints"), textOutput(ns("cmp_count"), inline = TRUE),
                                   class = "tiny"),
              uiOutput(ns("complaints"))
            ))
      )
    ),

    div(
      class = "row g-3 mt-0",
      div(class = "col-xl-4",
          card_panel(title = "Deliveries per Month", sub = "Last 6 months",
                     plotlyOutput(ns("bars"), height = 240))),
      div(class = "col-xl-4",
          card_panel(title = "Pending POD",
                     actions = actionLink(ns("go_pod"), "Go to POD", class = "tiny"),
                     body_class = "tms-card-body p-0",
                     div(class = "tms-table", DTOutput(ns("pod_tbl"))))),
      div(class = "col-xl-4",
          card_panel(title = "Payroll Summary",
                     actions = uiOutput(ns("pay_period"), inline = TRUE),
                     uiOutput(ns("payroll"))))
    )
  )
}

dashboard_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Every screen-level dataset is scoped once here and reused, so a Branch
    # Admin's dashboard is consistent across all twelve panels.
    d_branches <- reactive(store_get("branches"))
    d_vehicles <- reactive(scope_branch(get_vehicles(), user()))
    d_bookings <- reactive(scope_branch(get_bookings(), user()))
    d_clients  <- reactive(scope_branch(get_clients(),  user()))
    d_vendors  <- reactive(scope_branch(store_get("vendors"), user()))
    d_cn       <- reactive(scope_branch(get_consignments(), user()))
    d_cmp      <- reactive(scope_branch(get_complaints(), user()))

    output$cards <- renderUI({
      v  <- d_vehicles(); b <- d_bookings()
      active_v <- sum(v$status %in% c("Available", "Allocated", "In Transit"))
      today    <- sum(b$booking_date == Sys.Date(), na.rm = TRUE)
      yday     <- sum(b$booking_date == Sys.Date() - 1, na.rm = TRUE)
      delta    <- if (yday > 0) round((today - yday) / yday * 100) else NA

      # Branch Admins see one branch, so the branch card would always read "1".
      # Swap it for something useful at that scope.
      is_scoped <- !identical(user()$role, "Super Admin") &&
        !isTRUE(as.logical(user()$cross_branch %||% "FALSE"))

      stat_row(
        if (is_scoped)
          stat_card("MY BRANCH",
                    d_branches()$city[match(user()$branch_id, d_branches()$branch_id)] %||% "—",
                    sub = d_branches()$code[match(user()$branch_id, d_branches()$branch_id)],
                    icon = fontawesome::fa("building"))
        else
          stat_card("TOTAL BRANCHES", nrow(d_branches()),
                    sub = paste("across", dplyr::n_distinct(d_branches()$state), "states"),
                    icon = fontawesome::fa("building")),

        stat_card("TOTAL CUSTOMERS", nrow(d_clients()),
                  sub = paste(sum(as.Date(d_clients()$since) >= Sys.Date() - 30, na.rm = TRUE),
                              "new this month"),
                  icon = fontawesome::fa("book")),

        stat_card("TOTAL VENDORS", nrow(d_vendors()),
                  sub = paste(sum(store_get("vehicles")$owner_type == "Vendor"), "vendor vehicles"),
                  icon = fontawesome::fa("briefcase")),

        stat_card("ACTIVE VEHICLES", active_v, unit = paste0("/ ", nrow(v)),
                  sub = paste(sum(v$status == "In Transit"), "in transit ·",
                              sum(v$status == "Maintenance"), "in maintenance"),
                  icon = fontawesome::fa("truck")),

        stat_card("BOOKINGS TODAY", today,
                  sub = if (is.na(delta)) "no comparison" else
                    HTML(sprintf('<span class="%s">%s%s%%</span> vs yesterday',
                                 if (delta >= 0) "delta-up" else "delta-down",
                                 if (delta >= 0) "▲ " else "▼ ", abs(delta))),
                  icon = fontawesome::fa("box"))
      )
    })

    # ---------------- Fleet map ----------------

    gps <- reactive({
      g <- gps_latest()
      v <- d_vehicles()
      g[g$vehicle_id %in% v$vehicle_id, , drop = FALSE]
    })

    output$gps_sub <- renderText({
      g <- gps()
      if (!nrow(g)) return("No live feed")
      age <- round(as.numeric(difftime(Sys.time(), max(g$ts, na.rm = TRUE), units = "secs")))
      sprintf("GPS feed · updated %s ago · %d vehicles",
              if (age < 90) paste0(age, " s") else paste0(round(age / 60), " min"), nrow(g))
    })

    output$gps_health <- renderUI({
      if (nrow(gps())) pill("GPS feed healthy", "green") else pill("No feed", "grey")
    })

    output$map <- leaflet::renderLeaflet({
      req(HAS_LEAFLET)
      g <- gps()
      v <- store_get("vehicles")
      if (!nrow(g)) {
        return(leaflet::leaflet() |>
                 leaflet::addTiles(urlTemplate = CARTO_LIGHT, attribution = CARTO_ATTR, options = leaflet::tileOptions(maxZoom = 19)) |>
                 leaflet::setView(78.9, 22.5, zoom = 5))
      }
      g$reg <- v$reg_no[match(g$vehicle_id, v$vehicle_id)]
      cols <- c(Running = "#1A7F45", Halted = "#EFA31D", Alert = "#C0392B")

      leaflet::leaflet(g) |>
        leaflet::addTiles(urlTemplate = CARTO_LIGHT, attribution = CARTO_ATTR, options = leaflet::tileOptions(maxZoom = 19)) |>
        leaflet::addCircleMarkers(
          ~lon, ~lat, radius = 7, weight = 2, color = "#fff", opacity = 1,
          fillColor = ~unname(cols[status]), fillOpacity = .95,
          label = ~sprintf("%s · %s km/h · %s", reg, speed, status),
          popup = ~sprintf("<b>%s</b><br/>%s · %s km/h<br/>Trip %s", reg, status, speed, trip_no)
        ) |>
        leaflet::addLegend("bottomleft", colors = unname(cols), labels = names(cols),
                           opacity = .95, title = NULL)
    })

    observeEvent(input$open_gps, nav("livegps"))
    observeEvent(input$go_pod,   nav("pod"))
    observeEvent(input$all_complaints, nav("complaints"))

    # ---------------- Fleet donut ----------------

    fleet_counts <- reactive({
      v <- d_vehicles()
      tibble::tibble(status = VEHICLE_STATUS) |>
        dplyr::mutate(n = vapply(status, function(s) sum(v$status == s), integer(1)))
    })

    output$fleet_total <- renderText(paste(nrow(d_vehicles()), "vehicles"))

    output$donut <- renderPlotly({
      fc <- fleet_counts()
      cols <- c("Available" = "#1A7F45", "Allocated" = "#1667C7", "In Transit" = "#6F42C1",
                "Maintenance" = "#EFA31D", "Inactive" = "#94A3B8")
      plot_ly(fc, labels = ~status, values = ~n, type = "pie", hole = .68,
              marker = list(colors = unname(cols[fc$status]),
                            line = list(color = "#fff", width = 2)),
              textinfo = "none", hoverinfo = "label+value", sort = FALSE) |>
        layout(showlegend = FALSE,
               margin = list(l = 0, r = 0, t = 0, b = 0),
               paper_bgcolor = "rgba(0,0,0,0)",
               annotations = list(
                 text = paste0("<b>", nrow(d_vehicles()), "</b>"),
                 showarrow = FALSE, font = list(size = 20, color = "#12275C"))) |>
        config(displayModeBar = FALSE)
    })

    output$fleet_legend <- renderUI({
      fc <- fleet_counts()
      cols <- c("Available" = "#1A7F45", "Allocated" = "#1667C7", "In Transit" = "#6F42C1",
                "Maintenance" = "#EFA31D", "Inactive" = "#94A3B8")
      div(
        lapply(seq_len(nrow(fc)), function(i) {
          div(class = "d-flex align-items-center gap-2 py-1",
              span(style = sprintf("width:8px;height:8px;border-radius:50%%;background:%s;flex:0 0 8px;",
                                   cols[[fc$status[i]]])),
              span(style = "font-size:.8125rem;", fc$status[i]),
              span(class = "ms-auto", style = "font-weight:650;font-size:.8125rem;", fc$n[i]))
        })
      )
    })

    # ---------------- Complaints ----------------

    output$cmp_count <- renderText({
      paste0("View all (", sum(d_cmp()$status != "Resolved"), ")")
    })

    output$complaints <- renderUI({
      c <- d_cmp()
      cl <- store_get("clients"); br <- store_get("branches")
      c <- c[order(c$raised_dt, decreasing = TRUE), ][seq_len(min(4, nrow(c))), ]
      if (!nrow(c)) return(div(class = "muted tiny", "No complaints."))

      div(
        lapply(seq_len(nrow(c)), function(i) {
          r <- c[i, ]
          div(
            class = "d-flex gap-2 py-2",
            style = if (i < nrow(c)) "border-bottom:1px solid #EEF2F7;" else "",
            div(style = "flex:0 0 auto;", pill(r$status)),
            div(style = "min-width:0;",
                div(style = "font-size:.8125rem;font-weight:600;color:#12275C;",
                    paste0(r$subject, " — ", r$lr_no)),
                div(class = "tiny muted",
                    paste(cl$name[match(r$client_id, cl$client_id)],
                          "·", br$name[match(r$branch_id, br$branch_id)],
                          "·", fmt_date(r$raised_dt, TRUE))))
          )
        })
      )
    })

    # ---------------- Deliveries per month ----------------

    output$bars <- renderPlotly({
      cn <- d_cn()
      cn <- cn[cn$status == "Delivered" & !is.na(cn$delivered_date), ]
      months <- seq(lubridate::floor_date(Sys.Date() - 150, "month"),
                    lubridate::floor_date(Sys.Date(), "month"), by = "month")
      cnt <- vapply(months, function(m) {
        sum(lubridate::floor_date(cn$delivered_date, "month") == m, na.rm = TRUE)
      }, integer(1))
      df <- tibble::tibble(m = format(months, "%b"), n = cnt)
      # Highlight the current month, as the deck does.
      df$col <- c(rep("#AFC6DE", nrow(df) - 1), "#12275C")

      plot_ly(df, x = ~m, y = ~n, type = "bar",
              marker = list(color = ~col), hoverinfo = "y",
              text = ~ifelse(seq_along(n) == length(n), n, ""),
              textposition = "outside", textfont = list(size = 11, color = "#12275C")) |>
        layout(xaxis = list(title = "", tickfont = list(size = 11, color = "#64748B"),
                            showgrid = FALSE, zeroline = FALSE),
               yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
               margin = list(l = 4, r = 4, t = 18, b = 4),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
               bargap = .45, showlegend = FALSE) |>
        config(displayModeBar = FALSE)
    })

    # ---------------- Pending POD ----------------

    output$pod_tbl <- renderDT({
      p  <- scope_branch(get_pods(), user())
      p  <- p[p$status %in% c("Pending", "Uploaded"), ]
      cn <- get_consignments(); v <- store_get("vehicles"); dr <- store_get("drivers")
      if (!nrow(p)) return(tms_table(tibble::tibble(Message = "Nothing pending"), page = 4))

      i <- match(p$lr_no, cn$lr_no)
      # Age of the delivery, which is what makes a POD "overdue".
      due <- as.numeric(Sys.Date() - as.Date(cn$expected_delivery[i]))
      ord <- order(due, decreasing = TRUE)
      p <- p[ord, ]; i <- i[ord]; due <- due[ord]
      k <- seq_len(min(4, nrow(p)))

      # The age chip is coloured by how overdue the POD is, not by a status
      # word, so it is built directly rather than through pill_html().
      age_lbl <- ifelse(due[k] > 1, paste0(round(due[k]), " days"),
                        paste0(pmax(1, round(due[k] * 24)), " hrs"))
      age_col <- ifelse(due[k] > 3, "red", ifelse(due[k] > 1, "orange", "grey"))

      df <- tibble::tibble(
        Consignment = mapply(cell2, p$lr_no[k],
                             paste(v$reg_no[match(cn$vehicle_id[i][k], v$vehicle_id)], "·",
                                   dr$name[match(cn$driver_id[i][k], dr$driver_id)])),
        Route   = paste(cn$origin_city[i][k], "→", cn$dest_city[i][k]),
        Pending = sprintf('<span class="pill pill-%s">%s</span>', age_col, age_lbl)
      )
      tms_table(df, page = 4, selection = "none")
    })

    # ---------------- Payroll ----------------

    # The most recent run, which is the month that has closed — not today's.
    pay_run <- reactive({
      p <- get_payroll()
      if (!nrow(p)) return(p)
      periods <- unique(p$period)
      latest <- periods[which.max(as.Date(paste("01", periods), format = "%d %b %Y"))]
      p[p$period == latest, ]
    })

    output$pay_period <- renderUI({
      p <- pay_run()
      pill(if (nrow(p)) p$period[1] else format(Sys.Date(), "%b %Y"), "blue")
    })

    output$payroll <- renderUI({
      if (!can(user()$role, "hrms_payroll", "view")) {
        return(div(class = "muted tiny", "Payroll is not visible to your role."))
      }
      pr <- pay_run()
      emp <- get_employees()
      pr <- pr[pr$employee_id %in% scope_branch(emp, user())$employee_id, ]
      if (!nrow(pr)) return(div(class = "muted tiny", "No payroll run for this period."))

      ready <- sum(pr$status == "Ready")
      tagList(
        div(class = "stat-value", inr_compact(sum(pr$net, na.rm = TRUE))),
        div(class = "tiny muted mb-3", "total payroll this month"),
        dl_rows(
          "Employees paid"   = paste0(ready, " / ", nrow(pr)),
          "Pending disbursal"= inr(sum(pr$net[pr$status != "Ready"], na.rm = TRUE)),
          "Next pay run"     = fmt_date(lubridate::ceiling_date(Sys.Date(), "month") - 1, TRUE)
        ),
        div(class = "mt-3", btn_ghost(ns("view_payroll"), "View Payroll", class = "w-100"))
      )
    })

    observeEvent(input$view_payroll, nav("hrms_payroll"))
  })
}
