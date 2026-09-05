# ==================================================================
# Live GPS.
#
# Positions come from the gps_pings table, which in this build is seeded rather
# than fed by a telematics provider. The refresh loop, the deviation check and
# the alerting all work against that table, so wiring a real feed means writing
# into gps_pings on a schedule — no screen code changes. See DESIGN-REVIEW.md,
# gap #12.
# ==================================================================

livegps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("feed"), inline = TRUE),
            btn_ghost(ns("refresh"), "Refresh", fontawesome::fa("rotate")))),
    div(class = "row g-3",
        div(class = "col-xl-9",
            card_panel(body_class = "tms-card-body p-0",
                       if (HAS_LEAFLET) leaflet::leafletOutput(ns("map"), height = 520)
                       else div(class = "p-4",
                                callout("Map unavailable",
                                        "Install the leaflet package to render live positions.",
                                        "warn")))),
        div(class = "col-xl-3",
            card_panel(title = "Live GPS Features", uiOutput(ns("features"))),
            div(class = "mt-3", uiOutput(ns("deviation"))))),
    div(class = "mt-3",
        card_panel(title = "Vehicle List",
                   actions = span(class = "tiny muted", textOutput(ns("count"), inline = TRUE)),
                   body_class = "tms-card-body p-0",
                   div(class = "tms-table", DTOutput(ns("tbl")))))
  )
}

livegps_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")

    # Poll every 30s. invalidateLater is scoped to this reactive, so it only
    # runs while the GPS screen is actually on-screen and stops when it is not.
    pings <- reactive({
      invalidateLater(30000, session)
      input$refresh
      g <- gps_latest()
      v <- scope_branch(get_vehicles(), user())
      g <- g[g$vehicle_id %in% v$vehicle_id, , drop = FALSE]
      if (!nrow(g)) return(g)
      g$reg    <- v$reg_no[match(g$vehicle_id, v$vehicle_id)]
      g$body   <- v$body[match(g$vehicle_id, v$vehicle_id)]
      g$cap    <- v$capacity_t[match(g$vehicle_id, v$vehicle_id)]
      g$model  <- v$model[match(g$vehicle_id, v$vehicle_id)]
      tr <- get_trips(); dr <- store_get("drivers"); cn <- get_consignments()
      g$driver <- dr$name[match(tr$driver_id[match(g$trip_no, tr$trip_no)], dr$driver_id)]
      g$route  <- paste(tr$route_from[match(g$trip_no, tr$trip_no)], "→",
                        tr$route_to[match(g$trip_no, tr$trip_no)])
      g$eta    <- tr$eta[match(g$trip_no, tr$trip_no)]
      g$lr     <- cn$lr_no[match(g$trip_no, cn$trip_no)]
      g
    })

    output$chips <- renderUI({
      g <- pings()
      ks <- c("Running", "Halted", "Alert")
      items  <- setNames(c("All", ks), c("All", ks))
      counts <- as.list(c(nrow(g), vapply(ks, function(k) sum(g$status == k), integer(1))))
      names(counts) <- c("All", ks)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$feed <- renderUI({
      g <- pings()
      if (!nrow(g)) return(pill("No feed", "grey"))
      age <- round(as.numeric(difftime(Sys.time(), max(g$ts, na.rm = TRUE), units = "secs")))
      tagList(
        pill("GPS feed · live", "green"),
        span(class = "tiny muted ms-2",
             sprintf("Last poll %s", if (age < 90) paste0(age, " s") else paste0(round(age / 60), " min")))
      )
    })

    shown <- reactive({
      g <- pings()
      if (!identical(filt(), "All")) g <- g[g$status == filt(), ]
      g
    })

    output$count <- renderText(paste(nrow(shown()), "vehicles on live feed"))

    output$map <- leaflet::renderLeaflet({
      req(HAS_LEAFLET)
      g <- shown()
      if (!nrow(g)) {
        return(leaflet::leaflet() |>
                 leaflet::addTiles(urlTemplate = CARTO_LIGHT, attribution = CARTO_ATTR, options = leaflet::tileOptions(maxZoom = 19)) |>
                 leaflet::setView(78.9, 22.5, zoom = 5))
      }
      cols <- c(Running = "#1A7F45", Halted = "#EFA31D", Alert = "#C0392B")
      leaflet::leaflet(g) |>
        leaflet::addTiles(urlTemplate = CARTO_LIGHT, attribution = CARTO_ATTR, options = leaflet::tileOptions(maxZoom = 19)) |>
        leaflet::addCircleMarkers(
          ~lon, ~lat, radius = 8, weight = 2, color = "#fff", opacity = 1,
          fillColor = ~unname(cols[status]), fillOpacity = .95,
          layerId = ~vehicle_id,
          label = ~sprintf("%s · %s km/h · %s", reg, speed, status),
          popup = ~sprintf(
            "<b>%s</b><br/>%s · %s<br/>%s km/h · %s<br/>Route: %s<br/>LR: %s",
            reg, model, body, speed, status, route, lr %||% "—")
        ) |>
        leaflet::addLegend("bottomleft", colors = unname(cols), labels = names(cols),
                           opacity = .95, title = NULL)
    })

    output$features <- renderUI({
      g <- pings()
      dev <- sum(g$status == "Alert")
      feats <- list(
        list("Live Location & Route Monitoring", "green"),
        list("ETA & Driver Information", "green"),
        list("Speed Monitoring & Idle Time", "green"),
        list("Geo-fencing — 3 zones active", "orange"),
        list(sprintf("Route Deviation Alerts — %d active", dev),
             if (dev > 0) "red" else "green")
      )
      div(lapply(feats, function(f) {
        div(class = "d-flex align-items-center gap-2 py-1",
            span(style = sprintf("width:8px;height:8px;border-radius:50%%;background:%s;flex:0 0 8px;",
                                 kan_dot(f[[2]]))),
            span(style = "font-size:.8125rem;", f[[1]]))
      }))
    })

    output$deviation <- renderUI({
      g <- pings()
      alert <- g[g$status == "Alert", ]
      if (!nrow(alert)) {
        return(card_panel(title = "Route Deviation",
                          callout("All vehicles on corridor",
                                  "No active deviations across the live fleet.", "ok")))
      }
      a <- alert[1, ]
      card_panel(
        title = "Route Deviation",
        actions = pill(a$reg, "red"),
        p(class = "mb-1", style = "font-size:.8125rem;",
          sprintf("Vehicle is off the planned corridor on %s.", a$route)),
        p(class = "tiny muted", sprintf("Driver: %s · Trip %s", a$driver %||% "—", a$trip_no)),
        div(class = "mt-2",
            actionButton(ns("alert_driver"), "Alert Driver",
                         class = "btn w-100",
                         style = "background:#C0392B;color:#fff;border:none;border-radius:9px;font-weight:600;font-size:.8125rem;padding:.5rem;"))
      )
    })

    observeEvent(input$alert_driver, {
      g <- pings(); a <- g[g$status == "Alert", ]
      req(nrow(a))
      audit(user()$user_id, "alert-driver", "livegps", a$vehicle_id[1])
      showNotification(
        sprintf("Deviation alert queued for %s. Delivery depends on an SMS/WhatsApp gateway being connected.",
                a$driver[1] %||% a$reg[1]),
        type = "warning", duration = 7)
    })

    output$tbl <- renderDT({
      g <- shown()
      if (!nrow(g)) return(tms_table(tibble::tibble(Message = "No vehicles on feed"), page = 6))
      df <- tibble::tibble(
        `VEHICLE NO.` = mono(g$reg),
        DRIVER = g$driver %||% "—",
        SPEED  = paste0(g$speed, " km/h"),
        ROUTE  = g$route,
        `IDLE TODAY` = fmt_idle(g$idle_min),
        STATUS = pill_html(ifelse(g$status == "Halted",
                                  paste0("Halted · ", fmt_idle(g$idle_min)),
                            ifelse(g$status == "Alert", "Alert · off route", "Running")),
                           colour = NULL)
      )
      df$STATUS <- sprintf('<span class="pill pill-%s">%s</span>',
                           c(Running = "green", Halted = "orange", Alert = "red")[g$status],
                           ifelse(g$status == "Halted", paste0("Halted · ", fmt_idle(g$idle_min)),
                             ifelse(g$status == "Alert", "Alert · off route", "Running")))
      tms_table(df, page = 10, align = align_right(2))
    })

    # Clicking a marker opens the vehicle card the deck shows over the map.
    observeEvent(input$map_marker_click, {
      vid <- input$map_marker_click$id
      req(vid)
      g <- pings(); r <- g[g$vehicle_id == vid, ]
      req(nrow(r)); r <- r[1, ]
      showModal(modalDialog(
        title = r$reg, easyClose = TRUE,
        p(class = "tms-card-sub mb-3",
          paste(r$model, "·", r$body, "·", r$cap, "T")),
        dl_rows(
          "Speed / heading" = paste0(r$speed, " km/h · ", r$heading),
          "Ignition"        = pill(paste("Ignition", r$ignition),
                                   if (identical(r$ignition, "ON")) "green" else "grey"),
          "Route"           = r$route,
          "ETA"             = fmt_dt(r$eta),
          "Driver"          = r$driver %||% "—",
          "Idle time today" = fmt_idle(r$idle_min),
          "Current LR"      = r$lr %||% "—"
        ),
        footer = tagList(modalButton("Close"),
                         btn_dark(ns("go_track"), "Track shipment"))
      ))
      session$userData$gps_lr <- r$lr
    })

    observeEvent(input$go_track, {
      removeModal()
      nav("tracking")
    })
  })
}
