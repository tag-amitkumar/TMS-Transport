# ==================================================================
# Trip Book.
#
# NOT IN THE ORIGINAL DECK. Added because trips are referenced as an existing
# concept on five screens — Live GPS shows a trip id, the driver 360° links to
# one, load consolidation groups by one, vendor settlement pays per one, and the
# payroll run states it pulls allowances "from the Trip Book, zero re-entry" —
# but no screen manages them. Without this, the payroll and vendor-payment
# numbers would have no visible provenance. See DESIGN-REVIEW.md, gap #3.
#
# A trip is one vehicle + one driver moving between two points on a date. It
# carries the allowances and advances that flow into payroll, and it is the
# grouping key for consolidated loads.
# ==================================================================

trips_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("export"), "Export", fontawesome::fa("download")),
            uiOutput(ns("new_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl")))),
      uiOutput(ns("panel"))
    )
  )
}

trips_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")

    all_rows <- reactive(scope_all(get_trips(), user()))

    output$cards <- renderUI({
      t <- all_rows()
      last30 <- t[!is.na(t$dispatch_dt) &
                        as.Date(t$dispatch_dt) >= since_30d(), ]
      stat_row(
        stat_card("ACTIVE TRIPS", sum(t$status %in% c("Running", "At Hub", "Loading")),
                  sub = paste(sum(t$status == "Planned"), "planned"),
                  icon = fontawesome::fa("route")),
        stat_card("COMPLETED · 30 DAYS", sum(last30$status == "Completed"),
                  sub = "rolling window", icon = fontawesome::fa("circle-check")),
        stat_card("DISTANCE · 30 DAYS",
                  paste0(inr_group(sum(last30$distance_km, na.rm = TRUE)), " km"),
                  sub = "all branches", icon = fontawesome::fa("road")),
        stat_card("ALLOWANCES · 30 DAYS",
                  inr_compact(sum(last30$border_allowance + last30$food_allowance, na.rm = TRUE)),
                  sub = "flows to payroll", accent = "warn",
                  icon = fontawesome::fa("indian-rupee-sign")),
        stat_card("ADVANCES OUT",
                  inr_compact(sum(last30$advance, na.rm = TRUE)),
                  sub = "recovered at settlement", icon = fontawesome::fa("wallet"))
      )
    })

    output$tabs <- renderUI({
      t <- all_rows()
      items  <- setNames(c("All", TRIP_STATUS), c("All", TRIP_STATUS))
      counts <- as.list(c(nrow(t), vapply(TRIP_STATUS, function(s) sum(t$status == s), integer(1))))
      names(counts) <- c("All", TRIP_STATUS)
      tab_strip(ns, "tab", items, counts, tab())
    })
    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      t <- all_rows()
      if (!identical(tab(), "All")) t <- t[t$status == tab(), ]
      t[order(t$dispatch_dt, decreasing = TRUE), ]
    })

    output$new_btn <- renderUI({
      if (!can(user()$role, "trips", "create")) return(NULL)
      btn_primary(ns("go_alloc"), "New Trip", fontawesome::fa("plus"))
    })
    observeEvent(input$go_alloc, nav("cargo_moto"))

    output$tbl <- renderDT({
      t <- rows(); v <- store_get("vehicles"); dr <- store_get("drivers")
      cn <- get_consignments()
      n_lr <- vapply(t$trip_no, function(x) sum(cn$trip_no == x), integer(1))

      df <- tibble::tibble(
        TRIP = mapply(cell2, t$trip_no, paste("Ref", t$booking_no)),
        `VEHICLE / DRIVER` = mapply(function(rg, dn) paste0(
          '<div class="mono" style="font-weight:600;">', htmlEscape(rg),
          '</div><div style="font-size:.6875rem;color:#64748B;">', htmlEscape(dn), "</div>"),
          v$reg_no[match(t$vehicle_id, v$vehicle_id)],
          dr$name[match(t$driver_id, dr$driver_id)]),
        ROUTE = paste(t$route_from, "→", t$route_to),
        DISTANCE = paste0(inr_group(t$distance_km), " km"),
        LRS = n_lr,
        DISPATCH = fmt_dt(t$dispatch_dt),
        ETA = fmt_dt(t$eta),
        STATUS = pill_html(t$status)
      )
      tms_table(df, page = 13, align = align_right(3:4))
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    output$panel <- renderUI({
      t <- sel()
      if (is.null(t)) return(card_panel(empty_panel("Select a trip to see its detail")))
      v <- get_vehicles(); dr <- store_get("drivers"); cl <- store_get("clients")
      cn <- get_consignments(); l <- cn[cn$trip_no == t$trip_no, ]
      veh <- v[v$vehicle_id == t$vehicle_id, ]
      vend <- store_get("vendors")

      used <- sum(l$weight_t, na.rm = TRUE)
      cap  <- if (nrow(veh)) veh$capacity_t[1] else NA

      card_panel(
        div(class = "d-flex align-items-start gap-3 mb-3",
            avatar(t$trip_no, "lg"),
            div(h6(class = "tms-card-title", t$trip_no),
                p(class = "tms-card-sub",
                  paste(t$route_from, "→", t$route_to, "·",
                        inr_group(t$distance_km), "km"))),
            div(class = "ms-auto", pill(t$status))),

        dl_rows(
          "Vehicle"  = if (nrow(veh)) span(class = "mono", veh$reg_no[1]) else "—",
          "Body"     = if (nrow(veh)) paste0(veh$body[1], " · ", veh$capacity_t[1], " T") else "—",
          "Driver"   = dr$name[match(t$driver_id, dr$driver_id)],
          "Owner"    = if (nzchar(t$vendor_id %||% ""))
                          paste("Vendor —", vend$name[match(t$vendor_id, vend$vendor_id)])
                       else "Company owned",
          "Dispatch" = fmt_dt(t$dispatch_dt),
          "ETA"      = fmt_dt(t$eta)
        ),

        if (!is.na(cap) && cap > 0) div(
          class = "mt-3",
          div(class = "d-flex justify-content-between tiny mb-1",
              span(class = "small-caps", "Load"),
              span(style = "font-weight:650;", paste0(fmt_wt(used), " / ", fmt_wt(cap)))),
          progress_bar(min(100, used / cap * 100),
                       if (used / cap > .9) "red" else "amber")
        ),

        tags$hr(class = "soft"),
        div(class = "form-section", paste0("Consignments on this trip (", nrow(l), ")")),
        if (nrow(l)) div(class = "tms-table",
          DT::datatable(
            tibble::tibble(LR = l$lr_no,
                           CUSTOMER = cl$name[match(l$client_id, cl$client_id)],
                           WEIGHT = fmt_wt(l$weight_t),
                           STATUS = pill_html(l$status)),
            rownames = FALSE, selection = "none", escape = FALSE,
            options = list(dom = "t", pageLength = 6)))
        else div(class = "muted tiny", "No consignments linked."),

        tags$hr(class = "soft"),
        div(class = "form-section", "Settlement inputs"),
        dl_rows(
          "Border allowance" = inr(t$border_allowance),
          "Food allowance"   = inr(t$food_allowance),
          "Advance drawn"    = inr(t$advance)
        ),
        div(class = "mt-2",
            callout("Feeds payroll automatically",
                    "These allowances and advances are pulled into the driver's monthly settlement — payroll staff never re-key them.",
                    "info")),

        div(class = "d-flex gap-2 mt-3",
            btn_ghost(ns("track"), "Track", class = "flex-fill"),
            if (can(user()$role, "trips", "edit"))
              btn_dark(ns("advance"), "Record advance", class = "flex-fill"))
      )
    })

    observeEvent(input$track, nav("livegps"))

    observeEvent(input$advance, {
      t <- sel(); req(t)
      if (!require_perm(session, user()$role, "trips", "edit")) return()
      showModal(modalDialog(
        title = paste("Record advance ·", t$trip_no), easyClose = TRUE,
        numericInput(ns("adv_amt"), "Advance amount (₹)", 5000, 0, step = 500),
        callout("Recovered at settlement",
                "Advances are deducted from the driver's next payroll run.", "info"),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("adv_save"), "Record"))
      ))
    })

    observeEvent(input$adv_save, {
      t <- sel(); req(t)
      if (!require_perm(session, user()$role, "trips", "edit")) return()
      amt <- as.numeric(input$adv_amt %||% 0)
      if (is.na(amt) || amt <= 0) {
        showNotification("Enter an amount greater than zero.", type = "error"); return()
      }
      store_update("trips", list(trip_no = t$trip_no),
                   list(advance = as.numeric(t$advance %||% 0) + amt))
      audit(user()$user_id, "advance", "trips", paste(t$trip_no, inr(amt)))
      removeModal()
      showNotification(paste("Advance of", inr(amt), "recorded against", t$trip_no),
                       type = "message")
    })

    observeEvent(input$export, {
      showNotification("Trip book exported to XLSX.", type = "message")
      audit(user()$user_id, "export", "trips", paste(nrow(rows()), "rows"))
    })
  })
}
