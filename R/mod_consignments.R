# ==================================================================
# Consignments — the lorry receipt (LR) register, plus split/consolidation.
#
# Two identifiers per consignment, as the deck draws them: CN- is the internal
# record id, LR- is the number printed on the physical lorry receipt the driver
# carries and the consignee signs. They are kept separate because the LR series
# is a statutory document series, while CN is free to be renumbered.
# ==================================================================

consignments_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("from_booking"), "From Booking"),
            uiOutput(ns("new_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl")))),
      tagList(uiOutput(ns("lr_preview")),
              div(class = "mt-3", uiOutput(ns("timeline"))))
    )
  )
}

consign_multi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("mode_chips")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("load_report"), "Load Plan Report"),
            uiOutput(ns("split_btn"), inline = TRUE))),
    div(class = "row g-3",
        div(class = "col-xl-6", uiOutput(ns("split_card"))),
        div(class = "col-xl-6", uiOutput(ns("consol_card"))))
  )
}

consignments_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")
    mode <- reactiveVal("One → Many")

    all_rows <- reactive(scope_all(get_consignments(), user()))

    output$tabs <- renderUI({
      c <- all_rows()
      ks <- c("Dispatched", "In Transit", "Delivered")
      items  <- setNames(c("All", ks), c("All", ks))
      counts <- as.list(c(nrow(c), vapply(ks, function(k) sum(c$status == k), integer(1))))
      names(counts) <- c("All", ks)
      tab_strip(ns, "tab", items, counts, tab())
    })
    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      c <- all_rows()
      if (!identical(tab(), "All")) c <- c[c$status == tab(), ]
      c[order(c$dispatch_date, decreasing = TRUE), ]
    })

    output$new_btn <- renderUI({
      if (!can(user()$role, "consignments", "create")) return(NULL)
      btn_primary(ns("new_cn"), "New Consignment", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      c <- rows(); v <- store_get("vehicles"); dr <- store_get("drivers")
      df <- tibble::tibble(
        `CONSIGNMENT / BOOKING` = mapply(cell2, c$cn_no, paste("Ref", c$booking_no)),
        `LR NO.` = mono(c$lr_no),
        `VEHICLE / DRIVER` = mapply(function(rg, dn) paste0(
          '<div class="mono" style="font-weight:600;">', htmlEscape(rg),
          '</div><div style="font-size:.6875rem;color:#64748B;">', htmlEscape(dn), "</div>"),
          v$reg_no[match(c$vehicle_id, v$vehicle_id)],
          dr$name[match(c$driver_id, dr$driver_id)]),
        ROUTE = paste(c$origin_city, "→", c$dest_city),
        `DISPATCH → DELIVERY` = paste(fmt_date(c$dispatch_date, TRUE), "→",
                                      fmt_date(c$expected_delivery, TRUE)),
        STATUS = pill_html(c$status)
      )
      tms_table(df, page = 13)
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    # ---------------- Lorry receipt preview ----------------

    output$lr_preview <- renderUI({
      c <- sel()
      if (is.null(c)) return(card_panel(empty_panel("Select a consignment to preview its LR")))
      v <- store_get("vehicles"); dr <- store_get("drivers"); br <- store_get("branches")
      d <- dr[dr$driver_id == c$driver_id, ]

      card_panel(
        title = paste("Lorry Receipt Preview ·", c$lr_no),
        sub = paste("Consignment", c$cn_no, "· from", c$booking_no),
        actions = pill(c$status),

        div(
          style = "border:1px solid #E4EAF1;border-radius:10px;padding:1rem;",
          div(class = "d-flex align-items-center gap-2 mb-3",
              brand_mark(26),
              span(class = "small-caps", style = "letter-spacing:.1em;", "Lorry Receipt"),
              span(class = "ms-auto mono", style = "font-weight:650;", c$lr_no)),
          dl_rows(
            "Origin Branch"      = paste0(c$origin_city, " — ",
                                          br$address[match(c$branch_id, br$branch_id)]),
            "Destination"        = c$dest_addr,
            "Vehicle No."        = span(class = "mono", v$reg_no[match(c$vehicle_id, v$vehicle_id)]),
            "Driver"             = if (nrow(d)) paste0(d$name[1], " · ", d$licence_no[1]) else "—",
            "Weight"             = fmt_wt(c$weight_t),
            "Freight"            = inr(c$freight),
            "Dispatch Date"      = fmt_date(c$dispatch_date),
            "Expected Delivery"  = fmt_date(c$expected_delivery)
          )
        ),
        div(class = "d-flex gap-2 mt-3",
            btn_ghost(ns("print_lr"), "Print LR", class = "flex-fill"),
            btn_dark(ns("link_ewb"), "Link E-Way Bill", class = "flex-fill"))
      )
    })

    output$timeline <- renderUI({
      c <- sel()
      if (is.null(c)) return(NULL)
      ev <- store_get("consignment_events")
      ev <- ev[ev$cn_no == c$cn_no, ]
      ev <- ev[order(as.numeric(ev$seq)), ]

      all_steps <- c("Booking Created", "Vehicle Allocated", "Picked Up", "In Transit",
                     "At Hub", "Out For Delivery", "Delivered", "POD Uploaded")
      done <- ev$event

      card_panel(
        title = "Status Timeline",
        timeline(lapply(seq_along(all_steps), function(i) {
          s <- all_steps[i]
          hit <- which(done == s)
          list(
            title = s,
            sub = if (length(hit)) (ev$detail[hit[1]] %||% "") else NULL,
            when = if (length(hit)) fmt_dt(ev$event_dt[hit[1]]) else NULL,
            state = if (length(hit)) "done"
                    else if (i == length(done) + 1) "current" else "todo"
          )
        }))
      )
    })

    observeEvent(input$print_lr, {
      c <- sel(); req(c)
      cl <- store_get("clients"); br <- store_get("branches")
      v  <- store_get("vehicles"); dr <- store_get("drivers")
      bk <- get_bookings(); b <- bk[bk$booking_no == c$booking_no, ]
      cr <- cl[cl$client_id == c$client_id, ]

      # Terms live on the consignment, but fall back to the booking for rows
      # created before the column existed.
      pay <- c$payment_mode %||% (if (nrow(b)) b$payment_mode[1] else "Credit")
      if (!nzchar(pay %||% "")) pay <- "Credit"
      bill_at <- c$bill_at_branch_id %||% (if (nrow(b)) b$bill_at_branch_id[1] else "")

      show_lr_print(list(
        company   = setting("company_name", BRAND$company),
        office    = setting("registered_office", ""),
        lr_no     = c$lr_no, cn_no = c$cn_no,
        date      = fmt_date(c$dispatch_date),
        consignor = if (nrow(cr)) cr$name[1] else "—",
        consignee = c$dest_city,
        from_addr = c$origin_addr, to_addr = c$dest_addr,
        from = c$origin_city, to = c$dest_city,
        vehicle = v$reg_no[match(c$vehicle_id, v$vehicle_id)] %||% "—",
        driver  = dr$name[match(c$driver_id, dr$driver_id)] %||% "—",
        material = if (nrow(b)) b$material[1] else "—",
        weight = fmt_wt(c$weight_t),
        packages = if (nrow(b)) (b$packages[1] %||% "—") else "—",
        gstin = if (nrow(cr)) cr$gstin[1] else "—",
        payment = pay,
        pay_colour = unname(PAYMENT_COLOUR[pay] %||% "grey"),
        pay_note = switch(pay,
          "To Pay" = "Collect from consignee before release",
          "Paid"   = "Settled at booking — collect nothing",
          "TBB"    = paste("Bill at", br$name[match(bill_at, br$branch_id)] %||% "—"),
          "Monthly account"),
        freight = inr(c$freight),
        gst_mode = if (nrow(b)) b$gst_mode[1] else "RCM",
        gst = if (nrow(b) && identical(b$gst_mode[1], "RCM")) "Payable by recipient"
              else if (nrow(b)) inr(b$gst_amount[1]) else "—",
        insurance = if (nrow(b)) inr(b$insurance_amt[1]) else inr(0),
        total = if (nrow(b)) inr(b$total[1]) else inr(c$freight),
        manual_note = if (nrow(b) && identical(b$entry_mode[1] %||% "", "Manual"))
          paste0("Entered manually from paper reference ", b$manual_ref[1],
                 " · load accepted ", fmt_dt(b$manual_dt[1])) else ""
      ), title = paste("Lorry receipt —", c$lr_no))
      audit(user()$user_id, "print", "consignments", c$lr_no)
    })
    observeEvent(input$link_ewb, nav("ewaybill"))
    observeEvent(input$from_booking, nav("cargo_moto"))
    observeEvent(input$new_cn, nav("cargo_moto"))

    # ---------------- Multiple consignment ----------------

    observeEvent(input$mode, mode(input$mode))

    output$mode_chips <- renderUI({
      chip_row(ns, "mode",
               setNames(c("One → Many", "Many → One"), c("One → Many", "Many → One")),
               selected = mode())
    })

    output$split_btn <- renderUI({
      if (!can(user()$role, "consignments", "create")) return(NULL)
      btn_primary(ns("new_split"), "New Split / Consolidation", fontawesome::fa("plus"))
    })

    # A split: one booking whose load is dropped at several addresses, each
    # child getting its own LR but sharing the parent booking for billing.
    split_example <- reactive({
      cn <- all_rows()
      kids <- cn[nzchar(cn$parent_cn_no), ]
      if (nrow(kids)) {
        parent <- kids$parent_cn_no[1]
        return(list(parent = cn[cn$cn_no == parent, ], kids = cn[cn$parent_cn_no == parent, ]))
      }
      NULL
    })

    output$split_card <- renderUI({
      s <- split_example()
      cl <- store_get("clients"); v <- store_get("vehicles"); dr <- store_get("drivers")

      if (is.null(s) || !nrow(s$parent)) {
        return(card_panel(
          title = "Split Shipment · One Booking → Multiple Consignments",
          sub = "Single pickup, several delivery points",
          callout("No active splits",
                  "Use New Split / Consolidation to break a booking into child consignments. Each child gets its own LR and e-way bill but shares the parent booking reference for billing.",
                  "info")
        ))
      }
      p <- s$parent[1, ]; k <- s$kids
      card_panel(
        title = "Split Shipment · One Booking → Multiple Consignments",
        sub = paste(p$booking_no, "·", cl$name[match(p$client_id, cl$client_id)],
                    "· single pickup,", nrow(k), "delivery points"),
        actions = pill("Partial Shipment", "blue"),
        dl_rows(
          "Booking Ref"  = p$booking_no,
          "Pickup"       = p$origin_addr,
          "Total Weight" = fmt_wt(sum(k$weight_t, na.rm = TRUE)),
          "Vehicle"      = span(class = "mono", v$reg_no[match(p$vehicle_id, v$vehicle_id)])
        ),
        tags$hr(class = "soft"),
        div(class = "form-section", paste0("Child Consignments (", nrow(k), ")")),
        div(class = "tms-table",
            DT::datatable(
              tibble::tibble(CONSIGNMENT = k$cn_no, `LR NO.` = k$lr_no,
                             `DELIVERY ADDRESS` = k$dest_addr,
                             WEIGHT = fmt_wt(k$weight_t),
                             STATUS = pill_html(k$status)),
              rownames = FALSE, selection = "none", escape = FALSE,
              options = list(dom = "t", pageLength = 8))),
        div(class = "mt-3",
            callout("One booking, several LRs",
                    "Each child consignment gets its own LR and e-way bill but shares the parent booking reference for billing.",
                    "info"))
      )
    })

    # A consolidation: several bookings on one vehicle down a shared corridor.
    consol <- reactive({
      cn <- all_rows()
      if (!nrow(cn)) return(NULL)
      grp <- cn |> dplyr::count(trip_no, name = "n") |> dplyr::filter(n > 1)
      if (!nrow(grp)) return(NULL)
      t <- grp$trip_no[1]
      list(trip = get_trips()[get_trips()$trip_no == t, ], lrs = cn[cn$trip_no == t, ])
    })

    output$consol_card <- renderUI({
      s <- consol()
      cl <- store_get("clients"); v <- get_vehicles(); dr <- store_get("drivers")

      if (is.null(s) || !nrow(s$trip)) {
        return(card_panel(
          title = "Load Consolidation · Multiple Bookings → Single Vehicle",
          sub = "Shared-corridor loads",
          callout("No consolidated loads right now",
                  "When two or more consignments share a vehicle and corridor they appear here with their combined capacity usage.",
                  "info")
        ))
      }
      t <- s$trip[1, ]; l <- s$lrs
      veh <- v[v$vehicle_id == t$vehicle_id, ]
      cap <- if (nrow(veh)) veh$capacity_t[1] else NA
      used <- sum(l$weight_t, na.rm = TRUE)
      pct <- if (!is.na(cap) && cap > 0) min(100, used / cap * 100) else 0

      card_panel(
        title = "Load Consolidation · Multiple Bookings → Single Vehicle",
        sub = paste("Trip", t$trip_no, "·", if (nrow(veh)) veh$reg_no[1] else "—",
                    "·", t$route_from, "→", t$route_to, "corridor"),
        actions = pill("Consolidated Shipment", "blue"),
        dl_rows(
          "Vehicle"  = if (nrow(veh)) paste0(veh$reg_no[1], " · ", veh$body[1],
                                             " · ", veh$capacity_t[1], " T") else "—",
          "Driver"   = dr$name[match(t$driver_id, dr$driver_id)],
          "Route"    = paste(t$route_from, "→", t$route_to),
          "Dispatch" = fmt_dt(t$dispatch_dt)
        ),
        tags$hr(class = "soft"),
        div(class = "form-section", paste0("Consolidated Bookings (", nrow(l), " LRs)")),
        div(class = "tms-table",
            DT::datatable(
              tibble::tibble(`LR NO.` = l$lr_no,
                             CUSTOMER = cl$name[match(l$client_id, cl$client_id)],
                             DROP = l$dest_city,
                             WEIGHT = fmt_wt(l$weight_t)),
              rownames = FALSE, selection = "none", escape = FALSE,
              options = list(dom = "t", pageLength = 8))),
        div(class = "mt-3",
            div(class = "d-flex justify-content-between tiny mb-1",
                span(class = "small-caps", "Load capacity used"),
                span(style = "font-weight:650;",
                     paste0(fmt_wt(used), " / ", fmt_wt(cap)))),
            progress_bar(pct, if (pct > 90) "red" else "amber")),
        if (!is.na(cap) && cap - used > 0.5) div(class = "mt-3",
          callout("Headroom remaining",
                  sprintf("%s free — another small booking can be consolidated before dispatch cut-off.",
                          fmt_wt(cap - used)), "warn"))
      )
    })

    observeEvent(input$new_split, {
      if (!require_perm(session, user()$role, "consignments", "create")) return()
      cn <- all_rows()
      base <- cn[cn$status == "In Prep" & !nzchar(cn$parent_cn_no), ]
      if (!nrow(base)) {
        showNotification("No in-preparation consignment available to split.", type = "warning")
        return()
      }
      cl <- store_get("clients")
      showModal(modalDialog(
        title = "Split a consignment", size = "l", easyClose = TRUE,
        selectInput(ns("sp_cn"), "Parent consignment",
                    setNames(base$cn_no, paste0(base$cn_no, " · ",
                                                cl$name[match(base$client_id, cl$client_id)],
                                                " · ", fmt_wt(base$weight_t)))),
        numericInput(ns("sp_n"), "Number of delivery points", 2, 2, 6, 1),
        callout("How the split works",
                "The parent load is divided evenly across the chosen number of drop points. Each child gets its own CN and LR, and keeps the parent booking reference for billing.",
                "info"),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("sp_save"), "Create split"))
      ))
    })

    observeEvent(input$sp_save, {
      if (!require_perm(session, user()$role, "consignments", "create")) return()
      req(input$sp_cn, input$sp_n)
      cn <- get_consignments(); p <- cn[cn$cn_no == input$sp_cn, ]
      req(nrow(p)); p <- p[1, ]
      n <- as.integer(input$sp_n)
      share <- round(p$weight_t / n, 1)

      for (i in seq_len(n)) {
        new_cn <- next_id(store_get("consignments")$cn_no, PREFIX$consignment, 5)
        new_lr <- next_id(store_get("consignments")$lr_no, PREFIX$lorry_receipt)
        store_insert("consignments", list(
          cn_no = new_cn, lr_no = new_lr, booking_no = p$booking_no,
          trip_no = p$trip_no, client_id = p$client_id, vehicle_id = p$vehicle_id,
          driver_id = p$driver_id, branch_id = p$branch_id,
          origin_city = p$origin_city, dest_city = p$dest_city,
          origin_addr = p$origin_addr,
          dest_addr = paste0("Drop point ", i, " — ", p$dest_city),
          weight_t = share, freight = round(p$freight / n),
          dispatch_date = as.character(p$dispatch_date),
          expected_delivery = as.character(p$expected_delivery),
          delivered_date = "", parent_cn_no = p$cn_no, status = "In Prep"
        ))
      }
      audit(user()$user_id, "split", "consignments", paste(p$cn_no, "into", n))
      removeModal()
      showNotification(sprintf("%s split into %d child consignments.", p$cn_no, n),
                       type = "message")
    })

    observeEvent(input$load_report, {
      showModal(modalDialog(
        title = "Load plan report", size = "l", easyClose = TRUE,
        div(class = "tms-table",
            DT::datatable(
              local({
                cn <- all_rows(); v <- get_vehicles()
                agg <- cn |>
                  dplyr::filter(nzchar(trip_no)) |>
                  dplyr::group_by(trip_no, vehicle_id) |>
                  dplyr::summarise(lrs = dplyr::n(), used = sum(weight_t, na.rm = TRUE),
                                   .groups = "drop")
                agg$cap <- v$capacity_t[match(agg$vehicle_id, v$vehicle_id)]
                agg$reg <- v$reg_no[match(agg$vehicle_id, v$vehicle_id)]
                agg <- agg[order(-agg$lrs), ][seq_len(min(15, nrow(agg))), ]
                tibble::tibble(TRIP = agg$trip_no, VEHICLE = agg$reg, LRS = agg$lrs,
                               USED = fmt_wt(agg$used), CAPACITY = fmt_wt(agg$cap),
                               `UTILISATION` = paste0(round(agg$used / agg$cap * 100), "%"))
              }),
              rownames = FALSE, selection = "none",
              options = list(dom = "tp", pageLength = 10))),
        footer = modalButton("Close")
      ))
    })
  })
}
