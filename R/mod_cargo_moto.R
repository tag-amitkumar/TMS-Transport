# ==================================================================
# Cargo Moto — allocation kanban.
#
# The deck never defines "Cargo Moto" and gives it its own number series and a
# count (13) that cannot be reconciled with the 342 bookings on the list screen.
# Treated here as what it visibly is: a dispatcher's board over the *same*
# bookings, grouped by how far each has got through allocation. See
# DESIGN-REVIEW.md, gaps #1 and #2.
#
# Cards move by allocating a vehicle, which creates the Trip Book entry and the
# consignment — so this board is where the operations chain actually starts.
# ==================================================================

cargo_moto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("route_plan"), "Route Planning"),
            uiOutput(ns("alloc_btn"), inline = TRUE))),
    uiOutput(ns("board"))
  )
}

cargo_moto_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")

    # The board only concerns live work: drafts are not yet allocatable and
    # closed/cancelled bookings have left the pipeline.
    board_rows <- reactive({
      b <- scope_all(get_bookings(), user())
      b <- b[b$status %in% c("Confirmed", "Vehicle Allocated", "In Transit", "Delivered"), ]
      b[order(b$booking_date, decreasing = TRUE), ]
    })

    # Kanban lane for a booking. "Unassigned" means confirmed but with no trip.
    lane_of <- function(status) {
      dplyr::case_when(
        status == "Confirmed"         ~ "Unassigned",
        status == "Vehicle Allocated" ~ "Vehicle Allocated",
        status == "In Transit"        ~ "Dispatched",
        status == "Delivered"         ~ "Delivered",
        TRUE                          ~ "Unassigned"
      )
    }

    lanes <- reactive({
      b <- board_rows()
      b$lane <- lane_of(b$status)
      b
    })

    output$chips <- renderUI({
      b <- lanes()
      ks <- c("Unassigned", "Vehicle Allocated", "Dispatched", "Delivered")
      items  <- setNames(c("All", ks), c("All", ks))
      counts <- as.list(c(nrow(b), vapply(ks, function(k) sum(b$lane == k), integer(1))))
      names(counts) <- c("All", ks)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$alloc_btn <- renderUI({
      if (!can(user()$role, "cargo_moto", "create")) return(NULL)
      btn_primary(ns("allocate"), "Allocate Cargo", fontawesome::fa("plus"))
    })

    output$board <- renderUI({
      b  <- lanes()
      cl <- store_get("clients"); v <- store_get("vehicles")
      dr <- store_get("drivers"); tr <- get_trips(); cn <- get_consignments()
      pods <- get_pods()

      make_cards <- function(lane) {
        rowsl <- b[b$lane == lane, ]
        # Cap each lane so a busy board stays readable; the count in the header
        # still reports the true total.
        shown <- rowsl[seq_len(min(12, nrow(rowsl))), , drop = FALSE]
        if (!nrow(shown)) return(div(class = "muted tiny px-1", "Nothing here"))

        lapply(seq_len(nrow(shown)), function(i) {
          r  <- shown[i, ]
          t  <- tr[tr$booking_no == r$booking_no, ]
          c1 <- cn[cn$booking_no == r$booking_no, ]
          nm <- cl$name[match(r$client_id, cl$client_id)]

          reg <- if (nrow(t)) v$reg_no[match(t$vehicle_id[1], v$vehicle_id)] else NULL
          drv <- if (nrow(t)) dr$name[match(t$driver_id[1], dr$driver_id)] else NULL
          pod_ok <- nrow(c1) && c1$lr_no[1] %in%
            pods$lr_no[pods$status %in% c("Uploaded", "Verified", "Approved")]

          kanban_card(
            ns, "card", r$booking_no,
            title = paste(r$booking_no, "·", nm),
            meta = paste0(r$origin_city, " → ", r$dest_city, " · ",
                          fmt_wt(r$weight_t), " · ", r$material),
            footer = tagList(
              if (!is.null(reg)) span(class = "mono", style = "font-weight:600;", reg),
              if (lane == "Unassigned")
                pill(if (identical(r$priority, "Priority")) "Priority" else "Awaiting vehicle",
                     if (identical(r$priority, "Priority")) "orange" else "grey"),
              if (lane == "Vehicle Allocated")
                pill(if (is.null(drv) || !nzchar(drv)) "driver pending" else drv,
                     if (is.null(drv) || !nzchar(drv)) "blue" else "green"),
              if (lane == "Dispatched" && nrow(c1)) pill(c1$status[1]),
              if (lane == "Delivered")
                pill(if (pod_ok) "POD uploaded" else "POD pending",
                     if (pod_ok) "green" else "orange"),
              span(class = "ms-auto tiny faint", fmt_date(r$booking_date, TRUE))
            ),
            rail = if (identical(r$priority, "Priority") && lane == "Unassigned") "amber"
                   else if (lane == "Delivered") "green" else NULL
          )
        })
      }

      ks  <- c("Unassigned", "Vehicle Allocated", "Dispatched", "Delivered")
      col <- c("grey", "blue", "orange", "green")
      show <- if (identical(filt(), "All")) seq_along(ks) else which(ks == filt())

      do.call(kanban_board, lapply(show, function(i) {
        kanban_col(ks[i], col[i], make_cards(ks[i]),
                   note = sum(b$lane == ks[i]))
      }))
    })

    # ---------------- Card detail ----------------

    observeEvent(input$card, {
      no <- input$card
      b  <- get_bookings(); r <- b[b$booking_no == no, ]
      req(nrow(r))
      r <- r[1, ]
      cl <- store_get("clients"); v <- store_get("vehicles"); dr <- store_get("drivers")
      tr <- get_trips(); t <- tr[tr$booking_no == no, ]
      cn <- get_consignments(); c1 <- cn[cn$booking_no == no, ]

      showModal(modalDialog(
        title = paste(no, "·", cl$name[match(r$client_id, cl$client_id)]),
        size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-md-6", dl_rows(
              "Route"    = paste(r$origin_city, "→", r$dest_city),
              "Material" = r$material,
              "Weight"   = fmt_wt(r$weight_t),
              "Freight"  = inr(r$freight),
              "Status"   = pill(r$status))),
            div(class = "col-md-6",
                if (nrow(t)) dl_rows(
                  "Trip"    = t$trip_no[1],
                  "Vehicle" = span(class = "mono", v$reg_no[match(t$vehicle_id[1], v$vehicle_id)]),
                  "Driver"  = dr$name[match(t$driver_id[1], dr$driver_id)],
                  "Dispatch"= fmt_dt(t$dispatch_dt[1]),
                  "LR"      = if (nrow(c1)) c1$lr_no[1] else "—")
                else callout("No vehicle allocated",
                             "Use Allocate Cargo to assign a vehicle and driver.", "warn"))),
        footer = tagList(
          modalButton("Close"),
          if (!nrow(t) && can(user()$role, "cargo_moto", "create"))
            btn_primary(ns("alloc_this"), "Allocate vehicle")
        )
      ))
      session$userData$moto_booking <- no
    })

    observeEvent(input$alloc_this, {
      removeModal()
      show_alloc_modal(session$userData$moto_booking)
    })
    observeEvent(input$allocate, show_alloc_modal(NULL))

    show_alloc_modal <- function(preset) {
      if (!require_perm(session, user()$role, "cargo_moto", "create")) return()
      b <- scope_all(get_bookings(), user())
      pending <- b[b$status == "Confirmed", ]
      if (!nrow(pending)) {
        showNotification("No confirmed bookings are awaiting allocation.", type = "warning")
        return()
      }
      cl <- store_get("clients")
      v  <- get_vehicles()
      free <- v[v$status == "Available", ]
      dr <- get_drivers()
      avail_dr <- dr[dr$status == "Available", ]

      showModal(modalDialog(
        title = "Allocate cargo", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-12",
                selectInput(ns("a_booking"), "Booking",
                            setNames(pending$booking_no,
                                     paste0(pending$booking_no, " · ",
                                            cl$name[match(pending$client_id, cl$client_id)],
                                            " · ", pending$origin_city, " → ",
                                            pending$dest_city, " · ",
                                            fmt_wt(pending$weight_t))),
                            selected = preset)),
            div(class = "col-md-6",
                selectInput(ns("a_vehicle"), "Vehicle",
                            setNames(free$vehicle_id,
                                     paste0(free$reg_no, " · ", free$body,
                                            " · ", free$capacity_t, " T")))),
            div(class = "col-md-6",
                selectInput(ns("a_driver"), "Driver",
                            setNames(avail_dr$driver_id, avail_dr$name)))),
        uiOutput(ns("a_check")),
        footer = tagList(modalButton("Cancel"),
                         btn_primary(ns("a_save"), "Allocate & generate LR"))
      ))
    }

    # Capacity and licence-expiry checks before allocation, which is the whole
    # point of holding capacity on the vehicle master and expiry on the driver.
    output$a_check <- renderUI({
      req(input$a_booking, input$a_vehicle, input$a_driver)
      b <- get_bookings(); r <- b[b$booking_no == input$a_booking, ]
      v <- get_vehicles(); veh <- v[v$vehicle_id == input$a_vehicle, ]
      d <- get_drivers();  drv <- d[d$driver_id == input$a_driver, ]
      req(nrow(r), nrow(veh), nrow(drv))

      msgs <- list()
      if (r$weight_t[1] > veh$capacity_t[1]) {
        msgs <- c(msgs, list(callout(
          "Over capacity",
          sprintf("Load is %s but %s carries %s.", fmt_wt(r$weight_t[1]),
                  veh$reg_no[1], fmt_wt(veh$capacity_t[1])), "danger")))
      }
      if (!is.na(drv$licence_expiry[1]) && drv$licence_expiry[1] < Sys.Date()) {
        msgs <- c(msgs, list(callout(
          "Licence expired",
          sprintf("%s's licence expired %s.", drv$name[1], fmt_date(drv$licence_expiry[1])),
          "danger")))
      } else if (!is.na(drv$licence_expiry[1]) && days_to(drv$licence_expiry[1]) <= 30) {
        msgs <- c(msgs, list(callout(
          "Licence expiring",
          sprintf("%s's licence expires in %d days.", drv$name[1],
                  round(days_to(drv$licence_expiry[1]))), "warn")))
      }
      exp_docs <- c("Insurance" = veh$insurance_expiry[1], "Fitness" = veh$fitness_expiry[1],
                    "Permit" = veh$permit_expiry[1], "PUC" = veh$puc_expiry[1])
      bad <- names(exp_docs)[!is.na(exp_docs) & exp_docs < Sys.Date()]
      if (length(bad)) {
        msgs <- c(msgs, list(callout("Vehicle documents expired",
                                     paste(paste(bad, collapse = ", "),
                                           "expired for", veh$reg_no[1]), "danger")))
      }
      if (!length(msgs)) {
        msgs <- list(callout("Ready to allocate",
                             sprintf("%s (%s) with %s · %s headroom.",
                                     veh$reg_no[1], veh$body[1], drv$name[1],
                                     fmt_wt(veh$capacity_t[1] - r$weight_t[1])), "ok"))
      }
      div(class = "mt-3 d-grid gap-2", msgs)
    })

    observeEvent(input$a_save, {
      if (!require_perm(session, user()$role, "cargo_moto", "create")) return()
      req(input$a_booking, input$a_vehicle, input$a_driver)

      b <- get_bookings(); r <- b[b$booking_no == input$a_booking, ]
      v <- get_vehicles(); veh <- v[v$vehicle_id == input$a_vehicle, ]
      req(nrow(r), nrow(veh))
      r <- r[1, ]; veh <- veh[1, ]

      if (r$weight_t > veh$capacity_t) {
        showNotification("Cannot allocate: load exceeds vehicle capacity.", type = "error")
        return()
      }

      # Allocation writes three linked records at once — trip, consignment and
      # the first timeline events — because a vehicle assignment is meaningless
      # without the LR it produces.
      trip_no <- next_id(store_get("trips")$trip_no, PREFIX$trip)
      cn_no   <- next_id(store_get("consignments")$cn_no, PREFIX$consignment, 5)
      lr_no   <- next_id(store_get("consignments")$lr_no, PREFIX$lorry_receipt)

      rt <- store_get("pincode_routes")
      hit <- rt[rt$src_city == r$origin_city & rt$dst_city == r$dest_city, ]
      transit <- if (nrow(hit)) as.numeric(hit$transit_days[1]) else 3
      dist_km <- if (nrow(hit)) as.numeric(hit$distance_km[1]) else NA

      dispatch <- Sys.time()
      eta <- dispatch + transit * 86400

      store_insert("trips", list(
        trip_no = trip_no, booking_no = r$booking_no,
        vehicle_id = veh$vehicle_id, driver_id = input$a_driver,
        vendor_id = veh$vendor_id, branch_id = r$branch_id,
        route_from = r$origin_city, route_to = r$dest_city,
        distance_km = dist_km,
        dispatch_dt = format(dispatch, "%Y-%m-%d %H:%M:%S"),
        eta = format(eta, "%Y-%m-%d %H:%M:%S"),
        border_allowance = 1000, food_allowance = 400, advance = 0,
        status = "Planned"
      ))

      store_insert("consignments", list(
        cn_no = cn_no, lr_no = lr_no, booking_no = r$booking_no, trip_no = trip_no,
        client_id = r$client_id, vehicle_id = veh$vehicle_id, driver_id = input$a_driver,
        branch_id = r$branch_id, origin_city = r$origin_city, dest_city = r$dest_city,
        origin_addr = r$pickup_address, dest_addr = r$delivery_address,
        weight_t = r$weight_t, freight = r$freight,
        dispatch_date = as.character(Sys.Date()),
        expected_delivery = as.character(Sys.Date() + transit),
        delivered_date = "", parent_cn_no = "", status = "In Prep"
      ))

      for (i in seq_along(c("Booking Created", "Vehicle Allocated"))) {
        store_insert("consignment_events", list(
          cn_no = cn_no, seq = i,
          event = c("Booking Created", "Vehicle Allocated")[i],
          detail = if (i == 1) r$booking_no else paste(veh$reg_no, "·", input$a_driver),
          event_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ))
      }

      store_update("bookings", list(booking_no = r$booking_no),
                   list(status = "Vehicle Allocated"))
      store_update("vehicles", list(vehicle_id = veh$vehicle_id), list(status = "Allocated"))
      store_update("drivers",  list(driver_id = input$a_driver),  list(status = "On trip"))

      audit(user()$user_id, "allocate", "cargo_moto",
            paste(r$booking_no, "->", trip_no, "/", lr_no))
      removeModal()
      showNotification(sprintf("Allocated %s · trip %s · %s generated.",
                               r$booking_no, trip_no, lr_no),
                       type = "message", duration = 6)
    })

    observeEvent(input$route_plan, {
      showModal(modalDialog(
        title = "Route planning", size = "l", easyClose = TRUE,
        callout("Reads the pin-code route master",
                "Transit-day promises and branch routing come from Fleet & People → Pin Code Mapping. Open that screen to add or amend a lane.",
                "info"),
        div(class = "mt-3 tms-table",
            DT::datatable(
              local({
                rt <- store_get("pincode_routes")
                rt <- rt[rt$availability != "Not Serviceable", ]
                rt <- rt[order(as.numeric(rt$transit_days)), ][seq_len(min(12, nrow(rt))), ]
                tibble::tibble(ROUTE = rt$route_path,
                               `TRANSIT (D)` = rt$transit_days,
                               AVAILABILITY = pill_html(rt$availability))
              }),
              rownames = FALSE, selection = "none", escape = FALSE,
              options = list(dom = "t", pageLength = 12))),
        footer = modalButton("Close")
      ))
    })
  })
}
