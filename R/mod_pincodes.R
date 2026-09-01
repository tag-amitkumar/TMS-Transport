# ==================================================================
# Pin Code Mapping — the serviceability and transit-promise master.
#
# This table is what the booking form checks against: it decides whether a lane
# can be sold, how many days to promise, and which depots the load routes
# through. Everything downstream (ETA on the LR, the tracking timeline) inherits
# the transit days set here.
# ==================================================================

pincodes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("export"), "Export", fontawesome::fa("download")),
            uiOutput(ns("add_btn"), inline = TRUE))),
    card_panel(body_class = "tms-card-body p-0",
               div(class = "tms-table", DTOutput(ns("tbl"))),
               foot = "Pincode-to-pincode route mapping drives transit-day promise, branch routing and serviceability checks shown at booking time")
  )
}

pincodes_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")

    # Routes are a global master, not branch-scoped: a Nagpur clerk still needs
    # to quote a Delhi→Mumbai lane.
    all_rows <- reactive(store_typed("pincode_routes",
                                     num = c("transit_days", "distance_km")))

    output$cards <- renderUI({
      r <- all_rows(); b <- store_get("branches")
      stat_row(
        stat_card("TOTAL ROUTES MAPPED", nrow(r),
                  sub = paste("across", sum(b$is_depot == "TRUE"), "depots")),
        stat_card("AVG TRANSIT DAYS",
                  format(round(mean(r$transit_days, na.rm = TRUE), 1), nsmall = 1),
                  unit = "days", sub = "across serviceable lanes"),
        stat_card("BRANCHES COVERED", dplyr::n_distinct(c(r$src_city, r$dst_city)),
                  sub = "origin or destination"),
        stat_card("PINCODES COVERED",
                  inr_group(dplyr::n_distinct(c(r$src_pincode, r$dst_pincode))),
                  sub = "serviceable across India")
      )
    })

    output$chips <- renderUI({
      r <- all_rows()
      ks <- c("Available", "Limited", "Not Serviceable")
      items  <- setNames(c("All", ks), c("All routes", ks))
      counts <- as.list(c(nrow(r), vapply(ks, function(k) sum(r$availability == k), integer(1))))
      names(counts) <- c("All", ks)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$add_btn <- renderUI({
      if (!can(user()$role, "pincodes", "create")) return(NULL)
      btn_primary(ns("add"), "Add Route Mapping", fontawesome::fa("plus"))
    })

    rows <- reactive({
      r <- all_rows()
      if (!identical(filt(), "All")) r <- r[r$availability == filt(), ]
      r[order(r$transit_days), ]
    })

    output$tbl <- renderDT({
      r <- rows()
      df <- tibble::tibble(
        `SOURCE PINCODE` = paste0('<span class="mono" style="font-weight:600;">',
                                  htmlEscape(r$src_pincode),
                                  '</span> <span style="color:#94A3B8;">',
                                  htmlEscape(r$src_city), "</span>"),
        `DESTINATION PINCODE` = paste0('<span class="mono" style="font-weight:600;">',
                                       htmlEscape(r$dst_pincode),
                                       '</span> <span style="color:#94A3B8;">',
                                       htmlEscape(r$dst_city), "</span>"),
        `TRANSIT DAYS` = r$transit_days,
        `DISTANCE` = paste0(inr_group(r$distance_km), " km"),
        `ROUTE MAPPING` = r$route_path,
        `BRANCH MAPPING` = r$branch_mapping,
        `PICKUP ZONE` = r$pickup_zone,
        `DELIVERY ZONE` = r$delivery_zone,
        `SERVICE AVAILABILITY` = pill_html(r$availability)
      )
      tms_table(df, page = 13, align = align_right(2:3))
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "pincodes", "create")) return()
      b <- store_get("branches")
      showModal(modalDialog(
        title = "Add route mapping", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", selectInput(ns("n_src"), "Source city", sort(b$city))),
            div(class = "col-6", selectInput(ns("n_dst"), "Destination city", sort(b$city))),
            div(class = "col-4", textInput(ns("n_spin"), "Source pincode")),
            div(class = "col-4", textInput(ns("n_dpin"), "Destination pincode")),
            div(class = "col-4", numericInput(ns("n_days"), "Transit days", 2, 1, 15, 1)),
            div(class = "col-6", selectInput(ns("n_pzone"), "Pickup zone",
                                             paste("Zone", c("North","South","East","West","Central")))),
            div(class = "col-6", selectInput(ns("n_dzone"), "Delivery zone",
                                             paste("Zone", c("North","South","East","West","Central")))),
            div(class = "col-12", selectInput(ns("n_avail"), "Service availability",
                                              c("Available", "Limited", "Not Serviceable")))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("n_save"), "Add mapping"))
      ))
    })

    # Prefill the pincode from the branch master when a city is chosen, so the
    # operator does not have to look it up.
    observeEvent(input$n_src, {
      b <- store_get("branches")
      p <- b$pincode[match(input$n_src, b$city)]
      if (!is.na(p)) updateTextInput(session, "n_spin", value = p)
    }, ignoreInit = TRUE)

    observeEvent(input$n_dst, {
      b <- store_get("branches")
      p <- b$pincode[match(input$n_dst, b$city)]
      if (!is.na(p)) updateTextInput(session, "n_dpin", value = p)
    }, ignoreInit = TRUE)

    observeEvent(input$n_save, {
      if (!require_perm(session, user()$role, "pincodes", "create")) return()
      if (identical(input$n_src, input$n_dst)) {
        showNotification("Source and destination must differ.", type = "error"); return()
      }
      r <- store_get("pincode_routes")
      dup <- r[r$src_city == input$n_src & r$dst_city == input$n_dst, ]
      if (nrow(dup)) {
        showNotification("That lane is already mapped.", type = "error"); return()
      }
      b <- store_get("branches")
      id_new <- next_id(r$route_id, "RT-")
      store_insert("pincode_routes", list(
        route_id = id_new,
        src_pincode = input$n_spin, src_city = input$n_src,
        dst_pincode = input$n_dpin, dst_city = input$n_dst,
        distance_km = "", transit_days = input$n_days,
        route_path = paste(input$n_src, "→", input$n_dst),
        branch_mapping = paste(b$name[match(input$n_src, b$city)], "→",
                               b$name[match(input$n_dst, b$city)]),
        pickup_zone = input$n_pzone, delivery_zone = input$n_dzone,
        availability = input$n_avail
      ))
      audit(user()$user_id, "create", "pincodes", id_new)
      removeModal()
      showNotification("Route mapping added.", type = "message")
    })

    observeEvent(input$export, {
      audit(user()$user_id, "export", "pincodes", paste(nrow(rows()), "routes"))
      showNotification("Route master exported to XLSX.", type = "message")
    })
  })
}
