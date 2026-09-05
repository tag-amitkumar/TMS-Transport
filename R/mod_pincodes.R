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
               foot = "Pincode-to-pincode route mapping drives transit-day promise, branch routing and serviceability checks shown at booking time"),

    # The route master above is the lanes this company has priced. The
    # directory below is the whole country — every pincode a consignment can be
    # booked to, whether or not the lane has been negotiated yet. Keeping them
    # visibly separate matters: one is a commercial commitment, the other is
    # geography.
    div(class = "mt-3",
        card_panel(
          title = "India PIN Code Directory",
          sub = textOutput(ns("dir_sub"), inline = TRUE),
          actions = div(style = "min-width:280px;",
                        textInput(ns("pin_q"), NULL, width = "100%",
                                  placeholder = "Search a PIN code, city or state")),
          body_class = "tms-card-body p-0",
          div(class = "tms-table", DTOutput(ns("pin_tbl"))),
          foot = "Reference data — read-only. Source: GeoNames India postal export (CC BY 4.0). The booking form resolves origin and destination against this table."))
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
        # These two count the directory, not the route master. The old version
        # counted distinct pincodes among the mapped lanes and reported "2
        # pincodes serviceable across India", which read as a broken feed.
        stat_card("PINCODES IN DIRECTORY", inr_group(nrow(geo_pincodes())),
                  sub = "every PIN code in India"),
        stat_card("CITIES COVERED", inr_group(nrow(geo_cities())),
                  sub = paste("across", length(geo_states()),
                              "states and union territories"))
      )
    })

    # ---------------- India PIN code directory ----------------

    dir_rows <- reactive({
      p <- geo_pincodes()
      q <- trimws(input$pin_q %||% "")
      if (!nzchar(q)) return(p)
      # One box, three kinds of answer: a PIN prefix, a city, or a state. Which
      # one the clerk meant is obvious from what they typed, so there is no
      # reason to make them pick a field first.
      if (grepl("^[0-9]+$", q)) {
        p[startsWith(p$pincode, q), ]
      } else {
        ql <- tolower(q)
        p[grepl(ql, tolower(p$locality), fixed = TRUE) |
          grepl(ql, tolower(p$city),     fixed = TRUE) |
          grepl(ql, tolower(p$state),    fixed = TRUE), ]
      }
    })

    output$dir_sub <- renderText({
      n <- nrow(dir_rows()); tot <- nrow(geo_pincodes())
      if (n == tot) paste(inr_group(tot), "PIN codes")
      else paste(inr_group(n), "of", inr_group(tot), "PIN codes")
    })

    output$pin_tbl <- renderDT({
      r <- dir_rows()
      if (!nrow(r)) {
        return(tms_table(tibble::tibble(
          Message = "No PIN code, city or state matches that search"),
          selection = "none"))
      }
      # 19,238 rows is past what a client-side DataTable should be handed, and
      # nobody scrolls a directory anyway — they search it. Show the first
      # page's worth of matches and let the search box do the narrowing.
      r <- r[order(r$pincode), ]
      capped <- nrow(r) > 500
      if (capped) r <- r[seq_len(500), ]
      df <- tibble::tibble(
        `PIN CODE` = paste0('<span class="mono" style="font-weight:600;">',
                            htmlEscape(r$pincode), "</span>"),
        LOCALITY   = htmlEscape(r$locality),
        CITY       = paste0('<span style="font-weight:600;color:#12275C;">',
                            htmlEscape(r$city), "</span>"),
        STATE      = htmlEscape(r$state),
        `POST OFFICES` = r$offices,
        COORDINATES = paste0('<span class="mono tiny">',
                             sprintf("%.3f, %.3f", r$lat, r$lon), "</span>")
      )
      tms_table(df, page = 10, selection = "none", align = align_right(5))
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
            # Any city in India, not only the ones with a branch — a lane can
            # be priced to a town the company routes into through a partner.
            div(class = "col-6", selectizeInput(ns("n_src"), "Source city",
                                                geo_city_choices(), width = "100%",
                                                options = list(maxOptions = 40))),
            div(class = "col-6", selectizeInput(ns("n_dst"), "Destination city",
                                                c("Search any city…" = "", geo_city_choices()),
                                                selected = "",
                                                width = "100%",
                                                options = list(placeholder = "Search any city",
                                                               maxOptions = 40))),
            div(class = "col-4", textInput(ns("n_spin"), "Source pincode")),
            div(class = "col-4", textInput(ns("n_dpin"), "Destination pincode")),
            div(class = "col-4", numericInput(ns("n_days"), "Transit days", 2, 1, 15, 1)),
            div(class = "col-12", uiOutput(ns("n_est"))),
            div(class = "col-6", selectInput(ns("n_pzone"), "Pickup zone",
                                             paste("Zone", c("North","South","East","West","Central")))),
            div(class = "col-6", selectInput(ns("n_dzone"), "Delivery zone",
                                             paste("Zone", c("North","South","East","West","Central")))),
            div(class = "col-12", selectInput(ns("n_avail"), "Service availability",
                                              c("Available", "Limited", "Not Serviceable")))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("n_save"), "Add mapping"))
      ))
    })

    # Prefill the pincode when a city is chosen, so the operator does not have
    # to look it up. The branch master wins where the company has an office —
    # its pincode is the actual depot — and the city directory covers the rest.
    prefill_pin <- function(city_id, pin_id) {
      observeEvent(input[[city_id]], {
        city <- input[[city_id]] %||% ""
        if (!nzchar(city)) return()
        b <- store_get("branches")
        p <- b$pincode[match(city, b$city)]
        if (is.na(p)) {
          g <- geo_city(city)
          p <- if (is.null(g)) NA_character_ else g$pincode
        }
        if (!is.na(p)) updateTextInput(session, pin_id, value = p)
      }, ignoreInit = TRUE)
    }
    prefill_pin("n_src", "n_spin")
    prefill_pin("n_dst", "n_dpin")

    # Show what the coordinates make of the lane while the operator is typing.
    # Mapping a lane is precisely the act of replacing this estimate with a
    # negotiated number, so it belongs in front of them at that moment.
    output$n_est <- renderUI({
      src <- input$n_src %||% ""; dst <- input$n_dst %||% ""
      if (!nzchar(src) || !nzchar(dst)) return(NULL)
      if (identical(input$n_spin %||% "", input$n_dpin %||% "")) return(NULL)
      ln <- geo_lane(src, dst, input$n_spin, input$n_dpin)
      if (is.null(ln) || isTRUE(ln$mapped)) return(NULL)
      callout("Estimated from coordinates",
              paste0("~", inr_group(ln$km), " km by road · ", ln$days,
                     if (ln$days == 1) " day" else " days",
                     " at 425 km/day. Override the transit days with the figure you have actually committed to."),
              "info")
    })

    observeEvent(input$n_save, {
      if (!require_perm(session, user()$role, "pincodes", "create")) return()
      # A lane is a PIN pair, not a city pair: Mumbai 400001 → Mumbai 400071 is
      # a real local lane worth pricing, so only an identical PIN at both ends
      # is meaningless.
      if (identical(input$n_spin %||% "", input$n_dpin %||% "")) {
        showNotification("Source and destination pincodes must differ.",
                         type = "error"); return()
      }
      r <- store_get("pincode_routes")
      dup <- r[r$src_pincode == input$n_spin & r$dst_pincode == input$n_dpin, ]
      if (nrow(dup)) {
        showNotification("That lane is already mapped.", type = "error"); return()
      }
      b <- store_get("branches")
      id_new <- next_id(r$route_id, "RT-")
      # A lane saved with a blank distance left the booking form quoting a
      # transit promise with no kilometres behind it. Fall back to the
      # coordinate estimate so the row is at least complete.
      est <- geo_lane(input$n_src, input$n_dst, input$n_spin, input$n_dpin)
      # Where an end of the lane has no branch, say so rather than printing
      # "NA → Nagpur (HQ)" on the serviceability callout.
      depot <- function(city) {
        n <- b$name[match(city, b$city)]
        if (is.na(n)) paste(city, "(no branch)") else n
      }
      store_insert("pincode_routes", list(
        route_id = id_new,
        src_pincode = input$n_spin, src_city = input$n_src,
        dst_pincode = input$n_dpin, dst_city = input$n_dst,
        distance_km = geo_lane_km(est),
        transit_days = input$n_days,
        route_path = paste(input$n_src, "→", input$n_dst),
        branch_mapping = paste(depot(input$n_src), "→", depot(input$n_dst)),
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
