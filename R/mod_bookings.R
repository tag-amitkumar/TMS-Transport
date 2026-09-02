# ==================================================================
# Cargo Bookings — list and creation form.
#
# Two screens share one module because they share the booking vocabulary and
# the create handler: bookings_ui() is the list, booking_new_ui() the form.
#
# Numbering: the deck used BK-30241 on the list and BKG-44xx on four other
# screens for the same entity. Standardised on the BKG- series (PREFIX$booking)
# and allocated on save, which is what "auto-numbered on save" in the form's
# subtitle promises.
# ==================================================================

bookings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("filters"), "Filters", fontawesome::fa("filter")),
            uiOutput(ns("new_btn"), inline = TRUE))),
    card_panel(body_class = "tms-card-body p-0",
               div(class = "tms-table", DTOutput(ns("tbl")))),
    uiOutput(ns("detail"))
  )
}

booking_new_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "row g-3",
    div(
      class = "col-xl-8",
      card_panel(
        title = "Customer & Route",
        sub = textOutput(ns("f_no"), inline = TRUE),
        div(class = "row g-3",
            # These four selectors are rendered server-side rather than declared
            # with static choices. The page only exists while it is being
            # viewed, so anything pushed by an update*Input() before then — at
            # sign-in, say — is sent to an element that is not in the DOM and is
            # silently dropped, leaving the form permanently empty. Rendering
            # them here rebuilds the lists from the store every time the screen
            # opens, which also means a customer added mid-session shows up
            # without a reload.
            div(class = "col-md-5",
                tags$label(class = "form-label req", "Customer"),
                uiOutput(ns("sel_client"))),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Branch"),
                uiOutput(ns("sel_branch"))),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Booking date"),
                dateInput(ns("f_date"), NULL, value = Sys.Date(), width = "100%")),
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Pickup address"),
                textInput(ns("f_pickup"), NULL, width = "100%")),
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Delivery address"),
                textInput(ns("f_drop"), NULL, width = "100%")),
            div(class = "col-md-4",
                tags$label(class = "form-label", "Origin city"),
                uiOutput(ns("sel_from"))),
            div(class = "col-md-4",
                tags$label(class = "form-label", "Destination city"),
                uiOutput(ns("sel_to"))),
            div(class = "col-md-4",
                tags$label(class = "form-label", "Booking user"),
                div(class = "field-static", textOutput(ns("f_user"), inline = TRUE)))),
        # Serviceability comes from the pin-code route master, which is exactly
        # what the deck says that screen is for: "serviceability checks shown at
        # booking time".
        div(class = "mt-3", uiOutput(ns("route_check")))
      ),

      div(class = "mt-3", card_panel(
        title = "Material & Charges",
        div(class = "row g-3",
            div(class = "col-12",
                tags$label(class = "form-label req", "Material details"),
                textInput(ns("f_material"), NULL, width = "100%")),
            div(class = "col-md-2",
                tags$label(class = "form-label req", "Weight (T)"),
                numericInput(ns("f_weight"), NULL, 10, 0.1, 60, 0.1, width = "100%")),
            div(class = "col-md-2",
                tags$label(class = "form-label", "Quantity"),
                numericInput(ns("f_qty"), NULL, 100, 0, step = 1, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Packages"),
                textInput(ns("f_pkg"), NULL, width = "100%")),
            div(class = "col-md-5",
                tags$label(class = "form-label", "Insurance"),
                selectInput(ns("f_ins"), NULL,
                            c("Not insured", "Insured"), width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Freight charges (₹)"),
                numericInput(ns("f_freight"), NULL, 15000, 0, step = 100, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label", "GST"),
                div(class = "field-static", textOutput(ns("f_gst_lbl"), inline = TRUE))),
            div(class = "col-md-6",
                tags$label(class = "form-label", "Remarks"),
                textInput(ns("f_remarks"), NULL, width = "100%")))
      ))
    ),

    div(
      class = "col-xl-4",
      card_panel(title = "Booking Summary", uiOutput(ns("summary"))),
      div(class = "mt-3", card_panel(
        callout("Next steps",
                "Confirming this booking makes it available for vehicle allocation and consignment (LR) generation.",
                "info", fontawesome::fa("circle-info")),
        div(class = "mt-3 d-grid gap-2",
            btn_ghost(ns("save_draft"), "Save as Draft"),
            btn_primary(ns("confirm"), "Confirm Booking"))
      ))
    )
  )
}

bookings_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")

    all_rows <- reactive(scope_all(get_bookings(), user()))

    output$tabs <- renderUI({
      b <- all_rows()
      items  <- setNames(c("All", BOOKING_STATUS), c("All", BOOKING_STATUS))
      counts <- as.list(c(nrow(b), vapply(BOOKING_STATUS, function(s) sum(b$status == s), integer(1))))
      names(counts) <- c("All", BOOKING_STATUS)
      tab_strip(ns, "tab", items, counts, tab())
    })

    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      b <- all_rows()
      if (!identical(tab(), "All")) b <- b[b$status == tab(), ]
      b[order(b$booking_date, decreasing = TRUE), ]
    })

    output$new_btn <- renderUI({
      if (!can(user()$role, "bookings", "create")) return(NULL)
      btn_primary(ns("go_new"), "New Booking", fontawesome::fa("plus"))
    })
    observeEvent(input$go_new, nav("booking_new"))

    output$tbl <- renderDT({
      b <- rows(); cl <- store_get("clients"); br <- store_get("branches")
      df <- tibble::tibble(
        `BOOKING NO.` = paste0('<span style="font-weight:600;color:#12263F;">',
                               htmlEscape(b$booking_no), "</span>"),
        DATE     = fmt_date(b$booking_date, TRUE),
        BRANCH   = br$city[match(b$branch_id, br$branch_id)],
        CUSTOMER = cl$name[match(b$client_id, cl$client_id)],
        `PICKUP → DELIVERY` = paste(b$origin_city, "→", b$dest_city),
        MATERIAL = b$material,
        `WEIGHT / QTY` = fmt_wt(b$weight_t),
        FREIGHT  = inr(b$freight),
        STATUS   = pill_html(b$status)
      )
      tms_table(df, page = 14, align = align_right(6:7))
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    # Inline detail strip under the list, rather than a side panel — the deck's
    # booking list is full width and has no 360° pane.
    output$detail <- renderUI({
      b <- sel()
      if (is.null(b)) return(NULL)
      cl <- store_get("clients")
      cn <- get_consignments(); cn <- cn[cn$booking_no == b$booking_no, ]
      tr <- get_trips();        tr <- tr[tr$booking_no == b$booking_no, ]
      v  <- store_get("vehicles"); dr <- store_get("drivers")

      div(class = "mt-3", card_panel(
        title = paste(b$booking_no, "—", cl$name[match(b$client_id, cl$client_id)]),
        sub = paste(b$origin_city, "→", b$dest_city, "·", b$material),
        actions = pill(b$status),
        div(class = "row g-3",
            div(class = "col-md-4", dl_rows(
              "Booking date" = fmt_date(b$booking_date),
              "Weight"       = fmt_wt(b$weight_t),
              "Packages"     = b$packages,
              "Insurance"    = b$insurance)),
            div(class = "col-md-4", dl_rows(
              "Freight"   = inr(b$freight),
              "GST"       = paste0(b$gst_mode, " ", b$gst_pct, "% · ", inr(b$gst_amount)),
              "Insurance" = inr(b$insurance_amt),
              "Total"     = inr(b$total))),
            div(class = "col-md-4",
                if (nrow(tr)) dl_rows(
                  "Trip"    = tr$trip_no[1],
                  "Vehicle" = span(class = "mono", v$reg_no[match(tr$vehicle_id[1], v$vehicle_id)]),
                  "Driver"  = dr$name[match(tr$driver_id[1], dr$driver_id)],
                  "LR"      = if (nrow(cn)) cn$lr_no[1] else "—")
                else callout("Not yet allocated",
                             "This booking has no vehicle assigned. Allocate it from Cargo Moto.",
                             "warn")))
      ))
    })

    observeEvent(input$filters, {
      showModal(modalDialog(title = "Filter bookings", easyClose = TRUE,
        dateRangeInput(ns("f_range"), "Booking date",
                       start = Sys.Date() - 30, end = Sys.Date()),
        footer = modalButton("Close")))
    })

    # ---------------- New booking form ----------------

    # Selectors are rendered, not updated. See the note in booking_new_ui():
    # pushing choices from an observer that fires before the page is on screen
    # sends them nowhere, which left the customer list empty and made the form
    # impossible to submit.
    booking_cities <- reactive(sort(unique(store_get("branches")$city)))

    output$sel_client <- renderUI({
      cl <- scope_branch(store_get("clients"), user())
      if (!nrow(cl)) {
        return(div(class = "field-static", "No customers on file — add one first"))
      }
      selectizeInput(ns("f_client"), NULL, width = "100%",
                     choices = c("Select a customer…" = "",
                                 setNames(cl$client_id, paste0(cl$name, " · ", cl$gstin))),
                     selected = "")
    })

    output$sel_branch <- renderUI({
      br <- scope_branch(store_get("branches"), user())
      if (!nrow(br)) br <- store_get("branches")
      selectInput(ns("f_branch"), NULL, width = "100%",
                  choices = setNames(br$branch_id, br$name),
                  selected = if (user()$branch_id %in% br$branch_id) user()$branch_id else br$branch_id[1])
    })

    output$sel_from <- renderUI({
      selectInput(ns("f_from"), NULL, choices = booking_cities(), width = "100%")
    })

    output$sel_to <- renderUI({
      ct <- booking_cities()
      selectInput(ns("f_to"), NULL, choices = ct, width = "100%",
                  selected = if (length(ct) > 1) ct[2] else ct[1])
    })

    output$f_no <- renderText({
      paste0("Booking ", next_id(store_get("bookings")$booking_no, PREFIX$booking),
             " · auto-numbered on save")
    })
    output$f_user <- renderText(paste(user()$name, "·", user()$role))

    # Pre-fill addresses from the client master when one is picked.
    observeEvent(input$f_client, {
      req(nzchar(input$f_client %||% ""))
      cl <- store_get("clients")
      r <- cl[cl$client_id == input$f_client, ]
      if (!nrow(r)) return()
      updateTextInput(session, "f_pickup", value = r$pickup_address[1])
      updateTextInput(session, "f_drop",   value = r$delivery_address[1])

      ct <- booking_cities()
      if (r$city[1] %in% ct) {
        updateSelectInput(session, "f_from", selected = r$city[1])
        # Moving the origin onto the customer's city can collide with whatever
        # the destination happens to be sitting on, and the form then refuses
        # to submit with "same origin and destination" before the user has
        # touched anything. Step the destination aside.
        if (identical(input$f_to %||% "", r$city[1])) {
          alt <- setdiff(ct, r$city[1])
          if (length(alt)) updateSelectInput(session, "f_to", selected = alt[1])
        }
      }
    })

    # Same guard in the other direction: if the user picks a destination equal
    # to the origin, move the origin rather than leaving the form unsubmittable.
    observeEvent(input$f_to, {
      req(nzchar(input$f_to %||% ""), nzchar(input$f_from %||% ""))
      if (identical(input$f_from, input$f_to)) {
        alt <- setdiff(booking_cities(), input$f_to)
        if (length(alt)) updateSelectInput(session, "f_from", selected = alt[1])
      }
    }, ignoreInit = TRUE)

    # Returns NULL rather than req()-halting when nothing is picked yet. A
    # halt here propagates through gst_pct/charges and blanks the whole Booking
    # Summary card, so the form opens looking broken instead of empty.
    client_row <- reactive({
      cid <- input$f_client %||% ""
      if (!nzchar(cid)) return(NULL)
      cl <- store_get("clients")
      r <- cl[cl$client_id == cid, ]
      if (nrow(r)) r[1, ] else NULL
    })

    # GST is taken from the client's configured treatment, never typed by the
    # booking clerk — that is the rule the invoice screen depends on.
    gst_pct <- reactive({
      r <- client_row()
      if (is.null(r)) return(5)
      as.numeric(r$gst_pct)
    })
    gst_mode <- reactive({
      r <- client_row()
      if (is.null(r)) return("RCM")
      r$gst_mode
    })

    output$f_gst_lbl <- renderText({
      paste0(gst_pct(), "% (", gst_mode(),
             if (identical(gst_mode(), "RCM")) " · GTA" else "", ")")
    })

    output$route_check <- renderUI({
      req(nzchar(input$f_from %||% ""), nzchar(input$f_to %||% ""))
      if (identical(input$f_from, input$f_to)) {
        return(callout("Same origin and destination",
                       "Pick a different destination city.", "warn"))
      }
      rt <- store_get("pincode_routes")
      hit <- rt[rt$src_city == input$f_from & rt$dst_city == input$f_to, ]
      if (!nrow(hit)) {
        return(callout("Route not mapped",
                       paste0(input$f_from, " → ", input$f_to,
                              " has no entry in the pin-code route master. Transit promise cannot be quoted."),
                       "warn"))
      }
      h <- hit[1, ]
      type <- switch(h$availability, "Available" = "ok", "Limited" = "warn", "danger")
      callout(paste0("Serviceable · ", h$availability),
              paste0(h$route_path, " · ", h$transit_days, " day transit · ",
                     h$branch_mapping),
              type)
    })

    charges <- reactive({
      fr  <- as.numeric(input$f_freight %||% 0)
      if (is.na(fr)) fr <- 0
      gp  <- gst_pct()
      rcm <- identical(gst_mode(), "RCM")

      # Under reverse charge the carrier collects nothing — the recipient
      # discharges the tax directly. Charging it here would inflate the booking
      # total against an invoice that will (correctly) show zero, and overstate
      # revenue on every RCM job, which is most of the book.
      gst <- if (rcm) 0 else round(fr * gp / 100)
      gst_note <- if (rcm) round(fr * gp / 100) else 0   # shown, never charged

      # Insurance premium is 1.25% of the declared value, which the deck sets at
      # roughly 30x freight for a fully declared consignment.
      dv  <- if (identical(input$f_ins, "Insured")) round(fr * 30 / 1000) * 1000 else 0
      ins <- if (dv > 0) round(dv * 0.000125 / 10) * 10 else 0

      list(freight = fr, gst = gst, gst_pct = gp, rcm = rcm, gst_note = gst_note,
           insurance = ins, declared = dv, total = fr + gst + ins)
    })

    output$summary <- renderUI({
      ch <- charges(); r <- client_row()
      # The GST row's label carries the rate and mode, so it is built as a
      # named list and splatted rather than written inline. Under reverse charge
      # the amount is shown for information with a nil collected, because the
      # customer still needs to see what they will be discharging themselves.
      charge_rows <- list(
        inr(ch$freight),
        if (ch$rcm) HTML(paste0('<span class="muted">', inr(ch$gst_note),
                                " payable by recipient</span>")) else inr(ch$gst),
        inr(ch$insurance)
      )
      names(charge_rows) <- c("Freight charges",
                              sprintf("GST (%s%% · %s)", ch$gst_pct, gst_mode()),
                              "Insurance")
      tagList(
        do.call(dl_rows, charge_rows),
        tags$hr(class = "soft"),
        div(class = "d-flex justify-content-between align-items-center",
            span(style = "font-weight:650;", "Total"),
            span(style = "font-weight:700;font-size:1.05rem;color:#12263F;", inr(ch$total))),
        tags$hr(class = "soft"),
        dl_rows(
          "Customer" = if (is.null(r)) "—" else r$name,
          "Route"    = paste(input$f_from %||% "—", "→", input$f_to %||% "—"),
          "Weight"   = fmt_wt(input$f_weight),
          "Status"   = pill("Draft")
        )
      )
    })

    # Validation shared by both save paths.
    validate_form <- function() {
      msgs <- c()
      if (!nzchar(input$f_client %||% ""))   msgs <- c(msgs, "Customer is required.")
      if (!nzchar(input$f_pickup %||% ""))   msgs <- c(msgs, "Pickup address is required.")
      if (!nzchar(input$f_drop %||% ""))     msgs <- c(msgs, "Delivery address is required.")
      if (!nzchar(input$f_material %||% "")) msgs <- c(msgs, "Material details are required.")
      w <- as.numeric(input$f_weight %||% 0)
      if (is.na(w) || w <= 0)                msgs <- c(msgs, "Weight must be greater than zero.")
      f <- as.numeric(input$f_freight %||% 0)
      if (is.na(f) || f <= 0)                msgs <- c(msgs, "Freight charges must be greater than zero.")
      if (identical(input$f_from, input$f_to)) msgs <- c(msgs, "Origin and destination must differ.")
      msgs
    }

    save_booking <- function(status) {
      if (!require_perm(session, user()$role, "bookings", "create")) return()
      msgs <- validate_form()
      if (length(msgs)) {
        showNotification(HTML(paste(msgs, collapse = "<br/>")), type = "error", duration = 8)
        return()
      }
      ch <- charges(); r <- client_row()
      no <- next_id(store_get("bookings")$booking_no, PREFIX$booking)

      store_insert("bookings", list(
        booking_no = no,
        booking_date = as.character(input$f_date %||% Sys.Date()),
        branch_id = input$f_branch, client_id = input$f_client,
        pickup_address = input$f_pickup, delivery_address = input$f_drop,
        origin_city = input$f_from, dest_city = input$f_to,
        material = input$f_material,
        weight_t = input$f_weight, quantity = input$f_qty,
        packages = input$f_pkg, insurance = input$f_ins,
        declared_value = ch$declared, freight = ch$freight,
        gst_mode = gst_mode(), gst_pct = ch$gst_pct, gst_amount = ch$gst,
        insurance_amt = ch$insurance, total = ch$total,
        remarks = input$f_remarks, booking_user_id = user()$user_id,
        priority = "Normal", status = status
      ))
      audit(user()$user_id, tolower(status), "bookings", no)
      showNotification(sprintf("Booking %s saved as %s.", no, tolower(status)),
                       type = "message", duration = 5)
      nav("bookings")
    }

    observeEvent(input$save_draft, save_booking("Draft"))
    observeEvent(input$confirm,    save_booking("Confirmed"))
  })
}
