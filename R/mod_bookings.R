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
            uiOutput(ns("manual_btn"), inline = TRUE),
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
            # Pincode and city are two views of one answer, and the pair stays
            # in step both ways: type a PIN and the city resolves, pick a city
            # and the PIN drops to that city's head office. Which one the clerk
            # reaches for depends on what the consignor gave them — a printed
            # address carries a PIN, a phone call carries a city name.
            # Rendered server-side, not declared with a value and updated
            # afterwards. The branch's PIN is pushed by the city→PIN binding
            # the moment the origin city selector reports "Nagpur", and that
            # can land in the same flush as the page insertion — the update
            # goes to an element not yet in the DOM and Shiny drops it. The
            # field then sits empty showing its placeholder, which looks
            # filled, and the form refuses to save with "Origin PIN code is
            # not a valid Indian pincode" pointing at a PIN the clerk can
            # plainly see. Same lesson as the customer dropdown above.
            div(class = "col-md-2",
                tags$label(class = "form-label req", "Origin PIN"),
                uiOutput(ns("sel_from_pin"))),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Origin city"),
                uiOutput(ns("sel_from")),
                uiOutput(ns("from_hint"))),
            div(class = "col-md-2",
                tags$label(class = "form-label req", "Destination PIN"),
                textInput(ns("f_to_pin"), NULL, width = "100%",
                          placeholder = "110001")),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Destination city"),
                uiOutput(ns("sel_to")),
                uiOutput(ns("to_hint"))),
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
      )),

      div(class = "mt-3", card_panel(
        title = "Payment terms",
        sub = "Who settles the freight, and when — printed on the lorry receipt",
        div(class = "row g-3",
            div(class = "col-md-5",
                tags$label(class = "form-label req", "Payment mode"),
                # Defaults to To Pay, not Paid. Whichever term the list opens on
                # is the one that gets saved when a clerk is in a hurry, and
                # "Paid" writes an invoice that is already settled — money
                # recorded as collected that nobody actually took. To Pay errs
                # the safe way: the balance stays visible until someone clears
                # it at the door.
                selectInput(ns("f_pay"), NULL, width = "100%",
                            choices = setNames(PAYMENT_MODES, PAYMENT_LABEL[PAYMENT_MODES]),
                            selected = "To Pay")),
            # Only meaningful for TBB, so it appears only for TBB.
            div(class = "col-md-4", uiOutput(ns("sel_bill_branch")))),
        div(class = "mt-2", uiOutput(ns("pay_note")))
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

    output$manual_btn <- renderUI({
      if (!can(user()$role, "bookings", "create")) return(NULL)
      btn_ghost(ns("manual_entry"), "Manual Entry", fontawesome::fa("pen"))
    })

    output$tbl <- renderDT({
      b <- rows(); cl <- store_get("clients"); br <- store_get("branches")

      pay <- b$payment_mode %||% rep("Credit", nrow(b))
      pay[is.na(pay) | !nzchar(pay)] <- "Credit"
      pay_html <- sprintf('<span class="pill pill-%s nodot">%s</span>',
                          unname(PAYMENT_COLOUR[pay] %||% "grey"), htmlEscape(pay))
      # TBB is only half the story without the branch that will bill it.
      tbb <- pay == "TBB" & nzchar(b$bill_at_branch_id %||% "")
      pay_html[tbb] <- paste0(
        pay_html[tbb], '<div class="tiny muted">at ',
        htmlEscape(br$city[match(b$bill_at_branch_id[tbb], br$branch_id)]), "</div>")

      # A booking keyed in after the fact says so, with its paper reference.
      manual <- (b$entry_mode %||% "") == "Manual"
      no_html <- paste0('<span style="font-weight:600;color:#12263F;">',
                        htmlEscape(b$booking_no), "</span>")
      no_html[manual] <- paste0(
        no_html[manual],
        '<div><span class="pill pill-orange nodot" style="font-size:.6rem;">manual</span> ',
        '<span class="tiny muted mono">', htmlEscape(b$manual_ref[manual]), "</span></div>")

      df <- tibble::tibble(
        `BOOKING NO.` = no_html,
        DATE     = fmt_date(b$booking_date, TRUE),
        BRANCH   = br$city[match(b$branch_id, br$branch_id)],
        CUSTOMER = cl$name[match(b$client_id, cl$client_id)],
        `PICKUP → DELIVERY` = paste(b$origin_city, "→", b$dest_city),
        MATERIAL = b$material,
        `WEIGHT / QTY` = fmt_wt(b$weight_t),
        FREIGHT  = inr(b$freight),
        PAYMENT  = pay_html,
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
                             "warn"))),
        div(class = "d-flex gap-2 mt-3",
            btn_dark(ns("print_note"), "Print consignment note (3 copies)",
                     fontawesome::fa("print")))
      ))
    })

    # Assemble everything the printed slip needs. Kept in one place because the
    # same shape is printed from the consignment register, where the vehicle and
    # LR number are known and here they may not be yet.
    observeEvent(input$print_note, {
      b <- sel(); req(b)
      cl <- store_get("clients"); br <- store_get("branches")
      cn <- get_consignments(); c1 <- cn[cn$booking_no == b$booking_no, ]
      tr <- get_trips(); t <- tr[tr$booking_no == b$booking_no, ]
      v  <- store_get("vehicles"); dr <- store_get("drivers")
      cr <- cl[cl$client_id == b$client_id, ]

      pay <- b$payment_mode %||% "Credit"
      show_lr_print(list(
        company   = setting("company_name", BRAND$company),
        office    = setting("registered_office", ""),
        lr_no     = if (nrow(c1)) c1$lr_no[1] else paste0("(LR on allocation) ", b$booking_no),
        cn_no     = if (nrow(c1)) c1$cn_no[1] else b$booking_no,
        date      = fmt_date(b$booking_date),
        consignor = if (nrow(cr)) cr$name[1] else "—",
        consignee = b$dest_city,
        from_addr = b$pickup_address, to_addr = b$delivery_address,
        from = b$origin_city, to = b$dest_city,
        vehicle = if (nrow(t)) v$reg_no[match(t$vehicle_id[1], v$vehicle_id)] else "Not allocated",
        driver  = if (nrow(t)) dr$name[match(t$driver_id[1], dr$driver_id)] else "—",
        material = b$material, weight = fmt_wt(b$weight_t),
        packages = b$packages %||% "—",
        gstin = if (nrow(cr)) cr$gstin[1] else "—",
        payment = pay,
        pay_colour = unname(PAYMENT_COLOUR[pay] %||% "grey"),
        pay_note = switch(pay,
          "To Pay" = "Collect from consignee before release",
          "Paid"   = "Settled at booking — collect nothing",
          "TBB"    = paste("Bill at", br$name[match(b$bill_at_branch_id, br$branch_id)] %||% "—"),
          "Monthly account"),
        freight = inr(b$freight),
        gst_mode = b$gst_mode,
        gst = if (identical(b$gst_mode, "RCM")) "Payable by recipient" else inr(b$gst_amount),
        insurance = inr(b$insurance_amt), total = inr(b$total),
        manual_note = if (identical(b$entry_mode %||% "", "Manual"))
          paste0("Entered manually from paper reference ", b$manual_ref,
                 " · load accepted ", fmt_dt(b$manual_dt)) else ""
      ), title = paste("Consignment note —", b$booking_no))
      audit(user()$user_id, "print", "bookings", b$booking_no)
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
    # Every city in India, not just the ones the company has a branch in.
    # Origin and destination used to be drawn from the branch master, which on
    # a two-branch install meant exactly two choices and no way to book a load
    # to anywhere the company does not have an office — which is most loads.
    booking_cities <- reactive(geo_city_choices())

    # The clerk's own branch city is the sane default for origin: the great
    # majority of bookings are taken at the branch the goods leave from.
    home_city <- reactive({
      br <- store_get("branches")
      c0 <- br$city[match(user()$branch_id, br$branch_id)]
      if (length(c0) && !is.na(c0) && !is.null(geo_city(c0))) c0 else NA_character_
    })

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

    # selectize rather than a plain select: 630 cities is far past the point
    # where scrolling a list works, and its search is what makes the aliases
    # carried in each label usable — typing "Noida" finds Gautam Buddha Nagar.
    # A selectize with no selection silently takes the first option, which with
    # an alphabetical all-India list meant every new booking opened addressed
    # to Adilabad. An explicit blank choice is what makes "nothing picked yet"
    # representable — and it is what the placeholder needs to show at all.
    city_selector <- function(input_id, selected) {
      blank <- is.null(selected) || is.na(selected)
      selectizeInput(
        ns(input_id), NULL, width = "100%",
        choices  = if (blank) c("Search any city…" = "", booking_cities())
                   else booking_cities(),
        selected = if (blank) "" else selected,
        options  = list(placeholder = "Type a city, or a PIN alongside",
                        maxOptions = 40))
    }

    output$sel_from <- renderUI(city_selector("f_from", home_city()))
    output$sel_to   <- renderUI(city_selector("f_to", NULL))

    # The origin PIN carries its value from the first render, so the form is
    # submittable the moment it opens rather than one round-trip later.
    output$sel_from_pin <- renderUI({
      hc <- home_city()
      g  <- if (is.na(hc)) NULL else geo_city(hc)
      textInput(ns("f_from_pin"), NULL, width = "100%",
                value = if (is.null(g)) "" else g$pincode,
                placeholder = "440001")
    })

    # What the PIN actually resolved to. The city selector shows a district —
    # "Gautam Buddha Nagar" — which is not what the clerk typed or what the
    # consignor wrote, so echo the locality and state underneath to confirm the
    # right place was found.
    pin_hint <- function(pin_id) {
      renderUI({
        raw <- trimws(input[[pin_id]] %||% "")
        if (!nzchar(raw)) return(NULL)
        p <- geo_pin(raw)
        if (is.null(p)) {
          return(div(class = "tiny", style = "color:#C0392B;margin-top:.25rem;",
                     if (grepl("^[0-9]{6}$", raw)) "Not an Indian PIN code"
                     else "PIN codes are six digits"))
        }
        div(class = "tiny muted", style = "margin-top:.25rem;",
            p$locality, " · ", p$state)
      })
    }
    output$from_hint <- pin_hint("f_from_pin")
    output$to_hint   <- pin_hint("f_to_pin")

    # PIN -> city, and city -> PIN. Each observer no-ops when the pair is
    # already consistent, which is what stops the two from ping-ponging.
    bind_pin_city <- function(pin_id, city_id) {
      observeEvent(input[[pin_id]], {
        p <- geo_pin(input[[pin_id]])
        if (is.null(p)) return()
        if (!identical(input[[city_id]] %||% "", p$city)) {
          updateSelectizeInput(session, city_id, selected = p$city)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[city_id]], {
        city <- input[[city_id]] %||% ""
        if (!nzchar(city)) return()
        cur <- geo_pin(input[[pin_id]])
        if (!is.null(cur) && identical(cur$city, city)) return()
        cty <- geo_city(city)
        if (!is.null(cty)) updateTextInput(session, pin_id, value = cty$pincode)
      }, ignoreInit = TRUE)
    }
    bind_pin_city("f_from_pin", "f_from")
    bind_pin_city("f_to_pin",   "f_to")
    # The manual-entry dialog's fields only exist while it is open, but the
    # observers can be wired once here — they fire on the input values, which
    # simply do not arrive until the modal renders them.
    bind_pin_city("m_from_pin", "m_from")
    bind_pin_city("m_to_pin",   "m_to")

    # TBB means another branch raises the invoice against an account the
    # customer already holds there, so the field only exists for TBB.
    output$sel_bill_branch <- renderUI({
      if (!identical(input$f_pay %||% "", "TBB")) return(NULL)
      b <- store_get("branches")
      b <- b[b$branch_id != (input$f_branch %||% ""), ]
      tagList(
        tags$label(class = "form-label req", "Bill at branch"),
        selectInput(ns("f_bill_branch"), NULL, width = "100%",
                    choices = setNames(b$branch_id, b$name))
      )
    })

    output$pay_note <- renderUI({
      mode <- input$f_pay %||% "Credit"
      b <- store_get("branches")
      switch(mode,
        "Paid" = callout(
          "Settled at booking",
          "Freight is collected now. The invoice is raised already paid and nothing is due on delivery.",
          "ok"),
        "To Pay" = callout(
          "Collect on delivery",
          "The consignee pays before the goods are released. This prints large on all three copies of the LR so the driver cannot miss it.",
          "warn"),
        "Credit" = callout(
          "Booked to account",
          "Billed on the customer's monthly cycle. The invoice is raised on delivery once the POD is verified.",
          "info"),
        "TBB" = callout(
          "Billed by another branch",
          sprintf("The load moves from here, but %s raises the invoice against the account the customer holds there. Nothing is collected on delivery.",
                  b$name[match(input$f_bill_branch %||% "", b$branch_id)] %||% "the selected branch"),
          "info"),
        NULL)
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

      # Prefer the customer's own PIN and let the binding above resolve the
      # city from it, so a customer whose recorded city predates the master
      # (or is spelt differently) still lands on the right district.
      if ("pincode" %in% names(r) && !is.null(geo_pin(r$pincode[1]))) {
        updateTextInput(session, "f_from_pin", value = r$pincode[1])
      } else if (!is.null(geo_city(r$city[1]))) {
        updateSelectizeInput(session, "f_from", selected = geo_city(r$city[1])$city)
      }
    })

    # Origin and destination used to shove each other aside on a collision,
    # which made sense when there were two cities to choose between. Against
    # 630 it would fling the clerk to an arbitrary district, so the collision
    # is now just reported — route_check says so, and validate_form blocks the
    # save. Picking a different city is one keystroke away.

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

    lane <- reactive(geo_lane(input$f_from %||% "", input$f_to %||% "",
                              input$f_from_pin, input$f_to_pin))

    output$route_check <- renderUI({
      req(nzchar(input$f_from %||% ""), nzchar(input$f_to %||% ""))
      fp <- geo_pin(input$f_from_pin); tp <- geo_pin(input$f_to_pin)

      # Same PIN at both ends is a legitimate booking — a pickup and a drop
      # inside one PIN code area — so it is reported, not refused. There is no
      # distance to quote: the two ends resolve to one point by definition.
      if (!is.null(fp) && !is.null(tp) && identical(fp$pincode, tp$pincode)) {
        return(callout(
          "Local delivery · within one PIN code",
          HTML(paste0(
            htmlEscape(fp$pincode), " · ", htmlEscape(fp$locality), ", ",
            htmlEscape(fp$city), " at both ends.<br/>",
            '<span class="tiny muted">Pickup and delivery are inside the same PIN code area, so there is no lane distance to quote. The pickup and delivery addresses are what separate the two ends; price it from your local rate card.</span>')),
          "info"))
      }
      ln <- lane()
      if (is.null(ln)) {
        return(callout("Lane cannot be quoted",
                       "Neither end resolves against the PIN code master, so no distance or transit can be worked out.",
                       "warn"))
      }

      # A hand-mapped lane is a commercial commitment — negotiated transit
      # days, agreed depot routing — so it always wins over the estimate and
      # keeps saying so.
      if (isTRUE(ln$mapped)) {
        type <- switch(ln$status, "Available" = "ok", "Limited" = "warn", "danger")
        return(callout(
          paste0("Serviceable · ", ln$status),
          paste0(ln$path, " · ", inr_group(ln$km), " km · ", ln$days,
                 " day transit · ", ln$branches),
          type))
      }

      # Everywhere else, which with 630 cities is nearly everywhere. Saying
      # "not mapped, cannot quote" on 99% of lanes would make the city list
      # useless, so the coordinates carry it — labelled as an estimate, because
      # it is one and nobody should bill off it.
      # A local lane between two PIN codes the reference data could not place
      # separately has no distance worth printing. Say that, rather than
      # quoting the 0 km the coordinates would produce — a cartage job priced
      # at nothing is worse than one priced by hand.
      if (!isTRUE(ln$km_known)) {
        return(callout(
          "Local delivery",
          HTML(paste0(
            htmlEscape(ln$path), " · same city, cross-town.<br/>",
            '<span class="tiny muted">Distance not estimated: the PIN code master places both of these localities at the same city-level point, so any figure would be invented. Enter the freight from your local rate card, or map the lane on Pin Code Mapping to fix a distance.</span>')),
          "info"))
      }

      callout(
        if (isTRUE(ln$local)) "Local delivery" else "Estimated lane",
        HTML(paste0(
          htmlEscape(ln$path), " · <strong>~", inr_group(ln$km),
          " km</strong> by road · <strong>", ln$days,
          if (ln$days == 1) " day" else " days",
          "</strong> transit.<br/>",
          '<span class="tiny muted">',
          if (isTRUE(ln$local))
            "Both ends are in the same city — cross-town cartage, measured between the two PIN codes rather than taken off a trunk lane."
          else
            "Straight-line distance between the two PIN codes with a road factor applied. Not in the route master, so treat the transit as indicative until the lane is mapped.",
          "</span>")),
        "info")
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
          "Distance" = {
            ln <- lane()
            if (is.null(ln) || !isTRUE(ln$km_known)) "—"
            else paste0(if (isTRUE(ln$mapped)) "" else "~", inr_group(ln$km), " km")
          },
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
      # The PIN is what goes on the LR and what the e-way bill is raised
      # against, so an unresolvable one is not a cosmetic problem.
      fp <- geo_pin(input$f_from_pin); tp <- geo_pin(input$f_to_pin)
      if (is.null(fp)) msgs <- c(msgs, "Origin PIN code is not a valid Indian pincode.")
      if (is.null(tp)) msgs <- c(msgs, "Destination PIN code is not a valid Indian pincode.")
      # Nothing about the geography has to differ. A delivery from one address
      # to another inside a single PIN code area is ordinary local cartage —
      # two gates on the same industrial estate, two buildings on one street —
      # and it is the pickup and delivery *addresses* that distinguish the ends
      # of that job, not the PIN. Both are already required above.
      if (nzchar(input$f_pickup %||% "") &&
          identical(trimws(tolower(input$f_pickup %||% "")),
                    trimws(tolower(input$f_drop %||% "")))) {
        msgs <- c(msgs, "Pickup and delivery addresses are identical — one of them is wrong.")
      }
      # TBB without a billing branch is meaningless — the whole point of the
      # term is that some *other* branch raises the invoice.
      if (identical(input$f_pay %||% "", "TBB") && !nzchar(input$f_bill_branch %||% "")) {
        msgs <- c(msgs, "TBB requires the branch that will raise the bill.")
      }
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
        # The PIN pair is the precise geography; the city is the human label
        # for it. Both are stored because the LR prints the city and the
        # e-way bill needs the codes. The distance is frozen at booking time —
        # it is what was quoted, and re-deriving it later would let a refreshed
        # pincode master silently change an agreed figure.
        origin_pincode = geo_pin(input$f_from_pin)$pincode %||% "",
        dest_pincode   = geo_pin(input$f_to_pin)$pincode %||% "",
        origin_state   = geo_pin(input$f_from_pin)$state %||% "",
        dest_state     = geo_pin(input$f_to_pin)$state %||% "",
        distance_km    = geo_lane_km(lane()),
        material = input$f_material,
        weight_t = input$f_weight, quantity = input$f_qty,
        packages = input$f_pkg, insurance = input$f_ins,
        declared_value = ch$declared, freight = ch$freight,
        gst_mode = gst_mode(), gst_pct = ch$gst_pct, gst_amount = ch$gst,
        insurance_amt = ch$insurance, total = ch$total,
        remarks = input$f_remarks, booking_user_id = user()$user_id,
        priority = "Normal",
        payment_mode = input$f_pay %||% "Credit",
        bill_at_branch_id = if (identical(input$f_pay %||% "", "TBB"))
                              (input$f_bill_branch %||% "") else "",
        entry_mode = "Online", manual_ref = "", manual_dt = "",
        status = status
      ))
      audit(user()$user_id, tolower(status), "bookings",
            paste(no, "·", input$f_pay %||% "Credit"))
      showNotification(sprintf("Booking %s saved as %s.", no, tolower(status)),
                       type = "message", duration = 5)
      nav("bookings")
    }

    observeEvent(input$save_draft, save_booking("Draft"))
    observeEvent(input$confirm,    save_booking("Confirmed"))

    # ---------------- Manual / offline entry ----------------
    #
    # Loads accepted on a paper LR while TMS was unreachable. Keyed in
    # afterwards as an ordinary booking, but stamped with the original paper
    # reference and the time the load was actually taken — so the register
    # does not claim a job was booked at 4pm when the truck left at 7am.

    observeEvent(input$manual_entry, {
      if (!require_perm(session, user()$role, "bookings", "create")) return()
      cl <- scope_branch(get_clients(), user())
      br <- scope_branch(store_get("branches"), user())
      if (!nrow(br)) br <- store_get("branches")
      ct <- booking_cities()

      showModal(modalDialog(
        title = "Manual entry — booking taken offline", size = "l", easyClose = TRUE,
        callout("For loads booked while the system was unavailable",
                "Enter the details from the paper LR book. The booking is created normally and joins the same workflow, but is marked Manual and keeps the original paper reference and time for audit.",
                "warn"),
        div(class = "row g-3 mt-1",
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Paper LR / booking reference"),
                textInput(ns("m_ref"), NULL, placeholder = "NGP/LR/1042", width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Date taken"),
                dateInput(ns("m_date"), NULL, value = Sys.Date(), width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Time taken"),
                textInput(ns("m_time"), NULL, value = "09:00", width = "100%")),

            div(class = "col-md-6",
                tags$label(class = "form-label req", "Customer"),
                selectizeInput(ns("m_client"), NULL, width = "100%",
                               choices = c("Select a customer…" = "",
                                           setNames(cl$client_id, cl$name)))),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Branch"),
                selectInput(ns("m_branch"), NULL, setNames(br$branch_id, br$name),
                            selected = user()$branch_id, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Payment mode"),
                selectInput(ns("m_pay"), NULL, width = "100%",
                            choices = setNames(PAYMENT_MODES, PAYMENT_MODES))),

            # The paper LR carries a PIN far more reliably than a district
            # name, so offline entry gets the same PIN-first pair as the
            # online form.
            div(class = "col-md-2",
                tags$label(class = "form-label req", "Origin PIN"),
                textInput(ns("m_from_pin"), NULL, width = "100%",
                          value = if (!is.na(home_city())) geo_city(home_city())$pincode else "")),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Origin"),
                selectizeInput(ns("m_from"), NULL, ct, width = "100%",
                               selected = home_city(),
                               options = list(maxOptions = 40))),
            div(class = "col-md-2",
                tags$label(class = "form-label req", "Destination PIN"),
                textInput(ns("m_to_pin"), NULL, width = "100%")),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Destination"),
                selectizeInput(ns("m_to"), NULL, c("Search any city…" = "", ct),
                               width = "100%", selected = "",
                               options = list(placeholder = "Type a city, or a PIN",
                                              maxOptions = 40))),
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Material"),
                textInput(ns("m_material"), NULL, width = "100%")),

            div(class = "col-md-3",
                tags$label(class = "form-label req", "Weight (T)"),
                numericInput(ns("m_weight"), NULL, 10, 0.1, 60, 0.1, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Freight (₹)"),
                numericInput(ns("m_freight"), NULL, 15000, 0, step = 100, width = "100%")),
            div(class = "col-md-6",
                tags$label(class = "form-label", "Remarks"),
                textInput(ns("m_remarks"), NULL, width = "100%"))),
        footer = tagList(modalButton("Cancel"),
                         btn_primary(ns("m_save"), "Create booking"))
      ))
    })

    observeEvent(input$m_save, {
      if (!require_perm(session, user()$role, "bookings", "create")) return()

      msgs <- c()
      if (!nzchar(input$m_ref %||% ""))      msgs <- c(msgs, "Paper reference is required.")
      if (!nzchar(input$m_client %||% ""))   msgs <- c(msgs, "Customer is required.")
      if (!nzchar(input$m_material %||% "")) msgs <- c(msgs, "Material is required.")
      w <- as.numeric(input$m_weight %||% 0)
      if (is.na(w) || w <= 0)                msgs <- c(msgs, "Weight must be greater than zero.")
      f <- as.numeric(input$m_freight %||% 0)
      if (is.na(f) || f <= 0)                msgs <- c(msgs, "Freight must be greater than zero.")
      mfp <- geo_pin(input$m_from_pin); mtp <- geo_pin(input$m_to_pin)
      if (is.null(mfp)) msgs <- c(msgs, "Origin PIN code is not a valid Indian pincode.")
      if (is.null(mtp)) msgs <- c(msgs, "Destination PIN code is not a valid Indian pincode.")
      # Same rule as the online form: geography may repeat. A local job inside
      # one PIN code is ordinary freight and the paper LR will show it.
      # A paper reference is the only thing tying this row back to the book it
      # came from, so it has to stay unique.
      if (nzchar(input$m_ref %||% "") &&
          tolower(input$m_ref) %in% tolower(store_get("bookings")$manual_ref)) {
        msgs <- c(msgs, "That paper reference has already been entered.")
      }
      if (length(msgs)) {
        showNotification(HTML(paste(msgs, collapse = "<br/>")), type = "error", duration = 8)
        return()
      }

      cl <- get_clients(); cr <- cl[cl$client_id == input$m_client, ][1, ]
      # Same GST rule as the online form: taken from the client, and reverse
      # charge collects nothing.
      rcm <- identical(cr$gst_mode, "RCM")
      gst <- if (rcm) 0 else round(f * as.numeric(cr$gst_pct) / 100)
      no <- next_id(store_get("bookings")$booking_no, PREFIX$booking)

      store_insert("bookings", list(
        booking_no = no,
        booking_date = as.character(input$m_date %||% Sys.Date()),
        branch_id = input$m_branch, client_id = input$m_client,
        pickup_address = cr$pickup_address, delivery_address = cr$delivery_address,
        origin_city = input$m_from, dest_city = input$m_to,
        origin_pincode = geo_pin(input$m_from_pin)$pincode %||% "",
        dest_pincode   = geo_pin(input$m_to_pin)$pincode %||% "",
        origin_state   = geo_pin(input$m_from_pin)$state %||% "",
        dest_state     = geo_pin(input$m_to_pin)$state %||% "",
        distance_km    = geo_lane_km(geo_lane(input$m_from, input$m_to,
                                             input$m_from_pin, input$m_to_pin)),
        material = input$m_material, weight_t = w, quantity = "",
        packages = "", insurance = "Not insured", declared_value = 0,
        freight = f, gst_mode = cr$gst_mode, gst_pct = cr$gst_pct,
        gst_amount = gst, insurance_amt = 0, total = f + gst,
        remarks = input$m_remarks, booking_user_id = user()$user_id,
        priority = "Normal",
        payment_mode = input$m_pay %||% "Credit", bill_at_branch_id = "",
        entry_mode = "Manual",
        manual_ref = input$m_ref,
        manual_dt = paste(input$m_date, paste0(input$m_time %||% "09:00", ":00")),
        # Offline loads have already been accepted, so they enter Confirmed —
        # a Draft would hide them from dispatch, which is the opposite of what
        # a backlog entry is for.
        status = "Confirmed"
      ))
      audit(user()$user_id, "manual-entry", "bookings",
            paste(no, "from paper", input$m_ref))
      removeModal()
      showNotification(
        sprintf("Manual booking %s created from paper reference %s.", no, input$m_ref),
        type = "message", duration = 7)
    })
  })
}
