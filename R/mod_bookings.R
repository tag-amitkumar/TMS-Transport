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
    class = "row g-3 booking-form",
    div(
      # 9/3 rather than 8/4. The aside holds a payment select, a charge list
      # and two buttons — none of which needs 390px — while the form was
      # running a third of a screen past the fold. The width is worth more to
      # the column that has forty fields in it.
      class = "col-xl-8 col-xxl-9",
      # One card of sections rather than three stacked cards. Three cards cost
      # roughly 150px in headers, borders and gaps — enough that the Confirm
      # button fell below the fold on a laptop, and a booking screen whose
      # submit you have to scroll for is a booking screen people get wrong.
      card_panel(
        title = "Booking details",
        # The offline route, offered here as well as on the register (CR-26).
        # A clerk who has opened New Booking and then realises the load was
        # taken on paper should not have to go back a screen to say so — that
        # is the moment they are most likely to key it in as an online booking
        # instead, which loses the paper reference and the time the load was
        # actually accepted.
        actions = uiOutput(ns("offline_btn_form"), inline = TRUE),
        # The booking number and who is raising it. Both were fields — one a
        # caption, one a read-only box costing a whole grid row for a value
        # nobody can change. They belong in the header: they say what this
        # record is, not what to type into it.
        sub = tagList(textOutput(ns("f_no"), inline = TRUE),
                      span(class = "tms-footer-sep mx-1", "·"),
                      "Raised by ", textOutput(ns("f_user"), inline = TRUE)),
        div(class = "form-section", "Customer & route"),
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
            ),
        # Serviceability comes from the pin-code route master, which is exactly
        # what the deck says that screen is for: "serviceability checks shown at
        # booking time".
        div(class = "mt-2", uiOutput(ns("route_check"))),

        # Which branch delivers this PIN code, and whether it is inside that
        # branch's ordinary beat (CR-09, CR-11, CR-12). Shown at booking time
        # because an ODA address costs more and takes longer, and the customer
        # has to be told before the load is accepted, not after.
        div(class = "mt-2", uiOutput(ns("delivery_check"))),

        div(class = "form-group-sep"),
        # Consignor before consignee (CR-25) — the order the load actually
        # travels, and the order the consignment note prints.
        div(class = "form-section", "Consignor & consignee"),
        div(class = "row g-3",
            # Mobile is mandatory at both ends and email is not (CR-01, CR-02).
            # A delivery runs on a phone call: the driver ringing ahead from the
            # highway, the branch chasing a missing consignee. Nobody has ever
            # rerouted a truck by email.
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Consignor name"),
                textInput(ns("f_cnor_name"), NULL, width = "100%")),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Consignor mobile"),
                textInput(ns("f_cnor_mob"), NULL, width = "100%",
                          placeholder = "9876543210")),
            div(class = "col-md-4",
                tags$label(class = "form-label", "Consignor email (optional)"),
                textInput(ns("f_cnor_mail"), NULL, width = "100%")),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Consignee name"),
                textInput(ns("f_cnee_name"), NULL, width = "100%")),
            div(class = "col-md-4",
                tags$label(class = "form-label req", "Consignee mobile"),
                textInput(ns("f_cnee_mob"), NULL, width = "100%",
                          placeholder = "9876543210")),
            div(class = "col-md-4",
                tags$label(class = "form-label", "Consignee email (optional)"),
                textInput(ns("f_cnee_mail"), NULL, width = "100%"))),

        div(class = "form-group-sep"),
        div(class = "form-section", "Material & charges"),
        div(class = "row g-3",
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Material details"),
                textInput(ns("f_material"), NULL, width = "100%")),
            div(class = "col-md-6",
                tags$label(class = "form-label", "Remarks"),
                textInput(ns("f_remarks"), NULL, width = "100%")),
            div(class = "col-md-2",
                tags$label(class = "form-label req", "Weight (T)"),
                numericInput(ns("f_weight"), NULL, 10, 0.1, 60, 0.1, width = "100%")),
            div(class = "col-md-2",
                tags$label(class = "form-label", "Quantity"),
                numericInput(ns("f_qty"), NULL, 100, 0, step = 1, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Packages"),
                textInput(ns("f_pkg"), NULL, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Insurance"),
                selectInput(ns("f_ins"), NULL,
                            c("Not insured", "Insured"), width = "100%")),
            # Dispatch mode (CR-10). LTL first and selected, because on a
            # part-load network it is most of the book; defaulting to FTL would
            # mean the clerk unsetting it all day.
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Dispatch"),
                selectInput(ns("f_dispatch"), NULL, width = "100%",
                            choices = setNames(DISPATCH_TYPES,
                                               DISPATCH_LABEL[DISPATCH_TYPES]),
                            selected = "LTL")),
            # Product type drives the Rule 138(14) exemption (CR-17), so it is
            # asked here rather than inferred from the free-text material line.
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Product type"),
                uiOutput(ns("sel_product"))),
            # Freight is computed from the rate card now (CR-07); this box is
            # the exception, not the entry point. Left blank the quoted rate
            # stands, which is what should normally happen — a figure typed
            # over an agreed rate card is a negotiation, and it gets recorded
            # on the booking as one.
            div(class = "col-md-3",
                tags$label(class = "form-label", "Negotiated freight (₹)"),
                textInput(ns("f_freight"), NULL, width = "100%",
                          placeholder = "rate card")),
            ),

        div(class = "form-group-sep"),
        # Invoices (CR-15, CR-18). "Material value" is gone: what the
        # consignment is worth is what its invoices say it is worth, and one
        # consignment routinely carries several. They are captured as rows
        # rather than a single figure because the e-way bill threshold is
        # tested on their sum (CR-19), and a sum needs addends.
        div(class = "form-section", "Invoices"),
        div(class = "row g-2 align-items-end",
            div(class = "col-md-3",
                tags$label(class = "form-label", "Invoice no."),
                textInput(ns("i_no"), NULL, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label", "Invoice value (₹)"),
                textInput(ns("i_val"), NULL, width = "100%")),
            div(class = "col-md-4",
                tags$label(class = "form-label", "E-way bill no."),
                textInput(ns("i_ewb"), NULL, width = "100%",
                          placeholder = "12 digits")),
            div(class = "col-md-2",
                btn_ghost(ns("i_add"), "Add", fontawesome::fa("plus")))),
        div(class = "mt-2", uiOutput(ns("inv_table"))),
        div(class = "mt-2", uiOutput(ns("ewb_check")))
      )
    ),

    # Payment terms, the summary and the actions — all three about money, and
    # all three in the column that had 400px of empty space under it while the
    # form column ran off the bottom of the screen. Splitting the form across
    # two balanced columns is what makes the screen fit without cutting
    # anything from it.
    div(
      class = "col-xl-4 col-xxl-3",
      div(
        class = "form-aside form-aside-tight",
        card_panel(
          title = "Payment terms",
          sub = "Who settles the freight, and when — printed on the consignment note",
          div(class = "row g-3",
              div(class = "col-12",
                  tags$label(class = "form-label req", "Payment mode"),
                  # Opens on nothing (CR-04). It used to default to To Pay,
                  # on the reasoning that whichever term the list opens on is
                  # what a hurried clerk saves, and To Pay at least errs toward
                  # a visible balance. The objection raised was that a default
                  # is still a guess at the commercial terms, and a wrong guess
                  # that looks deliberate is worse than an empty field that
                  # refuses to save. validate_form() now blocks on it.
                  selectInput(ns("f_pay"), NULL, width = "100%",
                              choices = c(setNames("", "Select payment mode…"),
                                          setNames(PAYMENT_MODES,
                                                   PAYMENT_LABEL[PAYMENT_MODES])),
                              selected = "")),
              # Only meaningful for TBB, so it appears only for TBB.
              div(class = "col-12", uiOutput(ns("sel_bill_branch"))),
              # The billing branch's acceptance (CR-06), and the role gate on
              # raising TBB at all (CR-05).
              div(class = "col-12", uiOutput(ns("tbb_panel")))),
          div(class = "mt-2", uiOutput(ns("pay_note")))
        ),

        div(class = "mt-3", card_panel(
          title = "Booking Summary",
          uiOutput(ns("summary")),
          # Side by side, not stacked. Two full-width buttons one above the
          # other cost 40px for no gain — and the taller this column gets, the
          # sooner Confirm goes below the fold, which is the thing the whole
          # rearrangement was for.
          foot = tagList(
            div(class = "tiny muted mb-2",
                "Confirming enables vehicle allocation and consignment note generation."),
            div(class = "d-flex gap-2",
                btn_primary(ns("confirm"), "Confirm Booking", class = "flex-grow-1"),
                btn_ghost(ns("save_draft"), "Save Draft"))
          )
        ))
      )
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
      btn_ghost(ns("manual_entry"), "Offline Entry", fontawesome::fa("pen"))
    })

    # The same action from the New Booking header. Two buttons cannot share one
    # input id — the id has to be unique in the DOM or only one of them ever
    # reports — so it has its own, and both are routed into one handler.
    output$offline_btn_form <- renderUI({
      if (!can(user()$role, "bookings", "create")) return(NULL)
      btn_ghost(ns("manual_entry_form"), "Offline Entry",
                fontawesome::fa("pen"), class = "btn-tms-sm")
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
      manual <- (b$entry_mode %||% "") %in% c("Manual", "Offline")
      no_html <- paste0('<span style="font-weight:600;color:#12275C;">',
                        htmlEscape(b$booking_no), "</span>")
      no_html[manual] <- paste0(
        no_html[manual],
        '<div><span class="pill pill-orange nodot" style="font-size:.6rem;">offline</span> ',
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
                  "Consignment note" = if (nrow(cn)) cn$cn_no[1] else "—")
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
        lr_no     = if (nrow(c1)) c1$lr_no[1] else "",
        cn_no     = if (nrow(c1)) c1$cn_no[1] else b$booking_no,
        date      = fmt_date(b$booking_date),
        consignor = if (nzchar(b$consignor_name %||% "")) b$consignor_name
                    else if (nrow(cr)) cr$name[1] else "—",
        consignor_mobile = b$consignor_mobile %||% "",
        consignee_mobile = b$consignee_mobile %||% "",
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
        manual_note = if ((b$entry_mode %||% "") %in% c("Manual", "Offline"))
          paste0("Booked offline from paper reference ", b$manual_ref,
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
      # Operators are scoped to their branch (CR-13). This used to fall back
      # to every branch when the scope came back empty, which sounds harmless
      # and is not: an empty scope means the user's branch_id matches no
      # branch — bad data — and the response to that was to widen their access
      # to the whole company. Fail closed and say so instead.
      if (!nrow(br)) {
        return(div(class = "field-static text-danger",
                   "Your login is not attached to a branch. Ask an administrator to set one."))
      }
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
          "The consignee pays before the goods are released. This prints large on every copy of the consignment note so the driver cannot miss it.",
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

    # The GST control is gone from the form (CR-03). GST itself is not: it is
    # still computed, still shown in the summary and still printed on the
    # invoice — it is taken from the customer's configured treatment, which is
    # where it was already coming from. What has gone is the per-booking field
    # that implied a clerk could choose it.

    # Product type drives the e-way bill exemption, so the list is the master,
    # not a hardcoded vector.
    output$sel_product <- renderUI({
      pt <- store_get("product_types")
      ch <- if (nrow(pt)) pt$name else "General goods"
      selectInput(ns("f_product"), NULL, choices = ch,
                  selected = "General goods", width = "100%")
    })

    # Which branch delivers, and whether it is ODA (CR-09, CR-11, CR-12).
    delivery <- reactive(resolve_delivery(input$f_to_pin %||% ""))

    output$delivery_check <- renderUI({
      req(nzchar(input$f_to_pin %||% ""))
      d <- delivery()
      if (identical(d$delivery_type, "Unserviceable")) {
        return(callout(
          "No delivering branch for this PIN code",
          paste("Nothing in the branch lists covers it and it cannot be placed",
                "against the PIN code master, so neither a delivery branch nor",
                "a rate can be worked out. Check the PIN, or add it to a",
                "branch's list on the Branches screen."),
          "warn"))
      }
      oda <- identical(d$delivery_type, "ODA")
      callout(
        paste0("Delivered by ", d$branch_name,
               if (oda) " · ODA" else " · Normal delivery"),
        HTML(paste0(
          htmlEscape(d$basis),
          if (oda) paste0(
            '<br/><span class="tiny muted">Outer Delivery Area — a surcharge of ',
            htmlEscape(inr(oda_charge())),
            ' applies and transit runs longer than the quoted lane time. Tell the consignor before accepting the load.</span>')
          else "")),
        if (oda) "warn" else "info")
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
      # Freight now comes off the rate card by weight and destination (CR-07),
      # broken into named lines (CR-08). Weight is captured in tonnes and the
      # card is priced per kg, so the conversion happens once, here.
      d  <- delivery()
      wt <- suppressWarnings(as.numeric(input$f_weight %||% 0))
      if (is.na(wt)) wt <- 0
      bk <- freight_breakup(
        weight_kg     = wt * 1000,
        dest_pincode  = input$f_to_pin %||% "",
        branch_id     = d$branch_id,
        dispatch      = input$f_dispatch %||% "LTL",
        delivery_type = d$delivery_type)

      # A typed figure overrides the card, and is recorded as a negotiated
      # rate rather than quietly replacing it — the breakup keeps showing what
      # the card said so the difference is visible on the booking.
      override <- suppressWarnings(as.numeric(input$f_freight %||% ""))
      negotiated <- !is.na(override) && override > 0
      fr <- if (negotiated) override else bk$freight

      gp  <- gst_pct()
      rcm <- identical(gst_mode(), "RCM")

      # Under reverse charge the carrier collects nothing — the recipient
      # discharges the tax directly. Charging it here would inflate the booking
      # total against an invoice that will (correctly) show zero, and overstate
      # revenue on every RCM job, which is most of the book.
      gst <- if (rcm) 0 else round(fr * gp / 100)
      gst_note <- if (rcm) round(fr * gp / 100) else 0   # shown, never charged

      # Insurance premium is 1.25 per mille of the declared value. What the
      # goods are declared at now comes from the invoices (CR-15) rather than
      # from a multiple of freight.
      #
      # Freight x 30 was a stand-in for a figure nobody had. Now the invoices
      # are on the booking, that figure is known, and the stand-in is wrong in
      # both directions: a dense, cheap load over-insures, and a light,
      # valuable one under-insures — which is the expensive direction, because
      # a claim is settled against the declared value and the customer finds
      # out at the worst possible moment.
      #
      # The multiple survives only for a booking with no invoices, which after
      # CR-18 means an older record rather than a new one.
      inv_total <- sum(as_num(invoices()$invoice_value), na.rm = TRUE)
      dv <- if (!identical(input$f_ins, "Insured")) 0
            else if (inv_total > 0) inv_total
            else round(fr * 30 / 1000) * 1000
      ins <- if (dv > 0) round(dv * 0.000125 / 10) * 10 else 0

      list(freight = fr, gst = gst, gst_pct = gp, rcm = rcm, gst_note = gst_note,
           insurance = ins, declared = dv, total = fr + gst + ins,
           breakup = bk, negotiated = negotiated, card_freight = bk$freight,
           delivery = d)
    })

    output$summary <- renderUI({
      ch <- charges(); r <- client_row()
      # The GST row's label carries the rate and mode, so it is built as a
      # named list and splatted rather than written inline. Under reverse charge
      # the amount is shown for information with a nil collected, because the
      # customer still needs to see what they will be discharging themselves.
      # One row per charge line rather than a single freight figure (CR-08).
      # freight_breakup() decides the lines and their order so that this card,
      # the consignment note and the invoice cannot disagree about them.
      charge_rows <- list(); charge_names <- character(0)
      if (ch$negotiated) {
        charge_rows <- c(charge_rows, list(inr(ch$freight)))
        charge_names <- c(charge_names, "Freight — negotiated")
        charge_rows <- c(charge_rows, list(
          HTML(paste0('<span class="muted">', inr(ch$card_freight), "</span>"))))
        charge_names <- c(charge_names, "Rate card would have quoted")
      } else if (length(ch$breakup$lines)) {
        for (l in ch$breakup$lines) {
          charge_rows  <- c(charge_rows, list(inr(l$amount)))
          charge_names <- c(charge_names, l$label)
        }
      } else {
        charge_rows  <- c(charge_rows, list(
          HTML('<span class="muted">No rate — enter a negotiated figure</span>')))
        charge_names <- c(charge_names, "Freight")
      }
      charge_rows <- c(charge_rows, list(
        # The amount on its own line, the reason beneath it. Run together as
        # one string it wrapped into three ragged lines in the narrowed aside,
        # which read as a layout fault rather than as a figure with a note.
        if (ch$rcm) HTML(paste0(
          inr(ch$gst_note),
          '<span class="tiny muted d-block" style="font-weight:400;">',
          "payable by recipient</span>")) else inr(ch$gst),
        inr(ch$insurance)))
      charge_names <- c(charge_names,
                        sprintf("GST (%s%% · %s)", ch$gst_pct, gst_mode()),
                        "Insurance")
      names(charge_rows) <- charge_names
      tagList(
        do.call(dl_rows, charge_rows),
        tags$hr(class = "soft"),
        div(class = "d-flex justify-content-between align-items-center",
            span(style = "font-weight:650;", "Total"),
            span(style = "font-weight:700;font-size:1.05rem;color:#12275C;", inr(ch$total))),
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
    # ---------------- invoices (CR-15, CR-18) ----------------
    #
    # Held in the module until the booking is saved, then written as rows of
    # booking_invoices. Kept out of the store while the form is open so an
    # abandoned booking does not leave orphan invoice rows behind.
    inv_rows <- reactiveVal(tibble::tibble(
      invoice_no = character(), invoice_value = character(), ewb_no = character()))
    invoices <- reactive(inv_rows())

    observeEvent(input$i_add, {
      no  <- trimws(input$i_no  %||% "")
      val <- trimws(input$i_val %||% "")
      ewb <- trimws(input$i_ewb %||% "")
      v   <- suppressWarnings(as.numeric(val))

      if (!nzchar(no)) {
        showNotification("Invoice number is required.", type = "error"); return()
      }
      if (is.na(v) || v <= 0) {
        showNotification("Invoice value must be a number greater than zero.",
                         type = "error"); return()
      }
      cur <- inv_rows()
      if (no %in% cur$invoice_no) {
        showNotification(paste0("Invoice ", no, " is already on this booking."),
                         type = "error"); return()
      }
      # An e-way bill number is 12 digits. Checked when one is supplied, so a
      # transposed digit is caught here rather than at the checkpost.
      if (nzchar(ewb) && !grepl("^[0-9]{12}$", gsub("[ -]", "", ewb))) {
        showNotification("E-way bill number must be 12 digits.", type = "error"); return()
      }

      inv_rows(dplyr::bind_rows(cur, tibble::tibble(
        invoice_no = no, invoice_value = as.character(round(v)),
        ewb_no = gsub("[ -]", "", ewb))))
      updateTextInput(session, "i_no",  value = "")
      updateTextInput(session, "i_val", value = "")
      updateTextInput(session, "i_ewb", value = "")
    })

    observeEvent(input$i_del, {
      cur <- inv_rows()
      k <- input$i_del
      if (is.null(k) || !nzchar(k)) return()
      inv_rows(cur[cur$invoice_no != k, , drop = FALSE])
    })

    output$inv_table <- renderUI({
      iv <- invoices()
      if (!nrow(iv)) {
        return(div(class = "tiny muted", "No invoices added yet."))
      }
      rows <- lapply(seq_len(nrow(iv)), function(i) {
        tags$tr(
          tags$td(class = "mono", iv$invoice_no[i]),
          tags$td(class = "text-end", inr(as_num(iv$invoice_value[i]))),
          tags$td(class = "mono", if (nzchar(iv$ewb_no[i])) iv$ewb_no[i] else "—"),
          tags$td(class = "text-end",
                  tags$button(
                    class = "btn btn-sm btn-link p-0 text-danger",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', '%s', {priority:'event'})",
                      ns("i_del"), iv$invoice_no[i]),
                    "Remove")))
      })
      total <- sum(as_num(iv$invoice_value), na.rm = TRUE)
      div(class = "tms-table",
        tags$table(class = "table table-sm mb-0",
          tags$thead(tags$tr(
            tags$th("INVOICE"), tags$th(class = "text-end", "VALUE"),
            tags$th("E-WAY BILL"), tags$th(""))),
          tags$tbody(rows),
          tags$tfoot(tags$tr(
            tags$th("Total"), tags$th(class = "text-end", inr(total)),
            tags$th(colspan = 2, "")))))
    })

    # Whether an e-way bill is needed, and why (CR-16, CR-17, CR-19). Shown
    # while the form is open rather than only on save, so the clerk can ask the
    # consignor for the number before the driver is waiting at the counter.
    output$ewb_check <- renderUI({
      iv <- invoices()
      if (!nrow(iv)) return(NULL)
      e <- ewb_required(sum(as_num(iv$invoice_value), na.rm = TRUE),
                        geo_pin(input$f_from_pin)$state %||% "",
                        geo_pin(input$f_to_pin)$state %||% "",
                        input$f_product %||% "")
      missing <- sum(!nzchar(trimws(iv$ewb_no)))
      if (e$exempt) return(callout("E-way bill not required", e$reason, "info"))
      if (!e$required) return(callout("E-way bill not required", e$reason, "info"))
      callout(
        if (missing) sprintf("E-way bill required — %d invoice%s still without one",
                             missing, if (missing == 1) "" else "s")
        else "E-way bill required — all invoices carry one",
        e$reason, if (missing) "warn" else "ok")
    })

    # ---------------- TBB reference (CR-05, CR-06) ----------------
    #
    # A TBB load is billed by another branch, so that branch has to accept it
    # before the consignment moves on its account. The reference is that
    # acceptance. Requesting it is a deliberate act and the number comes back
    # from the billing branch, so it is not generated silently on save.
    tbb_ref <- reactiveVal("")

    observeEvent(input$f_pay, tbb_ref(""))
    observeEvent(input$f_bill_branch, tbb_ref(""))

    observeEvent(input$tbb_request, {
      if (!(user()$role %in% OPERATOR_ROLES)) {
        showNotification("Only a booking operator can raise a TBB request.",
                         type = "error"); return()
      }
      br <- input$f_bill_branch %||% ""
      if (!nzchar(br)) {
        showNotification("Choose the branch that will raise the bill first.",
                         type = "error"); return()
      }
      bx <- store_get("branches")
      code <- bx$code[match(br, bx$branch_id)] %||% br
      ref <- sprintf("TBB-%s-%s", gsub("^BR-", "", code),
                     format(Sys.time(), "%y%m%d%H%M%S"))
      tbb_ref(ref)
      audit(user()$user_id, "tbb-reference", "bookings", ref)
      showNotification(paste0("TBB reference ", ref, " received from ",
                              bx$name[match(br, bx$branch_id)] %||% br, "."),
                       type = "message", duration = 6)
    })

    output$tbb_panel <- renderUI({
      if (!identical(input$f_pay %||% "", "TBB")) return(NULL)
      if (!(user()$role %in% OPERATOR_ROLES)) {
        return(callout("TBB is not available to your role",
                       paste("To Be Billed is raised by a booking operator.",
                             "Ask the counter to book this one."), "warn"))
      }
      ref <- tbb_ref()
      if (nzchar(ref)) {
        return(callout("TBB reference received",
                       HTML(paste0('<span class="mono">', htmlEscape(ref),
                                   "</span> — this booking can be confirmed.")),
                       "ok"))
      }
      tagList(
        callout("TBB reference not yet received",
                paste("The billing branch has to accept this load before it can",
                      "be confirmed on their account."), "warn"),
        div(class = "mt-2",
            btn_primary(ns("tbb_request"), "Request TBB reference",
                        fontawesome::fa("paper-plane"))))
    })

    validate_form <- function() {
      msgs <- c()
      if (!nzchar(input$f_client %||% ""))   msgs <- c(msgs, "Customer is required.")
      if (!nzchar(input$f_pickup %||% ""))   msgs <- c(msgs, "Pickup address is required.")
      if (!nzchar(input$f_drop %||% ""))     msgs <- c(msgs, "Delivery address is required.")
      if (!nzchar(input$f_material %||% "")) msgs <- c(msgs, "Material details are required.")
      w <- as.numeric(input$f_weight %||% 0)
      if (is.na(w) || w <= 0)                msgs <- c(msgs, "Weight must be greater than zero.")
      # Freight is quoted from the rate card, so the check is that a figure
      # was arrived at at all — an unrated destination with no negotiated
      # figure typed leaves nothing to bill.
      if (as.numeric(charges()$freight %||% 0) <= 0) {
        msgs <- c(msgs, paste("No freight could be quoted. Either the destination has",
                              "no rate card entry, or the weight is zero — enter a",
                              "negotiated figure if the rate is agreed separately."))
      }
      if (!nzchar(input$f_pay %||% ""))      msgs <- c(msgs, "Payment mode is required.")
      if (!nzchar(input$f_product %||% ""))  msgs <- c(msgs, "Product type is required.")

      # Mobile mandatory at both ends, email optional (CR-01, CR-02, CR-27).
      if (!nzchar(input$f_cnor_name %||% "")) msgs <- c(msgs, "Consignor name is required.")
      if (!nzchar(input$f_cnee_name %||% "")) msgs <- c(msgs, "Consignee name is required.")
      for (f in list(list("f_cnor_mob", "Consignor"), list("f_cnee_mob", "Consignee"))) {
        v <- trimws(input[[f[[1]]]] %||% "")
        if (!nzchar(v)) {
          msgs <- c(msgs, paste(f[[2]], "mobile number is required."))
        } else if (!valid_mobile(v)) {
          msgs <- c(msgs, paste(f[[2]],
                                "mobile number is not a valid 10-digit Indian mobile."))
        }
      }
      for (f in list(list("f_cnor_mail", "Consignor"), list("f_cnee_mail", "Consignee"))) {
        v <- trimws(input[[f[[1]]]] %||% "")
        # Optional, but a typo in an address that was supplied is still a typo.
        if (nzchar(v) && !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[A-Za-z]{2,}$", v)) {
          msgs <- c(msgs, paste(f[[2]], "email is not a valid address."))
        }
      }

      # An address nothing delivers to cannot be booked (CR-12).
      d <- delivery()
      if (nzchar(input$f_to_pin %||% "") &&
          identical(d$delivery_type, "Unserviceable")) {
        msgs <- c(msgs, "No branch delivers this destination PIN code.")
      }

      # E-way bill, tested on the summed invoice value (CR-16, CR-17, CR-19).
      inv <- invoices()
      if (!nrow(inv)) {
        msgs <- c(msgs, "At least one invoice is required.")
      } else {
        e <- ewb_required(sum(as_num(inv$invoice_value), na.rm = TRUE),
                          geo_pin(input$f_from_pin)$state %||% "",
                          geo_pin(input$f_to_pin)$state %||% "",
                          input$f_product %||% "")
        if (e$required && any(!nzchar(trimws(inv$ewb_no)))) {
          msgs <- c(msgs, paste0("E-way bill number is required on every invoice. ",
                                 e$reason, "."))
        }
      }
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
      if (identical(input$f_pay %||% "", "TBB")) {
        if (!nzchar(input$f_bill_branch %||% "")) {
          msgs <- c(msgs, "TBB requires the branch that will raise the bill.")
        }
        # TBB is offered on request and only to an operator (CR-05). The
        # select is already filtered by role, but a filtered dropdown is a
        # convenience, not a control — the value still arrives from the
        # browser and is still checked here.
        if (!(user()$role %in% OPERATOR_ROLES)) {
          msgs <- c(msgs, "TBB can only be raised by a booking operator.")
        }
        # The billing branch has to acknowledge before the load moves on its
        # account (CR-06). Until that reference comes back the booking can be
        # held as a draft, but it cannot be confirmed.
        if (!nzchar(tbb_ref() %||% "")) {
          msgs <- c(msgs, paste("TBB reference not yet received from the billing",
                                "branch. Request it, then confirm — the booking",
                                "can be saved as a draft meanwhile."))
        }
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
      m_del <- resolve_delivery(input$m_to_pin %||% "")

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
        payment_mode = input$f_pay,
        bill_at_branch_id = if (identical(input$f_pay %||% "", "TBB"))
                              (input$f_bill_branch %||% "") else "",
        # The billing branch's acceptance is stored on the booking, not just
        # shown — it is the evidence that the load was accepted on their
        # account, and accounts will be asked for it later (CR-06).
        tbb_ref = if (identical(input$f_pay %||% "", "TBB")) tbb_ref() else "",
        # Consignor and consignee, in that order (CR-01, CR-02, CR-25).
        consignor_name = input$f_cnor_name, consignor_mobile = input$f_cnor_mob,
        consignor_email = input$f_cnor_mail %||% "",
        consignee_name = input$f_cnee_name, consignee_mobile = input$f_cnee_mob,
        consignee_email = input$f_cnee_mail %||% "",
        dispatch_type = input$f_dispatch %||% "LTL",
        product_type  = input$f_product %||% "",
        # Serviceability is frozen onto the booking rather than recomputed
        # later. A branch pincode list is editable, and a booking quoted as
        # Normal must not silently become ODA — or stop being ODA after the
        # surcharge was charged — because somebody edited the master next week.
        delivery_branch_id = ch$delivery$branch_id,
        delivery_type      = ch$delivery$delivery_type,
        oda_km             = ch$delivery$km %||% "",
        rate_per_kg        = ch$breakup$rate,
        freight_base       = ch$breakup$base,
        freight_min_topup  = ch$breakup$minimum_topup,
        oda_charge         = ch$breakup$oda,
        freight_negotiated = if (ch$negotiated) "TRUE" else "FALSE",
        entry_mode = "Online", manual_ref = "", manual_dt = "",
        status = status
      ))

      # Invoice rows are written against the booking number, which only exists
      # once the booking is saved (CR-15, CR-18).
      iv <- invoices()
      for (i in seq_len(nrow(iv))) {
        store_insert("booking_invoices", list(
          booking_no = no, invoice_no = iv$invoice_no[i],
          invoice_value = iv$invoice_value[i], ewb_no = iv$ewb_no[i]))
      }
      inv_rows(iv[0, , drop = FALSE])
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

    # Both Offline Entry buttons open the same modal. Kept as one function
    # rather than two observers with the same body: a second copy is a second
    # thing to forget when the form changes, and this form has changed a lot.
    show_offline_entry <- function() {
      if (!require_perm(session, user()$role, "bookings", "create")) return()
      cl <- scope_branch(get_clients(), user())
      br <- scope_branch(store_get("branches"), user())
      if (!nrow(br)) br <- store_get("branches")
      ct <- booking_cities()

      showModal(modalDialog(
        title = "Offline entry — booking taken on paper", size = "l", easyClose = TRUE,
        callout("For loads booked while the system was unavailable",
                "Enter the details from the paper consignment note book. The booking is created normally and joins the same workflow, but is marked Offline and keeps the original paper reference and time for audit.",
                "warn"),
        div(class = "row g-3 mt-1",
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Paper reference"),
                textInput(ns("m_ref"), NULL, placeholder = "NGP/CN/1042", width = "100%")),
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
                            choices = c(setNames("", "Select payment mode…"),
                                        setNames(PAYMENT_MODES, PAYMENT_MODES)),
                            selected = "")),

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
                tags$label(class = "form-label req", "Consignor mobile"),
                textInput(ns("m_cnor_mob"), NULL, width = "100%",
                          placeholder = "9876543210")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Consignee mobile"),
                textInput(ns("m_cnee_mob"), NULL, width = "100%",
                          placeholder = "9876543210")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Consignor name"),
                textInput(ns("m_cnor_name"), NULL, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Consignee name"),
                textInput(ns("m_cnee_name"), NULL, width = "100%")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Dispatch"),
                selectInput(ns("m_dispatch"), NULL, width = "100%",
                            choices = setNames(DISPATCH_TYPES,
                                               DISPATCH_LABEL[DISPATCH_TYPES]),
                            selected = "LTL")),
            div(class = "col-md-3",
                tags$label(class = "form-label req", "Product type"),
                uiOutput(ns("m_sel_product"))),

            # Invoices, and therefore the e-way bill test (CR-15 to CR-19).
            #
            # This form used to skip them entirely, which meant a load keyed in
            # offline was never checked against the e-way bill thresholds at
            # all — and that made the whole rule optional, since choosing this
            # form was enough to avoid it. Whether a bill was required is a
            # fact about the load that moved, not about how the booking
            # reached the system.
            div(class = "col-12", tags$hr(class = "soft")),
            div(class = "col-12",
                tags$label(class = "form-label req",
                           "Invoices on the paper docket")),
            div(class = "col-md-3",
                textInput(ns("mi_no"), NULL, placeholder = "Invoice no.", width = "100%")),
            div(class = "col-md-3",
                textInput(ns("mi_val"), NULL, placeholder = "Value (₹)", width = "100%")),
            div(class = "col-md-4",
                textInput(ns("mi_ewb"), NULL, placeholder = "E-way bill no. (12 digits)",
                          width = "100%")),
            div(class = "col-md-2",
                btn_ghost(ns("mi_add"), "Add", fontawesome::fa("plus"))),
            div(class = "col-12", uiOutput(ns("m_inv_table"))),
            div(class = "col-12", uiOutput(ns("m_ewb_check"))),

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
    }

    observeEvent(input$manual_entry,      show_offline_entry())
    observeEvent(input$manual_entry_form, show_offline_entry())

    # ---------------- offline entry: invoices and the e-way bill gate ----------------
    m_inv <- reactiveVal(tibble::tibble(
      invoice_no = character(), invoice_value = character(), ewb_no = character()))

    output$m_sel_product <- renderUI({
      pt <- store_get("product_types")
      selectInput(ns("m_product"), NULL,
                  choices = if (nrow(pt)) pt$name else "General goods",
                  selected = "General goods", width = "100%")
    })

    observeEvent(input$mi_add, {
      no  <- trimws(input$mi_no  %||% "")
      val <- trimws(input$mi_val %||% "")
      ewb <- gsub("[ -]", "", trimws(input$mi_ewb %||% ""))
      v   <- suppressWarnings(as.numeric(val))
      if (!nzchar(no)) { showNotification("Invoice number is required.", type = "error"); return() }
      if (is.na(v) || v <= 0) {
        showNotification("Invoice value must be a number greater than zero.", type = "error"); return() }
      cur <- m_inv()
      if (no %in% cur$invoice_no) {
        showNotification(paste0("Invoice ", no, " is already on this booking."), type = "error"); return() }
      if (nzchar(ewb) && !grepl("^[0-9]{12}$", ewb)) {
        showNotification("E-way bill number must be 12 digits.", type = "error"); return() }
      m_inv(dplyr::bind_rows(cur, tibble::tibble(
        invoice_no = no, invoice_value = as.character(round(v)), ewb_no = ewb)))
      updateTextInput(session, "mi_no", value = "")
      updateTextInput(session, "mi_val", value = "")
      updateTextInput(session, "mi_ewb", value = "")
    })

    observeEvent(input$mi_del, {
      cur <- m_inv(); k <- input$mi_del
      if (is.null(k) || !nzchar(k)) return()
      m_inv(cur[cur$invoice_no != k, , drop = FALSE])
    })

    output$m_inv_table <- renderUI({
      iv <- m_inv()
      if (!nrow(iv)) return(div(class = "tiny muted", "No invoices added yet."))
      rows <- lapply(seq_len(nrow(iv)), function(i) {
        tags$tr(
          tags$td(class = "mono", iv$invoice_no[i]),
          tags$td(class = "text-end", inr(as_num(iv$invoice_value[i]))),
          tags$td(class = "mono", if (nzchar(iv$ewb_no[i])) iv$ewb_no[i] else "—"),
          tags$td(class = "text-end",
                  tags$button(class = "btn btn-sm btn-link p-0 text-danger",
                    onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                      ns("mi_del"), iv$invoice_no[i]), "Remove")))
      })
      div(class = "tms-table",
        tags$table(class = "table table-sm mb-0",
          tags$thead(tags$tr(tags$th("INVOICE"), tags$th(class = "text-end", "VALUE"),
                             tags$th("E-WAY BILL"), tags$th(""))),
          tags$tbody(rows),
          tags$tfoot(tags$tr(tags$th("Total"),
            tags$th(class = "text-end", inr(sum(as_num(iv$invoice_value), na.rm = TRUE))),
            tags$th(colspan = 2, "")))))
    })

    m_ewb <- reactive({
      iv <- m_inv()
      ewb_required(sum(as_num(iv$invoice_value), na.rm = TRUE),
                   geo_pin(input$m_from_pin)$state %||% "",
                   geo_pin(input$m_to_pin)$state %||% "",
                   input$m_product %||% "")
    })

    output$m_ewb_check <- renderUI({
      iv <- m_inv()
      if (!nrow(iv)) return(NULL)
      e <- m_ewb(); missing <- sum(!nzchar(trimws(iv$ewb_no)))
      if (e$exempt || !e$required) return(callout("E-way bill not required", e$reason, "info"))
      callout(
        if (missing) sprintf("E-way bill required — %d invoice%s still without one",
                             missing, if (missing == 1) "" else "s")
        else "E-way bill required — all invoices carry one",
        e$reason, if (missing) "warn" else "ok")
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
      # Mobile is mandatory whichever channel the load came in through
      # (CR-27). A paper docket that reached the counter without a phone
      # number is a load nobody can chase, and keying it in does not fix that
      # — so it is caught here, while the paper is still on the desk.
      for (fld in list(list("m_cnor_mob", "Consignor"), list("m_cnee_mob", "Consignee"))) {
        v <- gsub("[ -]", "", trimws(input[[fld[[1]]]] %||% ""))
        if (!nzchar(v)) {
          msgs <- c(msgs, paste(fld[[2]], "mobile number is required."))
        } else if (!valid_mobile(v)) {
          msgs <- c(msgs, paste(fld[[2]], "mobile number is not a valid Indian mobile."))
        }
      }
      if (!nzchar(input$m_pay %||% ""))       msgs <- c(msgs, "Payment mode is required.")
      if (!nzchar(input$m_product %||% ""))   msgs <- c(msgs, "Product type is required.")
      if (!nzchar(input$m_cnor_name %||% "")) msgs <- c(msgs, "Consignor name is required.")
      if (!nzchar(input$m_cnee_name %||% "")) msgs <- c(msgs, "Consignee name is required.")

      # The same e-way bill test as the online form (CR-16, CR-17, CR-19).
      # A load that needed a bill needed one whether the booking was typed at
      # the counter or copied off a docket afterwards, and leaving this out
      # made the rule avoidable by choosing this screen.
      miv <- m_inv()
      if (!nrow(miv)) {
        msgs <- c(msgs, "At least one invoice from the paper docket is required.")
      } else {
        e <- m_ewb()
        if (e$required && any(!nzchar(trimws(miv$ewb_no)))) {
          msgs <- c(msgs, paste0("E-way bill number is required on every invoice. ",
                                 e$reason, "."))
        }
      }

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
        consignor_name   = input$m_cnor_name,
        consignee_name   = input$m_cnee_name,
        consignor_mobile = gsub("[ -]", "", trimws(input$m_cnor_mob %||% "")),
        consignee_mobile = gsub("[ -]", "", trimws(input$m_cnee_mob %||% "")),
        dispatch_type    = input$m_dispatch %||% "LTL",
        product_type     = input$m_product %||% "",
        # Serviceability is resolved and frozen here too, so an offline
        # booking carries the same delivery branch and ODA flag as any other.
        delivery_branch_id = m_del$branch_id,
        delivery_type      = m_del$delivery_type,
        oda_km             = m_del$km %||% "",
        entry_mode = "Offline",
        manual_ref = input$m_ref,
        manual_dt = paste(input$m_date, paste0(input$m_time %||% "09:00", ":00")),
        # Offline loads have already been accepted, so they enter Confirmed —
        # a Draft would hide them from dispatch, which is the opposite of what
        # a backlog entry is for.
        status = "Confirmed"
      ))
      miv <- m_inv()
      for (i in seq_len(nrow(miv))) {
        store_insert("booking_invoices", list(
          booking_no = no, invoice_no = miv$invoice_no[i],
          invoice_value = miv$invoice_value[i], ewb_no = miv$ewb_no[i]))
      }
      m_inv(miv[0, , drop = FALSE])

      audit(user()$user_id, "offline-entry", "bookings",
            paste(no, "from paper", input$m_ref))
      removeModal()
      showNotification(
        sprintf("Offline booking %s created from paper reference %s.", no, input$m_ref),
        type = "message", duration = 7)
    })
  })
}
