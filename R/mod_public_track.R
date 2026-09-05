# ==================================================================
# Public shipment tracking — the one thing this app does without a login.
#
# A consignee is not a user. They have no account, they will never have one,
# and the only question they have is "where is my stuff". Making them phone the
# branch to find out is the status quo this replaces.
#
# What it deliberately does NOT show
# ----------------------------------
# Anyone on the internet can reach this, so it shows the least that answers the
# question: LR number, the two cities, the status, the dates, and the event
# trail. It does not show the customer name, either address, the consignor or
# consignee, the freight, the GST treatment, the invoice, the GSTIN, the
# vehicle registration, or the driver. Those identify people and expose
# commercial terms, and none of them tell a consignee anything about where the
# goods are.
#
# Why it asks for the destination PIN as well
# -------------------------------------------
# LR numbers run in sequence — LR-0001, LR-0002 — so a lookup keyed on the LR
# alone lets anyone walk the whole register and read the movements of every
# customer the company has. Requiring the delivery PIN alongside is the same
# pattern the parcel carriers use: it is something the consignee always knows
# and an enumerator almost never does, and it turns a trivial scrape into a
# 19,238-way guess per LR. It is not authentication and is not pretending to
# be; it is the difference between a door on the latch and a door standing open.
# ==================================================================

public_track_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "text-center mt-4",
    tags$a(
      href = "#", class = "tiny", style = "color:#8FA3BA;font-weight:600;",
      onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                        ns("open")),
      fontawesome::fa("location-crosshairs"),
      " Track a shipment without signing in"
    )
  )
}

public_track_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- reactiveVal(NULL)

    observeEvent(input$open, {
      result(NULL)
      showModal(modalDialog(
        title = "Track your shipment", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-md-6",
                tags$label(class = "form-label req", "LR / consignment number"),
                textInput(ns("lr"), NULL, width = "100%",
                          placeholder = "LR-0002")),
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Delivery PIN code"),
                textInput(ns("pin"), NULL, width = "100%",
                          placeholder = "6 digits, as on your consignment note"))),
        div(class = "mt-2 tiny muted",
            "Both are printed on the consignment note the driver gave the consignor."),
        div(class = "mt-3", uiOutput(ns("out"))),
        footer = tagList(modalButton("Close"),
                         btn_primary(ns("go"), "Track"))
      ))
    })

    observeEvent(input$go, {
      lr  <- toupper(trimws(input$lr %||% ""))
      pin <- trimws(input$pin %||% "")

      if (!nzchar(lr) || !nzchar(pin)) {
        result(list(err = "Enter both the LR number and the delivery PIN code."))
        return()
      }

      cn <- get_consignments()
      # Accept the LR with or without its prefix, and accept the CN number too.
      # A consignee reads what is printed and should not have to know which of
      # the two numbers on the paper the system wants.
      if (!grepl("^(LR|CN)-", lr)) lr <- paste0("LR-", lr)
      hit <- cn[toupper(cn$lr_no) == lr | toupper(cn$cn_no) == lr, , drop = FALSE]

      # One message for "no such LR" and for "wrong PIN". Telling them apart
      # would confirm which LR numbers exist, which is the enumeration this is
      # meant to prevent.
      deny <- list(err = paste(
        "No shipment matches that LR number and delivery PIN.",
        "Check both against your consignment note."))

      if (!nrow(hit)) { result(deny); return() }
      c1 <- hit[1, ]

      bk <- get_bookings()
      b  <- bk[bk$booking_no == c1$booking_no, ]
      expect <- if (nrow(b)) b$dest_pincode[1] else NA_character_
      # Older rows predate the PIN columns; fall back to the destination city's
      # own PIN so tracking still works for them rather than refusing outright.
      if (is.na(expect) || !nzchar(expect %||% "")) {
        g <- geo_city(c1$dest_city)
        expect <- if (is.null(g)) NA_character_ else g$pincode
      }
      if (is.na(expect) || !identical(pin, expect)) { result(deny); return() }

      ev <- store_typed("consignment_events", num = "seq", datetime = "event_dt")
      ev <- ev[ev$cn_no == c1$cn_no, , drop = FALSE]
      ev <- ev[order(ev$seq), , drop = FALSE]
      result(list(cn = c1, events = ev))
    })

    output$out <- renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      if (!is.null(r$err)) return(callout("Not found", r$err, "warn"))

      c1 <- r$cn
      delivered <- identical(c1$status, "Delivered")

      # Late only counts while the goods are still out. A consignment that
      # arrived after its promise is history, not an alarm.
      overdue <- !delivered && !is.na(c1$expected_delivery) &&
        c1$expected_delivery < Sys.Date()

      tagList(
        div(class = "d-flex align-items-center justify-content-between gap-3 flex-wrap mb-3",
            div(h5(class = "mb-1", style = "font-weight:700;color:#12263F;",
                   htmlEscape(c1$lr_no)),
                div(class = "tiny muted",
                    paste(c1$origin_city, "→", c1$dest_city))),
            pill(c1$status, status_colour(c1$status))),

        if (overdue) callout(
          "Running late",
          paste0("This shipment was due on ", fmt_date(c1$expected_delivery),
                 ". Contact the branch that booked it for a revised date."),
          "warn"),

        div(class = "row g-3 mb-3",
            div(class = "col-6 col-md-3", dl_rows("Booked" = fmt_date(
              (function() { b <- get_bookings(); b <- b[b$booking_no == c1$booking_no, ]
                            if (nrow(b)) b$booking_date[1] else NA })()))),
            div(class = "col-6 col-md-3", dl_rows("Dispatched" = fmt_date(c1$dispatch_date))),
            div(class = "col-6 col-md-3", dl_rows(
              "Expected" = if (delivered) "—" else fmt_date(c1$expected_delivery))),
            div(class = "col-6 col-md-3", dl_rows(
              "Delivered" = if (delivered) fmt_date(c1$delivered_date) else "—"))),

        if (nrow(r$events)) tagList(
          tags$label(class = "form-label", "Movement history"),
          timeline(lapply(seq_len(nrow(r$events)), function(i) {
            e <- r$events[i, ]
            list(title = e$event,
                 sub   = if (nzchar(e$detail %||% "")) e$detail else NULL,
                 when  = fmt_dt(e$event_dt),
                 state = "done")
          }))
        ) else div(class = "tiny muted",
                   "No movement recorded yet. The consignment has been booked and is awaiting dispatch."),

        div(class = "tms-card-foot mt-3 tiny muted",
            "Status only. For commercial details — freight, invoicing, proof of delivery — contact the branch that booked the consignment.")
      )
    })
  })
}
