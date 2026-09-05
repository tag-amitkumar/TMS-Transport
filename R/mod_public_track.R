# ==================================================================
# Public shipment tracking — the one thing this app does without a login.
#
# A consignee is not a user. They have no account, they will never have one,
# and the only question they have is "where is my stuff". Making them phone the
# branch to find out is the status quo this replaces.
#
# Two surfaces, one lookup:
#
#   public_track_ui()  the box on the login page, inputs visible on arrival —
#                      a consignee should not have to find a link first
#   track_button_ui()  a button in the app's top bar, so staff can answer the
#                      same question from any screen without leaving it
#
# What it deliberately does NOT show
# ----------------------------------
# Anyone on the internet can reach the login page, so it shows the least that
# answers the question: LR number, the two cities, the status, the dates, and
# the event trail. It does not show the customer name, either address, the
# consignor or consignee, the freight, the GST treatment, the invoice, the
# GSTIN, the vehicle registration, or the driver. Those identify people and
# expose commercial terms, and none of them tell a consignee anything about
# where the goods are.
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

# ------------------------------------------------------------------
# The lookup, shared by both surfaces
# ------------------------------------------------------------------

#' Resolve an LR and delivery PIN to a consignment and its event trail.
#'
#' Returns `list(err = "...")` on any failure, or `list(cn =, events =)`.
track_lookup <- function(lr_in, pin_in) {
  lr  <- toupper(trimws(lr_in %||% ""))
  pin <- trimws(pin_in %||% "")

  if (!nzchar(lr) || !nzchar(pin)) {
    return(list(err = "Enter both the LR number and the delivery PIN code."))
  }

  cn <- get_consignments()
  # Accept the LR with or without its prefix, and accept the CN number too. A
  # consignee reads what is printed and should not have to know which of the
  # two numbers on the paper the system wants.
  if (!grepl("^(LR|CN)-", lr)) lr <- paste0("LR-", lr)
  hit <- cn[toupper(cn$lr_no) == lr | toupper(cn$cn_no) == lr, , drop = FALSE]

  # One message for "no such LR" and for "wrong PIN". Telling them apart would
  # confirm which LR numbers exist, which is the enumeration this prevents.
  deny <- list(err = paste(
    "No shipment matches that LR number and delivery PIN.",
    "Check both against your consignment note."))

  if (!nrow(hit)) return(deny)
  c1 <- hit[1, ]

  bk <- get_bookings()
  b  <- bk[bk$booking_no == c1$booking_no, ]
  expect <- if (nrow(b)) b$dest_pincode[1] else NA_character_
  # Older rows predate the PIN columns; fall back to the destination city's own
  # PIN so tracking still works for them rather than refusing outright.
  if (is.na(expect) || !nzchar(expect %||% "")) {
    g <- geo_city(c1$dest_city)
    expect <- if (is.null(g)) NA_character_ else g$pincode
  }
  if (is.na(expect) || !identical(pin, expect)) return(deny)

  ev <- store_typed("consignment_events", num = "seq", datetime = "event_dt")
  ev <- ev[ev$cn_no == c1$cn_no, , drop = FALSE]
  ev <- ev[order(ev$seq), , drop = FALSE]
  list(cn = c1, events = ev)
}

#' Render a lookup result. `compact` drops the date grid for the narrow box.
track_result_ui <- function(r, compact = FALSE) {
  if (is.null(r)) return(NULL)
  if (!is.null(r$err)) return(callout("Not found", r$err, "warn"))

  c1 <- r$cn
  delivered <- identical(c1$status, "Delivered")

  # Late only counts while the goods are still out. A consignment that arrived
  # after its promise is history, not an alarm.
  overdue <- !delivered && !is.na(c1$expected_delivery) &&
    c1$expected_delivery < Sys.Date()

  booked <- {
    b <- get_bookings(); b <- b[b$booking_no == c1$booking_no, ]
    if (nrow(b)) b$booking_date[1] else NA
  }

  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 flex-wrap mb-3",
        div(h5(class = "mb-1", style = "font-weight:700;color:#12275C;",
               htmlEscape(c1$lr_no)),
            div(class = "tiny muted", paste(c1$origin_city, "→", c1$dest_city))),
        pill(c1$status, status_colour(c1$status))),

    if (overdue) callout(
      "Running late",
      paste0("This shipment was due on ", fmt_date(c1$expected_delivery),
             ". Contact the branch that booked it for a revised date."),
      "warn"),

    div(class = if (compact) "row g-2 mb-3" else "row g-3 mb-3",
        div(class = if (compact) "col-6" else "col-6 col-md-3",
            dl_rows("Booked" = fmt_date(booked))),
        div(class = if (compact) "col-6" else "col-6 col-md-3",
            dl_rows("Dispatched" = fmt_date(c1$dispatch_date))),
        div(class = if (compact) "col-6" else "col-6 col-md-3",
            dl_rows("Expected" = if (delivered) "—" else fmt_date(c1$expected_delivery))),
        div(class = if (compact) "col-6" else "col-6 col-md-3",
            dl_rows("Delivered" = if (delivered) fmt_date(c1$delivered_date) else "—"))),

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
}

# ------------------------------------------------------------------
# Surface 1 — the login page box
# ------------------------------------------------------------------

#' The tracking form for the login screen.
#'
#' The fields are on the page, not behind a link. A consignee arriving here has
#' one question, and making them notice a small link before they can ask it is
#' the wrong order.
#'
#' Returns just the form: the card, the heading and the tab that selects it all
#' belong to the hero in mod_auth.R, because tracking and signing in share one
#' card there and neither half should own its chrome.
public_track_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2(class = "hero-card-title",
       tags$strong("Track"), " your shipment"),
    p(class = "hero-card-sub", "No account needed."),

    div(class = "mt-3",
        tags$label(class = "form-label req", "LR / consignment number"),
        textInput(ns("lr"), NULL, width = "100%", placeholder = "e.g. LR-0002")),
    div(
        tags$label(class = "form-label req", "Delivery PIN code"),
        textInput(ns("pin"), NULL, width = "100%", placeholder = "e.g. 440016")),

    actionButton(ns("go"), "Track shipment", class = "btn-tms-primary w-100 hero-cta"),

    div(class = "tiny muted mt-2",
        "Both are printed on the consignment note the driver gave the consignor."),

    uiOutput(ns("out"))
  )
}

# ------------------------------------------------------------------
# Surface 2 — the in-app top bar
# ------------------------------------------------------------------

#' A Track button for the shell's top bar.
#'
#' Deliberately reuses the module's own input rather than routing through the
#' app: one server, one lookup, two ways in.
track_button_ui <- function(id = "track") {
  ns <- NS(id)
  tags$button(
    class = "tms-trackbtn", type = "button", title = "Track a shipment by LR number",
    onclick = sprintf("Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                      ns("open")),
    fontawesome::fa("location-crosshairs"),
    span("Track Shipment")
  )
}

# ------------------------------------------------------------------

public_track_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    box_result   <- reactiveVal(NULL)   # login-page box
    modal_result <- reactiveVal(NULL)   # top-bar dialog

    # ---- login page box ----
    observeEvent(input$go, box_result(track_lookup(input$lr, input$pin)))
    output$out <- renderUI({
      r <- box_result()
      if (is.null(r)) return(NULL)
      div(class = "login-track-out", track_result_ui(r, compact = TRUE))
    })
    # Same reason as the sign-in error: whichever pane is behind the other tab
    # is inside a display:none element, and a suspended output comes back blank.
    outputOptions(output, "out", suspendWhenHidden = FALSE)
    # ---- top bar dialog ----
    observeEvent(input$open, {
      modal_result(NULL)
      showModal(modalDialog(
        title = "Track a shipment", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-md-6",
                tags$label(class = "form-label req", "LR / consignment number"),
                textInput(ns("m_lr"), NULL, width = "100%", placeholder = "e.g. LR-0002")),
            div(class = "col-md-6",
                tags$label(class = "form-label req", "Delivery PIN code"),
                textInput(ns("m_pin"), NULL, width = "100%",
                          placeholder = "e.g. 440016"))),
        div(class = "mt-3", uiOutput(ns("m_out"))),
        footer = tagList(modalButton("Close"), btn_primary(ns("m_go"), "Track"))
      ))
    })

    observeEvent(input$m_go, modal_result(track_lookup(input$m_lr, input$m_pin)))
    output$m_out <- renderUI(track_result_ui(modal_result()))
  })
}
