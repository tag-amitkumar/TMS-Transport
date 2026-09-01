# ==================================================================
# Shipment tracking.
#
# The one screen an external party reaches. The deck lets a customer search by
# consignment number, registered mobile, or a tracking ID shared over SMS or
# WhatsApp — any one of the three.
#
# Lookup is deliberately narrow: it returns a single consignment and nothing
# else, and it never lists. A mobile number that matches several consignments
# returns the most recent one only, so the form cannot be used to enumerate a
# customer's shipment history.
# ==================================================================

tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card_panel(
      title = "Track a Shipment",
      div(class = "row g-3 align-items-end",
          div(class = "col-md-4",
              tags$label(class = "form-label req", "Consignment No."),
              textInput(ns("q_cn"), NULL, placeholder = "CN-90838", width = "100%")),
          div(class = "col-md-3",
              tags$label(class = "form-label", "Mobile Number"),
              textInput(ns("q_mobile"), NULL, placeholder = "+91 98230 44112", width = "100%")),
          div(class = "col-md-3",
              tags$label(class = "form-label", "Tracking ID"),
              textInput(ns("q_track"), NULL, placeholder = "TRK-8834-LX", width = "100%")),
          div(class = "col-md-2",
              btn_primary(ns("track"), "Track Shipment",
                          fontawesome::fa("magnifying-glass"), class = "w-100"))),
      div(class = "tiny muted mt-2",
          "Customers can search by any one of Consignment No., registered mobile number, or the Tracking ID shared via SMS / WhatsApp.")
    ),
    div(class = "mt-3", uiOutput(ns("result")))
  )
}

tracking_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    hit <- reactiveVal(NULL)
    err <- reactiveVal(NULL)

    # A portal user may only ever see their own consignments, whatever they
    # type into the box.
    searchable <- reactive({
      cn <- get_consignments()
      if (identical(user()$role, "Customer")) return(scope_owner(cn, user()))
      if (identical(user()$role, "Vendor")) {
        t <- get_trips(); mine <- t$trip_no[t$vendor_id == (user()$vendor_id %||% "")]
        return(cn[cn$trip_no %in% mine, ])
      }
      scope_branch(cn, user())
    })

    # Tracking IDs are not stored; the deck shows them as a share token derived
    # from the LR. Deriving rather than storing keeps them stable and avoids a
    # column that can drift out of sync with the LR it points at.
    track_id <- function(lr) paste0("TRK-", sub("LR-", "", lr), "-LX")

    observeEvent(input$track, {
      err(NULL); hit(NULL)
      cn <- searchable()
      q_cn  <- toupper(trimws(input$q_cn %||% ""))
      q_mob <- gsub("[^0-9]", "", input$q_mobile %||% "")
      q_trk <- toupper(trimws(input$q_track %||% ""))

      if (!nzchar(q_cn) && !nzchar(q_mob) && !nzchar(q_trk)) {
        err("Enter a consignment number, mobile number or tracking ID.")
        return()
      }

      res <- cn[0, ]
      if (nzchar(q_cn)) {
        res <- cn[toupper(cn$cn_no) == q_cn | toupper(cn$lr_no) == q_cn, ]
      }
      if (!nrow(res) && nzchar(q_trk)) {
        res <- cn[toupper(track_id(cn$lr_no)) == q_trk, ]
      }
      if (!nrow(res) && nzchar(q_mob)) {
        cl <- store_get("clients")
        who <- cl$client_id[gsub("[^0-9]", "", cl$mobile) == q_mob]
        res <- cn[cn$client_id %in% who, ]
        # Most recent only — see the header note on enumeration.
        if (nrow(res)) res <- res[order(res$dispatch_date, decreasing = TRUE), ][1, ]
      }

      if (!nrow(res)) {
        err("No shipment found for those details.")
        return()
      }
      hit(res[1, ])
      audit(user()$user_id, "track", "tracking", res$cn_no[1])
    })

    output$result <- renderUI({
      if (!is.null(err())) return(card_panel(callout("Not found", err(), "warn")))
      c <- hit()
      if (is.null(c)) {
        return(card_panel(empty_panel(
          "Enter a consignment number, mobile or tracking ID above", "\U0001F50D")))
      }

      cl <- store_get("clients"); v <- store_get("vehicles"); dr <- store_get("drivers")
      pods <- get_pods(); pod <- pods[pods$cn_no == c$cn_no, ]
      ev <- store_get("consignment_events")
      ev <- ev[ev$cn_no == c$cn_no, ]
      ev <- ev[order(as.numeric(ev$seq)), ]

      all_steps <- c("Booking Created", "Vehicle Allocated", "Picked Up", "In Transit",
                     "At Hub", "Out For Delivery", "Delivered", "POD Uploaded")
      step_sub <- c(
        "Booking Created"  = c$booking_no,
        "Vehicle Allocated"= paste(v$reg_no[match(c$vehicle_id, v$vehicle_id)], "·",
                                   dr$name[match(c$driver_id, dr$driver_id)]),
        "Picked Up"        = c$origin_addr,
        "In Transit"       = paste(c$origin_city, "→", c$dest_city, "corridor"),
        "At Hub"           = paste(c$dest_city, "hub"),
        "Out For Delivery" = "Last-mile · consignee area",
        "Delivered"        = if (nzchar(c$delivered_date %||% "")) "Signed by consignee"
                             else "Awaiting consignee sign-off",
        "POD Uploaded"     = "Photo / signed copy from driver app"
      )
      done <- ev$event

      div(
        class = "row g-3",
        div(class = "col-xl-5", card_panel(
          div(class = "d-flex align-items-start justify-content-between gap-2 mb-3",
              div(h6(class = "tms-card-title", paste(c$cn_no, "·", c$lr_no)),
                  p(class = "tms-card-sub",
                    paste0(cl$name[match(c$client_id, cl$client_id)], " — ",
                           c$origin_city, " → ", c$dest_city))),
              pill(c$status)),
          dl_rows(
            "LR No."        = c$lr_no,
            "Customer"      = cl$name[match(c$client_id, cl$client_id)],
            "Route"         = paste(c$origin_city, "→", c$dest_city),
            "Vehicle / Driver" = HTML(paste0(
              '<span class="mono">', v$reg_no[match(c$vehicle_id, v$vehicle_id)],
              "</span> · ", dr$name[match(c$driver_id, dr$driver_id)])),
            "Weight"        = fmt_wt(c$weight_t),
            "Dispatched"    = fmt_date(c$dispatch_date),
            "ETA"           = fmt_date(c$expected_delivery),
            "Tracking ID"   = span(class = "mono", track_id(c$lr_no))
          ),
          div(class = "d-flex gap-2 mt-3",
              btn_ghost(ns("share"), "Share Link", class = "flex-fill"),
              if (nrow(pod) && pod$status[1] %in% c("Uploaded", "Verified", "Approved"))
                btn_dark(ns("dl_pod"), "Download POD", class = "flex-fill")
              else btn_ghost(ns("no_pod"), "POD pending", class = "flex-fill"))
        )),

        div(class = "col-xl-7", card_panel(
          title = "Delivery Timeline",
          timeline(lapply(seq_along(all_steps), function(i) {
            s <- all_steps[i]
            k <- which(done == s)
            list(title = s,
                 sub = unname(step_sub[[s]]),
                 when = if (length(k)) fmt_dt(ev$event_dt[k[1]]) else NULL,
                 state = if (length(k)) "done"
                         else if (i == length(done) + 1) "current" else "todo")
          }))
        ))
      )
    })

    observeEvent(input$share, {
      c <- hit(); req(c)
      showModal(modalDialog(
        title = "Share tracking link", easyClose = TRUE,
        p("Send the consignee a read-only link that shows the timeline without a sign-in:"),
        div(class = "field-static mono", paste0("https://track.amardiptms.in/", track_id(c$lr_no))),
        callout("Read-only",
                "The link exposes status and ETA only — no pricing, no other consignments.",
                "info"),
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$dl_pod, {
      c <- hit(); req(c)
      audit(user()$user_id, "download-pod", "tracking", c$lr_no)
      showNotification(paste("POD for", c$lr_no, "downloaded."), type = "message")
    })

    observeEvent(input$no_pod, {
      showNotification("The driver has not uploaded the signed copy yet.", type = "warning")
    })
  })
}
