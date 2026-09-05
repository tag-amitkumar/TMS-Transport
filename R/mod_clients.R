# ==================================================================
# Clients — customer master with the 360° panel (billing, credit, history).
#
# The deck labels this "Clients" in the rail and "Customer" on every screen.
# Same entity; the rail label is kept, and the table is stored as `clients`.
# ==================================================================

clients_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Client Master", textOutput(ns("sub"), inline = TRUE),
              actions = tagList(
                btn_ghost(ns("filters"), "Filters", fontawesome::fa("filter")),
                uiOutput(ns("add_btn"), inline = TRUE)
              )),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl")))),
      uiOutput(ns("panel"))
    )
  )
}

clients_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    detail_tab <- reactiveVal("Consignments")

    rows <- reactive(scope_all(get_clients(), user()))

    # Outstanding is derived from invoices rather than stored on the client, so
    # it cannot go stale when a payment lands.
    outstanding <- reactive({
      inv <- get_invoices()
      inv |>
        dplyr::mutate(due = total - tidyr::replace_na(paid_amount, 0)) |>
        dplyr::filter(due > 0) |>
        dplyr::group_by(client_id) |>
        dplyr::summarise(out = sum(due), n = dplyr::n(), .groups = "drop")
    })

    consign_count <- reactive({
      get_consignments() |> dplyr::count(client_id, name = "n")
    })

    output$sub <- renderText({
      sprintf("%d clients · billing, credit & consignment history", nrow(rows()))
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "clients", "create")) return(NULL)
      btn_primary(ns("add"), "Add Client", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      c <- rows()
      o <- outstanding(); cc <- consign_count()
      out_v <- o$out[match(c$client_id, o$client_id)]
      n_cn  <- cc$n[match(c$client_id, cc$client_id)]

      df <- tibble::tibble(
        CLIENT = mapply(function(n, p, m) paste0(
          '<div class="d-flex align-items-center gap-2">', avatar_html(n),
          '<div><div style="font-weight:600;color:#12275C;">', htmlEscape(n),
          '</div><div class="l2" style="font-size:.6875rem;color:#64748B;">',
          htmlEscape(p), " · ", htmlEscape(m), "</div></div></div>"),
          c$name, c$contact_person, c$mobile),
        `GSTIN / PAN` = paste0('<span class="mono">', htmlEscape(c$gstin),
                               '</span><br/><span class="mono" style="color:#64748B;">',
                               htmlEscape(c$pan), "</span>"),
        CITY = c$city,
        CONSIGNMENTS = tidyr::replace_na(n_cn, 0L),
        `CREDIT LIMIT` = inr_compact(c$credit_limit),
        OUTSTANDING = ifelse(is.na(out_v), "—",
                             paste0('<span style="color:#B3701A;font-weight:650;">',
                                    inr_compact(out_v), "</span>")),
        GST = pill_html(paste0(c$gst_mode, " ", c$gst_pct, "%"), colour = NULL)
      )
      df$GST <- sprintf('<span class="pill pill-%s nodot">%s %s%%</span>',
                        ifelse(c$gst_mode == "RCM", "blue", "purple"), c$gst_mode, c$gst_pct)
      tms_table(df, page = 12, align = align_right(3:5))
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    observeEvent(input$dtab, detail_tab(input$dtab))

    output$panel <- renderUI({
      c <- sel()
      if (is.null(c)) return(card_panel(empty_panel("Select a client for the 360° view")))

      o  <- outstanding(); out_v <- o$out[match(c$client_id, o$client_id)]
      cn <- get_consignments(); cn <- cn[cn$client_id == c$client_id, ]

      card_panel(
        div(class = "d-flex align-items-start gap-3 mb-3",
            avatar(c$name, "lg"),
            div(style = "min-width:0;",
                h6(class = "tms-card-title", paste0(c$name, " — 360° view")),
                p(class = "tms-card-sub",
                  paste(c$city, "· client since", format(as.Date(c$since), "%Y"),
                        "·", c$gst_mode, "freight"))),
            div(class = "ms-auto d-flex gap-1",
                div(class = "tms-iconbtn", title = c$mobile, fontawesome::fa("phone")),
                div(class = "tms-iconbtn", title = c$email, fontawesome::fa("comment")))),

        div(class = "row g-2 mb-3",
            div(class = "col-4", stat_card("CREDIT LIMIT", inr_compact(c$credit_limit))),
            div(class = "col-4", stat_card("OUTSTANDING",
                                           if (is.na(out_v)) "—" else inr_compact(out_v),
                                           accent = if (!is.na(out_v)) "warn" else "none")),
            div(class = "col-4", stat_card("CONSIGNMENTS", nrow(cn)))),

        dl_rows(
          "Company name"    = c$name,
          "Contact person"  = c$contact_person,
          "Mobile"          = span(class = "mono", c$mobile),
          "Email"           = c$email,
          "GSTIN"           = span(class = "mono", c$gstin),
          "GST treatment"   = paste0(c$gst_mode, " · ", c$gst_pct, "%"),
          "Billing address" = c$billing_address,
          "Pickup address"  = c$pickup_address,
          "Delivery address"= c$delivery_address
        ),

        tags$hr(class = "soft"),
        tab_strip(ns, "dtab", c("Consignments" = "Consignments", "Payments" = "Payments"),
                  selected = detail_tab()),
        div(class = "mt-2", uiOutput(ns("detail_body")))
      )
    })

    output$detail_body <- renderUI({
      c <- sel(); req(c)

      if (identical(detail_tab(), "Payments")) {
        p <- get_payments(); p <- p[p$client_id == c$client_id, ]
        p <- p[order(p$date, decreasing = TRUE), ][seq_len(min(6, nrow(p))), ]
        if (!nrow(p) || all(is.na(p$payment_id)))
          return(div(class = "muted tiny py-3", "No payments recorded."))
        return(div(class = "tms-table",
          DT::datatable(
            tibble::tibble(DATE = fmt_date(p$date, TRUE), INVOICE = p$invoice_no,
                           MODE = p$mode, AMOUNT = inr(p$amount)),
            rownames = FALSE, selection = "none", escape = FALSE,
            options = list(dom = "t", pageLength = 6))))
      }

      cn <- get_consignments(); cn <- cn[cn$client_id == c$client_id, ]
      cn <- cn[order(cn$dispatch_date, decreasing = TRUE), ][seq_len(min(6, nrow(cn))), ]
      if (!nrow(cn) || all(is.na(cn$lr_no)))
        return(div(class = "muted tiny py-3", "No consignments yet."))

      div(class = "tms-table",
        DT::datatable(
          tibble::tibble(
            LR = cn$lr_no,
            ROUTE = paste(cn$origin_city, "→", cn$dest_city),
            FREIGHT = inr(cn$freight),
            STATUS = pill_html(cn$status)),
          rownames = FALSE, selection = "none", escape = FALSE,
          options = list(dom = "t", pageLength = 6)))
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "clients", "create")) return()
      b <- store_get("branches")
      showModal(modalDialog(
        title = "Add client", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("n_name"), "Company name")),
            div(class = "col-6", textInput(ns("n_contact"), "Contact person")),
            div(class = "col-6", textInput(ns("n_mobile"), "Mobile")),
            div(class = "col-6", textInput(ns("n_email"), "Email")),
            div(class = "col-6", textInput(ns("n_gstin"), "GSTIN")),
            div(class = "col-6", textInput(ns("n_pan"), "PAN")),
            div(class = "col-4", textInput(ns("n_city"), "City")),
            div(class = "col-4", selectInput(ns("n_gm"), "GST mode", GST_MODES)),
            div(class = "col-4", numericInput(ns("n_gp"), "GST %", 5, 0, 28, 1)),
            div(class = "col-6", numericInput(ns("n_credit"), "Credit limit (₹)", 200000, 0, step = 50000)),
            div(class = "col-6", selectInput(ns("n_branch"), "Servicing branch",
                                             setNames(b$branch_id, b$name))),
            div(class = "col-12", textInput(ns("n_bill"), "Billing address"))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("save"), "Save client"))
      ))
    })

    # Keep the GST rate consistent with the mode: reverse charge on GTA freight
    # is 5% by statute, so offering 12/18 there would be wrong.
    observeEvent(input$n_gm, {
      if (identical(input$n_gm, "RCM")) updateNumericInput(session, "n_gp", value = 5)
    }, ignoreInit = TRUE)

    observeEvent(input$save, {
      if (!require_perm(session, user()$role, "clients", "create")) return()
      if (!nzchar(input$n_name %||% "")) {
        showNotification("Company name is required.", type = "error"); return()
      }
      id_new <- next_id(store_get("clients")$client_id, "CUST-")
      store_insert("clients", list(
        client_id = id_new, name = input$n_name, contact_person = input$n_contact,
        mobile = input$n_mobile, email = input$n_email, gstin = input$n_gstin,
        pan = input$n_pan, city = input$n_city, state = "",
        billing_address = input$n_bill, pickup_address = input$n_bill,
        delivery_address = "", credit_limit = input$n_credit,
        gst_mode = input$n_gm, gst_pct = input$n_gp,
        branch_id = input$n_branch, since = as.character(Sys.Date()), status = "Active"
      ))
      audit(user()$user_id, "create", "clients", id_new)
      removeModal()
      showNotification(paste("Client", input$n_name, "added."), type = "message")
    })

    observeEvent(input$filters, {
      showModal(modalDialog(title = "Filter clients", easyClose = TRUE,
        selectInput(ns("f_gst"), "GST mode", c("All", GST_MODES)),
        footer = modalButton("Close")))
    })
  })
}
