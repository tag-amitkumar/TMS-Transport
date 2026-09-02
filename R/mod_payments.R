# ==================================================================
# Payments & Ledger — customer receipts, vendor payouts, client ledger.
#
# Receipts reconcile against open invoices rather than being logged loose: a
# receipt updates the invoice's paid_amount and flips its status, so the
# outstanding figure on this screen, the client 360° and the dashboard all come
# from one calculation.
# ==================================================================

payments_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("filters"), "Filters", fontawesome::fa("filter")),
            uiOutput(ns("rec_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl"))),
                 foot = "Receipts reconcile automatically against open invoices; partial payments keep the invoice partially paid until settled in full."),
      tagList(uiOutput(ns("top_out")),
              div(class = "mt-3", uiOutput(ns("vendor_due"))))
    )
  )
}

payments_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("Customer Receipts")

    receipts <- reactive(scope_all(get_payments(), user()))
    invoices <- reactive(scope_all(get_invoices(), user()))
    vpay     <- reactive(get_vendor_payments())

    # Open balance per invoice, the basis for every outstanding number here.
    open_inv <- reactive({
      i <- invoices()
      if (!nrow(i)) return(i)
      i$balance <- i$total - tidyr::replace_na(i$paid_amount, 0)
      i[i$balance > 0 & i$status != "Draft", ]
    })

    output$cards <- renderUI({
      p <- receipts(); o <- open_inv(); v <- vpay()
      recent <- p[!is.na(p$date) & p$date >= since_30d(), ]
      aged <- o[!is.na(o$due_date) & o$due_date < Sys.Date() - 30, ]

      stat_row(
        stat_card("RECEIVED · 30 DAYS", inr_compact(sum(recent$amount, na.rm = TRUE)),
                  sub = paste(nrow(recent), "receipts received"), accent = "ok"),
        stat_card("CUSTOMER OUTSTANDING", inr_compact(sum(o$balance, na.rm = TRUE)),
                  sub = sprintf("%d clients · %s over 30 days",
                                dplyr::n_distinct(o$client_id),
                                inr_compact(sum(aged$balance, na.rm = TRUE))),
                  accent = "warn"),
        stat_card("VENDOR PAYMENTS DUE", inr_compact(sum(v$amount, na.rm = TRUE)),
                  sub = paste(nrow(v), "vendors awaiting settlement")),
        stat_card("OVERDUE > 30 DAYS", inr_compact(sum(aged$balance, na.rm = TRUE)),
                  sub = paste(dplyr::n_distinct(aged$client_id), "clients flagged for credit hold"),
                  accent = "danger")
      )
    })

    output$tabs <- renderUI({
      tab_strip(ns, "tab",
                setNames(c("Customer Receipts", "Vendor Payments", "Client Ledger"),
                         c("Customer Receipts", "Vendor Payments", "Client Ledger")),
                selected = tab())
    })
    observeEvent(input$tab, tab(input$tab))

    output$rec_btn <- renderUI({
      if (!can(user()$role, "payments", "create")) return(NULL)
      btn_primary(ns("record"), "Record Receipt", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      cl <- store_get("clients")

      if (identical(tab(), "Vendor Payments")) {
        v <- vpay(); ven <- store_get("vendors")
        if (!nrow(v)) return(tms_table(tibble::tibble(Message = "No vendor payments due")))
        df <- tibble::tibble(
          VENDOR = ven$name[match(v$vendor_id, ven$vendor_id)],
          OWNER  = ven$owner_name[match(v$vendor_id, ven$vendor_id)],
          TRIPS  = v$trips,
          AMOUNT = inr(v$amount),
          DUE    = fmt_date(v$due_date, TRUE),
          STATUS = pill_html(v$status, colour = NULL)
        )
        df$STATUS <- sprintf('<span class="pill pill-%s">%s</span>',
                             ifelse(v$status == "Approved", "green", "orange"),
                             htmlEscape(v$status))
        return(tms_table(df, page = 12, align = align_right(2:3)))
      }

      if (identical(tab(), "Client Ledger")) {
        o <- open_inv()
        if (!nrow(o)) return(tms_table(tibble::tibble(Message = "Nothing outstanding")))
        led <- o |>
          dplyr::group_by(client_id) |>
          dplyr::summarise(invoices = dplyr::n(), balance = sum(balance),
                           oldest = min(due_date, na.rm = TRUE), .groups = "drop") |>
          dplyr::arrange(dplyr::desc(balance))
        age <- as.numeric(Sys.Date() - led$oldest)
        df <- tibble::tibble(
          CLIENT   = cl$name[match(led$client_id, cl$client_id)],
          INVOICES = led$invoices,
          OUTSTANDING = inr(led$balance),
          `OLDEST DUE` = fmt_date(led$oldest, TRUE),
          AGEING = sprintf('<span class="pill pill-%s">%s d</span>',
                           ifelse(age > 45, "red", ifelse(age > 15, "orange", "grey")),
                           round(pmax(0, age)))
        )
        return(tms_table(df, page = 12, align = align_right(1:2)))
      }

      p <- receipts()
      p <- p[order(p$date, decreasing = TRUE), ]
      if (!nrow(p)) return(tms_table(tibble::tibble(Message = "No receipts recorded")))
      i <- invoices()
      full <- i$total[match(p$invoice_no, i$invoice_no)]
      amt <- ifelse(!is.na(full) & p$amount < full,
                    paste0(inr(p$amount),
                           ' <span style="color:#94A3B8;">/ ', inr(full), "</span>"),
                    inr(p$amount))

      df <- tibble::tibble(
        DATE = fmt_date(p$date, TRUE),
        CLIENT = cl$name[match(p$client_id, cl$client_id)],
        `AGAINST INVOICE` = p$invoice_no,
        MODE = p$mode,
        REFERENCE = mono(p$reference),
        AMOUNT = amt,
        `RECEIVED BY` = p$received_by
      )
      tms_table(df, page = 12, align = align_right(5))
    })

    # ---------------- Side panels ----------------

    output$top_out <- renderUI({
      o <- open_inv(); cl <- store_get("clients")
      if (!nrow(o)) return(card_panel(title = "Top Outstanding Clients",
                                      div(class = "muted tiny", "Nothing outstanding.")))
      led <- o |>
        dplyr::group_by(client_id) |>
        dplyr::summarise(bal = sum(balance), oldest = min(due_date, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::arrange(dplyr::desc(bal))
      led <- led[seq_len(min(5, nrow(led))), ]
      age <- as.numeric(Sys.Date() - led$oldest)

      card_panel(
        title = "Top Outstanding Clients",
        div(class = "tms-table",
            DT::datatable(
              tibble::tibble(
                CLIENT = cl$name[match(led$client_id, cl$client_id)],
                OUTSTANDING = inr(led$bal),
                AGEING = sprintf('<span class="pill pill-%s">%s d</span>',
                                 ifelse(age > 45, "red", ifelse(age > 15, "orange", "grey")),
                                 round(pmax(0, age)))),
              rownames = FALSE, selection = "none", escape = FALSE,
              options = list(dom = "t", pageLength = 5)))
      )
    })

    output$vendor_due <- renderUI({
      v <- vpay(); ven <- store_get("vendors")
      if (!nrow(v)) return(NULL)
      v <- v[order(-v$amount), ][seq_len(min(4, nrow(v))), ]
      pending <- vpay()
      big <- sum(pending$amount > 50000 & pending$status != "Approved")

      card_panel(
        title = "Vendor Payments Due",
        actions = pill(paste("next run", fmt_date(min(as.Date(pending$due_date), na.rm = TRUE), TRUE)), "blue"),
        div(class = "tms-table",
            DT::datatable(
              tibble::tibble(
                VENDOR = ven$owner_name[match(v$vendor_id, ven$vendor_id)],
                AMOUNT = inr(v$amount),
                TRIPS = v$trips),
              rownames = FALSE, selection = "none", escape = FALSE,
              options = list(dom = "t", pageLength = 4))),
        if (big > 0) div(class = "mt-3",
          callout("Payment approval pending",
                  sprintf("%d vendor payment%s above ₹50,000 await Branch Admin approval before the next run.",
                          big, if (big == 1) "" else "s"), "warn")),
        if (can(user()$role, "payments", "approve")) div(class = "mt-3",
          btn_dark(ns("approve_vendor"), "Approve vendor run", class = "w-100"))
      )
    })

    observeEvent(input$approve_vendor, {
      if (!require_perm(session, user()$role, "payments", "approve")) return()
      v <- vpay(); pend <- v[v$status != "Approved", ]
      if (!nrow(pend)) {
        showNotification("Nothing awaiting approval.", type = "warning"); return()
      }
      for (id_v in pend$vp_id) {
        store_update("vendor_payments", list(vp_id = id_v),
                     list(status = "Approved", approved_by = user()$name))
      }
      audit(user()$user_id, "approve", "payments", paste(nrow(pend), "vendor payments"))
      showNotification(sprintf("%d vendor payment%s approved.", nrow(pend),
                               if (nrow(pend) == 1) "" else "s"), type = "message")
    })

    # ---------------- Record a receipt ----------------

    observeEvent(input$record, {
      if (!require_perm(session, user()$role, "payments", "create")) return()
      o <- open_inv(); cl <- store_get("clients")
      if (!nrow(o)) {
        showNotification("No open invoices to receive against.", type = "warning"); return()
      }
      o <- o[order(o$due_date), ]
      showModal(modalDialog(
        title = "Record receipt", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-12",
                selectizeInput(ns("r_inv"), "Against invoice",
                               setNames(o$invoice_no,
                                        paste0(o$invoice_no, " · ",
                                               cl$name[match(o$client_id, cl$client_id)],
                                               " · ", inr(o$balance), " open")),
                               width = "100%")),
            div(class = "col-4", numericInput(ns("r_amt"), "Amount (₹)", 0, 0, step = 100)),
            div(class = "col-4", selectInput(ns("r_mode"), "Mode",
                                             c("NEFT", "RTGS", "UPI", "Cheque", "Cash"))),
            div(class = "col-4", dateInput(ns("r_date"), "Date", value = Sys.Date())),
            div(class = "col-12", textInput(ns("r_ref"), "Reference / UTR"))),
        uiOutput(ns("r_hint")),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("r_save"), "Record receipt"))
      ))
    })

    # Default the amount to the full open balance — the common case.
    observeEvent(input$r_inv, {
      o <- open_inv(); r <- o[o$invoice_no == input$r_inv, ]
      if (nrow(r)) updateNumericInput(session, "r_amt", value = r$balance[1])
    }, ignoreInit = TRUE)

    output$r_hint <- renderUI({
      req(input$r_inv)
      o <- open_inv(); r <- o[o$invoice_no == input$r_inv, ]
      if (!nrow(r)) return(NULL)
      amt <- as.numeric(input$r_amt %||% 0)
      bal <- r$balance[1]
      if (is.na(amt) || amt <= 0) return(NULL)
      if (amt > bal) {
        callout("Over-payment",
                sprintf("This is %s more than the %s open on %s.",
                        inr(amt - bal), inr(bal), r$invoice_no[1]), "warn")
      } else if (amt < bal) {
        callout("Partial payment",
                sprintf("%s will remain open on %s after this receipt.",
                        inr(bal - amt), r$invoice_no[1]), "info")
      } else {
        callout("Settles in full", sprintf("%s will be marked paid.", r$invoice_no[1]), "ok")
      }
    })

    # Rendered inside a modal, which is invisible while it fades in — without
    # this the reconciliation hint never computes and the operator gets no
    # feedback on what the receipt will do to the invoice. See mod_cargo_moto.R.
    outputOptions(output, "r_hint", suspendWhenHidden = FALSE)

    observeEvent(input$r_save, {
      if (!require_perm(session, user()$role, "payments", "create")) return()
      req(input$r_inv)
      amt <- as.numeric(input$r_amt %||% 0)
      if (is.na(amt) || amt <= 0) {
        showNotification("Enter an amount greater than zero.", type = "error"); return()
      }
      i <- get_invoices(); r <- i[i$invoice_no == input$r_inv, ]
      req(nrow(r)); r <- r[1, ]

      already <- tidyr::replace_na(r$paid_amount, 0)
      new_paid <- already + amt
      # Receipt and invoice move together; that is what keeps every outstanding
      # figure in the app derived from one number.
      new_status <- if (new_paid >= r$total) "Paid" else "Partially paid"

      store_insert("payments", list(
        payment_id = next_id(store_get("payments")$payment_id, "RCP-"),
        date = as.character(input$r_date), client_id = r$client_id,
        invoice_no = r$invoice_no, mode = input$r_mode,
        reference = input$r_ref, amount = amt, received_by = user()$name
      ))
      store_update("invoices", list(invoice_no = r$invoice_no),
                   list(paid_amount = new_paid, status = new_status))

      audit(user()$user_id, "receipt", "payments",
            paste(r$invoice_no, inr(amt), "->", new_status))
      removeModal()
      showNotification(sprintf("Receipt of %s recorded · %s is now %s.",
                               inr(amt), r$invoice_no, tolower(new_status)),
                       type = "message", duration = 6)
    })

    observeEvent(input$filters, {
      showModal(modalDialog(title = "Filter", easyClose = TRUE,
        dateRangeInput(ns("f_range"), "Date range",
                       start = Sys.Date() - 30, end = Sys.Date()),
        selectInput(ns("f_mode"), "Mode",
                    c("All", "NEFT", "RTGS", "UPI", "Cheque", "Cash")),
        footer = modalButton("Close")))
    })
  })
}
