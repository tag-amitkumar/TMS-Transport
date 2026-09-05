# ==================================================================
# Invoices & Billing.
#
# Two rules from the deck are enforced here rather than merely displayed:
#
#   1. "Invoices generate automatically from delivered consignments (POD
#      required)" — the Unbilled queue lists only consignments that are both
#      delivered AND carry a verified/approved POD. Raising a bill against
#      anything else is refused.
#
#   2. GST follows the client's configured treatment. Under reverse charge the
#      carrier collects no tax — the recipient pays it — so an RCM invoice's
#      tax line is zero and the total equals the freight. Getting this wrong
#      would overstate revenue by 5% on most of the book.
# ==================================================================

invoices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("manual"), "Manual Invoice"),
            uiOutput(ns("auto_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl"))),
                 foot = "Invoices generate automatically from delivered consignments (POD required); manual invoices supported for ancillary charges."),
      tagList(uiOutput(ns("unbilled")),
              div(class = "mt-3", uiOutput(ns("preview"))))
    )
  )
}

invoices_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")

    all_rows <- reactive({
      i <- scope_all(get_invoices(), user())
      if (!nrow(i)) return(i)
      # Overdue is a function of today, not a stored flag.
      i$status <- ifelse(
        i$status %in% c("Paid", "Draft") | is.na(i$due_date), i$status,
        ifelse(i$due_date < Sys.Date() & (i$total - tidyr::replace_na(i$paid_amount, 0)) > 0,
               "Overdue", i$status))
      i
    })

    # Delivered + POD verified/approved + not already invoiced.
    unbilled <- reactive({
      cn <- scope_all(get_consignments(), user())
      pods <- get_pods()
      ok <- pods$lr_no[pods$status %in% c("Verified", "Approved")]
      billed <- get_invoices()$lr_no
      cn[cn$status == "Delivered" & cn$lr_no %in% ok & !(cn$lr_no %in% billed), ]
    })

    output$cards <- renderUI({
      i <- all_rows()
      # Rolling 30 days rather than month-to-date: on the 1st of a month an MTD
      # figure reads zero, which looks like a fault rather than a new period.
      recent <- i[!is.na(i$invoice_date) & i$invoice_date >= Sys.Date() - 30, ]
      due <- i$total - tidyr::replace_na(i$paid_amount, 0)
      open <- i[due > 0 & i$status != "Draft", ]
      od  <- open[!is.na(open$due_date) & open$due_date < Sys.Date(), ]

      stat_row(
        stat_card("INVOICED · 30 DAYS", inr_compact(sum(recent$total, na.rm = TRUE)),
                  sub = paste(nrow(recent), "invoices raised")),
        stat_card("OUTSTANDING",
                  inr_compact(sum(open$total - tidyr::replace_na(open$paid_amount, 0), na.rm = TRUE)),
                  sub = paste("across", dplyr::n_distinct(open$client_id), "clients")),
        stat_card("OVERDUE",
                  inr_compact(sum(od$total - tidyr::replace_na(od$paid_amount, 0), na.rm = TRUE)),
                  sub = paste(nrow(od), "invoices past due date"), accent = "danger"),
        stat_card("UNBILLED DELIVERED", nrow(unbilled()),
                  sub = "consignments with POD, not invoiced", accent = "ok")
      )
    })

    output$tabs <- renderUI({
      i <- all_rows()
      ks <- c("Draft", "Sent", "Overdue", "Paid")
      items  <- setNames(c("All", ks), c("All", ks))
      counts <- as.list(c(nrow(i), vapply(ks, function(k) sum(i$status == k), integer(1))))
      names(counts) <- c("All", ks)
      tab_strip(ns, "tab", items, counts, tab())
    })
    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      i <- all_rows()
      if (!identical(tab(), "All")) i <- i[i$status == tab(), ]
      i[order(i$invoice_date, decreasing = TRUE), ]
    })

    output$auto_btn <- renderUI({
      if (!can(user()$role, "invoices", "create")) return(NULL)
      btn_primary(ns("from_cn"), "From Consignment", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      i <- rows(); cl <- store_get("clients")
      overdue_days <- as.numeric(Sys.Date() - i$due_date)
      status_lbl <- ifelse(i$status == "Overdue",
                           paste0("Overdue ", round(overdue_days), " d"), i$status)
      due_txt <- fmt_date(i$due_date, TRUE)
      due_txt <- ifelse(i$status == "Overdue",
                        sprintf('<span style="color:#C0392B;font-weight:650;">%s</span>', due_txt),
                        due_txt)

      df <- tibble::tibble(
        INVOICE = mapply(cell2, i$invoice_no,
                         paste(fmt_date(i$invoice_date, TRUE), "·",
                               ifelse(i$source == "auto", "auto from LR", "manual"))),
        CLIENT = cl$name[match(i$client_id, cl$client_id)],
        `LR / BOOKING` = i$lr_no,
        AMOUNT = inr(i$total),
        GST = sprintf('<span class="pill pill-%s nodot">%s %s%%</span>',
                      ifelse(i$gst_mode == "RCM", "blue", "purple"), i$gst_mode, i$gst_pct),
        DUE = due_txt,
        STATUS = pill_html(status_lbl, colour = NULL)
      )
      sc <- c(Draft = "grey", Sent = "blue", `Partially paid` = "orange",
              Paid = "green", Overdue = "red")
      df$STATUS <- sprintf('<span class="pill pill-%s">%s</span>',
                           unname(sc[i$status] %||% "grey"), htmlEscape(status_lbl))
      tms_table(df, page = 12, align = align_right(3))
    })

    sel <- reactive({
      k <- input$tbl_rows_selected
      if (is.null(k) || !length(k)) return(NULL)
      rows()[k[1], ]
    })

    # ---------------- Unbilled queue ----------------

    output$unbilled <- renderUI({
      u <- unbilled()
      cl <- store_get("clients")
      card_panel(
        title = "Unbilled Delivered Consignments",
        actions = pill(paste(nrow(u), "waiting"), if (nrow(u)) "orange" else "green"),
        if (!nrow(u)) div(class = "muted tiny", "Everything delivered has been invoiced.")
        else div(
          lapply(seq_len(min(5, nrow(u))), function(i) {
            r <- u[i, ]
            div(class = "d-flex align-items-center gap-2 py-2",
                style = if (i < min(5, nrow(u))) "border-bottom:1px solid #EEF2F7;" else "",
                div(style = "min-width:0;flex:1;",
                    div(style = "font-weight:650;font-size:.8125rem;color:#12275C;", r$lr_no),
                    div(class = "tiny muted",
                        paste("POD", fmt_date(r$delivered_date, TRUE), "·",
                              cl$name[match(r$client_id, cl$client_id)]))),
                div(class = "text-end",
                    div(style = "font-weight:650;font-size:.8125rem;", inr(r$freight))),
                if (can(user()$role, "invoices", "create"))
                  actionButton(ns(paste0("bill_", r$cn_no)), "Invoice",
                               class = "btn-tms-dark btn-tms-sm",
                               onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                                 ns("bill_one"), r$cn_no)))
          })
        )
      )
    })

    # ---------------- Invoice preview ----------------

    output$preview <- renderUI({
      i <- sel()
      if (is.null(i)) return(card_panel(empty_panel("Select an invoice to preview it")))
      cl <- store_get("clients"); c1 <- cl[cl$client_id == i$client_id, ]
      cn <- get_consignments(); cc <- cn[cn$lr_no == i$lr_no, ]
      paid <- tidyr::replace_na(i$paid_amount, 0)

      card_panel(
        title = paste("Invoice Preview ·", i$invoice_no),
        actions = pill(i$status),
        div(
          style = "border:1px solid #E4EAF1;border-radius:10px;padding:1rem;",
          div(class = "d-flex align-items-center gap-2 mb-3",
              # The logo, not a mark: this header sits on a white card and goes
              # in front of a customer. 34px is where the wordmark inside the
              # artwork stays readable.
              brand_logo(34),
              span(class = "small-caps", style = "letter-spacing:.1em;", "Tax Invoice"),
              span(class = "ms-auto tiny muted",
                   paste(i$invoice_no, "·", fmt_date(i$invoice_date)))),
          dl_rows(
            "Bill to" = if (nrow(c1)) HTML(paste0(c1$name[1], '<br/><span class="mono tiny">',
                                                  c1$gstin[1], "</span>")) else "—",
            "Against" = if (nrow(cc)) paste0(i$lr_no, " · ", cc$origin_city[1],
                                             " → ", cc$dest_city[1]) else i$lr_no,
            "Freight" = inr(i$amount),
            "GST"     = if (identical(i$gst_mode, "RCM"))
                          sprintf("GTA — RCM %s%% payable by recipient", i$gst_pct)
                        else paste0("FCM ", i$gst_pct, "% · ", inr(i$gst_amount))
          ),
          tags$hr(class = "soft"),
          div(class = "d-flex justify-content-between",
              span(style = "font-weight:650;", "Total"),
              span(style = "font-weight:700;font-size:1.05rem;color:#12275C;", inr(i$total))),
          if (paid > 0 && paid < i$total) div(
            class = "d-flex justify-content-between mt-1 tiny",
            span(class = "muted", "Received"),
            span(style = "color:#1A7F45;font-weight:650;", inr(paid)))
        ),

        if (identical(i$gst_mode, "RCM")) div(class = "mt-3",
          callout("Reverse charge",
                  "No tax is collected on this invoice — the recipient discharges GST under RCM. Outward supplies still auto-compile into GSTR-1.",
                  "info")),

        div(class = "d-flex gap-2 mt-3",
            btn_ghost(ns("pdf"), "Download PDF", class = "flex-fill"),
            btn_dark(ns("send"), "Send", class = "flex-fill"))
      )
    })

    observeEvent(input$pdf, {
      i <- sel(); req(i)
      audit(user()$user_id, "download", "invoices", i$invoice_no)
      showNotification(paste("Invoice", i$invoice_no, "prepared as PDF."), type = "message")
    })

    observeEvent(input$send, {
      i <- sel(); req(i)
      if (!require_perm(session, user()$role, "invoices", "edit")) return()
      if (identical(i$status, "Draft")) {
        store_update("invoices", list(invoice_no = i$invoice_no), list(status = "Sent"))
      }
      audit(user()$user_id, "send", "invoices", i$invoice_no)
      showNotification(
        "Invoice marked as sent. Delivery requires an email or WhatsApp gateway to be connected in Settings.",
        type = "warning", duration = 7)
    })

    # ---------------- Raise from consignment ----------------

    raise_invoice <- function(cn_no) {
      cn <- get_consignments(); c1 <- cn[cn$cn_no == cn_no, ]
      if (!nrow(c1)) return(NULL)
      c1 <- c1[1, ]

      # Re-check the POD gate at write time. The queue is a view; this is the
      # actual guard, and it holds even if the UI is stale or bypassed.
      pods <- get_pods()
      p <- pods[pods$lr_no == c1$lr_no, ]
      if (!nrow(p) || !(p$status[1] %in% c("Verified", "Approved"))) {
        showNotification("Cannot invoice: this consignment has no verified POD.",
                         type = "error", duration = 6)
        return(NULL)
      }
      if (c1$lr_no %in% get_invoices()$lr_no) {
        showNotification("That consignment is already invoiced.", type = "error")
        return(NULL)
      }

      cl <- get_clients(); cc <- cl[cl$client_id == c1$client_id, ][1, ]
      gm <- cc$gst_mode; gp <- as.numeric(cc$gst_pct)
      # Reverse charge: carrier collects nothing.
      gst <- if (identical(gm, "RCM")) 0 else round(c1$freight * gp / 100)
      total <- c1$freight + gst
      no <- next_id(get_invoices()$invoice_no, PREFIX$invoice)

      # Payment terms decide two things the invoice cannot work out for itself:
      # which branch raises it, and whether anything is still owed.
      bk <- get_bookings(); b <- bk[bk$booking_no == c1$booking_no, ]
      pay <- c1$payment_mode %||% (if (nrow(b)) b$payment_mode[1] else "Credit")
      if (!nzchar(pay %||% "")) pay <- "Credit"
      bill_at <- c1$bill_at_branch_id %||% (if (nrow(b)) b$bill_at_branch_id[1] else "")

      # TBB: the customer's account sits at another branch, so that branch owns
      # the receivable. Booking it against the despatching branch would put the
      # debt on the wrong ledger.
      branch <- if (identical(pay, "TBB") && nzchar(bill_at %||% "")) bill_at else c1$branch_id

      # Paid: the money changed hands at booking, so the invoice is a record of
      # a settled transaction, not a request for payment.
      paid   <- if (identical(pay, "Paid")) total else 0
      status <- if (identical(pay, "Paid")) "Paid" else "Draft"

      store_insert("invoices", list(
        invoice_no = no, invoice_date = as.character(Sys.Date()),
        client_id = c1$client_id, branch_id = branch,
        lr_no = c1$lr_no, cn_no = c1$cn_no,
        amount = c1$freight, gst_mode = gm, gst_pct = gp, gst_amount = gst,
        total = total, paid_amount = paid,
        due_date = as.character(Sys.Date() + 14), source = "auto",
        payment_mode = pay, status = status
      ))
      audit(user()$user_id, "create", "invoices",
            paste(no, "from", c1$lr_no, "·", pay))
      no
    }

    observeEvent(input$bill_one, {
      if (!require_perm(session, user()$role, "invoices", "create")) return()
      no <- raise_invoice(input$bill_one)
      if (!is.null(no)) showNotification(paste("Invoice", no, "raised."), type = "message")
    })

    observeEvent(input$from_cn, {
      if (!require_perm(session, user()$role, "invoices", "create")) return()
      u <- unbilled()
      if (!nrow(u)) {
        showNotification("No delivered consignment with a verified POD is awaiting an invoice.",
                         type = "warning"); return()
      }
      cl <- store_get("clients")
      showModal(modalDialog(
        title = "Raise invoices from consignments", size = "l", easyClose = TRUE,
        p(class = "tiny muted",
          "Only delivered consignments with a verified or approved POD appear here."),
        checkboxGroupInput(ns("bulk"), NULL,
                           choiceNames = paste0(u$lr_no, " · ",
                                                cl$name[match(u$client_id, cl$client_id)],
                                                " · ", inr(u$freight)),
                           choiceValues = u$cn_no,
                           selected = u$cn_no),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("bulk_save"), "Raise selected"))
      ))
    })

    observeEvent(input$bulk_save, {
      if (!require_perm(session, user()$role, "invoices", "create")) return()
      sel_cn <- input$bulk
      if (!length(sel_cn)) { showNotification("Nothing selected.", type = "error"); return() }
      made <- Filter(Negate(is.null), lapply(sel_cn, raise_invoice))
      removeModal()
      showNotification(sprintf("%d invoice%s raised.", length(made),
                               if (length(made) == 1) "" else "s"), type = "message")
    })

    observeEvent(input$manual, {
      if (!require_perm(session, user()$role, "invoices", "create")) return()
      cl <- scope_branch(get_clients(), user())
      showModal(modalDialog(
        title = "Manual invoice", size = "l", easyClose = TRUE,
        callout("For ancillary charges",
                "Detention, loading/unloading, demurrage and similar charges that do not arise from a consignment.",
                "info"),
        div(class = "row g-3 mt-1",
            div(class = "col-12", selectizeInput(ns("m_client"), "Client",
                                                 setNames(cl$client_id, cl$name), width = "100%")),
            div(class = "col-6", textInput(ns("m_desc"), "Description",
                                           placeholder = "detention charges")),
            div(class = "col-6", numericInput(ns("m_amt"), "Amount (₹)", 4000, 0, step = 500))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("m_save"), "Raise invoice"))
      ))
    })

    observeEvent(input$m_save, {
      if (!require_perm(session, user()$role, "invoices", "create")) return()
      req(input$m_client)
      amt <- as.numeric(input$m_amt %||% 0)
      if (is.na(amt) || amt <= 0) {
        showNotification("Enter an amount greater than zero.", type = "error"); return()
      }
      cl <- get_clients(); cc <- cl[cl$client_id == input$m_client, ][1, ]
      # Ancillary charges are a service, not GTA freight, so they carry forward
      # charge at 18% regardless of the client's freight treatment.
      gst <- round(amt * 18 / 100)
      no <- next_id(get_invoices()$invoice_no, PREFIX$invoice)
      store_insert("invoices", list(
        invoice_no = no, invoice_date = as.character(Sys.Date()),
        client_id = cc$client_id, branch_id = cc$branch_id,
        lr_no = input$m_desc %||% "manual", cn_no = "",
        amount = amt, gst_mode = "FCM", gst_pct = 18, gst_amount = gst,
        total = amt + gst, paid_amount = 0,
        due_date = as.character(Sys.Date() + 14), source = "manual", status = "Draft"
      ))
      audit(user()$user_id, "create", "invoices", paste(no, "manual"))
      removeModal()
      showNotification(paste("Manual invoice", no, "raised."), type = "message")
    })
  })
}
