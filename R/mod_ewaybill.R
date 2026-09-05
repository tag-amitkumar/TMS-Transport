# ==================================================================
# E-Way Bill management.
#
# Under Indian GST a consignment above the threshold must carry a valid e-way
# bill, whose validity is distance-based and expires — an expired EWB gets the
# vehicle held at a check-post. The screen therefore leads with the expiry
# queue rather than the register.
#
# Generation against the live GSP is stubbed: the app records intent and issues
# a locally-generated reference. Nothing here calls a government API, and no
# credential ships with the repo. See DESIGN-REVIEW.md, gap #12.
# ==================================================================

ewaybill_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    uiOutput(ns("cards")),
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("upload"), "Upload EWB"),
            uiOutput(ns("gen_btn"), inline = TRUE))),
    card_panel(body_class = "tms-card-body p-0",
               div(class = "tms-table", DTOutput(ns("tbl")))),
    div(class = "tms-card-foot mt-2 tiny muted",
        "E-way bills are linked to their consignment on generation. Validity is distance-based and blocks dispatch once expired.")
  )
}

ewaybill_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")

    # Status is recomputed from validity on every read rather than trusted from
    # storage — a bill stored as "Valid" yesterday may be expired now.
    all_rows <- reactive({
      e <- get_ewaybills()
      cn <- get_consignments()
      e <- e[e$cn_no %in% scope_all(cn, user())$cn_no, , drop = FALSE]
      if (!nrow(e)) return(e)
      e$status <- dplyr::case_when(
        is.na(e$valid_to)                    ~ "Valid",
        e$valid_to < Sys.time()              ~ "Expired",
        e$valid_to < Sys.time() + 24 * 3600  ~ "Expiring Soon",
        TRUE                                 ~ "Valid"
      )
      e
    })

    output$banner <- renderUI({
      e <- all_rows()
      n <- sum(e$status == "Expiring Soon")
      if (!n) return(NULL)
      div(class = "callout callout-warn mb-3 align-items-center",
          fontawesome::fa("triangle-exclamation"),
          div(class = "flex-grow-1",
              span(class = "ttl", sprintf("%d e-way bill%s expiring within 24 hours",
                                          n, if (n == 1) "" else "s")),
              "Renew now to avoid vehicles being held at check-posts."),
          if (can(user()$role, "ewaybill", "create"))
            btn_dark(ns("renew_all"), paste("Renew All", n), class = "btn-tms-sm"))
    })

    output$cards <- renderUI({
      e <- all_rows()
      # A rolling 30-day window rather than month-to-date: on the 1st of a
      # month MTD reads zero, which looks like a broken feed rather than a
      # quiet period.
      recent <- e[!is.na(e$valid_from) & as.Date(e$valid_from) >= Sys.Date() - 30, ]
      stat_row(
        stat_card("ACTIVE EWBs", sum(e$status != "Expired"),
                  sub = paste("across", dplyr::n_distinct(e$vehicle_id), "vehicles")),
        stat_card("EXPIRING < 24H", sum(e$status == "Expiring Soon"),
                  sub = "renewal reminder sent", accent = "warn"),
        stat_card("EXPIRED", sum(e$status == "Expired"),
                  sub = "blocks dispatch till renewed", accent = "danger"),
        stat_card("GENERATED · 30 DAYS", nrow(recent), sub = "rolling window")
      )
    })

    output$tabs <- renderUI({
      e <- all_rows()
      ks <- c("Valid", "Expiring Soon", "Expired")
      items  <- setNames(c("All", ks), c("All", ks))
      counts <- as.list(c(nrow(e), vapply(ks, function(k) sum(e$status == k), integer(1))))
      names(counts) <- c("All", ks)
      tab_strip(ns, "tab", items, counts, tab())
    })
    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      e <- all_rows()
      if (!identical(tab(), "All")) e <- e[e$status == tab(), ]
      # Soonest expiry first — this screen exists to surface what is about to
      # go stale, not to browse the archive.
      e[order(e$valid_to), ]
    })

    output$gen_btn <- renderUI({
      if (!can(user()$role, "ewaybill", "create")) return(NULL)
      btn_primary(ns("generate"), "Generate Reference", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      e <- rows(); v <- store_get("vehicles")
      # Highlight the expiry timestamp itself when it is the thing at risk.
      vt <- fmt_dt(e$valid_to)
      vt <- ifelse(e$status == "Expired",
                   sprintf('<span style="color:#C0392B;font-weight:650;">%s</span>', vt),
              ifelse(e$status == "Expiring Soon",
                   sprintf('<span style="color:#B3701A;font-weight:650;">%s</span>', vt), vt))

      df <- tibble::tibble(
        `EWB NUMBER` = mono(e$ewb_no),
        `INVOICE NO.` = mono(e$invoice_no),
        `VALID FROM` = fmt_dt(e$valid_from),
        `VALID TO`   = vt,
        GSTIN        = mono(e$gstin),
        VEHICLE      = mono(v$reg_no[match(e$vehicle_id, v$vehicle_id)]),
        `LINKED LR`  = sprintf('<a href="#" style="color:#1667C7;font-weight:600;">%s</a>',
                               htmlEscape(e$lr_no)),
        STATUS       = pill_html(e$status)
      )
      tms_table(df, page = 13)
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    observeEvent(input$tbl_rows_selected, {
      e <- sel(); req(e)
      v <- store_get("vehicles"); cn <- get_consignments()
      c1 <- cn[cn$cn_no == e$cn_no, ]

      showModal(modalDialog(
        title = paste("E-way bill", e$ewb_no), size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-md-6", dl_rows(
              "EWB number"   = span(class = "mono", e$ewb_no),
              "Invoice"      = span(class = "mono", e$invoice_no),
              "GSTIN"        = span(class = "mono", e$gstin),
              "Transporter"  = span(class = "mono", e$transporter_id))),
            div(class = "col-md-6", dl_rows(
              "Vehicle"    = span(class = "mono", v$reg_no[match(e$vehicle_id, v$vehicle_id)]),
              "Linked LR"  = e$lr_no,
              "Valid from" = fmt_dt(e$valid_from),
              "Valid to"   = fmt_dt(e$valid_to),
              "Status"     = pill(e$status)))),

        # The Part-B trail. Validity runs from the latest entry, so the only way
        # to see why a bill is still alive — and who or what kept it alive — is
        # to show every entry rather than the current window alone.
        # A plain table, not a DT. A DataTable inside a modal never gets
        # initialised — the widget's JS runs on page load, so the markup goes in
        # and nothing draws it, leaving a heading over an empty gap. For a trail
        # that is rarely more than three rows the paging and search were never
        # worth anything anyway.
        {
          pb <- ewb_partb(e$ewb_no)
          if (!nrow(pb)) NULL else div(
            class = "mt-3",
            tags$label(class = "form-label", "Part-B entries"),
            tags$table(
              class = "table tms-plain-table",
              tags$thead(tags$tr(lapply(
                c("#", "ENTERED", "VEHICLE", "VALID TO", "SOURCE", "REASON"),
                function(h) tags$th(h)))),
              tags$tbody(lapply(seq_len(nrow(pb)), function(i) {
                r <- pb[i, ]
                tags$tr(
                  tags$td(r$seq),
                  tags$td(fmt_dt(r$entered_dt)),
                  tags$td(span(class = "mono", r$reg_no)),
                  tags$td(fmt_dt(r$valid_to)),
                  tags$td(pill(r$mode, if (identical(r$mode, "Auto")) "blue" else "grey")),
                  tags$td(class = "tiny muted", r$reason))
              }))),
            if (nrow(pb) >= EWB_RENEW_ALERT_AFTER) callout(
              paste(nrow(pb), "Part-B entries on one consignment"),
              "Validity has been reopened this many times, which usually means the load is stuck rather than moving. Worth a look before it is renewed again.",
              "warn") else NULL)
        },
        if (e$status != "Valid") div(class = "mt-3",
          callout(if (e$status == "Expired") "Dispatch blocked" else "Expiring soon",
                  if (e$status == "Expired")
                    "This bill has lapsed. The vehicle cannot legally move on this consignment until it is extended."
                  else "Extend before the validity window closes to avoid a check-post hold.",
                  if (e$status == "Expired") "danger" else "warn")),
        footer = tagList(
          modalButton("Close"),
          # Available whatever the status. Re-entering Part-B is not only a
          # rescue for a lapsed bill — it is also what a transporter files on a
          # vehicle change or a transhipment, and the window reopening is a
          # side effect of that, not the point of it.
          if (can(user()$role, "ewaybill", "edit"))
            btn_primary(ns("extend_one"),
                        if (e$status == "Valid") "Re-enter Part-B" else "Re-enter Part-B and extend")
        )
      ))
      session$userData$ewb_sel <- e$ewb_no
    })

    # Extending a bill *is* filing a fresh Part-B — there is no other mechanism
    # in the rules — so it goes through the same path the automatic sweep uses
    # and leaves the same audit trail. It used to overwrite valid_to in place,
    # which produced a bill that was demonstrably still alive with nothing on
    # record to say why.
    extend <- function(ewb_nos) {
      n <- 0
      for (no in ewb_nos) {
        if (is.null(ewb_partb_add(no, reason = "Validity extended by operator",
                                  mode = "Manual", user_id = user()$user_id))) next
        n <- n + 1
      }
      audit(user()$user_id, "extend", "ewaybill", paste(n, "bills"))
      n
    }

    observeEvent(input$extend_one, {
      if (!require_perm(session, user()$role, "ewaybill", "edit")) return()
      n <- extend(session$userData$ewb_sel)
      removeModal()
      showNotification(sprintf("Part-B re-entered on %d e-way bill. Validity restarts from now.", n),
                       type = "message")
    })

    observeEvent(input$renew_all, {
      if (!require_perm(session, user()$role, "ewaybill", "create")) return()
      e <- all_rows()
      n <- extend(e$ewb_no[e$status == "Expiring Soon"])
      showNotification(sprintf("%d e-way bill%s renewed.", n, if (n == 1) "" else "s"),
                       type = "message")
    })

    observeEvent(input$generate, {
      if (!require_perm(session, user()$role, "ewaybill", "create")) return()
      cn <- scope_all(get_consignments(), user())
      have <- get_ewaybills()$cn_no
      need <- cn[!(cn$cn_no %in% have) & cn$status != "Delivered", ]
      if (!nrow(need)) {
        showNotification("Every active consignment already has an e-way bill.", type = "warning")
        return()
      }
      cl <- store_get("clients")
      showModal(modalDialog(
        title = "Generate e-way bill reference", size = "l", easyClose = TRUE,
        selectInput(ns("g_cn"), "Consignment",
                    setNames(need$cn_no, paste0(need$cn_no, " · ", need$lr_no, " · ",
                                                cl$name[match(need$client_id, cl$client_id)]))),
        callout("GSP not connected",
                "This build records a locally-generated reference. Connect a GST Suvidha Provider in Settings → Integrations to file against the live e-way bill portal.",
                "warn"),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("g_save"), "Generate reference"))
      ))
    })

    observeEvent(input$g_save, {
      if (!require_perm(session, user()$role, "ewaybill", "create")) return()
      req(input$g_cn)
      cn <- get_consignments(); c1 <- cn[cn$cn_no == input$g_cn, ]
      req(nrow(c1)); c1 <- c1[1, ]
      cl <- store_get("clients")
      ref <- safe_ref(12, existing = get_ewaybills()$ewb_no)

      store_insert("ewaybills", list(
        ewb_no = ref,
        invoice_no = paste0("TMS/26-27/", sub("LR-", "", c1$lr_no)),
        cn_no = c1$cn_no, lr_no = c1$lr_no,
        gstin = cl$gstin[match(c1$client_id, cl$client_id)],
        vehicle_id = c1$vehicle_id,
        transporter_id = paste0(substr(setting("company_gstin", ""), 1, 13), "ZT"),
        status = "Valid"
      ))
      # Validity does not start with Part-A — filing the first Part-B is what
      # starts the clock. Setting the window there rather than duplicating the
      # arithmetic here keeps one rule in one place for generation and renewal.
      ewb_partb_add(ref, vehicle_id = c1$vehicle_id,
                    reason  = "First Part-B entered at generation",
                    mode    = "Manual", user_id = user()$user_id)
      audit(user()$user_id, "generate", "ewaybill", paste(ref, "for", c1$lr_no))
      removeModal()
      showNotification(paste("Reference", ref, "generated for", c1$lr_no), type = "message")
    })

    observeEvent(input$upload, {
      showModal(modalDialog(
        title = "Upload e-way bill", easyClose = TRUE,
        fileInput(ns("ewb_file"), "EWB PDF or JSON", accept = c(".pdf", ".json")),
        callout("Manual capture",
                "Use this when a bill was generated outside TMS. The reference is recorded against the consignment for audit.",
                "info"),
        footer = modalButton("Close")
      ))
    })
  })
}
