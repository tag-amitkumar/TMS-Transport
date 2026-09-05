# ==================================================================
# Vendors — vehicle owners, agreements, bank details and document status.
# ==================================================================

vendors_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Vendor Master", textOutput(ns("sub"), inline = TRUE),
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

vendors_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rows <- reactive(scope_all(store_get("vendors"), user()))

    veh_count <- reactive({
      store_get("vehicles") |>
        dplyr::filter(nzchar(vendor_id)) |>
        dplyr::count(vendor_id, name = "n")
    })

    output$sub <- renderText({
      sprintf("%d vendors · vehicle owners, agreements & payments", nrow(rows()))
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "vendors", "create")) return(NULL)
      btn_primary(ns("add"), "Add Vendor", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      v <- rows(); vc <- veh_count()
      n_veh <- tidyr::replace_na(vc$n[match(v$vendor_id, vc$vendor_id)], 0L)
      df <- tibble::tibble(
        VENDOR = mapply(function(n, m) paste0(
          '<div class="d-flex align-items-center gap-2">', avatar_html(n),
          '<div><div style="font-weight:600;color:#12275C;">', htmlEscape(n),
          '</div><div style="font-size:.6875rem;color:#64748B;">', htmlEscape(m),
          "</div></div></div>"), v$name, v$mobile),
        `VEHICLE OWNER` = v$owner_name,
        GSTIN    = mono(v$gstin),
        VEHICLES = n_veh,
        `PAYMENT TERMS` = v$payment_terms,
        STATUS   = pill_html(v$status)
      )
      tms_table(df, page = 12, align = align_right(3))
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    output$panel <- renderUI({
      v <- sel()
      if (is.null(v)) return(card_panel(empty_panel("Select a vendor to see their fleet")))

      veh <- get_vehicles(); veh <- veh[veh$vendor_id == v$vendor_id, ]
      docs <- store_get("vendor_documents")
      docs <- docs[docs$vendor_id == v$vendor_id, ]

      card_panel(
        div(class = "d-flex align-items-start gap-3 mb-3",
            avatar(v$name, "lg"),
            div(h6(class = "tms-card-title", v$name),
                p(class = "tms-card-sub",
                  paste("Vehicle owner ·", v$city, "· vendor since",
                        format(as.Date(v$since), "%Y")))),
            div(class = "ms-auto", pill(paste(nrow(veh), "vehicles"), "orange"))),

        if (nrow(veh)) div(
          class = "tms-table mb-3",
          DT::datatable(
            tibble::tibble(
              VEHICLE = mono(veh$reg_no),
              TYPE    = veh$body,
              STATUS  = pill_html(veh$status)),
            rownames = FALSE, selection = "none", escape = FALSE,
            options = list(dom = "t", pageLength = 6))
        ) else div(class = "muted tiny mb-3", "No vehicles registered to this vendor."),

        dl_rows(
          "Bank name"     = v$bank_name,
          "Account no."   = span(class = "mono", v$account_no),
          "IFSC"          = span(class = "mono", v$ifsc),
          "PAN"           = span(class = "mono", v$pan),
          "Payment terms" = v$payment_terms
        ),

        tags$hr(class = "soft"),
        div(class = "form-section", "Documents"),
        div(
          lapply(seq_len(nrow(docs)), function(i) {
            div(class = "d-flex align-items-center justify-content-between py-1",
                span(style = "font-size:.8125rem;", docs$doc_type[i]),
                pill(docs$status[i], if (docs$status[i] == "Uploaded") "green" else "orange"))
          })
        ),

        # Vendor settlement is computed from completed trips, not stored — the
        # same figure the Payments & Ledger screen shows.
        local({
          vp <- get_vendor_payments()
          vp <- vp[vp$vendor_id == v$vendor_id, ]
          if (!nrow(vp)) return(NULL)
          tagList(
            tags$hr(class = "soft"),
            div(class = "d-flex align-items-center justify-content-between",
                div(div(class = "small-caps", "Pending settlement"),
                    div(style = "font-weight:680;font-size:1.1rem;color:#12275C;",
                        inr(sum(vp$amount)))),
                pill(vp$status[1], if (vp$status[1] == "Approved") "green" else "orange"))
          )
        })
      )
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "vendors", "create")) return()
      b <- store_get("branches")
      showModal(modalDialog(
        title = "Add vendor", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("n_name"), "Vendor name")),
            div(class = "col-6", textInput(ns("n_owner"), "Vehicle owner")),
            div(class = "col-6", textInput(ns("n_mobile"), "Mobile")),
            div(class = "col-6", textInput(ns("n_gstin"), "GSTIN")),
            div(class = "col-6", textInput(ns("n_pan"), "PAN")),
            div(class = "col-6", selectInput(ns("n_terms"), "Payment terms",
                                             c("15-day credit", "30-day credit",
                                               "Advance + 7-day", "To-pay"))),
            div(class = "col-6", textInput(ns("n_bank"), "Bank name")),
            div(class = "col-6", textInput(ns("n_acct"), "Account no.")),
            div(class = "col-6", textInput(ns("n_ifsc"), "IFSC")),
            div(class = "col-6", selectInput(ns("n_branch"), "Branch",
                                             setNames(b$branch_id, b$name)))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("save"), "Save vendor"))
      ))
    })

    observeEvent(input$save, {
      if (!require_perm(session, user()$role, "vendors", "create")) return()
      if (!nzchar(input$n_name %||% "")) {
        showNotification("Vendor name is required.", type = "error"); return()
      }
      id_new <- next_id(store_get("vendors")$vendor_id, "VND-")
      store_insert("vendors", list(
        vendor_id = id_new, name = input$n_name, owner_name = input$n_owner,
        mobile = input$n_mobile, gstin = input$n_gstin, pan = input$n_pan,
        city = "", branch_id = input$n_branch, payment_terms = input$n_terms,
        bank_name = input$n_bank, account_no = input$n_acct, ifsc = input$n_ifsc,
        since = as.character(Sys.Date()), status = "Active"
      ))
      # Every vendor needs the same three documents tracked from day one.
      for (d in c("Vendor Agreement", "PAN Card", "GST Certificate")) {
        store_insert("vendor_documents", list(vendor_id = id_new, doc_type = d, status = "Pending"))
      }
      audit(user()$user_id, "create", "vendors", id_new)
      removeModal()
      showNotification(paste("Vendor", input$n_name, "added."), type = "message")
    })

    observeEvent(input$filters, {
      showModal(modalDialog(title = "Filter vendors", easyClose = TRUE,
        selectInput(ns("f_terms"), "Payment terms",
                    c("All", unique(store_get("vendors")$payment_terms))),
        footer = modalButton("Close")))
    })
  })
}
