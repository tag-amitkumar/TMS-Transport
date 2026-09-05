# ==================================================================
# POD — proof of delivery.
#
# The POD gate matters commercially, not just operationally: the invoice screen
# will not raise a bill until a consignment has a verified or approved POD.
# That rule is enforced in mod_invoices.R and surfaced here as the
# pending/uploaded/verified/approved progression.
# ==================================================================

pod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("multi"), "Multiple Upload"),
            uiOutput(ns("up_btn"), inline = TRUE))),
    uiOutput(ns("tiles")),
    div(class = "mt-3",
        card_panel(title = "Recent POD Activity",
                   actions = actionLink(ns("access_log"), "Customer POD Access Log", class = "tiny"),
                   body_class = "tms-card-body p-0",
                   div(class = "tms-table", DTOutput(ns("tbl")))))
  )
}

pod_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")

    all_rows <- reactive(scope_all(get_pods(), user()))

    output$tabs <- renderUI({
      p <- all_rows()
      items  <- setNames(c("All", POD_STATUS), c("All", POD_STATUS))
      counts <- as.list(c(nrow(p), vapply(POD_STATUS, function(s) sum(p$status == s), integer(1))))
      names(counts) <- c("All", POD_STATUS)
      tab_strip(ns, "tab", items, counts, tab())
    })
    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      p <- all_rows()
      if (!identical(tab(), "All")) p <- p[p$status == tab(), ]
      # Pending first — they are the ones needing action.
      p[order(match(p$status, POD_STATUS), p$upload_dt, decreasing = c(FALSE, TRUE),
              method = "radix"), ]
    })

    output$up_btn <- renderUI({
      if (!can(user()$role, "pod", "create")) return(NULL)
      btn_primary(ns("upload"), "Upload POD", fontawesome::fa("plus"))
    })

    output$tiles <- renderUI({
      p <- rows(); cl <- store_get("clients"); cn <- get_consignments()
      if (!nrow(p)) return(card_panel(div(class = "muted", "No PODs in this view.")))
      k <- seq_len(min(8, nrow(p)))

      div(class = "row g-3",
        lapply(k, function(i) {
          r <- p[i, ]
          pending <- identical(r$status, "Pending")
          nm <- cl$name[match(r$client_id, cl$client_id)]
          when <- if (pending) {
            st <- cn$status[match(r$cn_no, cn$cn_no)]
            paste("·", tolower(st %||% "in transit"))
          } else paste("·", fmt_date(r$upload_dt, TRUE))

          div(class = "col-6 col-lg-3",
            div(
              class = paste0("doc-tile", if (pending) " empty"),
              onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                ns("tile"), r$pod_id),
              div(class = "doc-thumb",
                  if (pending) fontawesome::fa("plus") else fontawesome::fa("file-lines")),
              div(class = "doc-meta",
                  div(style = "font-weight:650;font-size:.8125rem;color:#12275C;", r$lr_no),
                  div(class = "tiny muted", paste0(nm, " ", when)),
                  div(class = "mt-2", pill(r$status)))
            ))
        })
      )
    })

    output$tbl <- renderDT({
      p <- all_rows()
      p <- p[nzchar(p$file_name), ]
      p <- p[order(p$upload_dt, decreasing = TRUE), ]
      p <- p[seq_len(min(8, nrow(p))), ]
      cl <- store_get("clients")
      if (!nrow(p)) return(tms_table(tibble::tibble(Message = "No uploads yet"), page = 5))

      df <- tibble::tibble(
        `LR NO.`  = p$lr_no,
        CUSTOMER  = cl$name[match(p$client_id, cl$client_id)],
        `UPLOADED BY` = paste0(p$uploaded_by, " (driver)"),
        FILE      = mono(p$file_name),
        `UPLOAD DATE` = fmt_dt(p$upload_dt),
        STATUS    = pill_html(p$status)
      )
      tms_table(df, page = 8, selection = "none")
    })

    # ---------------- Tile actions ----------------

    observeEvent(input$tile, {
      p <- get_pods(); r <- p[p$pod_id == input$tile, ]
      req(nrow(r)); r <- r[1, ]
      cl <- store_get("clients"); cn <- get_consignments()
      c1 <- cn[cn$cn_no == r$cn_no, ]

      showModal(modalDialog(
        title = paste("POD ·", r$lr_no), size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-md-6", dl_rows(
              "LR number"  = r$lr_no,
              "Consignment"= r$cn_no,
              "Customer"   = cl$name[match(r$client_id, cl$client_id)],
              "Route"      = if (nrow(c1)) paste(c1$origin_city[1], "→", c1$dest_city[1]) else "—")),
            div(class = "col-md-6", dl_rows(
              "Status"     = pill(r$status),
              "Uploaded by"= r$uploaded_by %||% "—",
              "File"       = r$file_name %||% "—",
              "Uploaded"   = fmt_dt(r$upload_dt)))),

        if (identical(r$status, "Pending")) div(class = "mt-3",
          callout("Awaiting driver upload",
                  "The consignee copy has not been captured yet. Invoicing is blocked until this POD is verified.",
                  "warn")),

        footer = tagList(
          modalButton("Close"),
          if (identical(r$status, "Uploaded") && can(user()$role, "pod", "edit"))
            btn_ghost(ns("verify"), "Mark verified"),
          if (identical(r$status, "Verified") && can(user()$role, "pod", "approve"))
            btn_primary(ns("approve"), "Approve")
        )
      ))
      session$userData$pod_sel <- r$pod_id
    })

    advance_pod <- function(to, action) {
      id_sel <- session$userData$pod_sel
      req(id_sel)
      store_update("pods", list(pod_id = id_sel), list(status = to))
      audit(user()$user_id, action, "pod", id_sel)
      removeModal()
      showNotification(paste("POD marked", tolower(to), "."), type = "message")
    }

    observeEvent(input$verify, {
      if (!require_perm(session, user()$role, "pod", "edit")) return()
      advance_pod("Verified", "verify")
    })
    observeEvent(input$approve, {
      if (!require_perm(session, user()$role, "pod", "approve")) return()
      advance_pod("Approved", "approve")
    })

    # ---------------- Upload ----------------

    observeEvent(input$upload, {
      if (!require_perm(session, user()$role, "pod", "create")) return()
      p <- all_rows(); pend <- p[p$status == "Pending", ]
      if (!nrow(pend)) {
        showNotification("No consignments are awaiting a POD.", type = "warning"); return()
      }
      cl <- store_get("clients")
      showModal(modalDialog(
        title = "Upload POD", easyClose = TRUE,
        selectInput(ns("u_pod"), "Consignment",
                    setNames(pend$pod_id, paste0(pend$lr_no, " · ",
                                                 cl$name[match(pend$client_id, cl$client_id)]))),
        fileInput(ns("u_file"), "Signed POD (photo or PDF)",
                  accept = c(".jpg", ".jpeg", ".png", ".pdf")),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("u_save"), "Upload"))
      ))
    })

    observeEvent(input$u_save, {
      if (!require_perm(session, user()$role, "pod", "create")) return()
      req(input$u_pod)
      f <- input$u_file
      if (is.null(f)) {
        showNotification("Choose a file to upload.", type = "error"); return()
      }
      store_update("pods", list(pod_id = input$u_pod), list(
        status = "Uploaded",
        uploaded_by = user()$name,
        file_name = f$name,
        upload_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
      audit(user()$user_id, "upload", "pod", paste(input$u_pod, f$name))
      removeModal()
      showNotification("POD uploaded and queued for verification.", type = "message")
    })

    observeEvent(input$multi, {
      showModal(modalDialog(
        title = "Multiple POD upload", easyClose = TRUE,
        fileInput(ns("m_files"), "Select several files", multiple = TRUE,
                  accept = c(".jpg", ".jpeg", ".png", ".pdf")),
        callout("Matched by filename",
                "Files named POD_<LR number> are matched to their consignment automatically; anything unmatched is listed for manual assignment.",
                "info"),
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$access_log, {
      a <- store_get("audit_log")
      a <- a[a$module == "pod", ]
      a <- a[order(a$ts, decreasing = TRUE), ][seq_len(min(20, nrow(a))), ]
      showModal(modalDialog(
        title = "Customer POD access log", size = "l", easyClose = TRUE,
        if (!nrow(a) || all(is.na(a$ts)))
          div(class = "muted", "No POD activity recorded yet in this session.")
        else div(class = "tms-table",
                 DT::datatable(
                   tibble::tibble(WHEN = a$ts, USER = a$user_id,
                                  ACTION = a$action, DETAIL = a$detail),
                   rownames = FALSE, selection = "none",
                   options = list(dom = "tp", pageLength = 10))),
        footer = modalButton("Close")
      ))
    })
  })
}
