# ==================================================================
# Complaints — SLA-tracked support kanban.
#
# SLA is two days from being raised, which the deck's target dates imply. Breach
# is computed from the dates on each read, so a ticket that goes past SLA while
# the screen is open turns red on the next refresh rather than staying stale.
# ==================================================================

complaints_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("sla_warn"), inline = TRUE),
            uiOutput(ns("new_btn"), inline = TRUE))),
    uiOutput(ns("board"))
  )
}

complaints_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All open")

    all_rows <- reactive({
      c <- scope_all(get_complaints(), user())
      if (!nrow(c)) return(c)
      # Breach: open past target, or resolved after target.
      c$overdue <- ifelse(
        c$status == "Resolved",
        !is.na(c$resolved_dt) & !is.na(c$target_dt) & c$resolved_dt > c$target_dt,
        !is.na(c$target_dt) & c$target_dt < Sys.Date()
      )
      c
    })

    output$cards <- renderUI({
      c <- all_rows()
      open <- c[c$status != "Resolved", ]
      res  <- c[c$status == "Resolved", ]
      week <- res[!is.na(res$resolved_dt) & res$resolved_dt >= Sys.Date() - 7, ]
      # Mean days from raised to resolved, over what has actually closed.
      art <- if (nrow(res)) mean(as.numeric(res$resolved_dt - res$raised_dt), na.rm = TRUE) else NA
      within <- if (nrow(week)) round(mean(!week$overdue) * 100) else 100

      stat_row(
        stat_card("OPEN COMPLAINTS", nrow(open),
                  sub = paste(sum(open$raised_dt == Sys.Date(), na.rm = TRUE), "raised today")),
        stat_card("OVERDUE VS SLA", sum(open$overdue, na.rm = TRUE),
                  sub = "escalation triggered", accent = "danger"),
        stat_card("AVG RESOLUTION TIME",
                  if (is.na(art)) "—" else format(round(art, 1), nsmall = 1),
                  unit = "days", sub = "raised to closed"),
        stat_card("RESOLVED THIS WEEK", nrow(week),
                  sub = paste0(within, "% within SLA"), accent = "ok")
      )
    })

    output$chips <- renderUI({
      c <- all_rows(); open <- c[c$status != "Resolved", ]
      ks <- c("High", "Medium", "Low")
      items  <- setNames(c("All open", ks), c("All open", paste(ks, "priority")))
      counts <- as.list(c(nrow(open), vapply(ks, function(k) sum(open$priority == k), integer(1))))
      names(counts) <- c("All open", ks)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$sla_warn <- renderUI({
      c <- all_rows(); n <- sum(c$status != "Resolved" & c$overdue, na.rm = TRUE)
      if (!n) return(NULL)
      div(class = "chip", style = "border-color:#C0392B;color:#C0392B;",
          span(style = "width:6px;height:6px;border-radius:50%;background:#C0392B;"),
          sprintf("%d past SLA", n))
    })

    output$new_btn <- renderUI({
      if (!can(user()$role, "complaints", "create")) return(NULL)
      btn_primary(ns("register"), "Register Complaint", fontawesome::fa("plus"))
    })

    output$board <- renderUI({
      c <- all_rows()
      if (!identical(filt(), "All open")) c <- c[c$priority == filt() | c$status == "Resolved", ]
      cl <- store_get("clients"); u <- store_get("users")

      make_cards <- function(lane) {
        rowsl <- c[c$status == lane, ]
        rowsl <- rowsl[order(rowsl$raised_dt, decreasing = TRUE), ]
        shown <- rowsl[seq_len(min(10, nrow(rowsl))), , drop = FALSE]
        if (!nrow(shown)) return(div(class = "muted tiny px-1", "Nothing here"))

        lapply(seq_len(nrow(shown)), function(i) {
          r <- shown[i, ]
          nm <- cl$name[match(r$client_id, cl$client_id)]
          who <- if (nzchar(r$assigned_to %||% "")) u$name[match(r$assigned_to, u$user_id)] else NULL

          kanban_card(
            ns, "card", r$complaint_id,
            title = paste(r$complaint_id, "·", nm),
            meta = paste0(r$lr_no, " · ", r$subject),
            footer = tagList(
              pill(r$category, "grey"),
              if (lane %in% c("New", "Assigned")) pill(r$priority),
              if (!is.null(who)) tagList(avatar(who, "sm"), span(class = "tiny muted", who)),
              if (lane == "In Progress" && isTRUE(r$overdue))
                pill("past SLA", "red"),
              if (lane == "Resolved") span(class = "tiny", style = "color:#1A7F45;",
                                           HTML("&#10003; "), r$resolution),
              if (lane != "Resolved")
                span(class = "ms-auto tiny faint",
                     paste("target", fmt_date(r$target_dt, TRUE)))
            ),
            rail = if (isTRUE(r$overdue) && lane != "Resolved") "red"
                   else if (lane == "Resolved") "green" else NULL
          )
        })
      }

      # Progress bar under in-progress cards, as the deck draws.
      ks  <- COMPLAINT_STATUS
      col <- c("red", "blue", "orange", "green")
      do.call(kanban_board, lapply(seq_along(ks), function(i) {
        kanban_col(ks[i], col[i], make_cards(ks[i]), note = sum(c$status == ks[i]))
      }))
    })

    observeEvent(input$card, {
      c <- get_complaints(); r <- c[c$complaint_id == input$card, ]
      req(nrow(r)); r <- r[1, ]
      cl <- store_get("clients"); u <- store_get("users"); b <- store_get("branches")
      overdue <- if (identical(r$status, "Resolved")) {
        !is.na(r$resolved_dt) && r$resolved_dt > r$target_dt
      } else !is.na(r$target_dt) && r$target_dt < Sys.Date()

      showModal(modalDialog(
        title = paste(r$complaint_id, "·", cl$name[match(r$client_id, cl$client_id)]),
        size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-md-6", dl_rows(
              "Consignment" = r$lr_no,
              "Branch"      = b$name[match(r$branch_id, b$branch_id)],
              "Category"    = r$category,
              "Priority"    = pill(r$priority),
              "Status"      = pill(r$status))),
            div(class = "col-md-6", dl_rows(
              "Raised"      = fmt_date(r$raised_dt),
              "SLA target"  = fmt_date(r$target_dt),
              "Assigned to" = if (nzchar(r$assigned_to %||% ""))
                                u$name[match(r$assigned_to, u$user_id)] else "Unassigned",
              "Resolved"    = fmt_date(r$resolved_dt),
              "Progress"    = paste0(r$progress_pct, "%")))),
        div(class = "mt-3", p(style = "font-size:.875rem;", r$subject)),
        if (isTRUE(overdue)) div(class = "mt-2",
          callout("Past SLA",
                  sprintf("Target was %s. Escalation has been triggered.", fmt_date(r$target_dt)),
                  "danger")),
        if (nzchar(r$resolution %||% "")) div(class = "mt-2",
          callout("Resolution", r$resolution, "ok")),
        footer = tagList(
          modalButton("Close"),
          if (r$status != "Resolved" && can(user()$role, "complaints", "edit"))
            btn_ghost(ns("assign"), "Assign / update"),
          if (r$status != "Resolved" && can(user()$role, "complaints", "approve"))
            btn_primary(ns("resolve"), "Mark resolved")
        )
      ))
      session$userData$cmp_sel <- r$complaint_id
    })

    observeEvent(input$assign, {
      id_sel <- session$userData$cmp_sel; req(id_sel)
      if (!require_perm(session, user()$role, "complaints", "edit")) return()
      c <- get_complaints(); r <- c[c$complaint_id == id_sel, ][1, ]
      u <- scope_branch(store_get("users"), user())
      showModal(modalDialog(
        title = paste("Update", id_sel), easyClose = TRUE,
        selectInput(ns("a_who"), "Assign to", setNames(u$user_id, u$name),
                    selected = r$assigned_to),
        selectInput(ns("a_status"), "Status", setdiff(COMPLAINT_STATUS, "Resolved"),
                    selected = r$status),
        sliderInput(ns("a_prog"), "Progress %", 0, 100,
                    value = as.numeric(r$progress_pct %||% 0), step = 5),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("a_save"), "Save"))
      ))
    })

    observeEvent(input$a_save, {
      id_sel <- session$userData$cmp_sel; req(id_sel)
      if (!require_perm(session, user()$role, "complaints", "edit")) return()
      store_update("complaints", list(complaint_id = id_sel), list(
        assigned_to = input$a_who, status = input$a_status, progress_pct = input$a_prog
      ))
      audit(user()$user_id, "update", "complaints", id_sel)
      removeModal()
      showNotification(paste(id_sel, "updated."), type = "message")
    })

    observeEvent(input$resolve, {
      id_sel <- session$userData$cmp_sel; req(id_sel)
      if (!require_perm(session, user()$role, "complaints", "approve")) return()
      showModal(modalDialog(
        title = paste("Resolve", id_sel), easyClose = TRUE,
        textAreaInput(ns("r_note"), "Resolution note", rows = 3,
                      placeholder = "What was done and how the customer was informed"),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("r_save"), "Mark resolved"))
      ))
    })

    observeEvent(input$r_save, {
      id_sel <- session$userData$cmp_sel; req(id_sel)
      if (!require_perm(session, user()$role, "complaints", "approve")) return()
      if (!nzchar(input$r_note %||% "")) {
        showNotification("A resolution note is required.", type = "error"); return()
      }
      store_update("complaints", list(complaint_id = id_sel), list(
        status = "Resolved", progress_pct = 100,
        resolved_dt = as.character(Sys.Date()),
        resolution = input$r_note
      ))
      audit(user()$user_id, "resolve", "complaints", id_sel)
      removeModal()
      showNotification(paste(id_sel, "resolved."), type = "message")
    })

    observeEvent(input$register, {
      if (!require_perm(session, user()$role, "complaints", "create")) return()
      cn <- scope_all(get_consignments(), user())
      cl <- store_get("clients")
      showModal(modalDialog(
        title = "Register complaint", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-12",
                selectizeInput(ns("c_cn"), "Consignment",
                               choices = setNames(cn$cn_no,
                                                  paste0(cn$lr_no, " · ",
                                                         cl$name[match(cn$client_id, cl$client_id)],
                                                         " · ", cn$origin_city, " → ", cn$dest_city)),
                               width = "100%")),
            div(class = "col-6", selectInput(ns("c_cat"), "Category",
                                             c("Delay", "Damaged Goods", "POD Issue",
                                               "Billing Issue", "Driver Complaint",
                                               "Wrong Delivery", "Service Complaint"))),
            div(class = "col-6", selectInput(ns("c_pri"), "Priority",
                                             c("High", "Medium", "Low"), selected = "Medium")),
            div(class = "col-12", textAreaInput(ns("c_sub"), "Description", rows = 3))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("c_save"), "Register"))
      ))
    })

    observeEvent(input$c_save, {
      if (!require_perm(session, user()$role, "complaints", "create")) return()
      if (!nzchar(input$c_cn %||% "") || !nzchar(input$c_sub %||% "")) {
        showNotification("Consignment and description are required.", type = "error"); return()
      }
      cn <- get_consignments(); c1 <- cn[cn$cn_no == input$c_cn, ][1, ]
      # SLA target is two days from now, matching the deck's raised/target gap.
      sla_days <- if (identical(input$c_pri, "High")) 1 else 2
      id_new <- next_id(store_get("complaints")$complaint_id, PREFIX$complaint)

      store_insert("complaints", list(
        complaint_id = id_new, client_id = c1$client_id, lr_no = c1$lr_no,
        cn_no = c1$cn_no, branch_id = c1$branch_id,
        category = input$c_cat, priority = input$c_pri, subject = input$c_sub,
        raised_dt = as.character(Sys.Date()),
        target_dt = as.character(Sys.Date() + sla_days),
        resolved_dt = "", assigned_to = "", progress_pct = 0,
        resolution = "", status = "New"
      ))
      audit(user()$user_id, "create", "complaints", id_new)
      removeModal()
      showNotification(paste("Complaint", id_new, "registered · SLA",
                             fmt_date(Sys.Date() + sla_days, TRUE)), type = "message")
    })
  })
}
