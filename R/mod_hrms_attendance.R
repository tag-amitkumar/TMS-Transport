# ==================================================================
# HRMS — Attendance & Leave.
#
# The interesting rule, stated on the deck and implemented here: a driver's duty
# day is DERIVED FROM THE TRIP BOOK, not punched. "T" days in the register come
# from trips.dispatch_dt, so a driver on the road is never marked absent for
# failing to clock in — and payroll picks up the same days.
#
# Legend: P present · A absent · L leave · T on trip (auto) · H holiday
# ==================================================================

hrms_attendance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("cards")),
    div(class = "row g-3",
        div(class = "col-xl-8",
            card_panel(
              title = textOutput(ns("reg_title"), inline = TRUE),
              sub = HTML("P present · A absent · L leave · <b style='color:#1667C7'>T on trip (auto from Trip Book)</b> · H holiday"),
              actions = uiOutput(ns("range_tabs"), inline = TRUE),
              body_class = "tms-card-body p-0",
              div(class = "tms-table", DTOutput(ns("reg"))),
              foot = "Driver duty days are derived automatically from trips — no separate punch needed while on the road. Attendance feeds payroll inputs.")),
        div(class = "col-xl-4",
            card_panel(title = "Leave Requests",
                       actions = uiOutput(ns("pend_pill"), inline = TRUE),
                       uiOutput(ns("requests"))),
            div(class = "mt-3",
                card_panel(title = "Leave Types & Balances",
                           actions = actionLink(ns("policy"), "Policy settings", class = "tiny"),
                           uiOutput(ns("balances")))))
    )
  )
}

hrms_attendance_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rng <- reactiveVal("Week")

    staff <- reactive(scope_branch(get_employees(), user()))

    att <- reactive({
      a <- get_attendance()
      a[a$employee_id %in% staff()$employee_id, , drop = FALSE]
    })

    reqs <- reactive({
      r <- store_get("leave_requests")
      r[r$employee_id %in% staff()$employee_id, , drop = FALSE]
    })

    output$cards <- renderUI({
      a <- att(); s <- staff(); tr <- get_trips(); d <- get_drivers()
      today <- a[a$date == Sys.Date(), ]
      present <- sum(today$status %in% c("P", "T"))
      on_trip <- sum(today$status == "T")
      on_leave <- sum(today$status == "L")
      pend <- sum(reqs()$status == "Pending")
      pct <- if (nrow(s)) round(present / nrow(s) * 100) else 0

      stat_row(
        stat_card("PRESENT TODAY", present, unit = paste0("/ ", nrow(s)),
                  sub = paste0(pct, "% · biometric / manual / mobile punch")),
        stat_card("ON TRIP (DRIVERS)", on_trip,
                  sub = "duty derived automatically from trips"),
        stat_card("ON LEAVE", on_leave,
                  sub = local({
                    lv <- today[today$status == "L", ]
                    if (!nrow(lv)) return("nobody on leave")
                    tb <- table(lv$leave_type[nzchar(lv$leave_type)])
                    if (!length(tb)) return(paste(nrow(lv), "on leave"))
                    paste(paste0(as.integer(tb), " ", tolower(names(tb))), collapse = " · ")
                  })),
        stat_card("PENDING LEAVE REQUESTS", pend,
                  sub = "apply → approve workflow", accent = if (pend) "warn" else "none")
      )
    })

    observeEvent(input$rng, rng(input$rng))

    output$range_tabs <- renderUI({
      tab_strip(ns, "rng", setNames(c("Day", "Week", "Month"), c("Day", "Week", "Month")),
                selected = rng())
    })

    period <- reactive({
      switch(rng(),
        "Day"   = Sys.Date(),
        "Month" = seq(lubridate::floor_date(Sys.Date(), "month"), Sys.Date(), by = "day"),
        seq(Sys.Date() - 5, Sys.Date(), by = "day"))
    })

    output$reg_title <- renderText({
      p <- period()
      if (length(p) == 1) paste("Attendance Register —", fmt_date(p))
      else sprintf("Attendance Register — %s to %s",
                   format(min(p), "%d"), format(max(p), "%d %b"))
    })

    output$reg <- renderDT({
      a <- att(); s <- staff(); p <- period(); b <- store_get("branches")
      s <- s[order(s$is_driver == "FALSE", s$name), ]
      s <- s[seq_len(min(14, nrow(s))), ]
      tr <- get_trips()

      # Build one column per day in the window.
      cells <- lapply(p, function(d) {
        vapply(s$employee_id, function(e) {
          r <- a[a$employee_id == e & a$date == d, ]
          if (!nrow(r)) return("")
          st <- r$status[1]
          if (identical(st, "off")) return('<span class="tiny faint">off</span>')
          col <- c(P = "green", A = "red", L = "orange", T = "blue", H = "grey")[st]
          lbl <- if (identical(st, "T") && nzchar(r$trip_no[1] %||% ""))
                   paste0("T · ", r$trip_no[1])
                 else if (identical(st, "L") && nzchar(r$leave_type[1] %||% ""))
                   paste0("L · ", tolower(r$leave_type[1]))
                 else st
          sprintf('<span class="pill pill-%s nodot">%s</span>', col, lbl)
        }, character(1), USE.NAMES = FALSE)
      })

      # Duty days = present + on-trip, which is the figure payroll consumes.
      days <- vapply(s$employee_id, function(e) {
        r <- a[a$employee_id == e & a$date %in% p, ]
        sum(r$status %in% c("P", "T")) + 0.5 * sum(r$status == "H")
      }, numeric(1), USE.NAMES = FALSE)

      df <- tibble::tibble(
        EMPLOYEE = mapply(function(n, ds, br) paste0(
          '<div style="font-weight:600;color:#12263F;">', htmlEscape(n),
          '</div><div style="font-size:.6875rem;color:#64748B;">', htmlEscape(ds),
          " · ", htmlEscape(br), "</div>"),
          s$name, s$designation, b$city[match(s$branch_id, b$branch_id)])
      )
      for (i in seq_along(p)) {
        df[[toupper(format(p[i], "%a %d"))]] <- cells[[i]]
      }
      df$DAYS <- format(days, nsmall = 1)

      tms_table(df, page = 14, selection = "none",
                align = align_right(ncol(df) - 1))
    })

    # ---------------- Leave requests ----------------

    output$pend_pill <- renderUI({
      n <- sum(reqs()$status == "Pending")
      pill(paste(n, "pending"), if (n) "orange" else "green")
    })

    output$requests <- renderUI({
      r <- reqs(); r <- r[r$status == "Pending", ]
      e <- staff(); lt <- store_get("leave_types")
      if (!nrow(r)) return(div(class = "muted tiny", "No pending leave requests."))
      r <- r[order(r$from_date), ][seq_len(min(4, nrow(r))), ]

      div(
        lapply(seq_len(nrow(r)), function(i) {
          x <- r[i, ]
          emp <- e[e$employee_id == x$employee_id, ]
          nm  <- if (nrow(emp)) emp$name[1] else x$employee_id
          dept <- if (nrow(emp)) emp$department[1] else ""

          # Overlap warning: a driver with a trip in the window needs the roster
          # checked before the leave is granted.
          tr <- get_trips()
          clash <- tr[tr$driver_id == x$employee_id & !is.na(tr$dispatch_dt) &
                        as.Date(tr$dispatch_dt) >= as.Date(x$from_date) &
                        as.Date(tr$dispatch_dt) <= as.Date(x$to_date), ]

          div(
            class = "mb-3 pb-3",
            style = if (i < nrow(r)) "border-bottom:1px solid #EEF2F7;" else "",
            div(class = "d-flex align-items-center gap-2 mb-1",
                avatar(nm, "sm"),
                div(div(style = "font-weight:600;font-size:.8125rem;color:#12263F;",
                        paste0(nm, " · ", dept)),
                    div(class = "tiny muted",
                        paste0(fmt_date(x$from_date, TRUE), "–", fmt_date(x$to_date, TRUE),
                               " · “", x$reason, "”"))),
                div(class = "ms-auto",
                    pill(paste0(lt$name[match(x$leave_type, lt$code)] %||% x$leave_type,
                                " · ", x$days, " d"), "grey"))),
            if (nrow(clash)) div(class = "tiny mb-2", style = "color:#C0392B;font-weight:600;",
                                 paste("check trip roster —", clash$trip_no[1])),
            if (can(user()$role, "hrms_attendance", "approve")) div(
              class = "d-flex gap-2",
              actionButton(ns(paste0("rej_", x$lr_id)), "Reject",
                           class = "btn-tms-ghost btn-tms-sm flex-fill",
                           onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                             ns("reject"), x$lr_id)),
              actionButton(ns(paste0("app_", x$lr_id)), "Approve",
                           class = "btn btn-tms-sm flex-fill",
                           style = "background:#1A7F45;color:#fff;border:none;font-weight:600;",
                           onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                             ns("approve"), x$lr_id))
            )
          )
        })
      )
    })

    decide <- function(lr_id, status) {
      if (!require_perm(session, user()$role, "hrms_attendance", "approve")) return()
      r <- store_get("leave_requests")
      x <- r[r$lr_id == lr_id, ]
      if (!nrow(x)) return()
      x <- x[1, ]
      store_update("leave_requests", list(lr_id = lr_id), list(status = status))

      # An approved leave writes the L days into the register so attendance and
      # payroll agree without a second step.
      if (identical(status, "Approved")) {
        days <- seq(as.Date(x$from_date), as.Date(x$to_date), by = "day")
        a <- store_get("attendance")
        for (d in as.character(days)) {
          hit <- a$employee_id == x$employee_id & a$date == d
          if (any(hit, na.rm = TRUE)) {
            store_update("attendance", list(employee_id = x$employee_id, date = d),
                         list(status = "L", leave_type = x$leave_type))
          } else {
            store_insert("attendance", list(employee_id = x$employee_id, date = d,
                                            status = "L", trip_no = "",
                                            leave_type = x$leave_type))
          }
        }
      }
      audit(user()$user_id, tolower(status), "hrms_attendance", lr_id)
      showNotification(sprintf("Leave request %s %s.", lr_id, tolower(status)),
                       type = "message")
    }

    observeEvent(input$approve, decide(input$approve, "Approved"))
    observeEvent(input$reject,  decide(input$reject,  "Rejected"))

    # ---------------- Balances ----------------

    output$balances <- renderUI({
      lt <- store_get("leave_types")
      r <- reqs(); r <- r[r$status == "Approved", ]
      n_staff <- max(1, nrow(staff()))

      div(class = "tms-table",
        DT::datatable(
          tibble::tibble(
            TYPE = lt$name,
            `ANNUAL QUOTA` = lt$annual_quota,
            `AVG USED` = vapply(lt$code, function(c) {
              used <- sum(as.numeric(r$days[r$leave_type == c]), na.rm = TRUE)
              format(round(used / n_staff, 1), nsmall = 1)
            }, character(1), USE.NAMES = FALSE)),
          rownames = FALSE, selection = "none",
          options = list(dom = "t", pageLength = 5,
                         columnDefs = list(list(className = "dt-right", targets = 1:2)))))
    })

    observeEvent(input$policy, {
      showModal(modalDialog(
        title = "Leave policy", easyClose = TRUE,
        p(class = "tiny muted",
          "Annual quotas are set per leave type and apply company-wide. Editing a quota does not retrospectively change balances already accrued."),
        div(class = "tms-table",
            DT::datatable(store_get("leave_types"), rownames = FALSE, selection = "none",
                          options = list(dom = "t"))),
        footer = modalButton("Close")
      ))
    })
  })
}
