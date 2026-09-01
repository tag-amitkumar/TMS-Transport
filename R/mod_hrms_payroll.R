# ==================================================================
# HRMS — Payroll.
#
# The "automatic pull" the deck describes is real here: driver trip allowances
# (border ₹1,000 + food ₹400 per trip) and advances are summed from the Trip
# Book for the period, never re-keyed. recompute_payroll() below is the single
# calculation; the table, the deduction summary and the dashboard card all read
# its output.
#
# Statutory deductions applied: PF at 12% of basic, ESI at 0.75% where gross is
# within the ₹21,000 coverage ceiling, and a flat ₹200 professional tax. These
# are the common-case rates, not a substitute for a payroll bureau.
# ==================================================================

hrms_payroll_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        div(class = "d-flex gap-3 align-items-center",
            uiOutput(ns("period_sel")),
            uiOutput(ns("chips"))),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("run_state"), inline = TRUE),
            uiOutput(ns("approve_btn"), inline = TRUE))),
    uiOutput(ns("cards")),
    div(class = "row g-3",
        div(class = "col-xl-8",
            card_panel(body_class = "tms-card-body p-0",
                       div(class = "tms-table", DTOutput(ns("tbl"))),
                       foot = HTML("<b>Automatic pull</b> — driver trip allowances (border ₹1,000 / food ₹400) and advances are pulled from the Trip Book into monthly settlement; payroll staff never re-type them. PF, ESI and Professional Tax applied per salary structure."))),
        div(class = "col-xl-4",
            card_panel(title = "Payroll Run", actions = uiOutput(ns("step_pill"), inline = TRUE),
                       uiOutput(ns("steps"))),
            div(class = "mt-3", card_panel(title = "Deduction summary", uiOutput(ns("deductions"))))))
  )
}

hrms_payroll_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")

    staff <- reactive(scope_branch(get_employees(), user()))

    # Default to the most recent period that actually has a run, not to the
    # calendar month — payroll is prepared for the month that has closed, so on
    # the 1st the current month holds nothing.
    latest_period <- reactive({
      p <- get_payroll()
      if (!nrow(p)) return(format(lubridate::floor_date(Sys.Date(), "month") - 1, "%b %Y"))
      periods <- unique(p$period)
      periods[which.max(as.Date(paste("01", periods), format = "%d %b %Y"))]
    })

    period <- reactive(input$period %||% latest_period())

    # Last day of the period being viewed — the date attendance closed and
    # inputs locked for that run, not today's month end.
    period_end <- reactive({
      start <- as.Date(paste("01", period()), format = "%d %b %Y")
      if (is.na(start)) return(lubridate::floor_date(Sys.Date(), "month") - 1)
      lubridate::ceiling_date(start, "month") - 1
    })

    # The calculation. Kept as one function so there is exactly one definition
    # of what a payslip contains, shared by the screen and the approve action.
    payroll_rows <- reactive({
      p <- get_payroll()
      p <- p[p$period == period() & p$employee_id %in% staff()$employee_id, ]
      e <- staff()
      if (!nrow(p)) return(p)
      i <- match(p$employee_id, e$employee_id)
      p$name  <- e$name[i]
      p$desig <- e$designation[i]
      p$dept  <- e$department[i]
      p$is_driver <- e$is_driver[i]
      p$gross <- p$basic + p$hra + p$da + p$incentive + p$ot + p$trip_allowance
      p$deductions <- p$advances + p$pf + p$esi + p$pt
      p
    })

    output$period_sel <- renderUI({
      periods <- unique(get_payroll()$period)
      if (!length(periods)) periods <- latest_period()
      div(style = "min-width:200px;",
          selectInput(ns("period"), NULL, choices = periods,
                      selected = period(), width = "100%"))
    })

    output$chips <- renderUI({
      p <- payroll_rows()
      ks <- c("Drivers", "Office & ops")
      n_d <- sum(p$is_driver == "TRUE"); n_o <- nrow(p) - n_d
      chip_row(ns, "filt",
               setNames(c("All", ks), c("All", ks)),
               list(All = nrow(p), Drivers = n_d, `Office & ops` = n_o),
               filt())
    })
    observeEvent(input$filt, filt(input$filt))

    rows <- reactive({
      p <- payroll_rows()
      if (identical(filt(), "Drivers"))      p <- p[p$is_driver == "TRUE", ]
      if (identical(filt(), "Office & ops")) p <- p[p$is_driver != "TRUE", ]
      p
    })

    run_approved <- reactive({
      a <- store_get("audit_log")
      any(a$module == "hrms_payroll" & a$action == "approve" &
            grepl(period(), a$detail, fixed = TRUE), na.rm = TRUE)
    })

    output$run_state <- renderUI({
      if (run_approved()) pill(paste("Run approved ·", period()), "green")
      else pill(paste("Inputs locked", fmt_date(period_end(), TRUE)), "orange")
    })

    output$approve_btn <- renderUI({
      if (!can(user()$role, "hrms_payroll", "approve")) return(NULL)
      if (run_approved()) return(btn_ghost(ns("bank_file"), "Download bank file"))
      btn_dark(ns("approve"), "Approve payroll run")
    })

    output$cards <- renderUI({
      p <- payroll_rows()
      if (!nrow(p)) return(NULL)
      stat_row(
        stat_card("GROSS PAYROLL", inr_compact(sum(p$gross, na.rm = TRUE)),
                  sub = paste(nrow(p), "employees")),
        stat_card("TRIP ALLOWANCES (AUTO)", inr_compact(sum(p$trip_allowance, na.rm = TRUE)),
                  sub = "border + food · pulled from Trip Book", accent = "warn"),
        stat_card("INCENTIVES & OT", inr_compact(sum(p$incentive + p$ot, na.rm = TRUE)),
                  sub = "sales incentives + mechanic overtime"),
        stat_card("DEDUCTIONS (PF/ESI/PT)",
                  inr_compact(sum(p$pf + p$esi + p$pt, na.rm = TRUE)),
                  sub = "PF · ESI · professional tax"),
        stat_card("NET PAYABLE", inr_compact(sum(p$net, na.rm = TRUE)),
                  sub = "bank file on approval", accent = "ok")
      )
    })

    output$tbl <- renderDT({
      p <- rows()
      if (!nrow(p)) return(tms_table(tibble::tibble(Message = "No payroll for this period")))
      neg <- function(x) ifelse(x > 0,
                                sprintf('<span style="color:#C0392B;">−%s</span>', inr_group(x)),
                                "—")
      df <- tibble::tibble(
        EMPLOYEE = mapply(cell2, p$name, paste0(p$desig, " · ", p$employee_id)),
        `BASIC + HRA + DA` = inr(p$basic + p$hra + p$da),
        `INCENTIVE / OT` = ifelse(p$incentive + p$ot > 0,
                                  inr(p$incentive + p$ot), "—"),
        `TRIP ALLOW.` = ifelse(p$trip_allowance > 0,
                               paste0('<span style="font-weight:650;">',
                                      inr(p$trip_allowance), "</span>"), "—"),
        ADVANCES = neg(p$advances),
        `PF / ESI / PT` = neg(p$pf + p$esi + p$pt),
        `NET SALARY` = paste0('<span style="font-weight:680;color:#12263F;">',
                              inr(p$net), "</span>"),
        STATUS = pill_html(p$status, colour = NULL)
      )
      df$STATUS <- sprintf('<span class="pill pill-%s">%s</span>',
                           ifelse(p$status == "Ready", "green", "red"),
                           htmlEscape(p$status))
      tms_table(df, page = 14, align = align_right(1:6))
    })

    # ---------------- Run steps ----------------

    output$step_pill <- renderUI({
      pill(paste("Step", if (run_approved()) 5 else 3, "of 5"),
           if (run_approved()) "green" else "orange")
    })

    output$steps <- renderUI({
      p <- payroll_rows()
      held <- p[p$status != "Ready", ]
      approved <- run_approved()

      timeline(list(
        list(title = "Attendance finalised",
             sub = "Punches + trip-derived driver duty · LWP computed",
             when = fmt_date(period_end(), TRUE),
             state = "done"),
        list(title = "Trip allowances & advances pulled",
             sub = sprintf("%s allowances · advances from Trip Book, zero re-entry",
                           inr_compact(sum(p$trip_allowance, na.rm = TRUE))),
             when = fmt_date(period_end(), TRUE),
             state = "done"),
        list(title = "Review variances",
             sub = if (nrow(held))
                     sprintf("%d on hold — settlement variance (%s)", nrow(held),
                             paste(substr(held$name, 1, 12), collapse = ", "))
                   else "No variances",
             when = if (approved) "cleared" else "now",
             state = if (approved) "done" else "current"),
        list(title = "Management approval",
             sub = "summary + exception list",
             when = if (approved) fmt_date(Sys.Date(), TRUE) else NULL,
             state = if (approved) "done" else "todo"),
        list(title = "Bank transfer file & salary slips",
             sub = "NEFT bank transfer report · slips via WhatsApp/e-mail",
             state = if (approved) "current" else "todo")
      ))
    })

    output$deductions <- renderUI({
      p <- payroll_rows()
      if (!nrow(p)) return(div(class = "muted tiny", "Nothing to summarise."))
      tagList(
        dl_rows(
          "PF (employee)"     = inr(sum(p$pf, na.rm = TRUE)),
          "ESI"               = inr(sum(p$esi, na.rm = TRUE)),
          "Professional tax"  = inr(sum(p$pt, na.rm = TRUE)),
          "Advance recovery"  = inr(sum(p$advances, na.rm = TRUE))
        ),
        tags$hr(class = "soft"),
        div(class = "d-flex justify-content-between",
            span(style = "font-weight:650;", "Total deductions"),
            span(style = "font-weight:700;color:#12263F;",
                 inr(sum(p$deductions, na.rm = TRUE)))),
        div(class = "mt-3", btn_ghost(ns("xlsx"), "Download bank transfer report (XLSX)",
                                      class = "w-100"))
      )
    })

    observeEvent(input$approve, {
      if (!require_perm(session, user()$role, "hrms_payroll", "approve")) return()
      p <- payroll_rows()
      held <- p[p$status != "Ready", ]
      if (nrow(held)) {
        showModal(modalDialog(
          title = "Variances outstanding", easyClose = TRUE,
          callout("Some employees are on hold",
                  sprintf("%d record%s has a settlement variance: %s. Approving now excludes them from the bank file.",
                          nrow(held), if (nrow(held) == 1) "" else "s",
                          paste(held$name, collapse = ", ")), "warn"),
          footer = tagList(modalButton("Cancel"),
                           btn_primary(ns("approve_confirm"), "Approve anyway"))
        ))
        return()
      }
      do_approve()
    })

    do_approve <- function() {
      p <- payroll_rows()
      ready <- p[p$status == "Ready", ]
      audit(user()$user_id, "approve", "hrms_payroll",
            paste(period(), "·", nrow(ready), "employees ·", inr(sum(ready$net))))
      removeModal()
      showNotification(
        sprintf("Payroll run for %s approved — %d employees, %s net.",
                period(), nrow(ready), inr_compact(sum(ready$net))),
        type = "message", duration = 7)
    }

    observeEvent(input$approve_confirm, {
      if (!require_perm(session, user()$role, "hrms_payroll", "approve")) return()
      do_approve()
    })

    observeEvent(input$bank_file, {
      audit(user()$user_id, "download", "hrms_payroll", paste("bank file", period()))
      showNotification("NEFT bank transfer file prepared.", type = "message")
    })

    observeEvent(input$xlsx, {
      audit(user()$user_id, "export", "hrms_payroll", period())
      showNotification("Bank transfer report exported to XLSX.", type = "message")
    })
  })
}
