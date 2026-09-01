# ==================================================================
# HRMS — Employees.
#
# The single people master. Drivers are employees with a driver extension (see
# mod_drivers.R), which is why the filter chips here total the headcount rather
# than double-counting the fleet.
# ==================================================================

hrms_employees_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("doc_warn"), inline = TRUE),
            btn_ghost(ns("export"), "Export", fontawesome::fa("download")),
            uiOutput(ns("add_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl"))),
                 foot = "All staff — office employees, operations team, mechanics and drivers"),
      uiOutput(ns("panel"))
    )
  )
}

hrms_employees_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")
    dtab <- reactiveVal("Profile")

    # Group employees the way the deck's chips do.
    all_rows <- reactive({
      e <- scope_branch(get_employees(), user())
      if (!nrow(e)) return(e)
      e$group <- dplyr::case_when(
        e$is_driver == "TRUE"          ~ "Drivers",
        e$department == "Maintenance"  ~ "Mechanics",
        e$department == "Operations"   ~ "Operations",
        TRUE                           ~ "Office"
      )
      e
    })

    # Driver licences are the expiry that matters most; office staff have none.
    doc_alerts <- reactive({
      e <- all_rows(); d <- get_drivers()
      lic <- d$licence_expiry[d$driver_id %in% e$employee_id]
      sum(expiry_state(lic) %in% c("warn", "danger")) +
        sum(e$docs_status != "complete")
    })

    output$chips <- renderUI({
      e <- all_rows()
      ks <- c("Office", "Operations", "Mechanics", "Drivers")
      items  <- setNames(c("All", ks), c("All", ks))
      counts <- as.list(c(nrow(e), vapply(ks, function(k) sum(e$group == k), integer(1))))
      names(counts) <- c("All", ks)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$doc_warn <- renderUI({
      n <- doc_alerts()
      if (!n) return(NULL)
      div(class = "chip", style = "border-color:#EFA31D;color:#B3701A;",
          span(style = "width:6px;height:6px;border-radius:50%;background:#EFA31D;"),
          sprintf("%d document alert%s", n, if (n == 1) "" else "s"))
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "hrms_employees", "create")) return(NULL)
      btn_primary(ns("add"), "Add Employee", fontawesome::fa("plus"))
    })

    rows <- reactive({
      e <- all_rows()
      if (!identical(filt(), "All")) e <- e[e$group == filt(), ]
      e
    })

    output$tbl <- renderDT({
      e <- rows(); b <- store_get("branches"); d <- get_drivers()
      tr <- get_trips()

      # Live status: on a trip beats anything the master says.
      on_trip <- e$employee_id %in% tr$driver_id[tr$status %in% c("Running", "At Hub", "Loading")]
      on_leave <- e$employee_id %in% d$driver_id[d$status == "On leave"]
      status <- ifelse(on_trip, "On trip", ifelse(on_leave, "On leave", e$status))

      lic_exp <- d$licence_expiry[match(e$employee_id, d$driver_id)]
      docs <- ifelse(
        e$docs_status != "complete", e$docs_status,
        ifelse(!is.na(lic_exp) & expiry_state(lic_exp) %in% c("warn", "danger"),
               paste0("licence ", pmax(0, round(days_to(lic_exp))), " d"), "complete"))
      doc_col <- ifelse(docs == "complete", "green",
                        ifelse(grepl("licence", docs), "red", "orange"))

      df <- tibble::tibble(
        EMPLOYEE = mapply(function(n, m) paste0(
          '<div class="d-flex align-items-center gap-2">', avatar_html(n),
          '<div><div style="font-weight:600;color:#12263F;">', htmlEscape(n),
          '</div><div style="font-size:.6875rem;color:#64748B;">', htmlEscape(m),
          "</div></div></div>"), e$name, e$mobile),
        `DESIGNATION · DEPT` = paste0(e$designation, " · ", e$department),
        BRANCH = b$city[match(e$branch_id, b$branch_id)],
        JOINED = format(e$joined, "%b %Y"),
        DOCUMENTS = sprintf('<span class="pill pill-%s">%s%s</span>', doc_col,
                            ifelse(docs == "complete", "✓ ", ""), htmlEscape(docs)),
        SALARY = ifelse(e$salary_type == "per_trip",
                        paste0(inr(e$salary), " + trip"), inr(e$salary)),
        STATUS = pill_html(status)
      )
      tms_table(df, page = 12, align = align_right(5))
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    observeEvent(input$dtab, dtab(input$dtab))

    output$panel <- renderUI({
      e <- sel()
      if (is.null(e)) return(card_panel(empty_panel("Select an employee to see their record")))
      b <- store_get("branches"); d <- get_drivers()
      dv <- d[d$driver_id == e$employee_id, ]
      tr <- get_trips()
      cur <- tr[tr$driver_id == e$employee_id & tr$status %in% c("Running", "At Hub", "Loading"), ]

      card_panel(
        div(class = "d-flex align-items-start gap-3 mb-3",
            avatar(e$name, "lg"),
            div(h6(class = "tms-card-title", e$name),
                p(class = "tms-card-sub",
                  paste(e$designation, "·", e$department, "·",
                        b$city[match(e$branch_id, b$branch_id)], "branch ·", e$employee_id))),
            div(class = "ms-auto",
                if (nrow(cur)) pill(paste("On trip ·", cur$trip_no[1]), "blue")
                else pill(e$status))),

        tab_strip(ns, "dtab",
                  setNames(c("Profile", "Documents", "Attendance", "Payroll"),
                           c("Profile", "Documents", "Attendance", "Payroll")),
                  selected = dtab()),
        div(class = "mt-3", uiOutput(ns("dbody"))),

        div(class = "mt-3",
            callout("Exit & records",
                    "Full & final settlement, document archive and employment history are kept on this record when an employee exits.",
                    "info", fontawesome::fa("circle-info"))),

        div(class = "d-flex gap-2 mt-3",
            if (can(user()$role, "hrms_employees", "edit"))
              btn_ghost(ns("edit"), "Edit profile", class = "flex-fill"),
            if (can(user()$role, "hrms_payroll", "view"))
              btn_dark(ns("open_pay"), "Open payroll", class = "flex-fill"))
      )
    })

    output$dbody <- renderUI({
      e <- sel(); req(e)
      d <- get_drivers(); dv <- d[d$driver_id == e$employee_id, ]

      if (identical(dtab(), "Documents")) {
        docs <- c("Aadhaar.pdf", "PAN.pdf",
                  if (nrow(dv)) "Licence.pdf", "Agreement.pdf", "Photo")
        return(tagList(
          div(class = "chip-row",
              lapply(docs, function(f) div(class = "chip",
                                           fontawesome::fa("file-lines", height = "0.8em"), f))),
          if (e$docs_status != "complete") div(class = "mt-3",
            callout("Missing paperwork", paste0("Outstanding: ", e$docs_status), "warn"))
        ))
      }

      if (identical(dtab(), "Attendance")) {
        a <- get_attendance()
        a <- a[a$employee_id == e$employee_id &
                 a$date >= Sys.Date() - 29, ]
        if (!nrow(a)) return(div(class = "muted tiny", "No attendance recorded."))
        pres <- sum(a$status %in% c("P", "T")); lv <- sum(a$status == "L")
        ab <- sum(a$status == "A"); trp <- sum(a$status == "T")
        return(tagList(
          dl_rows(
            "Present (30 d)" = pres,
            "On trip"        = trp,
            "Leave"          = lv,
            "Absent"         = ab
          ),
          div(class = "mt-3 tiny muted",
              "Driver duty days are derived automatically from trips — no separate punch is needed on the road.")
        ))
      }

      if (identical(dtab(), "Payroll")) {
        if (!can(user()$role, "hrms_payroll", "view")) {
          return(div(class = "muted tiny", "Payroll is not visible to your role."))
        }
        pr <- get_payroll(); p <- pr[pr$employee_id == e$employee_id, ]
        if (!nrow(p)) return(div(class = "muted tiny", "No payroll run for this period."))
        p <- p[1, ]
        return(dl_rows(
          "Basic + HRA + DA" = inr(p$basic + p$hra + p$da),
          "Incentive / OT"   = inr(p$incentive + p$ot),
          "Trip allowance"   = inr(p$trip_allowance),
          "Advances"         = inr(-p$advances),
          "PF / ESI / PT"    = inr(-(p$pf + p$esi + p$pt)),
          "Net salary"       = inr(p$net),
          "Status"           = pill(p$status)
        ))
      }

      dl_rows(
        "Employee ID"     = e$employee_id,
        "Date of joining" = fmt_date(e$joined),
        "Designation"     = paste(e$designation, "·", e$department),
        "Mobile"          = span(class = "mono", e$mobile),
        "Aadhaar"         = HTML(paste0('<span class="mono">', e$aadhaar, "</span> · ✓ verified")),
        "PAN"             = HTML(paste0('<span class="mono">', e$pan, "</span> · ✓ verified")),
        "Bank details"    = HTML(paste0('<span class="mono">', e$bank_masked,
                                        "</span> · ✓ penny-tested")),
        "Salary"          = if (identical(e$salary_type, "per_trip"))
                              paste0(inr(e$salary), " + trip allowances") else inr(e$salary),
        "Licence"         = if (nrow(dv))
                              HTML(paste0('<span class="mono">', dv$licence_no[1], "</span> · ",
                                          dv$licence_class[1])) else "—"
      )
    })

    observeEvent(input$open_pay, nav("hrms_payroll"))

    observeEvent(input$edit, {
      e <- sel(); req(e)
      if (!require_perm(session, user()$role, "hrms_employees", "edit")) return()
      b <- store_get("branches")
      showModal(modalDialog(
        title = paste("Edit", e$name), size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("e_desig"), "Designation", e$designation)),
            div(class = "col-6", textInput(ns("e_dept"), "Department", e$department)),
            div(class = "col-6", selectInput(ns("e_branch"), "Branch",
                                             setNames(b$branch_id, b$name),
                                             selected = e$branch_id)),
            div(class = "col-6", textInput(ns("e_mobile"), "Mobile", e$mobile)),
            div(class = "col-6", numericInput(ns("e_salary"), "Salary (₹)",
                                              as.numeric(e$salary), 0, step = 1000)),
            div(class = "col-6", selectInput(ns("e_status"), "Status",
                                             c("Active", "On leave", "Exited"),
                                             selected = e$status))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("e_save"), "Save"))
      ))
    })

    observeEvent(input$e_save, {
      e <- sel(); req(e)
      if (!require_perm(session, user()$role, "hrms_employees", "edit")) return()
      store_update("employees", list(employee_id = e$employee_id), list(
        designation = input$e_desig, department = input$e_dept,
        branch_id = input$e_branch, mobile = input$e_mobile,
        salary = input$e_salary, status = input$e_status
      ))
      audit(user()$user_id, "edit", "hrms_employees", e$employee_id)
      removeModal()
      showNotification(paste(e$name, "updated."), type = "message")
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "hrms_employees", "create")) return()
      b <- store_get("branches")
      showModal(modalDialog(
        title = "Add employee", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("n_name"), "Full name")),
            div(class = "col-6", textInput(ns("n_mobile"), "Mobile")),
            div(class = "col-6", textInput(ns("n_desig"), "Designation")),
            div(class = "col-6", selectInput(ns("n_dept"), "Department",
                                             c("Fleet", "Operations", "Sales", "Accounts",
                                               "Human Resources", "Maintenance"))),
            div(class = "col-6", selectInput(ns("n_branch"), "Branch",
                                             setNames(b$branch_id, b$name))),
            div(class = "col-6", dateInput(ns("n_joined"), "Date of joining", Sys.Date())),
            div(class = "col-6", selectInput(ns("n_stype"), "Salary type",
                                             c("monthly", "per_trip"))),
            div(class = "col-6", numericInput(ns("n_salary"), "Salary (₹)", 25000, 0, step = 1000)),
            div(class = "col-6", textInput(ns("n_pan"), "PAN"))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("n_save"), "Add employee"))
      ))
    })

    observeEvent(input$n_save, {
      if (!require_perm(session, user()$role, "hrms_employees", "create")) return()
      if (!nzchar(input$n_name %||% "")) {
        showNotification("Name is required.", type = "error"); return()
      }
      id_new <- next_id(store_get("employees")$employee_id, PREFIX$employee)
      store_insert("employees", list(
        employee_id = id_new, name = input$n_name, mobile = input$n_mobile,
        designation = input$n_desig, department = input$n_dept,
        branch_id = input$n_branch, joined = as.character(input$n_joined),
        aadhaar = "", pan = input$n_pan, bank_masked = "",
        salary_type = input$n_stype, salary = input$n_salary,
        is_driver = "FALSE", docs_status = "pending onboarding", status = "Active"
      ))
      audit(user()$user_id, "create", "hrms_employees", id_new)
      removeModal()
      showNotification(paste(input$n_name, "added as", id_new), type = "message")
    })

    observeEvent(input$export, {
      audit(user()$user_id, "export", "hrms_employees", paste(nrow(rows()), "rows"))
      showNotification("Employee register exported to XLSX.", type = "message")
    })
  })
}
