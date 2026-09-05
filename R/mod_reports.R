# ==================================================================
# Reports & Analytics — six report groups, as the deck lays them out.
#
# Every figure is computed live from the operational tables rather than from a
# reporting snapshot, so a report can never disagree with the screen it came
# from. Export writes a real XLSX via openxlsx.
# ==================================================================

reports_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Reports & Analytics", textOutput(ns("sub"), inline = TRUE),
              actions = tagList(
                btn_ghost(ns("schedule"), "Schedule Report", fontawesome::fa("clock")),
                downloadButton(ns("export_all"), "Export All",
                               class = "btn-tms-primary", icon = fontawesome::fa("download"))
              )),
    div(class = "row g-3",
        div(class = "col-xl-4", card_panel(
          title = "Booking Reports", sub = "Daily bookings · last 7 days",
          actions = downloadButton(ns("dl_book"), "Export", class = "btn-tms-ghost btn-tms-sm"),
          plotlyOutput(ns("p_book"), height = 210),
          div(class = "chip-row mt-3",
              div(class = "chip", "Branch-wise"), div(class = "chip", "Customer-wise"),
              div(class = "chip", "User-wise")))),
        div(class = "col-xl-4", card_panel(
          title = "Consignment Reports", sub = "By status · last 30 days",
          actions = downloadButton(ns("dl_cn"), "Export", class = "btn-tms-ghost btn-tms-sm"),
          uiOutput(ns("t_cn")))),
        div(class = "col-xl-4", card_panel(
          title = "Vehicle Reports", sub = "Utilisation by body type",
          actions = downloadButton(ns("dl_veh"), "Export", class = "btn-tms-ghost btn-tms-sm"),
          plotlyOutput(ns("p_veh"), height = 210),
          div(class = "chip-row mt-3",
              div(class = "chip", "Running Trips"), div(class = "chip", "Maintenance"),
              div(class = "chip", "Fuel Reports"))))),

    div(class = "row g-3 mt-0",
        div(class = "col-xl-4", card_panel(
          title = "Financial Reports", sub = "Customer outstanding · top accounts",
          actions = downloadButton(ns("dl_fin"), "Export", class = "btn-tms-ghost btn-tms-sm"),
          uiOutput(ns("t_fin")),
          div(class = "chip-row mt-3",
              div(class = "chip", "Vendor Payments"), div(class = "chip", "Booking Revenue"),
              div(class = "chip", "Branch Revenue"), div(class = "chip", "Profit Analysis")))),
        div(class = "col-xl-4", card_panel(
          title = "Complaint Reports", sub = "SLA compliance % by branch",
          actions = downloadButton(ns("dl_cmp"), "Export", class = "btn-tms-ghost btn-tms-sm"),
          plotlyOutput(ns("p_cmp"), height = 210),
          div(class = "chip-row mt-3",
              div(class = "chip", "Pending"), div(class = "chip", "Resolved"),
              div(class = "chip", "Satisfaction")))),
        div(class = "col-xl-4", card_panel(
          title = "HR Reports", sub = "Attendance % by department",
          actions = downloadButton(ns("dl_hr"), "Export", class = "btn-tms-ghost btn-tms-sm"),
          uiOutput(ns("t_hr")),
          div(class = "chip-row mt-3",
              div(class = "chip", "Leave"), div(class = "chip", "Payroll"),
              div(class = "chip", "Employee Performance")))))
  )
}

reports_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    bk  <- reactive(scope_branch(get_bookings(), user()))
    cn  <- reactive(scope_branch(get_consignments(), user()))
    veh <- reactive(scope_branch(get_vehicles(), user()))
    inv <- reactive(scope_branch(get_invoices(), user()))
    cmp <- reactive(scope_branch(get_complaints(), user()))
    emp <- reactive(scope_branch(get_employees(), user()))

    output$sub <- renderText("6 report groups · data current as of today")

    # ---------------- Bookings ----------------

    book_data <- reactive({
      b <- bk()
      days <- seq(Sys.Date() - 6, Sys.Date(), by = "day")
      tibble::tibble(
        day = format(days, "%a"),
        date = days,
        n = vapply(days, function(d) sum(b$booking_date == d, na.rm = TRUE), integer(1))
      )
    })

    output$p_book <- renderPlotly({
      d <- book_data()
      d$col <- ifelse(d$n == max(d$n), "#12263F", "#AFC6DE")
      plot_ly(d, x = ~day, y = ~n, type = "bar", marker = list(color = ~col),
              hoverinfo = "y",
              text = ~ifelse(n == max(n), n, ""), textposition = "outside",
              textfont = list(size = 11, color = "#12263F")) |>
        layout(xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                            tickfont = list(size = 11, color = "#64748B")),
               yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
               margin = list(l = 4, r = 4, t = 20, b = 4), bargap = .45,
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    # ---------------- Consignments ----------------

    cn_data <- reactive({
      c <- cn()
      c <- c[!is.na(c$dispatch_date) &
               c$dispatch_date >= since_30d(), ]
      grp <- c("In Transit", "Delivered", "Pending")
      n <- c(sum(c$status %in% c("In Transit", "At Hub", "Out for Delivery", "Dispatched")),
             sum(c$status == "Delivered"),
             sum(c$status == "In Prep"))
      tibble::tibble(status = grp, count = n,
                     share = if (sum(n)) round(n / sum(n) * 100) else 0)
    })

    output$t_cn <- renderUI({
      d <- cn_data()
      div(class = "tms-table",
        DT::datatable(
          tibble::tibble(STATUS = pill_html(d$status), COUNT = d$count,
                         SHARE = paste0(d$share, "%")),
          rownames = FALSE, selection = "none", escape = FALSE,
          options = list(dom = "t",
                         columnDefs = list(list(className = "dt-right", targets = 1:2)))))
    })

    # ---------------- Vehicles ----------------

    veh_data <- reactive({
      v <- veh()
      tr <- get_trips()
      # Utilisation = share of a body type's vehicles currently on a live trip.
      v |>
        dplyr::group_by(body) |>
        dplyr::summarise(
          total = dplyr::n(),
          running = sum(vehicle_id %in% tr$vehicle_id[tr$status %in% c("Running", "At Hub", "Loading")]),
          .groups = "drop") |>
        dplyr::mutate(pct = ifelse(total > 0, round(running / total * 100), 0)) |>
        dplyr::arrange(dplyr::desc(total)) |>
        head(4)
    })

    output$p_veh <- renderPlotly({
      d <- veh_data()
      if (!nrow(d)) return(plotly_empty())
      d$col <- ifelse(d$pct == max(d$pct), "#12263F", "#AFC6DE")
      plot_ly(d, x = ~body, y = ~pct, type = "bar", marker = list(color = ~col),
              hoverinfo = "text",
              text = ~paste0(pct, "% of ", total),
              textposition = "outside", textfont = list(size = 11, color = "#12263F")) |>
        layout(xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                            tickfont = list(size = 10, color = "#64748B")),
               yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE,
                            zeroline = FALSE, range = c(0, 115)),
               margin = list(l = 4, r = 4, t = 20, b = 4), bargap = .45,
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    # ---------------- Financial ----------------

    fin_data <- reactive({
      i <- inv(); cl <- store_get("clients")
      if (!nrow(i)) return(tibble::tibble())
      i$balance <- i$total - tidyr::replace_na(i$paid_amount, 0)
      o <- i[i$balance > 0 & i$status != "Draft", ]
      if (!nrow(o)) return(tibble::tibble())
      o |>
        dplyr::group_by(client_id) |>
        dplyr::summarise(out = sum(balance), oldest = min(due_date, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::arrange(dplyr::desc(out)) |>
        head(4) |>
        dplyr::mutate(name = cl$name[match(client_id, cl$client_id)],
                      age = as.numeric(Sys.Date() - oldest))
    })

    output$t_fin <- renderUI({
      d <- fin_data()
      if (!nrow(d)) return(div(class = "muted tiny", "Nothing outstanding."))
      div(class = "tms-table",
        DT::datatable(
          tibble::tibble(
            CUSTOMER = d$name,
            OUTSTANDING = inr_compact(d$out),
            AGEING = sprintf('<span class="pill pill-%s">%s d</span>',
                             ifelse(d$age > 45, "red", ifelse(d$age > 15, "orange", "grey")),
                             round(pmax(0, d$age)))),
          rownames = FALSE, selection = "none", escape = FALSE,
          options = list(dom = "t",
                         columnDefs = list(list(className = "dt-right", targets = 1)))))
    })

    # ---------------- Complaints ----------------

    cmp_data <- reactive({
      c <- cmp(); b <- store_get("branches")
      if (!nrow(c)) return(tibble::tibble())
      c$ontime <- ifelse(c$status == "Resolved",
                         !is.na(c$resolved_dt) & c$resolved_dt <= c$target_dt,
                         !is.na(c$target_dt) & c$target_dt >= Sys.Date())
      c |>
        dplyr::group_by(branch_id) |>
        dplyr::summarise(n = dplyr::n(), pct = round(mean(ontime) * 100), .groups = "drop") |>
        dplyr::filter(n >= 2) |>
        dplyr::arrange(dplyr::desc(n)) |>
        head(4) |>
        dplyr::mutate(city = b$city[match(branch_id, b$branch_id)])
    })

    output$p_cmp <- renderPlotly({
      d <- cmp_data()
      if (!nrow(d)) return(plotly_empty())
      d$col <- ifelse(d$pct == max(d$pct), "#12263F", "#AFC6DE")
      plot_ly(d, x = ~city, y = ~pct, type = "bar", marker = list(color = ~col),
              hoverinfo = "text", text = ~paste0(pct, "% within SLA"),
              textposition = "outside", textfont = list(size = 11, color = "#12263F")) |>
        layout(xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                            tickfont = list(size = 11, color = "#64748B")),
               yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE,
                            zeroline = FALSE, range = c(0, 115)),
               margin = list(l = 4, r = 4, t = 20, b = 4), bargap = .45,
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") |>
        config(displayModeBar = FALSE)
    })

    # ---------------- HR ----------------

    hr_data <- reactive({
      e <- emp(); a <- get_attendance()
      a <- a[a$date == Sys.Date() & a$employee_id %in% e$employee_id, ]
      if (!nrow(a)) return(tibble::tibble())
      a$dept <- e$department[match(a$employee_id, e$employee_id)]
      a |>
        dplyr::group_by(dept) |>
        dplyr::summarise(present = sum(status %in% c("P", "T")),
                         total = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(pct = ifelse(total > 0, round(present / total * 100), 0)) |>
        dplyr::arrange(dplyr::desc(total)) |>
        head(4)
    })

    output$t_hr <- renderUI({
      d <- hr_data()
      if (!nrow(d)) return(div(class = "muted tiny", "No attendance for today."))
      div(class = "tms-table",
        DT::datatable(
          tibble::tibble(DEPARTMENT = d$dept,
                         PRESENT = paste0(d$present, " / ", d$total),
                         `%` = paste0(d$pct, "%")),
          rownames = FALSE, selection = "none",
          options = list(dom = "t",
                         columnDefs = list(list(className = "dt-right", targets = 1:2)))))
    })

    # ---------------- Exports ----------------

    # Each download builds a real workbook from the same reactives the charts
    # read, so an exported figure always matches what was on screen.
    mk_download <- function(name, data_fn) {
      downloadHandler(
        filename = function() sprintf("TMS-%s-%s.xlsx", name, format(Sys.Date(), "%Y%m%d")),
        content = function(file) {
          audit(user()$user_id, "export", "reports", name)
          openxlsx::write.xlsx(data_fn(), file)
        }
      )
    }

    output$dl_book <- mk_download("bookings", function() book_data())
    output$dl_cn   <- mk_download("consignments", function() cn_data())
    output$dl_veh  <- mk_download("vehicles", function() veh_data())
    output$dl_fin  <- mk_download("financial", function() fin_data())
    output$dl_cmp  <- mk_download("complaints", function() cmp_data())
    output$dl_hr   <- mk_download("hr", function() hr_data())

    output$export_all <- downloadHandler(
      filename = function() sprintf("TMS-all-reports-%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        audit(user()$user_id, "export", "reports", "all")
        openxlsx::write.xlsx(
          list(Bookings = book_data(), Consignments = cn_data(), Vehicles = veh_data(),
               Financial = fin_data(), Complaints = cmp_data(), HR = hr_data()),
          file)
      }
    )

    observeEvent(input$schedule, {
      showModal(modalDialog(
        title = "Schedule a report", easyClose = TRUE,
        selectInput(ns("s_report"), "Report",
                    c("Bookings", "Consignments", "Vehicles", "Financial",
                      "Complaints", "HR")),
        selectInput(ns("s_freq"), "Frequency", c("Daily", "Weekly", "Monthly")),
        textInput(ns("s_to"), "Send to", placeholder = paste0("ops@", BRAND$domain)),
        callout("Delivery needs a mail gateway",
                "Scheduling is recorded, but reports will only be delivered once a transactional email provider is connected in Settings → Integrations.",
                "warn"),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("s_save"), "Schedule"))
      ))
    })

    observeEvent(input$s_save, {
      audit(user()$user_id, "schedule", "reports",
            paste(input$s_report, input$s_freq, input$s_to))
      removeModal()
      showNotification(sprintf("%s report scheduled %s.", input$s_report,
                               tolower(input$s_freq)), type = "message")
    })
  })
}
