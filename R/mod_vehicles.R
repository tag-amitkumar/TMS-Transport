# ==================================================================
# Vehicles — the fleet master, with document-expiry surveillance.
#
# Four statutory documents each carry an expiry that grounds the vehicle when it
# lapses (insurance, fitness, national permit, PUC). Expiry state is computed
# from the dates on every read via expiry_state(), never stored, so a badge is
# never stale by a day.
# ==================================================================

vehicles_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("chips")),
        div(class = "d-flex gap-2 align-items-center",
            uiOutput(ns("doc_warn"), inline = TRUE),
            uiOutput(ns("add_btn"), inline = TRUE))),
    split_view(
      card_panel(body_class = "tms-card-body p-0",
                 div(class = "tms-table", DTOutput(ns("tbl"))),
                 foot = "Vehicle master carries registration, capacity, owner, permits, insurance, fitness & PUC with expiry alerts"),
      uiOutput(ns("panel"))
    )
  )
}

# Shared by the vehicle table and the detail panel so a date renders the same
# way in both places.
expiry_badge <- function(d, warn = 30) {
  st <- expiry_state(d, warn)
  n  <- days_to(d)
  lbl <- dplyr::case_when(
    st == "none"   ~ "—",
    st == "danger" ~ "expired",
    n <= warn      ~ paste0(round(n), " d"),
    TRUE           ~ format(as.Date(d), "%b %y")
  )
  col <- c(none = "grey", danger = "red", warn = "orange", ok = "green")[st]
  sprintf('<span class="pill pill-%s nodot">%s</span>', col, lbl)
}

vehicles_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    filt <- reactiveVal("All")

    all_rows <- reactive(scope_branch(get_vehicles(), user()))

    # A vehicle is "due" if any of its four documents lapses within 30 days.
    due_count <- reactive({
      v <- all_rows()
      if (!nrow(v)) return(0L)
      sum(vapply(seq_len(nrow(v)), function(i) {
        any(expiry_state(c(v$insurance_expiry[i], v$fitness_expiry[i],
                           v$permit_expiry[i], v$puc_expiry[i])) %in% c("warn", "danger"))
      }, logical(1)))
    })

    output$chips <- renderUI({
      v <- all_rows()
      items  <- setNames(c("All", VEHICLE_STATUS), c("All", VEHICLE_STATUS))
      counts <- as.list(c(nrow(v), vapply(VEHICLE_STATUS, function(s) sum(v$status == s), integer(1))))
      names(counts) <- c("All", VEHICLE_STATUS)
      chip_row(ns, "filt", items, counts, filt())
    })
    observeEvent(input$filt, filt(input$filt))

    output$doc_warn <- renderUI({
      n <- due_count()
      if (!n) return(NULL)
      div(class = "chip", style = "border-color:#EFA31D;color:#B3701A;",
          span(style = "width:6px;height:6px;border-radius:50%;background:#EFA31D;"),
          sprintf("%d document%s due ≤ 30 d", n, if (n == 1) "" else "s"))
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "vehicles", "create")) return(NULL)
      btn_primary(ns("add"), "Add Vehicle", fontawesome::fa("plus"))
    })

    rows <- reactive({
      v <- all_rows()
      if (!identical(filt(), "All")) v <- v[v$status == filt(), ]
      v
    })

    output$tbl <- renderDT({
      v <- rows(); dr <- store_get("drivers"); ven <- store_get("vendors")
      df <- tibble::tibble(
        VEHICLE = mapply(function(r, rc) paste0(
          '<div class="mono" style="font-weight:650;color:#12275C;">', htmlEscape(r),
          '</div><div style="font-size:.6875rem;color:#64748B;">RC No. ', htmlEscape(rc), "</div>"),
          v$reg_no, v$rc_number),
        `TYPE · CAPACITY` = paste0(v$body, " · ", v$capacity_t, " T"),
        OWNER = ifelse(v$owner_type == "Company", "Company owned",
                       paste("Vendor —", ven$name[match(v$vendor_id, ven$vendor_id)])),
        DRIVER = ifelse(nzchar(v$driver_id), dr$name[match(v$driver_id, dr$driver_id)], "—"),
        INSURANCE = expiry_badge(v$insurance_expiry),
        FITNESS   = expiry_badge(v$fitness_expiry),
        PERMIT    = expiry_badge(v$permit_expiry),
        PUC       = expiry_badge(v$puc_expiry),
        STATUS    = pill_html(v$status)
      )
      tms_table(df, page = 12)
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    output$panel <- renderUI({
      v <- sel()
      if (is.null(v)) return(card_panel(empty_panel("Select a vehicle to see its master record")))
      dr <- store_get("drivers"); ven <- store_get("vendors")
      tr <- get_trips(); t <- tr[tr$vehicle_id == v$vehicle_id & tr$status != "Completed", ]

      docs <- list(
        list("Insurance", v$insurance_expiry),
        list("Fitness certificate", v$fitness_expiry),
        list("National permit", v$permit_expiry),
        list("PUC", v$puc_expiry)
      )

      card_panel(
        div(class = "d-flex align-items-start justify-content-between gap-2 mb-3",
            div(h6(class = "tms-card-title", paste(v$reg_no, "— Vehicle Master")),
                p(class = "tms-card-sub",
                  paste(v$model, "·", v$body, "·", v$capacity_t, "T · joined fleet",
                        format(as.Date(v$joined_fleet), "%Y")))),
            pill(v$status)),

        dl_rows(
          "Vehicle type"    = paste("Truck ·", v$body),
          "Capacity"        = paste0(v$capacity_t, " T"),
          "Owner"           = if (v$owner_type == "Company") "Company owned"
                              else paste("Vendor —", ven$name[match(v$vendor_id, ven$vendor_id)]),
          "Driver"          = if (nzchar(v$driver_id)) dr$name[match(v$driver_id, dr$driver_id)] else "Unassigned",
          "RC number"       = span(class = "mono", v$rc_number),
          "Service schedule"= paste("next due at", inr_group(v$service_due_km), "km"),
          "Current trip"    = if (nrow(t)) t$trip_no[1] else "—"
        ),

        tags$hr(class = "soft"),
        div(class = "form-section", "Documents & expiry alerts"),
        div(
          lapply(docs, function(d) {
            st <- expiry_state(d[[2]])
            div(class = "d-flex align-items-center justify-content-between py-2",
                style = "border-bottom:1px solid #EEF2F7;",
                span(style = "font-size:.8125rem;", d[[1]]),
                span(class = "muted tiny", fmt_date(d[[2]])),
                span(HTML(sprintf('<span class="pill pill-%s">%s</span>',
                                  c(none = "grey", danger = "red", warn = "orange", ok = "green")[st],
                                  c(none = "—", danger = "Expired", warn = "Due soon", ok = "OK")[st]))))
          })
        ),

        div(class = "d-flex gap-2 mt-3",
            if (can(user()$role, "vehicles", "edit"))
              btn_ghost(ns("edit"), "Edit master", class = "flex-fill"),
            btn_dark(ns("track"), "Open live tracking", class = "flex-fill"))
      )
    })

    observeEvent(input$track, nav("livegps"))

    observeEvent(input$edit, {
      v <- sel(); req(v)
      if (!require_perm(session, user()$role, "vehicles", "edit")) return()
      showModal(modalDialog(
        title = paste("Edit", v$reg_no), size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", dateInput(ns("e_ins"), "Insurance expiry",
                                           value = v$insurance_expiry)),
            div(class = "col-6", dateInput(ns("e_fit"), "Fitness expiry",
                                           value = v$fitness_expiry)),
            div(class = "col-6", dateInput(ns("e_per"), "Permit expiry",
                                           value = v$permit_expiry)),
            div(class = "col-6", dateInput(ns("e_puc"), "PUC expiry",
                                           value = v$puc_expiry)),
            div(class = "col-6", numericInput(ns("e_svc"), "Service due (km)",
                                              as.numeric(v$service_due_km), step = 5000)),
            div(class = "col-6", selectInput(ns("e_status"), "Status", VEHICLE_STATUS,
                                             selected = v$status))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("e_save"), "Save changes"))
      ))
    })

    observeEvent(input$e_save, {
      v <- sel(); req(v)
      if (!require_perm(session, user()$role, "vehicles", "edit")) return()
      store_update("vehicles", list(vehicle_id = v$vehicle_id), list(
        insurance_expiry = as.character(input$e_ins),
        fitness_expiry   = as.character(input$e_fit),
        permit_expiry    = as.character(input$e_per),
        puc_expiry       = as.character(input$e_puc),
        service_due_km   = input$e_svc,
        status           = input$e_status
      ))
      audit(user()$user_id, "edit", "vehicles", v$vehicle_id)
      removeModal()
      showNotification(paste(v$reg_no, "updated."), type = "message")
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "vehicles", "create")) return()
      b <- store_get("branches"); ven <- store_get("vendors")
      showModal(modalDialog(
        title = "Add vehicle", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("n_reg"), "Registration no.",
                                           placeholder = "MH-31 GH 6612")),
            div(class = "col-6", textInput(ns("n_model"), "Model")),
            div(class = "col-4", textInput(ns("n_body"), "Body", value = "32 ft MXL")),
            div(class = "col-4", numericInput(ns("n_cap"), "Capacity (T)", 25, 1, 60)),
            div(class = "col-4", selectInput(ns("n_owner"), "Owner", c("Company", "Vendor"))),
            div(class = "col-6", selectInput(ns("n_vendor"), "Vendor (if vendor-owned)",
                                             c("—" = "", setNames(ven$vendor_id, ven$name)))),
            div(class = "col-6", selectInput(ns("n_branch"), "Branch",
                                             setNames(b$branch_id, b$name))),
            div(class = "col-6", textInput(ns("n_rc"), "RC number")),
            div(class = "col-6", dateInput(ns("n_ins"), "Insurance expiry",
                                           value = Sys.Date() + 365))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("n_save"), "Add vehicle"))
      ))
    })

    observeEvent(input$n_save, {
      if (!require_perm(session, user()$role, "vehicles", "create")) return()
      if (!nzchar(input$n_reg %||% "")) {
        showNotification("Registration number is required.", type = "error"); return()
      }
      v <- store_get("vehicles")
      if (toupper(input$n_reg) %in% toupper(v$reg_no)) {
        showNotification("That registration is already on the fleet.", type = "error"); return()
      }
      id_new <- next_id(v$vehicle_id, "VEH-", 3)
      store_insert("vehicles", list(
        vehicle_id = id_new, reg_no = toupper(input$n_reg), model = input$n_model,
        body = input$n_body, capacity_t = input$n_cap,
        owner_type = input$n_owner,
        vendor_id = if (identical(input$n_owner, "Vendor")) input$n_vendor else "",
        driver_id = "", branch_id = input$n_branch, rc_number = input$n_rc,
        insurance_expiry = as.character(input$n_ins),
        fitness_expiry = as.character(Sys.Date() + 365),
        permit_expiry  = as.character(Sys.Date() + 1095),
        puc_expiry     = as.character(Sys.Date() + 180),
        service_due_km = 100000,
        joined_fleet   = as.character(Sys.Date()),
        status = "Available"
      ))
      audit(user()$user_id, "create", "vehicles", id_new)
      removeModal()
      showNotification(paste("Vehicle", input$n_reg, "added."), type = "message")
    })
  })
}
