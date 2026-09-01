# ==================================================================
# Branches — branch master with a per-branch mini dashboard.
# ==================================================================

branches_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Branch Master", textOutput(ns("sub"), inline = TRUE),
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

branches_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rows <- reactive({
      b <- store_get("branches")
      # A branch-scoped user still needs to see their own branch's record.
      scope_branch(b, user(), col = "branch_id")
    })

    output$sub <- renderText({
      b <- rows()
      sprintf("%d branches across %d states", nrow(b), dplyr::n_distinct(b$state))
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "branches", "create")) return(NULL)
      btn_primary(ns("add"), "Add Branch", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      b <- rows()
      df <- tibble::tibble(
        CODE    = mono(b$code),
        BRANCH  = mapply(cell2, b$name, b$address),
        CITY    = b$city,
        MANAGER = b$manager,
        CONTACT = mono(b$contact),
        GSTIN   = mono(b$gstin),
        STATUS  = pill_html(b$status)
      )
      tms_table(df, page = 12)
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    output$panel <- renderUI({
      b <- sel()
      if (is.null(b)) return(card_panel(empty_panel("Select a branch to see its dashboard")))

      # Everything in this panel is computed live from the operational tables,
      # so the branch dashboard and the operations screens always agree.
      bk <- get_bookings();  bk <- bk[bk$branch_id == b$branch_id, ]
      v  <- get_vehicles();  v  <- v[v$branch_id == b$branch_id, ]
      cn <- get_consignments(); cn <- cn[cn$branch_id == b$branch_id, ]
      cp <- get_complaints();   cp <- cp[cp$branch_id == b$branch_id, ]
      inv <- get_invoices();    inv <- inv[inv$branch_id == b$branch_id, ]

      recent <- inv[!is.na(inv$invoice_date) &
                   inv$invoice_date >= since_30d(), ]

      card_panel(
        div(class = "d-flex align-items-center gap-3 mb-3",
            avatar(b$name, "lg"),
            div(h6(class = "tms-card-title", paste0(b$name, " — Branch Dashboard")),
                p(class = "tms-card-sub",
                  paste(b$code, "·", b$address, "· since", format(as.Date(b$since), "%Y"))))),

        div(class = "row g-2",
            div(class = "col-6", stat_card("DAILY BOOKINGS",
                                           sum(bk$booking_date == Sys.Date(), na.rm = TRUE))),
            div(class = "col-6", stat_card("REVENUE · 30 DAYS", inr_compact(sum(recent$total, na.rm = TRUE)))),
            div(class = "col-6", stat_card("VEHICLES AVAILABLE", sum(v$status == "Available"))),
            div(class = "col-6", stat_card("VEHICLES IN TRANSIT", sum(v$status == "In Transit"))),
            div(class = "col-6", stat_card("PENDING DELIVERIES",
                                           sum(cn$status != "Delivered"))),
            div(class = "col-6", stat_card("COMPLAINTS", sum(cp$status != "Resolved")))),

        div(class = "mt-3",
            callout("Branch scoping",
                    "Users assigned to this branch see only its own data unless granted cross-branch access.",
                    "info", fontawesome::fa("circle-info"))),

        div(class = "mt-3", btn_ghost(ns("view_users"), "View Branch Users", class = "w-100"))
      )
    })

    observeEvent(input$view_users, {
      b <- sel(); req(b)
      u <- store_get("users"); u <- u[u$branch_id == b$branch_id, ]
      showModal(modalDialog(
        title = paste(b$name, "— users"), size = "l", easyClose = TRUE,
        footer = modalButton("Close"),
        if (!nrow(u)) div(class = "muted", "No users assigned to this branch.")
        else div(class = "tms-table",
                 DT::datatable(
                   tibble::tibble(User = u$name, Email = u$email,
                                  Role = u$role, Status = u$status),
                   rownames = FALSE, selection = "none",
                   options = list(dom = "tp", pageLength = 10)))
      ))
    })

    observeEvent(input$filters, {
      showModal(modalDialog(
        title = "Filter branches", easyClose = TRUE,
        selectInput(ns("f_state"), "State",
                    c("All", sort(unique(store_get("branches")$state)))),
        selectInput(ns("f_status"), "Status", c("All", "Active", "Inactive")),
        footer = tagList(modalButton("Cancel"), btn_dark(ns("apply_filter"), "Apply"))
      ))
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "branches", "create")) return()
      showModal(modalDialog(
        title = "Add branch", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("n_name"), "Branch name")),
            div(class = "col-6", textInput(ns("n_code"), "Code", placeholder = "BR-XXX")),
            div(class = "col-12", textInput(ns("n_addr"), "Address")),
            div(class = "col-4", textInput(ns("n_city"), "City")),
            div(class = "col-4", textInput(ns("n_state"), "State")),
            div(class = "col-4", textInput(ns("n_pin"), "Pincode")),
            div(class = "col-6", textInput(ns("n_mgr"), "Manager")),
            div(class = "col-6", textInput(ns("n_contact"), "Contact")),
            div(class = "col-6", textInput(ns("n_gstin"), "GSTIN"))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("save"), "Save branch"))
      ))
    })

    observeEvent(input$save, {
      if (!require_perm(session, user()$role, "branches", "create")) return()
      if (!nzchar(input$n_name %||% "")) {
        showNotification("Branch name is required.", type = "error"); return()
      }
      id_new <- next_id(store_get("branches")$branch_id, "BR-", 3)
      store_insert("branches", list(
        branch_id = id_new,
        code = input$n_code %||% paste0("BR-", toupper(substr(input$n_name, 1, 3))),
        name = input$n_name, address = input$n_addr, city = input$n_city,
        state = input$n_state, pincode = input$n_pin,
        lat = "", lon = "", is_depot = "FALSE",
        manager = input$n_mgr, contact = input$n_contact, gstin = input$n_gstin,
        since = as.character(Sys.Date()), status = "Active"
      ))
      audit(user()$user_id, "create", "branches", id_new)
      removeModal()
      showNotification(paste("Branch", id_new, "created."), type = "message")
    })
  })
}
