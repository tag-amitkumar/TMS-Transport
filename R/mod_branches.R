# ==================================================================
# Branches — branch master with a per-branch mini dashboard.
# ==================================================================

branches_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Branch Master", textOutput(ns("sub"), inline = TRUE),
              actions = tagList(
                btn_ghost(ns("filters"), "Filters", fontawesome::fa("filter")),
                uiOutput(ns("pins_btn"), inline = TRUE),
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

    # ---------------- served pincodes (CR-14, CR-20) ----------------
    #
    # The list that decides who delivers a pincode and whether it is ODA.
    # Editable by whoever can change the branch master, because it is part of
    # the branch master: a branch is defined as much by its beat as by its
    # address.
    output$pins_btn <- renderUI({
      if (!can(user()$role, "branches", "update")) return(NULL)
      btn_ghost(ns("pins"), "Served PIN codes", fontawesome::fa("map-location-dot"))
    })

    observeEvent(input$pins, {
      if (!require_perm(session, user()$role, "branches", "update")) return()
      br <- rows()
      showModal(modalDialog(
        title = "Served PIN codes", size = "l", easyClose = TRUE,
        p(class = "tiny muted",
          paste("A PIN code listed here is delivered by that branch, as Normal or",
                "as ODA. A PIN code listed nowhere falls back to distance —",
                "inside", ODA_RADIUS_KM, "km of the nearest branch counts as",
                "Normal, beyond it as ODA. Listing one always wins over the",
                "distance rule, so this is where an exception goes.")),
        div(class = "row g-2 align-items-end mt-1",
            div(class = "col-md-4",
                selectInput(ns("p_branch"), "Branch",
                            choices = setNames(br$branch_id, br$name), width = "100%")),
            div(class = "col-md-3",
                textInput(ns("p_pin"), "PIN code", placeholder = "440001", width = "100%")),
            div(class = "col-md-3",
                selectInput(ns("p_type"), "Delivers as",
                            choices = DELIVERY_TYPES, width = "100%")),
            div(class = "col-md-2",
                btn_primary(ns("p_add"), "Add", fontawesome::fa("plus")))),
        div(class = "mt-3", uiOutput(ns("p_list"))),
        footer = modalButton("Done")
      ))
    })

    output$p_list <- renderUI({
      bp <- store_get("branch_pincodes")
      br <- store_get("branches")
      if (!nrow(bp)) return(div(class = "tiny muted", "No PIN codes listed yet."))
      bp <- bp[order(bp$branch_id, bp$pincode), , drop = FALSE]
      rows_ui <- lapply(seq_len(nrow(bp)), function(i) {
        tags$tr(
          tags$td(br$name[match(bp$branch_id[i], br$branch_id)] %||% bp$branch_id[i]),
          tags$td(class = "mono", bp$pincode[i]),
          tags$td(pill(bp$delivery_type[i],
                       if (identical(bp$delivery_type[i], "ODA")) "orange" else "green")),
          tags$td(class = "text-end",
                  tags$button(
                    class = "btn btn-sm btn-link p-0 text-danger",
                    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                                      ns("p_del"),
                                      paste0(bp$branch_id[i], "|", bp$pincode[i])),
                    "Remove")))
      })
      div(class = "tms-table",
          tags$table(class = "table table-sm mb-0",
            tags$thead(tags$tr(tags$th("BRANCH"), tags$th("PIN CODE"),
                               tags$th("DELIVERS AS"), tags$th(""))),
            tags$tbody(rows_ui)))
    })

    observeEvent(input$p_add, {
      if (!require_perm(session, user()$role, "branches", "update")) return()
      pin <- trimws(input$p_pin %||% "")
      if (!grepl("^[1-9][0-9]{5}$", pin)) {
        showNotification("PIN code must be six digits.", type = "error"); return()
      }
      # Refuse a PIN code the master does not know. A typo here silently
      # reroutes every future booking for the real pincode.
      if (is.null(geo_pin(pin))) {
        showNotification(paste0(pin, " is not in the PIN code master."),
                         type = "error"); return()
      }
      bp <- store_get("branch_pincodes")
      dup <- nrow(bp) && any(bp$branch_id == input$p_branch & bp$pincode == pin)
      if (dup) {
        store_update("branch_pincodes",
                     list(branch_id = input$p_branch, pincode = pin),
                     list(delivery_type = input$p_type))
        showNotification(paste0(pin, " updated to ", input$p_type, "."),
                         type = "message")
      } else {
        store_insert("branch_pincodes", list(
          branch_id = input$p_branch, pincode = pin,
          delivery_type = input$p_type %||% "Normal"))
        showNotification(paste0(pin, " added as ", input$p_type, "."),
                         type = "message")
      }
      audit(user()$user_id, "serviceability", "branches",
            paste(input$p_branch, pin, input$p_type))
      updateTextInput(session, "p_pin", value = "")
    })

    observeEvent(input$p_del, {
      if (!require_perm(session, user()$role, "branches", "update")) return()
      parts <- strsplit(input$p_del, "|", fixed = TRUE)[[1]]
      if (length(parts) != 2) return()
      bp <- store_get("branch_pincodes")
      keep <- !(bp$branch_id == parts[1] & bp$pincode == parts[2])
      store_set("branch_pincodes", bp[keep, , drop = FALSE])
      audit(user()$user_id, "serviceability-remove", "branches",
            paste(parts, collapse = " "))
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
            # Mobile, and it is required (CR-14, CR-27). A branch nobody can
            # telephone is no use to a driver at its gate at 2am.
            div(class = "col-6", textInput(ns("n_contact"), "Branch mobile *",
                                           placeholder = "9876543210")),
            div(class = "col-6", textInput(ns("n_gstin"), "GSTIN")),
            # The branch's own pincode is claimed as it is created (CR-20),
            # so a new branch is never a hole in the serviceability map.
            div(class = "col-6",
                selectInput(ns("n_delivery"), "Own pincode delivers as",
                            choices = DELIVERY_TYPES, selected = "Normal"))),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("save"), "Save branch"))
      ))
    })

    observeEvent(input$save, {
      if (!require_perm(session, user()$role, "branches", "create")) return()
      if (!nzchar(input$n_name %||% "")) {
        showNotification("Branch name is required.", type = "error"); return()
      }
      mob <- gsub("[ -]", "", trimws(input$n_contact %||% ""))
      if (!nzchar(mob)) {
        showNotification("Branch mobile number is required.", type = "error"); return()
      }
      if (!grepl("^(\\+?91)?[6-9][0-9]{9}$", mob)) {
        showNotification("Branch mobile is not a valid 10-digit Indian mobile.",
                         type = "error"); return()
      }
      pin <- trimws(input$n_pin %||% "")
      if (nzchar(pin) && !grepl("^[1-9][0-9]{5}$", pin)) {
        showNotification("Pincode must be six digits.", type = "error"); return()
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
      # Claim the branch's own pincode straight away (CR-20). Without this a
      # brand-new branch delivers nothing: resolve_delivery() would fall
      # through to the distance rule for its own front door.
      if (nzchar(pin)) {
        store_insert("branch_pincodes", list(
          branch_id = id_new, pincode = pin,
          delivery_type = input$n_delivery %||% "Normal"))
      }
      audit(user()$user_id, "create", "branches", id_new)
      removeModal()
      showNotification(paste("Branch", id_new, "created."), type = "message")
    })
  })
}
