# ==================================================================
# Users — internal logins, with the per-user permission strip from the deck.
# ==================================================================

users_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2",
            btn_ghost(ns("filters"), "Filters", fontawesome::fa("filter")),
            uiOutput(ns("add_btn"), inline = TRUE))),
    card_panel(body_class = "tms-card-body p-0",
               div(class = "tms-table", DTOutput(ns("tbl")))),
    div(class = "mt-3", uiOutput(ns("perms")))
  )
}

users_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("All")

    all_rows <- reactive(scope_branch(store_get("users"), user()))

    output$tabs <- renderUI({
      u <- all_rows()
      roles <- c("Branch Admin", "Dispatcher", "Accountant", "HR Manager", "Booking Executive")
      items <- setNames(c("All", roles), c("All roles", roles))
      counts <- as.list(c(nrow(u), vapply(roles, function(r) sum(u$role == r), integer(1))))
      names(counts) <- c("All", roles)
      tab_strip(ns, "tab", items, counts, tab())
    })

    observeEvent(input$tab, tab(input$tab))

    rows <- reactive({
      u <- all_rows()
      if (!identical(tab(), "All")) u <- u[u$role == tab(), ]
      u
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "users", "create")) return(NULL)
      btn_primary(ns("add"), "Add User", fontawesome::fa("plus"))
    })

    output$tbl <- renderDT({
      u <- rows(); b <- store_get("branches")
      df <- tibble::tibble(
        USER = mapply(function(n) paste0(
          '<div class="d-flex align-items-center gap-2">', avatar_html(n),
          '<span style="font-weight:600;color:#12275C;">', htmlEscape(n), "</span></div>"), u$name),
        `LOGIN / EMAIL` = mono(u$email),
        BRANCH     = b$name[match(u$branch_id, b$branch_id)],
        ROLE       = pill_html(u$role, colour = NULL),
        DEPARTMENT = u$department,
        MOBILE     = mono(u$mobile),
        STATUS     = pill_html(u$status)
      )
      # Role is a vocabulary of its own, so give each level a stable colour
      # rather than falling through to grey.
      rc <- c("Super Admin" = "blue", "Branch Admin" = "blue", "Operations Manager" = "purple",
              "Booking Executive" = "grey", "Dispatcher" = "purple", "Accountant" = "orange",
              "HR Manager" = "red", "Driver" = "grey", "Vendor" = "grey", "Customer" = "grey")
      df$ROLE <- sprintf('<span class="pill pill-%s">%s</span>',
                         unname(rc[u$role] %||% "grey"), htmlEscape(u$role))
      tms_table(df, page = 12)
    })

    sel <- reactive({
      i <- input$tbl_rows_selected
      if (is.null(i) || !length(i)) return(NULL)
      rows()[i[1], ]
    })

    # Per-user permission strip. Reads the live matrix so it reflects whatever
    # the Security & Roles screen last saved.
    output$perms <- renderUI({
      u <- sel()
      if (is.null(u)) return(NULL)
      b <- store_get("branches")

      # Show the coarse capabilities the deck lists, derived from the matrix
      # rather than stored per user — the deck's own note says access is
      # "role-based access, editable per user".
      caps <- list(
        "Create Booking"       = can(u$role, "bookings", "create"),
        "Edit Booking"         = can(u$role, "bookings", "edit"),
        "Delete Booking"       = can(u$role, "bookings", "delete"),
        "View Reports"         = can(u$role, "reports", "view"),
        "Vehicle Allocation"   = can(u$role, "cargo_moto", "create"),
        "HR Access"            = can(u$role, "hrms_employees", "view"),
        "Financial Access"     = can(u$role, "invoices", "view"),
        "Complaint Management" = can(u$role, "complaints", "edit")
      )

      card_panel(
        title = paste0(u$name, " — Permissions"),
        sub = paste(u$role, "·", b$name[match(u$branch_id, b$branch_id)],
                    "· role-based access, editable per user"),
        actions = if (can(user()$role, "security", "view"))
          actionLink(ns("manage_roles"), "Manage roles", class = "tiny"),
        div(class = "chip-row",
            lapply(names(caps), function(k) {
              div(class = paste0("chip", if (isTRUE(caps[[k]])) " active"), k)
            }))
      )
    })

    observeEvent(input$add, {
      if (!require_perm(session, user()$role, "users", "create")) return()
      b <- store_get("branches")
      showModal(modalDialog(
        title = "Add user", size = "l", easyClose = TRUE,
        div(class = "row g-3",
            div(class = "col-6", textInput(ns("n_name"), "Full name")),
            div(class = "col-6", textInput(ns("n_email"), "Email")),
            div(class = "col-6", selectInput(ns("n_role"), "Role", ROLE_LEVELS)),
            div(class = "col-6", selectInput(ns("n_branch"), "Branch",
                                             setNames(b$branch_id, b$name))),
            div(class = "col-6", textInput(ns("n_dept"), "Department")),
            div(class = "col-6", textInput(ns("n_mobile"), "Mobile")),
            div(class = "col-12",
                checkboxInput(ns("n_cross"), "Grant cross-branch access", FALSE))),
        div(class = "callout callout-info mt-2",
            div(span(class = "ttl", "Initial password"),
                "The account is created with the standard temporary password and must be changed on first sign-in.")),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("save"), "Create user"))
      ))
    })

    observeEvent(input$save, {
      if (!require_perm(session, user()$role, "users", "create")) return()
      if (!nzchar(input$n_name %||% "") || !nzchar(input$n_email %||% "")) {
        showNotification("Name and email are required.", type = "error"); return()
      }
      u <- store_get("users")
      if (tolower(input$n_email) %in% tolower(u$email)) {
        showNotification("That email is already registered.", type = "error"); return()
      }
      id_new <- next_id(u$user_id, "USR-")
      store_insert("users", list(
        user_id = id_new, name = input$n_name, email = tolower(input$n_email),
        password_hash = bcrypt::hashpw(DEMO_PASSWORD),
        role = input$n_role, branch_id = input$n_branch,
        department = input$n_dept, mobile = input$n_mobile,
        cross_branch = as.character(isTRUE(input$n_cross)),
        client_id = "", vendor_id = "", status = "Active"
      ))
      audit(user()$user_id, "create", "users", id_new)
      removeModal()
      showNotification(paste("User", input$n_name, "created."), type = "message")
    })

    observeEvent(input$filters, {
      showModal(modalDialog(title = "Filter users", easyClose = TRUE,
        selectInput(ns("f_status"), "Status", c("All", "Active", "Suspended")),
        footer = modalButton("Close")))
    })
  })
}
