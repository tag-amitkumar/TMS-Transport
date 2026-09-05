# ==================================================================
# Security & Roles — the permission matrix, editable.
#
# Edits here write to permissions.csv, which can() reads on every check, so a
# change takes effect on the next click without a restart. The matrix is the
# real authority: rbac.R's DEFAULT_PERMISSIONS is only the fallback for a
# missing file.
#
# Super Admin is deliberately not editable — locking yourself out of the screen
# that grants access is unrecoverable without editing CSVs by hand.
# ==================================================================

security_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_head("Security & Role-Based Access Control",
              "10 access levels · permissions configurable at module, feature & action level",
              actions = tagList(
                btn_ghost(ns("audit"), "Audit Log", fontawesome::fa("chart-line")),
                uiOutput(ns("add_btn"), inline = TRUE))),
    uiOutput(ns("role_chips")),
    div(class = "mt-3", uiOutput(ns("matrix")))
  )
}

security_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    role_sel <- reactiveVal("Super Admin")

    perms <- reactive({
      p <- store_get("permissions")
      if (!nrow(p)) DEFAULT_PERMISSIONS else p
    })

    role_counts <- reactive({
      u <- store_get("users")
      cl <- nrow(store_get("clients")); vn <- nrow(store_get("vendors"))
      dr <- nrow(store_get("drivers"))
      counts <- vapply(ROLE_LEVELS, function(r) sum(u$role == r), integer(1))
      # Driver, Vendor and Customer are not internal logins — their population
      # is the size of the corresponding master, which is what the deck's
      # much larger counts on those three chips actually represent.
      counts[["Driver"]]   <- dr
      counts[["Vendor"]]   <- vn
      counts[["Customer"]] <- cl
      counts
    })

    output$add_btn <- renderUI({
      if (!can(user()$role, "security", "create")) return(NULL)
      btn_primary(ns("add_role"), "Add Role", fontawesome::fa("plus"))
    })

    output$role_chips <- renderUI({
      cnt <- role_counts()
      chip_row(ns, "role", setNames(ROLE_LEVELS, ROLE_LEVELS),
               as.list(cnt), role_sel())
    })
    observeEvent(input$role, role_sel(input$role))

    output$matrix <- renderUI({
      r <- role_sel()
      p <- perms()
      roles <- store_get("roles")
      editable <- can(user()$role, "security", "edit") && !identical(r, "Super Admin")

      # Summary row per access level, as the deck draws it, then a per-module
      # grid for the selected role underneath.
      card_panel(
        title = "Permission Matrix",
        sub = "View · Create · Edit · Delete · Approve · Export — configured per role, module & feature",
        actions = if (identical(r, "Super Admin"))
          pill("Locked — full access", "grey")
        else if (editable) btn_dark(ns("save"), "Save changes", class = "btn-tms-sm")
        else pill("Read-only for your role", "grey"),

        # ---- all roles, coarse view ----
        div(class = "tms-table mb-4",
          DT::datatable(
            local({
              rows <- lapply(ROLE_LEVELS, function(rl) {
                pr <- p[p$role == rl, ]
                tick <- function(a) {
                  # A role "has" an action if any module grants it.
                  if (!nrow(pr)) return("—")
                  if (any(as.numeric(pr[[a]]) == 1, na.rm = TRUE))
                    '<span style="color:#1A7F45;font-weight:700;">&#10003;</span>' else "—"
                }
                tibble::tibble(
                  `ACCESS LEVEL` = sprintf(
                    '<div style="font-weight:600;color:#12275C;">%s</div><div style="font-size:.6875rem;color:#64748B;">%s</div>',
                    htmlEscape(rl),
                    htmlEscape(roles$description[match(rl, roles$role)] %||% "")),
                  VIEW = tick("view"), CREATE = tick("create"), EDIT = tick("edit"),
                  DELETE = tick("delete"), APPROVE = tick("approve"), EXPORT = tick("export"))
              })
              dplyr::bind_rows(rows)
            }),
            rownames = FALSE, selection = "none", escape = FALSE,
            options = list(dom = "t", pageLength = 10, ordering = FALSE,
                           columnDefs = list(list(className = "dt-center", targets = 1:6))))),

        # ---- selected role, per-module grid ----
        div(class = "form-section", paste(r, "— per-module permissions")),
        if (identical(r, "Super Admin"))
          callout("Full system access",
                  "Super Admin holds every permission on every module and cannot be edited here — removing its own access would lock this screen.",
                  "info")
        else div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm align-middle mb-0",
            style = "font-size:.8125rem;",
            tags$thead(tags$tr(
              tags$th(style = "font-size:.625rem;letter-spacing:.08em;text-transform:uppercase;color:#64748B;", "Module"),
              lapply(ACTIONS, function(a)
                tags$th(class = "text-center",
                        style = "font-size:.625rem;letter-spacing:.08em;text-transform:uppercase;color:#64748B;",
                        a)))),
            tags$tbody(
              lapply(MODULES, function(m) {
                row <- p[p$role == r & p$module == m, ]
                tags$tr(
                  tags$td(style = "color:#12275C;", m),
                  lapply(ACTIONS, function(a) {
                    val <- if (nrow(row)) isTRUE(as.numeric(row[[a]][1]) == 1) else FALSE
                    tags$td(class = "text-center",
                      if (editable)
                        tags$input(type = "checkbox",
                                   id = ns(paste0("chk_", m, "_", a)),
                                   class = "form-check-input perm-chk",
                                   `data-module` = m, `data-action` = a,
                                   checked = if (val) "checked" else NULL)
                      else if (val) HTML('<span style="color:#1A7F45;font-weight:700;">&#10003;</span>')
                      else HTML('<span style="color:#CBD5E1;">—</span>'))
                  }))
              }))
          )
        ),

        # Collect every checkbox into one input on save, rather than binding 168
        # separate Shiny inputs.
        if (editable) tags$script(HTML(sprintf("
          document.getElementById('%s').addEventListener('click', function() {
            var out = [];
            document.querySelectorAll('.perm-chk').forEach(function(c) {
              if (c.checked) out.push(c.dataset.module + '|' + c.dataset.action);
            });
            Shiny.setInputValue('%s', out.join(','), {priority: 'event'});
          });", ns("save"), ns("granted"))))
      )
    })

    observeEvent(input$granted, {
      if (!require_perm(session, user()$role, "security", "edit")) return()
      r <- role_sel()
      if (identical(r, "Super Admin")) return()

      granted <- strsplit(input$granted %||% "", ",", fixed = TRUE)[[1]]
      granted <- granted[nzchar(granted)]

      p <- store_get("permissions")
      if (!nrow(p)) p <- DEFAULT_PERMISSIONS

      for (m in MODULES) {
        vals <- lapply(ACTIONS, function(a) {
          as.character(as.integer(paste0(m, "|", a) %in% granted))
        })
        names(vals) <- ACTIONS
        hit <- p$role == r & p$module == m
        if (any(hit, na.rm = TRUE)) {
          store_update("permissions", list(role = r, module = m), vals)
        } else {
          store_insert("permissions", c(list(role = r, module = m), vals))
        }
      }
      audit(user()$user_id, "edit", "security", paste("permissions for", r))
      showNotification(paste("Permissions saved for", r), type = "message")
    })

    observeEvent(input$add_role, {
      if (!require_perm(session, user()$role, "security", "create")) return()
      showModal(modalDialog(
        title = "Add role", easyClose = TRUE,
        textInput(ns("nr_name"), "Role name"),
        textInput(ns("nr_desc"), "Description"),
        selectInput(ns("nr_base"), "Copy permissions from",
                    setdiff(ROLE_LEVELS, "Super Admin")),
        footer = tagList(modalButton("Cancel"), btn_primary(ns("nr_save"), "Create role"))
      ))
    })

    observeEvent(input$nr_save, {
      if (!require_perm(session, user()$role, "security", "create")) return()
      nm <- trimws(input$nr_name %||% "")
      if (!nzchar(nm)) { showNotification("Role name is required.", type = "error"); return() }
      if (nm %in% store_get("roles")$role) {
        showNotification("That role already exists.", type = "error"); return()
      }
      store_insert("roles", list(role = nm, description = input$nr_desc))
      base <- store_get("permissions")
      base <- base[base$role == input$nr_base, ]
      for (i in seq_len(nrow(base))) {
        row <- as.list(base[i, ]); row$role <- nm
        store_insert("permissions", row)
      }
      audit(user()$user_id, "create", "security", paste("role", nm))
      removeModal()
      showNotification(paste("Role", nm, "created from", input$nr_base), type = "message")
    })

    observeEvent(input$audit, {
      a <- store_get("audit_log")
      a <- a[order(a$ts, decreasing = TRUE), ]
      a <- a[seq_len(min(50, nrow(a))), ]
      u <- store_get("users")
      showModal(modalDialog(
        title = "Audit log", size = "l", easyClose = TRUE,
        if (!nrow(a) || all(is.na(a$ts)))
          div(class = "muted", "Nothing logged yet in this session.")
        else div(class = "tms-table",
                 DT::datatable(
                   tibble::tibble(
                     WHEN = a$ts,
                     USER = ifelse(is.na(match(a$user_id, u$user_id)), a$user_id,
                                   u$name[match(a$user_id, u$user_id)]),
                     ACTION = a$action, MODULE = a$module, DETAIL = a$detail),
                   rownames = FALSE, selection = "none",
                   options = list(dom = "tp", pageLength = 15))),
        footer = modalButton("Close")
      ))
    })
  })
}
