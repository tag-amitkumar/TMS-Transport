# ==================================================================
# Settings — company, notifications, integrations, data retention.
#
# On integrations: this build ships every connector in a "Not configured" state
# and stores no credentials. The screen records which providers are intended and
# lets an admin mark one connected, but the app never holds a key — that would
# put a secret in a public repository. Real wiring belongs in environment
# variables read at startup, which is why the panel points at .Renviron rather
# than offering a key field. See DESIGN-REVIEW.md, gap #12.
# ==================================================================

settings_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex align-items-center justify-content-between gap-3 mb-3 flex-wrap",
        uiOutput(ns("tabs")),
        div(class = "d-flex gap-2 align-items-center",
            span(class = "tiny muted", "Changes are audit-logged with user + timestamp"),
            uiOutput(ns("save_btn"), inline = TRUE))),
    uiOutput(ns("body"))
  )
}

settings_server <- function(id, user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab <- reactiveVal("Company")
    # Bumped by the sync buttons so the status card re-renders with the new
    # result — gh_status() reads a plain environment, which nothing observes.
    sync_tick <- reactiveVal(0L)

    output$tabs <- renderUI({
      tab_strip(ns, "tab",
                setNames(c("Company", "Notifications", "Integrations", "Data retention"),
                         c("Company", "Notifications", "Integrations", "Data retention")),
                selected = tab())
    })
    observeEvent(input$tab, tab(input$tab))

    output$save_btn <- renderUI({
      if (!can(user()$role, "settings", "edit")) return(NULL)
      btn_dark(ns("save"), "Save changes")
    })

    output$body <- renderUI({
      switch(tab(),
        "Notifications"  = notif_panel(),
        "Integrations"   = integ_panel(),
        "Data retention" = retain_panel(),
        company_panel())
    })

    ro <- reactive(!can(user()$role, "settings", "edit"))

    # ---------------- Company ----------------

    company_panel <- function() {
      div(class = "row g-3",
        div(class = "col-xl-6", card_panel(
          title = "Company Details",
          sub = "Shown on LRs, invoices & customer-facing documents",
          div(class = "row g-3",
              div(class = "col-8",
                  tags$label(class = "form-label req", "Company Name"),
                  textInput(ns("c_name"), NULL, setting("company_name"), width = "100%")),
              div(class = "col-4",
                  tags$label(class = "form-label", "GSTIN"),
                  textInput(ns("c_gstin"), NULL, setting("company_gstin"), width = "100%")),
              div(class = "col-12",
                  tags$label(class = "form-label", "Registered Office"),
                  textInput(ns("c_office"), NULL, setting("registered_office"), width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Support Email"),
                  textInput(ns("c_email"), NULL, setting("support_email"), width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Support Phone"),
                  textInput(ns("c_phone"), NULL, setting("support_phone"), width = "100%"))))),

        div(class = "col-xl-6", card_panel(
          title = "Regional & Currency",
          sub = "Applied across bookings, invoices & reports",
          div(class = "row g-3",
              div(class = "col-6",
                  tags$label(class = "form-label", "Base Currency"),
                  selectInput(ns("c_curr"), NULL, c("INR (₹)"),
                              selected = "INR (₹)", width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Time Zone"),
                  selectInput(ns("c_tz"), NULL, c("Asia/Kolkata (IST)"),
                              selected = "Asia/Kolkata (IST)", width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Date Format"),
                  selectInput(ns("c_df"), NULL, c("DD-MMM-YYYY", "DD/MM/YYYY", "YYYY-MM-DD"),
                              selected = setting("date_format", "DD-MMM-YYYY"), width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Financial Year Start"),
                  selectInput(ns("c_fy"), NULL, c("01 April", "01 January"),
                              selected = setting("fy_start", "01 April"), width = "100%"))),
          div(class = "tiny muted mt-3",
              "Regional settings apply system-wide; historical records keep the format they were created with.")))
      )
    }

    # ---------------- Notifications ----------------

    notif_panel <- function() {
      ev <- store_get("notification_events")
      chans <- c("email", "sms", "whatsapp", "inapp")
      labs  <- c("EMAIL", "SMS", "WHATSAPP", "IN-APP")

      div(class = "row g-3",
        div(class = "col-xl-4", card_panel(
          title = "Notification Channels", sub = "Global on/off per delivery channel",
          div(class = "chip-row",
              lapply(seq_along(chans), function(i) {
                on <- any(as.numeric(ev[[chans[i]]]) == 1, na.rm = TRUE)
                div(class = paste0("chip", if (on) " active"),
                    if (on) HTML("&#10003; "), labs[i])
              })),
          div(class = "mt-3",
              callout("Channels need a provider",
                      "A channel only delivers once its gateway is connected under Integrations. Until then events are recorded in-app only.",
                      "warn")))),

        div(class = "col-xl-8", card_panel(
          title = "Notification Events", sub = "Per-event channel routing",
          div(class = "tms-table",
            DT::datatable(
              local({
                out <- tibble::tibble(EVENT = ev$event)
                for (i in seq_along(chans)) {
                  out[[labs[i]]] <- ifelse(
                    as.numeric(ev[[chans[i]]]) == 1,
                    '<span style="color:#1A7F45;font-weight:700;">&#10003;</span>',
                    '<span style="color:#CBD5E1;">—</span>')
                }
                out
              }),
              rownames = FALSE, selection = "none", escape = FALSE,
              options = list(dom = "t", pageLength = 12, ordering = FALSE,
                             columnDefs = list(list(className = "dt-center", targets = 1:4))))))))
    }

    # ---------------- Integrations ----------------

    integ_panel <- function() {
      ig <- store_get("integrations")
      div(class = "row g-3",
        div(class = "col-xl-7", card_panel(
          title = "Integrations", sub = "Status & credentials",
          div(
            lapply(seq_len(nrow(ig)), function(i) {
              r <- ig[i, ]
              connected <- identical(r$status, "Connected")
              div(class = "d-flex align-items-center gap-3 py-3",
                  style = if (i < nrow(ig)) "border-bottom:1px solid #EEF2F7;" else "",
                  div(class = "stat-ico", fontawesome::fa("plug")),
                  div(style = "min-width:0;",
                      div(style = "font-weight:650;font-size:.8125rem;color:#12275C;", r$name),
                      div(class = "tiny muted", r$provider)),
                  div(class = "ms-auto d-flex align-items-center gap-2",
                      pill(r$status, if (connected) "green" else "grey"),
                      if (can(user()$role, "settings", "edit"))
                        actionButton(ns(paste0("tog_", i)),
                                     if (connected) "Disconnect" else "Configure",
                                     class = "btn-tms-ghost btn-tms-sm",
                                     onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})",
                                                       ns("toggle"), r$name))))
            })))),

        div(class = "col-xl-5",
          card_panel(
            title = "How credentials are handled",
            callout("No keys are stored in this app",
                    "Connectors read their credentials from environment variables at startup (.Renviron locally, platform secrets when deployed). Nothing sensitive is written to the data files or committed to the repository.",
                    "info"),
            div(class = "mt-3 form-section", "Expected variables"),
            div(class = "field-static mono", style = "line-height:1.8;",
                HTML(paste(c("TMS_WHATSAPP_TOKEN", "TMS_SENDGRID_KEY", "TMS_MSG91_KEY",
                             "TMS_GSP_CLIENT_ID", "TMS_GSP_SECRET", "TMS_GPS_API_KEY",
                             "TMS_RAZORPAY_KEY"), collapse = "<br/>"))),
            div(class = "mt-3",
                callout("Demo build",
                        "All connectors ship disconnected. Marking one connected here records the intent for the audit log; it does not establish a live session.",
                        "warn"))),
          div(class = "mt-3", sync_panel()))
      )
    }

    # ---------------- Data sync ----------------
    #
    # Its own card rather than a row in the connector list above: those are
    # outbound notification channels a branch admin chooses to switch on, this
    # is infrastructure that decides whether today's bookings still exist
    # tomorrow. Read-only — it reports what the environment says, and offers
    # the one action (push now) that is useful without a restart.

    sync_panel <- function() {
      sync_tick()
      on  <- gh_enabled()
      cfg <- gh_config()
      st  <- gh_status()
      pending <- gh_pending()

      card_panel(
        title = "Data sync",
        sub = "Where the running app's records are persisted",
        div(class = "d-flex align-items-center gap-2 mb-3",
            pill(if (on) "Active" else "Off", if (on) "green" else "grey"),
            span(class = "tiny muted",
                 if (on) paste("branch", cfg$branch) else "no token set")),

        if (on) tagList(
          dl_rows("Last result" = st$msg %||% "—",
                  "Waiting to push" = if (length(pending))
                    paste(pending, collapse = ", ") else "nothing"),
          div(class = "mt-3 d-flex gap-2",
              btn_primary(ns("sync_now"), "Push now"),
              actionButton(ns("sync_pull"), "Pull from repo",
                           class = "btn-tms-ghost btn-tms-sm")),
          div(class = "tiny muted mt-2",
              sprintf("Changes are pushed automatically every %d seconds while anyone is signed in.",
                      SYNC_INTERVAL_MS %/% 1000))
        ) else callout(
          "Records live only on this server",
          paste("Bookings, PODs and GPS trails are written to the container's own",
                "filesystem, which a hosted platform discards on restart. Set",
                "TMS_GITHUB_REPO_URL and TMS_GITHUB_PAT to have every change",
                "committed to the repository as it happens."),
          "warn")
      )
    }

    observeEvent(input$sync_now, {
      if (!require_perm(session, user()$role, "settings", "edit")) return()
      done <- gh_flush()
      sync_tick(sync_tick() + 1L)
      showNotification(
        if (length(done)) paste("Pushed:", paste(done, collapse = ", "))
        else "Nothing waiting to push.",
        type = "message", duration = 5)
    })

    observeEvent(input$sync_pull, {
      if (!require_perm(session, user()$role, "settings", "edit")) return()
      gh_pull_all()
      sync_tick(sync_tick() + 1L)
      audit(user()$user_id, "data-pull", "settings", "pulled data/ from GitHub")
      showNotification(gh_status()$msg, type = "message", duration = 6)
    })

    observeEvent(input$toggle, {
      if (!require_perm(session, user()$role, "settings", "edit")) return()
      ig <- store_get("integrations")
      r <- ig[ig$name == input$toggle, ]
      if (!nrow(r)) return()
      new <- if (identical(r$status[1], "Connected")) "Not configured" else "Connected"
      store_update("integrations", list(name = input$toggle), list(status = new))
      audit(user()$user_id, "integration", "settings", paste(input$toggle, "->", new))
      showNotification(
        if (new == "Connected")
          paste0(input$toggle, " marked connected. Set its environment variables for live delivery.")
        else paste0(input$toggle, " disconnected."),
        type = if (new == "Connected") "warning" else "message", duration = 6)
    })

    # ---------------- Data retention ----------------

    retain_panel <- function() {
      div(class = "row g-3",
        div(class = "col-xl-6", card_panel(
          title = "Data Retention",
          sub = "How long each class of record is kept before archival",
          div(class = "row g-3",
              div(class = "col-6",
                  tags$label(class = "form-label", "GPS trails"),
                  selectInput(ns("r_gps"), NULL, c("6 months", "12 months", "18 months", "24 months"),
                              selected = paste(setting("retain_gps_months", "18"), "months"),
                              width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Documents / POD scans"),
                  selectInput(ns("r_docs"), NULL, c("12 months", "24 months", "36 months", "60 months"),
                              selected = paste(setting("retain_docs_months", "36"), "months"),
                              width = "100%")),
              div(class = "col-6",
                  tags$label(class = "form-label", "Financial records"),
                  div(class = "field-static",
                      paste(setting("retain_finance_years", "8"), "years · statutory"))),
              div(class = "col-6",
                  tags$label(class = "form-label", "Notification logs"),
                  selectInput(ns("r_notif"), NULL, c("3 months", "6 months", "12 months"),
                              selected = paste(setting("retain_notif_months", "12"), "months"),
                              width = "100%"))),
          div(class = "mt-3",
              callout("Financial retention is fixed",
                      "Books of account must be kept eight years under the Companies Act and the GST rules, so that period is not configurable here.",
                      "info")))),

        div(class = "col-xl-6", card_panel(
          title = "Current data volume",
          div(class = "tms-table",
            DT::datatable(
              local({
                counts <- vapply(TABLES, function(t) nrow(store_get(t)), integer(1))
                counts <- counts[order(-counts)][seq_len(min(10, length(counts)))]
                tibble::tibble(TABLE = names(counts), ROWS = inr_group(counts))
              }),
              rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 10,
                             columnDefs = list(list(className = "dt-right", targets = 1))))))))
    }

    # ---------------- Save ----------------

    observeEvent(input$save, {
      if (!require_perm(session, user()$role, "settings", "edit")) return()
      changed <- 0

      put <- function(k, v) {
        if (is.null(v) || !nzchar(as.character(v))) return()
        if (!identical(setting(k), as.character(v))) {
          setting_set(k, v); changed <<- changed + 1
        }
      }

      if (identical(tab(), "Company")) {
        put("company_name", input$c_name)
        put("company_gstin", input$c_gstin)
        put("registered_office", input$c_office)
        put("support_email", input$c_email)
        put("support_phone", input$c_phone)
        put("date_format", input$c_df)
        put("fy_start", input$c_fy)
      }
      if (identical(tab(), "Data retention")) {
        num <- function(x) sub(" months?$", "", x %||% "")
        put("retain_gps_months",   num(input$r_gps))
        put("retain_docs_months",  num(input$r_docs))
        put("retain_notif_months", num(input$r_notif))
      }

      if (changed) {
        audit(user()$user_id, "edit", "settings", paste(tab(), "·", changed, "fields"))
        showNotification(sprintf("%d setting%s saved.", changed, if (changed == 1) "" else "s"),
                         type = "message")
      } else {
        showNotification("Nothing changed.", type = "default")
      }
    })
  })
}
