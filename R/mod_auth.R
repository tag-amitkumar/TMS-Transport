# ==================================================================
# Authentication.
#
# Reproduces the deck's login screen, including the "Quick preview as" role
# chips. Those chips are a demo affordance: they sign in as a representative
# account for each role so a reviewer can see the RBAC behaviour without
# managing five sets of credentials. They are gated on DEMO_LOGIN so a real
# deployment can switch them off with one environment variable.
# ==================================================================

# Enabled unless explicitly turned off. A deployment sets TMS_DEMO_LOGIN=false
# to hide the role chips and require a real password.
DEMO_LOGIN <- !identical(tolower(Sys.getenv("TMS_DEMO_LOGIN", "true")), "false")

# Password every seeded account shares. Documented in the README — this is a
# public demo dataset, not a secret, and it is stored bcrypt-hashed regardless.
DEMO_PASSWORD <- "tms@2026"

auth_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "login-wrap",
    div(
      div(
        class = "login-card",
        # The login card is white, so it gets the full artwork. That lock-up
        # already carries the company name, so the wordmark below it is dropped
        # when the file is present — otherwise the card says MoveWing twice.
        div(
          class = "text-center mb-4",
          div(class = "d-flex justify-content-center mb-3",
              brand_logo(if (brand_has_logo()) 86 else 52)),
          if (!brand_has_logo())
            h4(class = "mb-0", style = "font-weight:700;color:#12263F;letter-spacing:-.01em;",
               BRAND$short),
          div(class = "small-caps mt-1", BRAND$product)
        ),

        div(
          class = "mb-3",
          tags$label(class = "form-label req", "Email or Username"),
          textInput(ns("email"), NULL, value = "", width = "100%",
                    placeholder = paste0("you@", BRAND$domain))
        ),
        div(
          class = "mb-2",
          tags$label(class = "form-label req", "Password"),
          passwordInput(ns("password"), NULL, value = "", width = "100%")
        ),

        div(
          class = "d-flex align-items-center justify-content-between mb-3",
          checkboxInput(ns("remember"), "Remember me", value = TRUE, width = "auto"),
          tags$a(href = "#", class = "tiny",
                 onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("forgot")),
                 "Forgot password?")
        ),

        actionButton(ns("signin"), "Sign In",
                     class = "btn-tms-primary w-100", style = "padding:.6rem;"),

        uiOutput(ns("error")),

        tags$hr(class = "soft mt-4"),
        div(class = "text-center tiny muted", "Need help signing in? Contact your branch admin")
      ),

      # Tracking sits outside the sign-in box on purpose. A consignee is not a
      # user of this system and never will be — offering them a password field
      # first, and a way to answer their actual question second, has it
      # backwards.
      public_track_ui("track"),

      if (DEMO_LOGIN) div(
        class = "text-center mt-4",
        div(class = "small-caps mb-2", style = "color:#7F93AB;", "Quick preview as"),
        div(
          class = "d-flex flex-wrap justify-content-center gap-2",
          lapply(DEMO_ROLES, function(r) {
            div(class = "login-role-chip",
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                                  ns("quick"), r),
                r)
          })
        )
      )
    )
  )
}

#' @return reactiveVal holding the signed-in user row (a one-row list), or NULL.
auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    user <- reactiveVal(NULL)
    err  <- reactiveVal(NULL)

    output$error <- renderUI({
      e <- err()
      if (is.null(e)) return(NULL)
      div(class = "callout callout-danger mt-3", style = "font-size:.75rem;", e)
    })

    # Build the session user object. Portal roles (Customer/Vendor) carry the
    # id of the party they represent so scope_owner() can filter to their rows.
    make_user <- function(row) {
      list(
        user_id      = row$user_id,
        name         = row$name,
        email        = row$email,
        role         = row$role,
        branch_id    = row$branch_id,
        department   = row$department,
        cross_branch = row$cross_branch,
        client_id    = row$client_id %||% "",
        vendor_id    = row$vendor_id %||% ""
      )
    }

    observeEvent(input$signin, {
      err(NULL)
      email <- trimws(input$email %||% "")
      pw    <- input$password %||% ""

      if (!nzchar(email) || !nzchar(pw)) {
        err("Enter both an email and a password.")
        return()
      }

      u <- store_get("users")
      # Match on email or the local-part, so "amardip" works as a username.
      row <- u[tolower(u$email) == tolower(email) |
                 tolower(sub("@.*$", "", u$email)) == tolower(email), , drop = FALSE]

      if (!nrow(row)) {
        # Deliberately identical to the wrong-password message so the form does
        # not reveal which addresses exist.
        err("Those credentials were not recognised.")
        return()
      }
      row <- row[1, ]

      if (!identical(row$status, "Active")) {
        err("This account is suspended. Contact your branch admin.")
        return()
      }

      ok <- tryCatch(bcrypt::checkpw(pw, row$password_hash), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        err("Those credentials were not recognised.")
        return()
      }

      audit(row$user_id, "sign-in", "auth", paste("role:", row$role))
      user(make_user(row))
    })

    # Demo role chips: pick the first active account holding that role.
    observeEvent(input$quick, {
      req(DEMO_LOGIN)
      role <- input$quick

      if (role %in% c("Customer", "Vendor")) {
        # No stored login rows for external parties — synthesise a session from
        # the client/vendor master so the portals have a real subject.
        if (role == "Customer") {
          c1 <- store_get("clients")[1, ]
          user(list(user_id = paste0("PORTAL-", c1$client_id), name = c1$name,
                    email = c1$email, role = "Customer", branch_id = c1$branch_id,
                    department = "External", cross_branch = "FALSE",
                    client_id = c1$client_id, vendor_id = ""))
        } else {
          v1 <- store_get("vendors")[1, ]
          user(list(user_id = paste0("PORTAL-", v1$vendor_id), name = v1$name,
                    email = "", role = "Vendor", branch_id = v1$branch_id,
                    department = "External", cross_branch = "FALSE",
                    client_id = "", vendor_id = v1$vendor_id))
        }
        return()
      }

      u <- store_get("users")
      row <- u[u$role == role & u$status == "Active", , drop = FALSE]
      if (!nrow(row)) {
        err(paste("No active", role, "account in the seed data."))
        return()
      }
      audit(row$user_id[1], "sign-in (demo)", "auth", paste("role:", role))
      user(make_user(row[1, ]))
    })

    observeEvent(input$forgot, {
      showModal(modalDialog(
        title = "Password reset",
        p("Password resets are handled by your branch admin, who can issue a new ",
          "temporary password from Administration → Users."),
        if (DEMO_LOGIN) div(
          class = "callout callout-info mt-3",
          div(span(class = "ttl", "Demo build"),
              paste0("Every seeded account uses the password ", DEMO_PASSWORD, "."))
        ),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    })

    user
  })
}
