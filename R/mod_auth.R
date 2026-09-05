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

#' Truck artwork for the login backdrop.
#'
#' Drawn rather than photographed. A stock lorry photograph would be another
#' binary asset to ship, would fight the brand palette, and would have to be
#' licensed; an SVG is a few hundred bytes, scales to any display, and takes
#' its colours from the same two the logo uses.
#'
#' Deliberately low-contrast. This is the backdrop to a password field — the
#' moment it competes with the form for attention it has failed.
login_backdrop <- function() {
  truck <- paste0(
    # Cab-over tractor with a box trailer, in profile, facing right — the
    # shape in the MoveWing mark itself.
    '<g id="mw-lorry">',
    '<rect x="4" y="14" width="132" height="54" rx="5"/>',
    '<path d="M142 68 V30 q0-6 6-6 h38 l12 20 v24 z"/>',
    '<path d="M152 30 h30 l9 15 h-39 z" opacity=".45"/>',
    '<circle cx="44" cy="74" r="10"/><circle cx="76" cy="74" r="10"/>',
    '<circle cx="170" cy="74" r="10"/>',
    '</g>')

  HTML(paste0(
    '<svg class="login-bg" viewBox="0 0 1600 900" preserveAspectRatio="xMidYMax slice"',
    ' xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false">',
    '<defs>', truck,
    # A single warm bloom behind the form, so the card sits on light rather
    # than on flat navy.
    '<radialGradient id="mw-glow" cx="50%" cy="18%" r="70%">',
    '<stop offset="0%" stop-color="#1D3A78"/>',
    '<stop offset="100%" stop-color="#0E1E48" stop-opacity="0"/>',
    '</radialGradient>',
    '</defs>',

    '<rect width="1600" height="900" fill="url(#mw-glow)"/>',

    # The watermark: one lorry at scale, bled off the left edge.
    '<g fill="#fff" opacity=".05" transform="translate(-150 250) scale(4.6)">',
    '<use href="#mw-lorry"/></g>',

    # The road. A single rule with a dashed centre line, and a convoy on it.
    '<g opacity=".22">',
    '<line x1="0" y1="852" x2="1600" y2="852" stroke="#fff" stroke-width="2"/>',
    '<line x1="0" y1="852" x2="1600" y2="852" stroke="', BRAND$orange,
    '" stroke-width="2" stroke-dasharray="26 34"/>',
    '</g>',
    '<g fill="#fff" opacity=".17">',
    '<use href="#mw-lorry" transform="translate(60 800) scale(.62)"/>',
    '<use href="#mw-lorry" transform="translate(430 787) scale(.78)"/>',
    '<use href="#mw-lorry" transform="translate(900 806) scale(.55)"/>',
    '<use href="#mw-lorry" transform="translate(1470 795) scale(.68)"/>',
    '</g>',
    # One lorry picked out in the brand orange, so the convoy is not a
    # monochrome frieze.
    '<g fill="', BRAND$orange, '" opacity=".55">',
    '<use href="#mw-lorry" transform="translate(1240 780) scale(.86)"/>',
    '</g>',
    '</svg>'))
}
auth_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "login-wrap",

    # ---- header bar ----
    #
    # White, not navy. The logo is dark navy on a transparent ground, so a dark
    # bar would swallow it — and the whole point of putting it up here is that
    # it is the first thing on the page.
    div(
      class = "login-header",
      div(
        class = "login-header-inner",
        # fill = TRUE crops the empty bands the artwork carries above and below,
        # so the mark reads at masthead size instead of floating in its own
        # padding. Width is set in CSS; the height follows from the crop.
        div(class = "login-header-logo", brand_logo(fill = TRUE)),
        div(class = "login-header-right", BRAND$product)
      )
    ),

    # ---- hero ----
    div(
      class = "login-hero-band",

      # The backdrop, behind everything and inert. aria-hidden and
      # pointer-events:none — it is decoration, and it must never intercept a
      # click meant for the form or reach a screen reader.
      login_backdrop(),

      div(
        class = "login-hero-inner",

        # ---- left: what this is ----
        div(
          class = "login-hero",
          h1(class = "login-hero-title",
             "Every load, ", tags$span(class = "hl", "tracked"), br(), "end to end."),
          div(
            class = "login-hero-tags",
            lapply(c("Bookings", "Consignments", "Fleet", "E-way bill",
                     "POD", "Invoicing"), function(t) span(t))
          ),
          p(class = "login-hero-sub",
            "One system from the booking counter to the settled invoice — ",
            "every PIN code in India, live position from the driver's phone, ",
            "and e-way bill validity that renews itself.")
        ),

        # ---- right: one card, two tabs ----
        #
        # A person arriving here wants exactly one of two things and only one
        # of them has a password. Tracking leads, because the people who arrive
        # without an account outnumber the people with one; signing in is one
        # click away.
        #
        # The panes are toggled in the browser, not through Shiny: a round-trip
        # would rebuild the inputs and throw away whatever had already been
        # typed into the other tab.
        div(
          class = "hero-card",
          div(
            class = "hero-tabs",
            tags$button(class = "hero-tab is-active", type = "button",
                        `data-pane` = "track", "Track shipment"),
            tags$button(class = "hero-tab", type = "button",
                        `data-pane` = "signin", "Sign in")
          ),

          div(
            class = "hero-pane", `data-pane` = "signin",
            h2(class = "hero-card-title", tags$strong("Sign in"), " to your account"),
            p(class = "hero-card-sub", "For branch, operations and fleet staff."),

            div(class = "mt-3",
                tags$label(class = "form-label req", "Email or Username"),
                textInput(ns("email"), NULL, value = "", width = "100%",
                          placeholder = paste0("you@", BRAND$domain))),
            div(
                tags$label(class = "form-label req", "Password"),
                passwordInput(ns("password"), NULL, value = "", width = "100%")),

            div(class = "d-flex align-items-center justify-content-between mb-3",
                checkboxInput(ns("remember"), "Remember me", value = TRUE, width = "auto"),
                tags$a(href = "#", class = "tiny",
                       onclick = sprintf("Shiny.setInputValue('%s', Math.random())",
                                         ns("forgot")),
                       "Forgot password?")),

            actionButton(ns("signin"), "Sign In",
                         class = "btn-tms-primary w-100 hero-cta"),

            uiOutput(ns("error")),

            div(class = "tiny muted mt-3 text-center",
                "Need help signing in? Contact your branch admin")
          ),

          div(class = "hero-pane is-active", `data-pane` = "track",
              public_track_ui("track")),

          # Delegated from document, and installed once.
          #
          # This page arrives through renderUI, and a <script> inserted that way
          # runs with document.currentScript set to null — so binding handlers
          # to "the buttons in my own card" attached to nothing and the tabs
          # silently did not switch. A listener on document survives the
          # element being replaced, which it is on every sign-out.
          tags$script(HTML("
if (!window.__mwHeroTabs) {
  window.__mwHeroTabs = true;
  document.addEventListener('click', function (e) {
    var btn = e.target.closest ? e.target.closest('.hero-tab') : null;
    if (!btn) return;
    var card = btn.closest('.hero-card');
    if (!card) return;
    var want = btn.getAttribute('data-pane');
    card.querySelectorAll('.hero-tab').forEach(function (b) {
      b.classList.toggle('is-active', b === btn);
    });
    card.querySelectorAll('.hero-pane').forEach(function (p) {
      p.classList.toggle('is-active', p.getAttribute('data-pane') === want);
    });
    // Anything Shiny sized while the pane was display:none measured zero.
    window.dispatchEvent(new Event('resize'));
  });
}
"))
        )
      )
    ),

    # ---- demo role chips ----
    if (DEMO_LOGIN) div(
      class = "login-foot",
      div(class = "small-caps mb-2", "Quick preview as"),
      div(
        class = "d-flex flex-wrap justify-content-center gap-2",
        lapply(DEMO_ROLES, function(r) {
          div(class = "login-role-chip",
              onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                                ns("quick"), r),
              r)
        })
      ),
      div(class = "login-foot-note",
          "Demo dataset — every name, GSTIN and number in it is fictional.")
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

    # Both panes live in one card and only one is on screen at a time, so
    # whichever is behind the other tab is inside a display:none element —
    # and Shiny suspends outputs there. Without this, a message written while
    # the pane was hidden is simply gone when the visitor switches to it.
    outputOptions(output, "error", suspendWhenHidden = FALSE)

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
        vendor_id    = row$vendor_id %||% "",
        # Drivers are staff, not an external party, so they scope by their
        # employee record rather than by client_id/vendor_id. It is what ties
        # a phone reporting a position to the trip that position belongs to.
        employee_id  = row$employee_id %||% ""
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
