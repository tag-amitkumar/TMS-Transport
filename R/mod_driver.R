# ==================================================================
# Driver console — the screen a driver sees on their phone.
#
# This is the only screen in the app designed for a 5-inch display held in one
# hand at a truck stop, and the only one that writes GPS. Everything else in
# the app *reads* gps_pings; this is where the readings come from.
#
# How the position gets in
# ------------------------
# The browser's Geolocation API, running on the driver's own handset. No
# telematics box, no SIM tracker, no third-party feed — the driver signs in,
# taps Start sharing, and the phone reports its position on a timer. Each
# reading is written to gps_pings against the vehicle and trip the driver is
# currently on, which is the same table Live GPS and Shipment Tracking already
# read. Nothing downstream changes: the map that showed a simulated feed now
# shows a real one.
#
# Constraints worth knowing before trusting it
# --------------------------------------------
#  * Geolocation only works in a secure context. https:// and localhost are
#    fine; a plain http:// deployment gets nothing and the screen says so
#    rather than sitting silently at "waiting for a fix".
#  * The driver must grant permission, and can revoke it. Both states are
#    reported on screen, because a tracking feature that fails quietly is
#    worse than one that is obviously off.
#  * Readings stop when the handset sleeps or the browser is backgrounded.
#    This is a phone, not a hardwired tracker: it is a good approximation of
#    where the load is, not a tachograph.
#
# Scope: Driver only. The RBAC matrix blocks every other role from the module,
# and the write path additionally refuses a session whose signed-in user is not
# the driver on the trip being updated.
# ==================================================================

# How often the handset reports in. Thirty seconds is frequent enough to draw a
# smooth line down a highway and slow enough not to flatten a phone battery on
# a twelve-hour run.
DRIVER_PING_SECONDS <- 30

driver_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # The browser half of the feature. It asks for a position, hands it to
    # Shiny, and repeats. watchPosition is deliberately not used: it fires on
    # every sensor twitch, which on a moving vehicle means hundreds of writes
    # an hour for no extra fidelity.
    tags$script(HTML(sprintf("
(function() {
  var NS = '%s';
  var timer = null;

  function send(id, val) { Shiny.setInputValue(NS + id, val, {priority: 'event'}); }

  function ok(pos) {
    send('-pos', {
      lat: pos.coords.latitude,
      lon: pos.coords.longitude,
      acc: pos.coords.accuracy,
      spd: pos.coords.speed,
      hdg: pos.coords.heading,
      at:  new Date().toISOString()
    });
  }

  function fail(err) {
    send('-geoerr', { code: err.code, msg: err.message });
  }

  function read() {
    navigator.geolocation.getCurrentPosition(ok, fail, {
      enableHighAccuracy: true, timeout: 20000, maximumAge: 10000
    });
  }

  Shiny.addCustomMessageHandler(NS + '-share', function(on) {
    if (timer) { clearInterval(timer); timer = null; }
    if (!on) { send('-geostate', 'off'); return; }
    if (!('geolocation' in navigator)) { send('-geostate', 'unsupported'); return; }
    if (!window.isSecureContext) { send('-geostate', 'insecure'); return; }
    send('-geostate', 'on');
    read();
    timer = setInterval(read, %d);
  });
})();
", id, DRIVER_PING_SECONDS * 1000))),

    uiOutput(ns("head")),
    uiOutput(ns("share")),
    uiOutput(ns("trip")),
    uiOutput(ns("recent"))
  )
}

driver_server <- function(id, user, nav) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sharing <- reactiveVal(FALSE)
    geo     <- reactiveVal(NULL)   # last browser status message
    last    <- reactiveVal(NULL)   # last position accepted

    # The trip this driver is actually on. A driver may have exactly one live
    # trip; anything else and there is nothing to attach a position to.
    my_trip <- reactive({
      store_get("gps_pings")           # re-read when a ping lands
      tr <- get_trips()
      t <- tr[tr$driver_id == (user()$user_id %||% "") |
                tr$driver_id == (user()$employee_id %||% ""), , drop = FALSE]
      t <- t[t$status %in% c("Planned", "Loading", "Running", "At Hub"), , drop = FALSE]
      if (!nrow(t)) NULL else t[1, ]
    })

    output$head <- renderUI({
      t <- my_trip()
      div(class = "d-flex align-items-start justify-content-between gap-3 mb-3 flex-wrap",
          div(h5(class = "tms-card-title", style = "font-size:1.0625rem;",
                 paste("Welcome,", user()$name)),
              p(class = "tms-card-sub",
                if (is.null(t)) "No trip assigned to you right now."
                else paste0("Trip ", t$trip_no, " · ", t$route_from, " → ", t$route_to))),
          if (!is.null(t)) pill(t$status))
    })

    # ---------------- location sharing ----------------

    output$share <- renderUI({
      t <- my_trip()
      on <- sharing()
      st <- geo()
      l  <- last()

      msg <- switch(st %||% "",
        "unsupported" = list("danger", "This phone's browser cannot report a location.",
                             "Location sharing needs a browser with GPS support. Ask the branch to log the position by phone instead."),
        "insecure"    = list("danger", "Location needs a secure connection",
                             "The browser only releases GPS over https. Open the app on its https address and try again."),
        "denied"      = list("warn", "Location permission refused",
                             "The phone blocked the request. Allow location for this site in the browser settings, then tap Start again."),
        "error"       = list("warn", "Could not get a fix",
                             "The phone could not reach a satellite. This is normal indoors or in a tunnel — it will retry on its own."),
        NULL)

      card_panel(
        title = "Share my location",
        sub = if (is.null(t))
                "Available once a trip is allocated to you."
              else paste("Reports every", DRIVER_PING_SECONDS, "seconds while this screen is open"),
        div(
          if (!is.null(msg)) div(class = "mb-3", callout(msg[[2]], msg[[3]], msg[[1]])),

          div(class = "d-flex align-items-center gap-3 flex-wrap",
              if (is.null(t))
                div(class = "field-static", "Nothing to track yet")
              else if (on)
                btn_dark(ns("stop"), "Stop sharing", fontawesome::fa("circle-stop"))
              else
                btn_primary(ns("start"), "Start sharing", fontawesome::fa("location-crosshairs")),

              if (on) span(class = "pill pill-green", "Live") else NULL,

              if (!is.null(l)) div(
                class = "tiny muted",
                sprintf("Last reported %s · %.4f, %.4f%s",
                        fmt_dt(l$ts), l$lat, l$lon,
                        if (!is.na(l$acc)) sprintf(" · ±%d m", round(l$acc)) else ""))
          ),

          # Said plainly, once, where the driver can read it. A tracking
          # feature people do not understand is a tracking feature people
          # switch off.
          div(class = "tiny muted mt-3",
              "Your position is recorded against this trip only, and only while sharing is on. ",
              "It is used to show the branch and the customer where the load is.")
        )
      )
    })

    observeEvent(input$start, {
      if (is.null(my_trip())) return()
      sharing(TRUE)
      session$sendCustomMessage(paste0(id, "-share"), TRUE)
      audit(user()$user_id, "gps-start", "driver", my_trip()$trip_no)
    })

    observeEvent(input$stop, {
      sharing(FALSE)
      session$sendCustomMessage(paste0(id, "-share"), FALSE)
      audit(user()$user_id, "gps-stop", "driver", my_trip()$trip_no %||% "")
    })

    observeEvent(input$geostate, {
      s <- input$geostate
      geo(if (identical(s, "on")) NULL else s)
      if (!identical(s, "on")) sharing(FALSE)
    })

    observeEvent(input$geoerr, {
      e <- input$geoerr
      # 1 = PERMISSION_DENIED in the Geolocation spec. The others are
      # position-unavailable and timeout, which are transient.
      geo(if (identical(as.integer(e$code), 1L)) "denied" else "error")
      if (identical(as.integer(e$code), 1L)) sharing(FALSE)
    })

    # ---------------- the write ----------------

    observeEvent(input$pos, {
      p <- input$pos
      t <- my_trip()

      # Refuse anything that is not a plausible reading against a trip this
      # user is actually driving. The browser is not a trusted input: a forged
      # Shiny message must not be able to move somebody else's truck.
      if (is.null(t)) return()
      lat <- suppressWarnings(as.numeric(p$lat))
      lon <- suppressWarnings(as.numeric(p$lon))
      if (is.na(lat) || is.na(lon)) return()
      if (lat < -90 || lat > 90 || lon < -180 || lon > 180) return()

      acc <- suppressWarnings(as.numeric(p$acc %||% NA))
      # A fix good to worse than 5 km is a cell-tower guess, not a position.
      # Recording it would draw the truck through the middle of a district.
      if (!is.na(acc) && acc > 5000) {
        geo("error")
        return()
      }

      # Geolocation reports speed in metres per second, and NULL when the
      # handset cannot work it out (stationary, or a single fix).
      spd <- suppressWarnings(as.numeric(p$spd %||% NA))
      kmh <- if (is.na(spd)) NA_real_ else round(spd * 3.6)

      store_insert("gps_pings", list(
        vehicle_id = t$vehicle_id,
        trip_no    = t$trip_no,
        lat        = round(lat, 5),
        lon        = round(lon, 5),
        speed      = if (is.na(kmh)) "" else kmh,
        heading    = compass(p$hdg),
        ignition   = if (!is.na(kmh) && kmh > 3) "ON" else "OFF",
        idle_min   = "",
        ts         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        # Source is recorded so a reading from a handset is never mistaken for
        # one from a hardwired tracker.
        source     = "driver-app",
        status     = if (!is.na(kmh) && kmh > 3) "Running" else "Halted"
      ))

      geo(NULL)
      last(list(lat = lat, lon = lon, acc = acc, ts = Sys.time()))
    })

    # ---------------- what the driver can see ----------------

    output$trip <- renderUI({
      t <- my_trip()
      if (is.null(t)) return(NULL)
      cn <- get_consignments(); c1 <- cn[cn$trip_no == t$trip_no, , drop = FALSE]
      v  <- store_get("vehicles")

      div(class = "mt-3", card_panel(
        title = "This trip",
        div(class = "row g-3",
            div(class = "col-md-6", dl_rows(
              "Vehicle"     = span(class = "mono", v$reg_no[match(t$vehicle_id, v$vehicle_id)]),
              "Route"       = paste(t$route_from, "→", t$route_to),
              "Distance"    = if (is.na(t$distance_km)) "—" else paste(inr_group(t$distance_km), "km"))),
            div(class = "col-md-6", dl_rows(
              "Consignment" = if (nrow(c1)) c1$lr_no[1] else "—",
              "Dispatched"  = fmt_dt(t$dispatch_dt),
              "ETA"         = fmt_dt(t$eta))))
      ))
    })

    output$recent <- renderUI({
      t <- my_trip()
      if (is.null(t)) return(NULL)
      g <- get_gps()
      g <- g[g$trip_no == t$trip_no, , drop = FALSE]
      if (!nrow(g)) return(NULL)
      g <- g[order(g$ts, decreasing = TRUE), , drop = FALSE]
      g <- g[seq_len(min(8, nrow(g))), , drop = FALSE]

      div(class = "mt-3", card_panel(
        title = "Recent positions",
        sub = "Most recent first — this is what the branch and the customer see",
        body_class = "tms-card-body p-0",
        tags$table(
          class = "table tms-plain-table",
          tags$thead(tags$tr(lapply(c("TIME", "POSITION", "SPEED", "SOURCE"),
                                    function(h) tags$th(h)))),
          tags$tbody(lapply(seq_len(nrow(g)), function(i) {
            r <- g[i, ]
            tags$tr(
              tags$td(fmt_dt(r$ts)),
              tags$td(span(class = "mono tiny", sprintf("%.4f, %.4f", r$lat, r$lon))),
              tags$td(if (is.na(r$speed)) "—" else paste0(round(r$speed), " km/h")),
              tags$td(class = "tiny muted",
                      if (identical(r$source %||% "", "driver-app")) "Driver phone" else "Telematics"))
          })))
      ))
    })
  })
}

#' Degrees to an eight-point compass label, matching what the GPS table stores.
compass <- function(deg) {
  d <- suppressWarnings(as.numeric(deg %||% NA))
  if (length(d) != 1 || is.na(d)) return("")
  pts <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  pts[(round(d %% 360 / 45) %% 8) + 1]
}
