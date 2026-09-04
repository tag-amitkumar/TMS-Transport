# ==================================================================
# Shared UI vocabulary.
#
# The deck reuses the same dozen shapes across all 30 screens — KPI card, status
# pill, tab strip, master–detail split, kanban column, timeline, callout. Each is
# built once here so the screens stay short and a styling change lands
# everywhere at once instead of in 31 places.
# ==================================================================

# ------------------------------------------------------------------
# Status -> pill colour
#
# One lookup shared by every screen, so "Delivered" is the same green on the
# booking list, the consignment table, the kanban and the customer portal.
# ------------------------------------------------------------------
STATUS_COLOUR <- c(
  # Bookings / consignments
  "Draft" = "grey", "Confirmed" = "blue", "Vehicle Allocated" = "blue",
  "In Transit" = "purple", "Delivered" = "green", "Closed" = "green",
  "Cancelled" = "red", "In Prep" = "blue", "Dispatched" = "blue",
  "At Hub" = "orange", "Out for Delivery" = "orange", "Pending" = "grey",
  "Unassigned" = "grey", "Priority" = "orange", "Awaiting vehicle" = "grey",

  # Vehicles / drivers
  "Available" = "green", "Allocated" = "blue", "Maintenance" = "orange",
  "Inactive" = "grey", "On trip" = "blue", "On leave" = "orange",

  # Documents
  "Uploaded" = "orange", "Verified" = "blue", "Approved" = "green",
  "Valid" = "green", "Expiring Soon" = "orange", "Expired" = "red",

  # Money
  "Sent" = "blue", "Paid" = "green", "Partially paid" = "orange",
  "Overdue" = "red", "Unbilled" = "grey",

  # Complaints
  "New" = "red", "Assigned" = "blue", "In Progress" = "orange",
  "Resolved" = "green", "In review" = "orange", "Open" = "red",

  # Generic
  "Active" = "green", "Suspended" = "orange", "Running" = "green",
  "Halted" = "orange", "Alert" = "red", "Planned" = "grey",
  "Loading" = "blue", "Completed" = "green", "Limited" = "orange",
  "Not Serviceable" = "grey", "High" = "red", "Medium" = "orange", "Low" = "grey"
)

#' Colour for a status label, falling back to grey.
#'
#' Single-bracket lookup on purpose: `STATUS_COLOUR[["4 days"]]` throws
#' "subscript out of bounds" for any label not in the table, and plenty of
#' labels are computed at render time (ageing buckets, "Halted · 38 min",
#' "Overdue 5 d"). Single brackets return NA for a miss, which is recoverable.
status_colour <- function(label) {
  hit <- unname(STATUS_COLOUR[as.character(label)])
  ifelse(is.na(hit), "grey", hit)
}

#' Coloured status pill.
pill <- function(label, colour = NULL, dot = TRUE) {
  if (is.null(label) || length(label) == 0) return(NULL)
  if (is.na(label) || !nzchar(as.character(label))) return(NULL)
  label <- as.character(label)
  colour <- colour %||% status_colour(label)
  span(class = paste0("pill pill-", colour, if (!dot) " nodot"), label)
}

#' Vectorised pill for use inside a DT column (returns HTML strings).
pill_html <- function(labels, colour = NULL) {
  labels <- as.character(labels)
  if (!length(labels)) return(character(0))
  cols <- if (is.null(colour)) status_colour(labels) else rep(colour, length.out = length(labels))
  out <- sprintf('<span class="pill pill-%s">%s</span>', cols, htmlEscape(labels))
  out[is.na(labels) | !nzchar(labels)] <- ""
  out
}

# ------------------------------------------------------------------
# Avatars
# ------------------------------------------------------------------

# Deterministic colour from the name, so a person keeps the same avatar colour
# on every screen without storing one.
AVATAR_COLOURS <- c("#1B3A5C", "#1667C7", "#6F42C1", "#1A7F45",
                    "#B3701A", "#C0392B", "#0E7490", "#7C3AED")

initials <- function(name) {
  parts <- strsplit(trimws(name %||% "?"), "\\s+")[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("?")
  toupper(paste0(substr(parts[1], 1, 1), if (length(parts) > 1) substr(parts[length(parts)], 1, 1) else ""))
}

avatar_colour <- function(name) {
  h <- sum(utf8ToInt(as.character(name %||% "?")))
  AVATAR_COLOURS[(h %% length(AVATAR_COLOURS)) + 1]
}

avatar <- function(name, size = c("md", "sm", "lg")) {
  size <- match.arg(size)
  cls <- c(md = "avatar", sm = "avatar avatar-sm", lg = "avatar avatar-lg")[[size]]
  div(class = cls, style = paste0("background:", avatar_colour(name)), initials(name))
}

avatar_html <- function(name, size = "sm") {
  sprintf('<div class="avatar avatar-%s" style="background:%s">%s</div>',
          size, avatar_colour(name), initials(name))
}

# ------------------------------------------------------------------
# Cards
# ------------------------------------------------------------------

#' KPI stat card.
#'
#' `accent` draws the coloured left rail the deck uses to flag cards that need
#' attention (expiring documents, overdue invoices).
stat_card <- function(label, value, sub = NULL, icon = NULL, unit = NULL,
                      accent = c("none", "warn", "danger", "ok")) {
  accent <- match.arg(accent)
  div(
    class = paste0("stat-card", if (accent != "none") paste0(" accent-", accent)),
    div(
      class = "stat-head",
      div(
        div(class = "stat-label", label),
        div(class = "stat-value", value, if (!is.null(unit)) span(class = "unit", unit))
      ),
      if (!is.null(icon)) div(class = "stat-ico", icon)
    ),
    if (!is.null(sub)) div(class = "stat-sub", sub)
  )
}

#' Row of stat cards on an even grid.
stat_row <- function(...) {
  cards <- list(...)
  cards <- cards[!vapply(cards, is.null, logical(1))]
  div(
    class = "row g-3 mb-3",
    lapply(cards, function(c) div(class = "col", c))
  )
}

#' Standard content card with an optional header and right-aligned actions.
card_panel <- function(title = NULL, sub = NULL, ..., actions = NULL,
                       foot = NULL, body_class = "tms-card-body") {
  div(
    class = "tms-card",
    if (!is.null(title)) div(
      class = "tms-card-head",
      div(h6(class = "tms-card-title", title),
          if (!is.null(sub)) p(class = "tms-card-sub", sub)),
      if (!is.null(actions)) div(class = "ms-auto d-flex gap-2 align-items-center", actions)
    ),
    div(class = body_class, ...),
    if (!is.null(foot)) div(class = "tms-card-foot", foot)
  )
}

# ------------------------------------------------------------------
# Navigation widgets
# ------------------------------------------------------------------

#' Underlined tab strip. `items` is a named character vector: label = value.
#'
#' Implemented as plain divs plus a hidden Shiny input rather than tabsetPanel
#' because the deck's strip carries per-tab counts and sits inline with action
#' buttons, neither of which tabsetPanel allows.
tab_strip <- function(ns, id, items, counts = NULL, selected = NULL) {
  selected <- selected %||% unname(items)[1]
  div(
    class = "tab-row",
    lapply(seq_along(items), function(i) {
      val <- unname(items)[i]
      lbl <- names(items)[i]
      cnt <- if (!is.null(counts)) counts[[val]] else NULL
      div(
        class = paste0("tab-item", if (identical(val, selected)) " active"),
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns(id), val),
        lbl,
        if (!is.null(cnt)) span(class = "count", cnt)
      )
    })
  )
}

#' Pill-shaped filter chips with counts.
chip_row <- function(ns, id, items, counts = NULL, selected = NULL) {
  selected <- selected %||% unname(items)[1]
  div(
    class = "chip-row",
    lapply(seq_along(items), function(i) {
      val <- unname(items)[i]
      lbl <- names(items)[i]
      cnt <- if (!is.null(counts)) counts[[val]] else NULL
      div(
        class = paste0("chip", if (identical(val, selected)) " active"),
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns(id), val),
        lbl,
        if (!is.null(cnt)) span(class = "count", cnt)
      )
    })
  )
}

# ------------------------------------------------------------------
# Layout
# ------------------------------------------------------------------

#' Master–detail split: wide list on the left, 360° panel on the right.
split_view <- function(main, side) div(class = "split", div(main), div(side))

#' Definition list used in every detail panel.
dl_rows <- function(...) {
  pairs <- list(...)
  tags$dl(
    class = "dl",
    lapply(seq_along(pairs), function(i) {
      v <- pairs[[i]]
      if (is.null(v)) return(NULL)
      tagList(tags$dt(names(pairs)[i]), tags$dd(v))
    })
  )
}

#' Coloured callout box.
callout <- function(title, body, type = c("info", "warn", "danger", "ok"), icon = NULL) {
  type <- match.arg(type)
  div(
    class = paste0("callout callout-", type),
    if (!is.null(icon)) div(icon),
    div(span(class = "ttl", title), body)
  )
}

#' Thin progress bar.
progress_bar <- function(pct, colour = c("amber", "green", "blue", "red")) {
  colour <- match.arg(colour)
  pct <- max(0, min(100, as.numeric(pct %||% 0)))
  div(class = paste("bar", if (colour != "amber") colour else ""),
      tags$i(style = paste0("width:", pct, "%")))
}

# ------------------------------------------------------------------
# Timeline
# ------------------------------------------------------------------

#' Vertical status timeline.
#'
#' `steps` is a list of list(title=, sub=, when=, state=) where state is
#' "done" / "current" / "todo". Numbered dots are used for pending steps and a
#' tick for completed ones, matching the deck.
timeline <- function(steps) {
  div(
    class = "tl",
    lapply(seq_along(steps), function(i) {
      s <- steps[[i]]
      state <- s$state %||% "todo"
      div(
        class = "tl-item",
        div(class = paste0("tl-dot ", state),
            if (identical(state, "done")) HTML("&#10003;") else as.character(i)),
        div(
          class = "d-flex justify-content-between align-items-start gap-2",
          div(div(class = "tl-title", s$title),
              if (!is.null(s$sub)) div(class = "tl-sub", s$sub)),
          if (!is.null(s$when)) div(class = "tl-when", s$when)
        )
      )
    })
  )
}

# ------------------------------------------------------------------
# Kanban
# ------------------------------------------------------------------

KAN_DOT <- c(grey = "#94A3B8", blue = "#1667C7", orange = "#EFA31D",
             green = "#1A7F45", red = "#C0392B", purple = "#6F42C1")

# Same single-bracket rule as status_colour(): a miss must degrade to grey,
# not throw.
kan_dot <- function(colour) {
  hit <- unname(KAN_DOT[as.character(colour)])
  if (length(hit) != 1 || is.na(hit)) "#94A3B8" else hit
}

#' One kanban column.
kanban_col <- function(title, colour, cards, note = NULL) {
  div(
    class = "kan-col",
    div(class = "kan-head",
        span(class = "dot", style = paste0("background:", kan_dot(colour))),
        title,
        span(class = "count", note %||% length(cards))),
    cards
  )
}

#' One kanban card. Clicking sets `input$<id>` to `value`.
kanban_card <- function(ns, id, value, title, meta = NULL, footer = NULL, rail = NULL) {
  div(
    class = paste0("kan-card", if (!is.null(rail)) paste0(" rail-", rail)),
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})", ns(id), value),
    div(class = "t", title),
    if (!is.null(meta)) div(class = "m", meta),
    if (!is.null(footer)) div(class = "f", footer)
  )
}

#' Grid wrapper sizing the columns evenly.
kanban_board <- function(...) {
  cols <- list(...)
  div(class = "kanban",
      style = sprintf("grid-template-columns: repeat(%d, minmax(0,1fr));", length(cols)),
      cols)
}

# ------------------------------------------------------------------
# Tables
# ------------------------------------------------------------------

#' DT wrapper with the deck's table styling and sane defaults.
#'
#' Search box is suppressed (screens provide their own tab/chip filters), rows
#' are single-select so the 360° panel has an unambiguous subject, and HTML in
#' cells is trusted because every column is built by the app, never by a user.
tms_table <- function(df, colnames = NULL, page = 12, selection = "single",
                      align = NULL, order = NULL, height = NULL) {
  DT::datatable(
    df,
    rownames   = FALSE,
    colnames   = colnames %||% names(df),
    escape     = FALSE,
    selection  = list(mode = selection, target = "row"),
    class      = "compact stripe-none",
    options = list(
      dom          = "tip",
      pageLength   = page,
      lengthChange = FALSE,
      ordering     = !is.null(order),
      order        = order %||% list(),
      scrollX      = TRUE,
      scrollY      = height,
      columnDefs   = c(
        list(list(className = "dt-head-left", targets = "_all")),
        if (!is.null(align)) align else list()
      ),
      language = list(
        info          = "Showing _START_ to _END_ of _TOTAL_",
        infoEmpty     = "No records",
        emptyTable    = "Nothing here yet",
        paginate      = list(previous = "‹", `next` = "›")
      )
    )
  )
}

#' Two-line table cell: bold primary above muted secondary.
cell2 <- function(l1, l2) {
  sprintf('<div class="cell-2"><div class="l1">%s</div><div class="l2">%s</div></div>',
          htmlEscape(l1 %||% ""), htmlEscape(l2 %||% ""))
}

#' Monospaced code cell (vehicle numbers, GSTIN, LR numbers).
mono <- function(x) sprintf('<span class="mono">%s</span>', htmlEscape(x %||% ""))

#' Right-aligned column definition helper for tms_table(align=).
align_right <- function(idx) list(list(className = "dt-right", targets = idx))

# ------------------------------------------------------------------
# Page header
# ------------------------------------------------------------------

#' Title block sitting above a screen's content, with optional right actions.
page_head <- function(title, sub = NULL, actions = NULL) {
  div(
    class = "d-flex align-items-start justify-content-between gap-3 mb-3 flex-wrap",
    div(h5(class = "tms-card-title", style = "font-size:1.0625rem;", title),
        if (!is.null(sub)) p(class = "tms-card-sub", sub)),
    if (!is.null(actions)) div(class = "d-flex gap-2 align-items-center flex-wrap", actions)
  )
}

#' Empty-state placeholder for a panel with nothing selected.
empty_panel <- function(msg = "Select a row to see details", icon = "\U0001F4C4") {
  div(class = "text-center py-5",
      div(style = "font-size:1.6rem;opacity:.35;", icon),
      div(class = "muted tiny mt-2", msg))
}

# ------------------------------------------------------------------
# Buttons
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# Printable consignment note
#
# Indian road freight moves on a paper LR, and the same details are needed by
# three different people at once: the consignor keeps one, the consignee signs
# one, and one rides in the cab. So the sheet is three identical slips on a
# single A4 with cut lines between them, not three separate pages.
#
# Rendered as HTML and printed by the browser rather than generated as a PDF:
# no LaTeX or headless-Chrome dependency, and the operator gets the familiar
# print dialog with their own paper size and printer already selected.
# ------------------------------------------------------------------

#' One slip. `copy` is the label along the top ("Consignor Copy" etc).
lr_slip <- function(copy, d) {
  div(
    class = "lr-slip",
    div(class = "lr-head",
        div(class = "lr-brand",
            div(class = "lr-tile", "A"),
            div(div(class = "lr-co", d$company),
                div(class = "lr-sub", d$office))),
        div(class = "lr-title",
            div(class = "lr-doc", "LORRY RECEIPT"),
            div(class = "lr-copy", copy)),
        div(class = "lr-nos",
            div(tags$b(d$lr_no)),
            div(class = "lr-sub", d$cn_no),
            div(class = "lr-sub", paste("Date", d$date)))),

    div(class = "lr-grid",
        div(class = "lr-cell",
            div(class = "lr-lbl", "Consignor"),
            div(class = "lr-val", d$consignor),
            div(class = "lr-sm", d$from_addr)),
        div(class = "lr-cell",
            div(class = "lr-lbl", "Consignee"),
            div(class = "lr-val", d$consignee),
            div(class = "lr-sm", d$to_addr))),

    div(class = "lr-grid4",
        div(class = "lr-cell", div(class = "lr-lbl", "From"),    div(class = "lr-val", d$from)),
        div(class = "lr-cell", div(class = "lr-lbl", "To"),      div(class = "lr-val", d$to)),
        div(class = "lr-cell", div(class = "lr-lbl", "Vehicle"), div(class = "lr-val lr-mono", d$vehicle)),
        div(class = "lr-cell", div(class = "lr-lbl", "Driver"),  div(class = "lr-val", d$driver))),

    div(class = "lr-grid4",
        div(class = "lr-cell", div(class = "lr-lbl", "Material"), div(class = "lr-val", d$material)),
        div(class = "lr-cell", div(class = "lr-lbl", "Weight"),   div(class = "lr-val", d$weight)),
        div(class = "lr-cell", div(class = "lr-lbl", "Packages"), div(class = "lr-val", d$packages)),
        div(class = "lr-cell", div(class = "lr-lbl", "GSTIN"),    div(class = "lr-val lr-mono", d$gstin))),

    div(class = "lr-foot",
        # Payment terms are the loudest thing on the slip: the driver decides
        # whether to release the goods on the strength of it.
        div(class = paste0("lr-pay lr-pay-", d$pay_colour),
            div(class = "lr-lbl", "Payment"),
            div(class = "lr-pay-val", d$payment),
            if (nzchar(d$pay_note)) div(class = "lr-sm", d$pay_note)),
        div(class = "lr-charges",
            div(span("Freight"), span(d$freight)),
            div(span(paste0("GST (", d$gst_mode, ")")), span(d$gst)),
            div(span("Insurance"), span(d$insurance)),
            div(class = "lr-total", span("Total"), span(d$total))),
        div(class = "lr-sign",
            div(class = "lr-sm", "Received the goods in good condition"),
            div(class = "lr-sigline"),
            div(class = "lr-lbl", "Consignee signature & stamp"))),

    if (nzchar(d$manual_note)) div(class = "lr-manual", d$manual_note)
  )
}

#' The full A4 sheet: three slips, cut lines between.
lr_print_sheet <- function(d) {
  copies <- c("Consignor Copy", "Consignee Copy", "Driver Copy")
  div(
    id = "tms-print",
    div(class = "lr-sheet",
        lapply(seq_along(copies), function(i) {
          tagList(
            lr_slip(copies[i], d),
            if (i < length(copies)) div(class = "lr-cut")
          )
        }))
  )
}

#' Show the sheet in a modal with a Print button.
show_lr_print <- function(d, title = "Print consignment note") {
  showModal(modalDialog(
    # xl, because the preview is a full A4 sheet — anything narrower crops the
    # slips and the operator cannot check the paperwork before committing it
    # to paper.
    title = title, size = "xl", easyClose = TRUE,
    div(class = "tiny muted mb-2",
        "Three copies on one A4 sheet — consignor, consignee and driver. ",
        "Cut along the dashed lines."),
    lr_print_sheet(d),
    footer = tagList(
      modalButton("Close"),
      tags$button(class = "btn btn-tms-primary", onclick = "window.print()",
                  "Print / Save as PDF")
    )
  ))
}

btn_primary <- function(id, label, icon = NULL, class = "") {
  actionButton(id, tagList(icon, label), class = paste("btn-tms-primary", class))
}
btn_dark <- function(id, label, icon = NULL, class = "") {
  actionButton(id, tagList(icon, label), class = paste("btn-tms-dark", class))
}
btn_ghost <- function(id, label, icon = NULL, class = "") {
  actionButton(id, tagList(icon, label), class = paste("btn-tms-ghost", class))
}
