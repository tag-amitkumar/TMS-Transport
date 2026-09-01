# ==================================================================
# Theme — Bootstrap 5 variables matched to the TMS design deck.
#
# Colour values are read off the mockups rather than invented, so the built app
# and the PDF sit side by side without drift. Anything structural (sidebar,
# stat cards, kanban, pills) lives in www/styles.css; this file only sets the
# Bootstrap tokens those rules build on.
# ==================================================================

TMS <- list(
  # Sidebar / brand
  navy_900   = "#0F2338",  # sidebar background
  navy_800   = "#17324D",  # sidebar active row
  navy_700   = "#1B3A5C",  # dark buttons
  navy_ink   = "#12263F",  # page headings, big numbers

  # Accent — the amber used for every primary action in the deck
  amber      = "#EFA31D",
  amber_dark = "#D18E12",
  amber_soft = "#FDF8EC",  # selected table row

  # Surfaces
  body_bg    = "#F4F7FB",
  card_bg    = "#FFFFFF",
  border     = "#E4EAF1",
  border_sub = "#EEF2F7",

  # Text
  ink        = "#1E2E44",
  muted      = "#64748B",
  faint      = "#94A3B8",

  # Status palette. Each pairs a tinted background with a readable foreground;
  # both are needed because the deck's pills are filled, not outlined.
  green      = "#1A7F45", green_bg  = "#E9F7EF",
  blue       = "#1667C7", blue_bg   = "#E7F1FD",
  purple     = "#6F42C1", purple_bg = "#F1EAFB",
  orange     = "#B3701A", orange_bg = "#FDF3E0",
  red        = "#C0392B", red_bg    = "#FDECEB",
  grey       = "#64748B", grey_bg   = "#EEF1F5"
)

app_theme <- function() {
  bs_theme(
    version = 5,
    bg = TMS$body_bg,
    fg = TMS$ink,
    primary   = TMS$amber,
    secondary = TMS$navy_700,
    success   = TMS$green,
    info      = TMS$blue,
    warning   = TMS$orange,
    danger    = TMS$red,

    # Inter carries the deck's UI text. A local fallback stack matters because
    # font_google() fetches at build time and a blocked/offline host would
    # otherwise drop the app to a serif default.
    base_font    = font_google("Inter", local = TRUE),
    heading_font = font_google("Inter", local = TRUE),
    code_font    = font_google("JetBrains Mono", local = TRUE),

    "body-bg"          = TMS$body_bg,
    "body-color"       = TMS$ink,
    "card-bg"          = TMS$card_bg,
    "card-border-color"= TMS$border,
    "card-cap-bg"      = TMS$card_bg,
    "border-color"     = TMS$border,
    "border-radius"    = "10px",
    "border-radius-sm" = "7px",
    "border-radius-lg" = "12px",
    "font-size-base"   = "0.875rem",
    "headings-font-weight" = "650",
    "table-border-color"   = TMS$border_sub,
    "input-border-color"   = "#D6DEE8",
    "input-focus-border-color" = TMS$amber,
    "input-btn-focus-color"    = "rgba(239,163,29,.22)",
    "link-color"           = TMS$blue,
    "link-decoration"      = "none"
  )
}
