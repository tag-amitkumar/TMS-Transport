# ------------------------------------------------------------------
# One-shot dependency install for the TMS app.
#   Rscript install_packages.R
# ------------------------------------------------------------------
pkgs <- c(
  "shiny", "bslib", "htmltools",          # app framework + theming
  "dplyr", "tidyr", "purrr", "stringr",   # data wrangling
  "readr", "lubridate", "scales",         # io / dates / formatting
  "DT", "plotly",                         # tables + charts
  "leaflet",                              # Live GPS map
  "shinyWidgets", "fontawesome",          # inputs + icons
  "bcrypt", "uuid", "jsonlite",           # auth, ids, config
  "openxlsx"                              # xlsx export
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All dependencies already present.")
}
