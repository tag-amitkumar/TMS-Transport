# ------------------------------------------------------------------
# Container dependency install.
#
# Mirrors the package list in ../install_packages.R, with two changes that
# matter only inside Docker:
#
#   1. Packages come from Posit Package Manager's Ubuntu channel rather than
#      cloud.r-project.org. P3M serves precompiled binaries for this exact
#      distro, so the image builds in minutes instead of the hour-plus a
#      source build of dplyr/plotly/leaflet costs on a 4-core Mac mini.
#   2. Failure is fatal. install.packages() only warns when a package does not
#      install, which would otherwise bake a broken image that fails at
#      runtime instead of at build time.
# ------------------------------------------------------------------

options(
  repos = c(CRAN = Sys.getenv(
    "CRAN_REPO",
    "https://p3m.dev/cran/__linux__/noble/latest"
  )),
  Ncpus = max(1L, parallel::detectCores()),
  warn = 2
)

pkgs <- c(
  "shiny", "bslib", "htmltools",          # app framework + theming
  "dplyr", "tidyr", "purrr", "stringr",   # data wrangling
  "tibble",                               # used namespaced in R/mod_pod.R
  "readr", "lubridate", "scales",         # io / dates / formatting
  "DT", "plotly",                         # tables + charts
  "leaflet",                              # Live GPS map
  "shinyWidgets", "fontawesome",          # inputs + icons
  "bcrypt", "uuid", "jsonlite",           # auth, ids, config
  "httr",                                 # R/github_sync.R (inert unless a PAT is set)
  "openxlsx"                              # xlsx export
)

install.packages(pkgs)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Image build aborted — these packages did not install: ",
       paste(missing, collapse = ", "))
}

message("All ", length(pkgs), " packages installed.")
