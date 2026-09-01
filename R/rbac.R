# ==================================================================
# Role-based access control and branch scoping.
#
# Implements the permission matrix drawn on the Security & Roles screen:
# ten access levels x six actions (view / create / edit / delete / approve /
# export), configurable per module.
#
# Two rules this file exists to enforce:
#
#   1. Hiding a button is not access control. Every guard here is callable from
#      the server side, and mutating handlers call require_perm() before they
#      touch the store — a user who forges an input id still gets refused.
#
#   2. Branch scoping is applied at the data layer, not the view layer. The deck
#      states "Users assigned to this branch see only its own data unless
#      granted cross-branch access", so scope_branch() filters rows before they
#      ever reach a table or a chart.
# ==================================================================

MODULES <- c(
  "dashboard", "branches", "users", "clients", "vendors",
  "bookings", "cargo_moto", "consignments", "trips", "ewaybill",
  "pod", "tracking", "livegps", "vehicles", "drivers", "pincodes",
  "complaints", "invoices", "payments",
  "hrms_employees", "hrms_attendance", "hrms_payroll",
  "reports", "portal_branch", "portal_customer", "portal_vendor",
  "security", "settings"
)

ACTIONS <- c("view", "create", "edit", "delete", "approve", "export")

# Default matrix, transcribed from the Security & Roles screen. Rows the deck
# marked with a tick get the action; everything else is denied. The seed writes
# this into permissions.csv, after which the Security screen edits the table and
# this constant is only the fallback for a missing file.
#
# Per-module refinements the matrix implies but could not show in one grid:
#   - Accountant has no create/edit on operations, but full rights on invoices
#     and payments (its row on the deck reads "Invoices, payments, GST, payroll").
#   - HR Manager's create/edit applies to the HRMS modules only.
#   - Driver, Vendor and Customer are portal-only: view, and only their own rows
#     (enforced additionally by scope_owner()).
DEFAULT_PERMISSIONS <- local({
  grid <- list(
    #                     view  create  edit  delete approve export
    "Super Admin"        = c(1, 1, 1, 1, 1, 1),
    "Branch Admin"       = c(1, 1, 1, 0, 1, 1),
    "Operations Manager" = c(1, 1, 1, 0, 1, 0),
    "Booking Executive"  = c(1, 1, 1, 0, 0, 0),
    "Dispatcher"         = c(1, 1, 0, 0, 0, 0),
    "Accountant"         = c(1, 0, 0, 0, 1, 1),
    "HR Manager"         = c(1, 1, 1, 0, 1, 1),
    "Driver"             = c(1, 0, 0, 0, 0, 0),
    "Vendor"             = c(1, 0, 0, 0, 0, 0),
    "Customer"           = c(1, 0, 0, 0, 0, 0)
  )

  # Modules each role may not see at all, regardless of its action row.
  blocked <- list(
    "Operations Manager" = c("users", "security", "settings", "invoices",
                             "payments", "hrms_payroll", "branches"),
    "Booking Executive"  = c("users", "security", "settings", "invoices",
                             "payments", "hrms_employees", "hrms_attendance",
                             "hrms_payroll", "branches", "reports"),
    "Dispatcher"         = c("users", "security", "settings", "invoices",
                             "payments", "hrms_employees", "hrms_attendance",
                             "hrms_payroll", "branches", "reports"),
    "Accountant"         = c("users", "security", "settings", "livegps",
                             "cargo_moto", "pincodes"),
    "HR Manager"         = c("users", "security", "settings", "bookings",
                             "cargo_moto", "consignments", "ewaybill", "pod",
                             "livegps", "invoices", "payments", "pincodes"),
    "Branch Admin"       = c("security", "settings"),
    "Driver"             = setdiff(MODULES, c("tracking", "pod", "trips")),
    "Vendor"             = setdiff(MODULES, c("portal_vendor", "pod", "tracking")),
    "Customer"           = setdiff(MODULES, c("portal_customer", "tracking"))
  )

  # Modules an otherwise-restricted role keeps full create/edit on, because its
  # job is that module (HR Manager on HRMS, Accountant on money).
  owned <- list(
    "Accountant" = c("invoices", "payments"),
    "HR Manager" = c("hrms_employees", "hrms_attendance", "hrms_payroll")
  )

  rows <- list()
  for (role in names(grid)) {
    base  <- grid[[role]]
    block <- blocked[[role]] %||% character(0)
    own   <- owned[[role]]   %||% character(0)
    for (m in MODULES) {
      vals <- if (m %in% block) rep(0, 6) else base
      if (m %in% own) vals <- c(1, 1, 1, 0, 1, 1)
      rows[[length(rows) + 1]] <- tibble::tibble(
        role = role, module = m,
        view = vals[1], create = vals[2], edit = vals[3],
        delete = vals[4], approve = vals[5], export = vals[6]
      )
    }
  }
  dplyr::bind_rows(rows)
})

#' Can this role perform `action` on `module`?
#'
#' Reads the live permissions table so edits made on the Security screen take
#' effect without a restart, falling back to the compiled-in matrix if the file
#' is missing.
can <- function(role, module, action = "view") {
  if (identical(role, "Super Admin")) return(TRUE)
  if (!action %in% ACTIONS) return(FALSE)

  p <- store_get("permissions")
  if (!nrow(p)) p <- DEFAULT_PERMISSIONS

  row <- p[p$role == role & p$module == module, , drop = FALSE]
  if (!nrow(row)) return(FALSE)
  isTRUE(as.numeric(row[[action]][1]) == 1)
}

#' Server-side guard. Returns TRUE, or shows a refusal and returns FALSE.
#'
#' Called at the top of every observeEvent that mutates data, so a hidden button
#' is defence in depth rather than the only defence.
require_perm <- function(session, role, module, action, quiet = FALSE) {
  if (can(role, module, action)) return(TRUE)
  if (!quiet) {
    showNotification(
      sprintf("Your role (%s) is not permitted to %s here.", role, action),
      type = "error", duration = 5, session = session
    )
  }
  FALSE
}

#' Modules this role may see, in sidebar order.
visible_modules <- function(role) MODULES[vapply(MODULES, function(m) can(role, m, "view"), logical(1))]

# ------------------------------------------------------------------
# Branch scoping
# ------------------------------------------------------------------

#' Restrict a data frame to the user's branch.
#'
#' A no-op for Super Admin and for any user flagged cross_branch, matching the
#' deck's note that scoping is bypassable by grant. Frames without the branch
#' column pass through untouched — several reference tables (leave_types,
#' settings) are global by nature.
scope_branch <- function(df, user, col = "branch_id") {
  if (is.null(user) || !nrow(df)) return(df)
  if (identical(user$role, "Super Admin")) return(df)
  if (isTRUE(as.logical(user$cross_branch %||% "FALSE"))) return(df)
  if (!col %in% names(df)) return(df)
  df[df[[col]] == (user$branch_id %||% ""), , drop = FALSE]
}

#' Restrict a data frame to rows owned by the signed-in portal user.
#'
#' Customer and Vendor logins are external parties: the deck's Security matrix
#' limits them to "own shipments only" / "assigned trips". Branch scoping is not
#' enough for them, because a customer's consignments span every branch.
scope_owner <- function(df, user, client_col = "client_id", vendor_col = "vendor_id") {
  if (is.null(user) || !nrow(df)) return(df)
  if (identical(user$role, "Customer") && client_col %in% names(df)) {
    return(df[df[[client_col]] == (user$client_id %||% ""), , drop = FALSE])
  }
  if (identical(user$role, "Vendor") && vendor_col %in% names(df)) {
    return(df[df[[vendor_col]] == (user$vendor_id %||% ""), , drop = FALSE])
  }
  df
}

#' Both scopes in the order they must be applied.
scope_all <- function(df, user, ...) scope_owner(scope_branch(df, user), user, ...)
