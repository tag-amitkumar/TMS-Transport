# ==================================================================
# GitHub data sync — keeping data/ in the repo current with what the
# running app has written.
#
# The problem this solves
# -----------------------
# data/ is committed, so a fresh clone starts with a working dataset. But
# once the app is running, every booking, POD and GPS ping is written to
# the container's own filesystem and goes nowhere else. On shinyapps.io
# that filesystem is ephemeral: the instance idles out, the next visitor
# gets a fresh one, and the day's work is gone. The repo, meanwhile,
# still shows the seed.
#
# How it works
# ------------
# Writes go to GitHub's Contents API over plain HTTPS — no local git
# repository, no libgit2, no .git folder. That matters because rsconnect
# excludes .git from the deployment bundle, so anything git2r-based
# simply cannot work on the host where this is actually needed.
#
# Why writes are queued rather than pushed immediately
# ----------------------------------------------------
# The obvious design — push inside .write_tbl() — is wrong here. A driver
# on the road pings their position every DRIVER_PING_SECONDS, and each
# ping is a store write. Pushing per write would mean a commit every 30
# seconds per driver, a repo history no human can read, and a save path
# that blocks on a network round-trip while someone is trying to book a
# consignment. So .write_tbl() only marks the table dirty — an in-memory
# set operation, microseconds — and a timer flushes whatever is dirty at
# most once every SYNC_INTERVAL_MS. Fifty pings inside that window
# collapse into one commit, and nobody waits on the network.
#
# Configuration — .Renviron, which is gitignored but IS shipped by
# rsconnect, so the same file configures local dev and the deployment:
#   TMS_GITHUB_REPO_URL = "https://github.com/tag-amitkumar/TMS-Transport.git"
#   TMS_GITHUB_PAT      = "github_pat_..."     # Contents: read/write
#   TMS_GITHUB_BRANCH   = "minimal-setup"      # optional
#
# The PAT must never be committed. This repo is public: a token in a
# commit is compromised the moment it is pushed, and GitHub will revoke
# it for you. Set it in .Renviron locally, and in the shinyapps.io
# dashboard (Settings -> Variables) for the deployment.
#
# Unconfigured is a supported state. With no PAT the app behaves exactly
# as it did before — writes go to disk, nothing is pushed, nothing errors.
# ==================================================================

SYNC_INTERVAL_MS <- 60 * 1000

# Tables never worth pushing. The reference data is a 19k-row static
# master built by tools/build-pincodes.R; it is not runtime state, and
# re-uploading it because a lookup touched the cache is pure waste.
SYNC_SKIP <- c("pincodes", "cities", "city_aliases")

.sync <- new.env(parent = emptyenv())
.sync$dirty  <- character(0)
.sync$status <- list(ok = NA, msg = "Not configured.")

gh_config <- function() {
  list(
    repo_url = Sys.getenv("TMS_GITHUB_REPO_URL", unset = ""),
    pat      = Sys.getenv("TMS_GITHUB_PAT",      unset = ""),
    branch   = Sys.getenv("TMS_GITHUB_BRANCH",   unset = "minimal-setup")
  )
}

#' Is data sync switched on? Everything below is a no-op when it is not.
gh_enabled <- function() {
  cfg <- gh_config()
  nzchar(cfg$repo_url) && nzchar(cfg$pat)
}

#' Last sync outcome, for the Settings screen badge.
gh_status <- function() .sync$status

#' Tables currently waiting to be pushed.
gh_pending <- function() .sync$dirty

# ------------------------------------------------------------------
# Contents API plumbing
# ------------------------------------------------------------------

gh_parse_repo <- function(repo_url) {
  m <- regmatches(repo_url,
                  regexec("github\\.com[:/]+([^/]+)/([^/.]+)(\\.git)?/?$", repo_url))[[1]]
  if (length(m) < 3) return(NULL)
  list(owner = m[2], repo = m[3])
}

.gh_headers <- function(pat) {
  httr::add_headers(Authorization = paste("token", pat),
                    Accept = "application/vnd.github+json")
}

.gh_url <- function(owner, repo, path) {
  sprintf("https://api.github.com/repos/%s/%s/contents/%s",
          owner, repo, utils::URLencode(path))
}

# The blob sha of the file as GitHub currently holds it, or NULL if it is
# not there. The API requires it to update an existing file — which is
# what makes a concurrent overwrite fail loudly instead of silently
# clobbering, the behaviour we want when two instances are up.
.gh_sha <- function(owner, repo, path, cfg) {
  res <- tryCatch(
    httr::GET(.gh_url(owner, repo, path), .gh_headers(cfg$pat),
              query = list(ref = cfg$branch)),
    error = function(e) NULL)
  if (!is.null(res) && httr::status_code(res) == 200) {
    httr::content(res, as = "parsed")$sha
  } else NULL
}

#' Create or update one file in the repo. Returns list(ok, msg).
gh_put_file <- function(local_path, repo_path, message, cfg = gh_config()) {
  parsed <- gh_parse_repo(cfg$repo_url)
  if (is.null(parsed)) {
    return(list(ok = FALSE, msg = "Could not read owner/repo from TMS_GITHUB_REPO_URL."))
  }
  if (!file.exists(local_path)) {
    return(list(ok = FALSE, msg = paste("No such file:", local_path)))
  }

  raw <- readBin(local_path, "raw", file.info(local_path)$size)
  body <- list(message = message,
               content = jsonlite::base64_enc(raw),
               branch  = cfg$branch)
  sha <- .gh_sha(parsed$owner, parsed$repo, repo_path, cfg)
  if (!is.null(sha)) body$sha <- sha

  res <- tryCatch(
    httr::PUT(.gh_url(parsed$owner, parsed$repo, repo_path),
              .gh_headers(cfg$pat), body = body, encode = "json"),
    error = function(e) NULL)

  if (is.null(res)) return(list(ok = FALSE, msg = "Could not reach GitHub."))
  code <- httr::status_code(res)
  if (code %in% c(200, 201)) {
    return(list(ok = TRUE, msg = paste("Synced", basename(repo_path),
                                       "at", format(Sys.time(), "%H:%M:%S"))))
  }
  err <- tryCatch(httr::content(res, as = "parsed")$message,
                  error = function(e) NULL)
  if (is.null(err)) err <- "unknown error"
  list(ok = FALSE, msg = paste0("GitHub API ", code, ": ", err))
}

#' Overwrite one local file with the repo's copy. Silent on any failure —
#' a missing or unreachable file must not stop the app from starting.
gh_get_file <- function(local_path, repo_path, cfg = gh_config()) {
  parsed <- gh_parse_repo(cfg$repo_url)
  if (is.null(parsed)) return(invisible(FALSE))
  res <- tryCatch(
    httr::GET(.gh_url(parsed$owner, parsed$repo, repo_path), .gh_headers(cfg$pat),
              query = list(ref = cfg$branch)),
    error = function(e) NULL)
  if (is.null(res) || httr::status_code(res) != 200) return(invisible(FALSE))
  body <- httr::content(res, as = "parsed")
  if (is.null(body$content)) return(invisible(FALSE))
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  writeBin(jsonlite::base64_dec(gsub("\\s", "", body$content)), local_path)
  invisible(TRUE)
}

# ------------------------------------------------------------------
# The queue
# ------------------------------------------------------------------

#' Mark a table as needing a push. Called from .write_tbl() on every
#' write, so it must stay cheap and must never raise.
gh_mark_dirty <- function(name) {
  if (name %in% SYNC_SKIP) return(invisible(FALSE))
  if (!gh_enabled()) return(invisible(FALSE))
  .sync$dirty <- union(.sync$dirty, name)
  invisible(TRUE)
}

#' Push every dirty table, one commit each, and clear the queue.
#'
#' A table that fails goes back on the queue rather than being dropped —
#' a transient network failure should cost a minute's delay, not a day's
#' bookings. Returns the tables actually pushed.
gh_flush <- function() {
  if (!gh_enabled() || !length(.sync$dirty)) return(invisible(character(0)))
  cfg <- gh_config()
  todo <- .sync$dirty
  .sync$dirty <- character(0)
  done <- character(0); failed <- character(0); last <- NULL

  for (name in todo) {
    p <- tbl_path(name)
    res <- tryCatch(
      gh_put_file(p, p, sprintf("Auto-sync: %s — %s", name,
                                format(Sys.time(), "%Y-%m-%d %H:%M:%S")), cfg),
      error = function(e) list(ok = FALSE,
                               msg = paste("Sync failed:", conditionMessage(e))))
    last <- res
    if (isTRUE(res$ok)) done <- c(done, name) else failed <- c(failed, name)
  }

  if (length(failed)) .sync$dirty <- union(.sync$dirty, failed)
  .sync$status <- if (length(failed)) {
    list(ok = FALSE, msg = paste0(last$msg, " (", length(failed), " queued for retry)"))
  } else {
    list(ok = TRUE, msg = paste(length(done), "table(s) synced at",
                                format(Sys.time(), "%H:%M:%S")))
  }
  invisible(done)
}

#' Pull every table from the repo. Called once at startup so a restarted
#' instance picks up what the last one saved instead of the seed.
gh_pull_all <- function() {
  if (!gh_enabled()) return(invisible(FALSE))
  cfg <- gh_config()
  n <- 0L
  for (name in TABLES) {
    if (name %in% SYNC_SKIP) next
    p <- tbl_path(name)
    if (isTRUE(tryCatch(gh_get_file(p, p, cfg), error = function(e) FALSE))) n <- n + 1L
  }
  # Whatever is cached was read before the pull replaced the files.
  if (n > 0L) store_refresh()
  .sync$status <- list(ok = TRUE, msg = paste(n, "table(s) pulled at",
                                              format(Sys.time(), "%H:%M:%S")))
  invisible(TRUE)
}
