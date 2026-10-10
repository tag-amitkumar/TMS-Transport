# ==================================================================
# Meta Embedded Signup — connecting the business's WhatsApp account.
#
# WHAT THIS IS, AND WHAT IT IS NOT
# --------------------------------
# This is Facebook Login used the one way it genuinely applies here: the
# BUSINESS signs in with Facebook and grants this app permission to send on
# its WhatsApp Business Account. Meta calls it Embedded Signup, and it is how
# every platform onboards a WABA without the operator hand-copying ids and
# tokens out of Business Manager.
#
# It is still not a way to reach customers. A consignee does not sign in to
# anything; they are messaged on the number written on the consignment note,
# with consent recorded. The two uses of "Facebook login" are easy to conflate
# and only one of them exists.
#
# THE FLOW
# --------
#   1. The operator clicks Connect. The Facebook JS SDK opens Meta's own
#      dialog, where they pick or create a WhatsApp Business Account and a
#      sender number. We never see their Facebook password.
#   2. The dialog returns an authorisation CODE to the browser, plus the
#      waba_id and phone_number_id it provisioned.
#   3. The code is exchanged for a business access token SERVER-SIDE, because
#      the exchange needs the app secret and a secret that reaches a browser
#      is not a secret.
#   4. The app subscribes to the WABA's webhooks and registers the number.
#
# WHAT IT NEEDS FIRST
# -------------------
# A Meta App of type Business with the WhatsApp product added, and an
# Embedded Signup configuration. From those: APP_ID, APP_SECRET, CONFIG_ID.
# The app must also list this deployment's domain, and for production use it
# needs App Review for whatsapp_business_management and
# whatsapp_business_messaging.
#
# WHERE THE TOKEN GOES
# --------------------
# Not into data/. That directory is committed and baked into the image, and a
# token in a commit is compromised the moment it is pushed. It goes to a file
# outside the repo with owner-only permissions, which meta_config() reads
# after the environment. Nothing ever renders it.
# ==================================================================

#' Where the obtained credentials live. Outside the repo, deliberately.
meta_cred_path <- function() {
  Sys.getenv("TMS_WA_CREDENTIALS", "~/.tms/wa_credentials.json")
}

#' App-level configuration — what is needed before anyone can click Connect.
meta_app_config <- function() {
  list(
    app_id      = Sys.getenv("TMS_META_APP_ID", ""),
    app_secret  = Sys.getenv("TMS_META_APP_SECRET", ""),
    config_id   = Sys.getenv("TMS_META_CONFIG_ID", ""),
    api_version = Sys.getenv("TMS_WA_API_VERSION", META_API_VERSION)
  )
}

#' TRUE when Embedded Signup can actually be offered.
#'
#' The app secret is required because the code exchange happens here, not in
#' the browser. Without it the button would open a dialog that cannot be
#' completed, which is worse than not offering it.
meta_signup_available <- function() {
  a <- meta_app_config()
  nzchar(a$app_id) && nzchar(a$app_secret) && nzchar(a$config_id)
}

#' Credentials obtained through signup, if any.
meta_saved_credentials <- function() {
  p <- path.expand(meta_cred_path())
  if (!file.exists(p)) return(NULL)
  tryCatch(jsonlite::fromJSON(p), error = function(e) NULL)
}

#' Persist them with owner-only permissions.
meta_save_credentials <- function(waba_id, phone_number_id, token,
                                  display_number = "", business_name = "") {
  p <- path.expand(meta_cred_path())
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(
    waba_id = waba_id, phone_number_id = phone_number_id, token = token,
    display_number = display_number, business_name = business_name,
    connected_dt = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ), p, auto_unbox = TRUE)
  # 0600 before anything else can read it. The default umask would leave a
  # long-lived token world-readable on a shared machine.
  Sys.chmod(p, "0600")
  invisible(TRUE)
}

#' Forget them. Used by Disconnect, and after a token is revoked at Meta.
meta_forget_credentials <- function() {
  p <- path.expand(meta_cred_path())
  if (file.exists(p)) unlink(p)
  invisible(TRUE)
}

#' Exchange the authorisation code for a business access token.
#'
#' Server-side only — the app secret must never reach a browser.
meta_exchange_code <- function(code, cfg = meta_app_config()) {
  if (!nzchar(code %||% "")) return(list(ok = FALSE, message = "No authorisation code returned."))
  if (!meta_signup_available())
    return(list(ok = FALSE, message = "Meta app is not configured on this deployment."))
  url <- sprintf("https://graph.facebook.com/%s/oauth/access_token", cfg$api_version)
  tryCatch({
    resp <- httr::GET(url, query = list(
      client_id = cfg$app_id, client_secret = cfg$app_secret, code = code),
      httr::timeout(30))
    body <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    if (httr::status_code(resp) >= 400 || is.null(body$access_token)) {
      return(list(ok = FALSE,
                  message = paste("Meta refused the code exchange:",
                                  body$error$message %||% "no token returned")))
    }
    list(ok = TRUE, token = body$access_token, message = "Token obtained")
  }, error = function(e)
    list(ok = FALSE, message = paste("Could not reach Meta:", conditionMessage(e))))
}

#' Subscribe this app to the WABA so delivery statuses arrive.
meta_subscribe_waba <- function(waba_id, token, cfg = meta_app_config()) {
  url <- sprintf("https://graph.facebook.com/%s/%s/subscribed_apps",
                 cfg$api_version, waba_id)
  tryCatch({
    resp <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", token)),
                       httr::timeout(30))
    list(ok = httr::status_code(resp) < 400,
         message = httr::content(resp, as = "text", encoding = "UTF-8"))
  }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
}

#' What the Settings screen shows about the connection.
#'
#' Never the token. Only whether one is held, and what it is for.
meta_connection_summary <- function() {
  cred <- meta_saved_credentials()
  env_configured <- meta_configured()
  list(
    connected      = env_configured,
    via            = if (!is.null(cred)) "Facebook (Embedded Signup)"
                     else if (env_configured) "Environment variables" else "",
    waba_id        = cred$waba_id %||% Sys.getenv("TMS_WA_BUSINESS_ACCOUNT_ID", ""),
    phone_number_id= cred$phone_number_id %||% Sys.getenv("TMS_WA_PHONE_NUMBER_ID", ""),
    display_number = cred$display_number %||% "",
    business_name  = cred$business_name %||% "",
    connected_dt   = cred$connected_dt %||% "",
    signup_offered = meta_signup_available()
  )
}

# ------------------------------------------------------------------
# Template submission
# ------------------------------------------------------------------

#' Submit one template to Meta for review.
#'
#' Meta requires an example for every variable: the reviewer reads the message
#' as a customer would, and a body full of {{1}} is rejected for being
#' unreviewable. The examples come from the template's own `vars` map, so the
#' thing a reviewer sees matches what the field actually carries.
#'
#' Returns the created id and status. A name already in the account comes back
#' as an error and is reported, not retried under another name — duplicate
#' near-identical templates are themselves a rejection reason.
meta_submit_template <- function(stage, cfg = meta_config()) {
  tpl <- template_for(stage)
  if (is.null(tpl)) return(list(ok = FALSE, message = paste("No template for", stage)))
  if (!meta_configured()) return(list(ok = FALSE, message = "WhatsApp is not connected."))

  # One plausible example per {{n}}, taken from what the variable means.
  n_vars <- length(gregexpr("\\{\\{[0-9]+\\}\\}", tpl$body)[[1]])
  n_vars <- if (n_vars == 1 && attr(gregexpr("\\{\\{[0-9]+\\}\\}", tpl$body)[[1]], "match.length")[1] == -1) 0 else n_vars
  labels <- strsplit(tpl$vars, "|", fixed = TRUE)[[1]]
  example <- vapply(seq_len(max(n_vars, 0)), function(i) {
    lbl <- tolower(sub("^[0-9]+=", "", labels[i] %||% ""))
    if (grepl("name", lbl))        "Ravi Kumar"
    else if (grepl("consignment", lbl)) "CN-00042"
    else if (grepl("vehicle", lbl))     "MH-31 GH 6612"
    else if (grepl("mobile", lbl))      "9876543210"
    else if (grepl("date", lbl))        "14 Oct 2026"
    else if (grepl("origin", lbl))      "Nagpur"
    else if (grepl("destination|dest", lbl)) "Kochi"
    else if (grepl("package", lbl))     "12"
    else if (grepl("reason", lbl))      "Vehicle breakdown en route"
    else if (grepl("branch phone|phone", lbl)) "0712 2345678"
    else if (grepl("received", lbl))    "Store supervisor"
    else "Sample"
  }, character(1))

  body <- list(
    name = tpl$meta_name,
    language = tpl$language,
    category = tpl$category,
    components = list(c(
      list(type = "BODY", text = tpl$body),
      if (length(example)) list(example = list(body_text = list(example))) else NULL
    ))
  )

  url <- sprintf("https://graph.facebook.com/%s/%s/message_templates",
                 cfg$api_version, cfg$waba_id)
  tryCatch({
    resp <- httr::POST(url,
      httr::add_headers(Authorization = paste("Bearer", cfg$token),
                        "Content-Type" = "application/json"),
      body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
      encode = "raw", httr::timeout(30))
    out <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    if (httr::status_code(resp) >= 400) {
      # error_user_msg is the one a human can act on; error.message is the
      # generic. "Invalid parameter" says nothing, while "English content is
      # being deleted, try again in under a minute" says exactly what to do.
      e <- out$error
      return(list(ok = FALSE,
                  message = e$error_user_msg %||% e$message %||% "rejected",
                  subcode = e$error_subcode %||% NA))
    }
    list(ok = TRUE, id = out$id, status = out$status %||% "PENDING",
         message = paste("submitted as", out$status %||% "PENDING"))
  }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
}

#' Pull template approval status back from Meta.
#'
#' Approval happens at Meta, on Meta's timetable, and nothing tells this app
#' when it lands. Without a sync the local status stays Pending for ever and
#' notify_stage() goes on refusing to send a template that was approved hours
#' ago — the most frustrating possible failure, because everything looks
#' correct from both ends.
#'
#' Returns what changed, so the operator sees it rather than having to compare.
meta_sync_templates <- function(cfg = meta_config()) {
  if (!meta_configured()) return(list(ok = FALSE, message = "WhatsApp is not connected."))
  url <- sprintf("https://graph.facebook.com/%s/%s/message_templates?limit=100&fields=name,status,rejected_reason",
                 cfg$api_version, cfg$waba_id)
  res <- tryCatch({
    r <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", cfg$token)),
                   httr::timeout(30))
    jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"))
  }, error = function(e) NULL)
  if (is.null(res) || is.null(res$data))
    return(list(ok = FALSE, message = "Could not read templates from Meta."))

  remote <- res$data
  local  <- store_get("message_templates")
  changed <- character()
  for (i in seq_len(nrow(local))) {
    hit <- remote[remote$name == local$meta_name[i], , drop = FALSE]
    if (!nrow(hit)) next
    # Meta's vocabulary is upper case; ours is title case, because it is read
    # on a settings screen rather than in a payload.
    new <- switch(toupper(hit$status[1]),
                  "APPROVED" = "Approved", "PENDING" = "Pending",
                  "REJECTED" = "Rejected", "PAUSED" = "Paused",
                  "DISABLED" = "Disabled", hit$status[1])
    if (!identical(new, local$status[i])) {
      store_update("message_templates", list(meta_name = local$meta_name[i]),
                   list(status = new))
      changed <- c(changed, sprintf("%s: %s -> %s", local$meta_name[i],
                                    local$status[i], new))
    }
  }
  list(ok = TRUE, changed = changed,
       message = if (length(changed)) paste(length(changed), "template(s) updated")
                 else "No change")
}
